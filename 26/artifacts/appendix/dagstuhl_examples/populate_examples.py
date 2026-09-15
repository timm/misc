#!/usr/bin/env python3
"""Populate ``examples.json`` from ReproDB artifact-evaluation data.

ReproDB (https://reprodb.github.io) scrapes the sysartifacts / secartifacts
results pages and publishes one normalised record per evaluated artifact.
This script selects a subset of those records and rewrites them into the
flatter "paper / conference / artifact / tar / repository" shape that the
Triple-A example collection uses.

Only the Python standard library is used, so the script runs anywhere.

Besides writing the JSON index, the script mirrors every example into
``artifacts/<example-id>/``:

* ``zenodo/``   — every file of the Zenodo deposit
* ``repo/``     — the GitHub repository source at the evaluated ref
* ``appendix/`` — the artifact appendix, for venues that publish it separately

Venues differ in how the appendix is published: EuroSys 2022 prints it inside
the paper, other editions link a standalone PDF. Both cases are handled — see
``appendix_form`` in each entry.

Only the Zenodo archives are versioned; ``repo/`` is gitignored because a single
artifact can vendor thousands of files. After a fresh clone, rebuild the mirror
from the checked-in index with ``--restore``.

Examples
--------
Regenerate the checked-in EuroSys 2022 examples::

    python3 populate_examples.py --venue EUROSYS --year 2022 \
        --title Virtines --title SafePM --title VMSH

Re-download everything listed in examples.json (after a clone)::

    python3 populate_examples.py --restore

Collect every reproduced USENIX Security 2023 artifact::

    python3 populate_examples.py --venue USENIXSEC --year 2023 \
        --badge reproduced --output usenixsec2023.json
"""

from __future__ import annotations

import argparse
import hashlib
import json
import logging
import re
import shutil
import sys
import tarfile
import tempfile
import urllib.error
import urllib.parse
import urllib.request
from datetime import date
from pathlib import Path
from typing import Any, Iterable

# --------------------------------------------------------------------------- #
# Configuration
# --------------------------------------------------------------------------- #

REPRODB_ARTIFACTS_URL = "https://reprodb.github.io/assets/data/artifacts.json"
ZENODO_RECORD_API = "https://zenodo.org/api/records/{record_id}"
GITHUB_ARCHIVE_URL = "https://codeload.github.com/{owner}/{repo}/tar.gz/{ref}"

USER_AGENT = "triple-a-examples/1.0 (+https://github.com/ReproDB)"
HTTP_TIMEOUT_SECONDS = 30

# ReproDB stores venues upper-cased; these are the human-readable spellings.
VENUE_DISPLAY_NAMES = {
    "ATC": "USENIX ATC",
    "EUROSYS": "EuroSys",
    "OSDI": "OSDI",
    "SOSP": "SOSP",
    "SP": "IEEE S&P",
    "USENIXSEC": "USENIX Security",
}

# Sub-directories created below artifacts/<example-id>/.
ZENODO_SUBDIR = "zenodo"
REPOSITORY_SUBDIR = "repo"
APPENDIX_SUBDIR = "appendix"

# Fields ReproDB cannot supply; they are curated by hand directly in
# examples.json and carried over whenever the file is regenerated.
MANUAL_FIELDS = (
    ("paper", "local_pdf"),
    ("paper", "local_appendix"),
    ("repository", "url"),
    ("repository", "ref"),
)

DOWNLOAD_CHUNK_BYTES = 1 << 20

log = logging.getLogger("populate_examples")


# --------------------------------------------------------------------------- #
# HTTP helpers
# --------------------------------------------------------------------------- #


def fetch_json(url: str) -> Any:
    """Download ``url`` and parse it as JSON."""
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(request, timeout=HTTP_TIMEOUT_SECONDS) as response:
        return json.load(response)


def load_reprodb_artifacts(source: str) -> list[dict[str, Any]]:
    """Load ReproDB records from a URL or a local file path."""
    if source.startswith(("http://", "https://")):
        log.info("Downloading ReproDB artifacts from %s", source)
        return fetch_json(source)

    log.info("Reading ReproDB artifacts from %s", source)
    return json.loads(Path(source).read_text(encoding="utf-8"))


# --------------------------------------------------------------------------- #
# Record selection
# --------------------------------------------------------------------------- #


def matches_filters(
    record: dict[str, Any],
    venue: str | None,
    year: int | None,
    title_patterns: list[str],
    required_badges: list[str],
) -> bool:
    """Return True when a ReproDB record passes every active filter."""
    if venue and record.get("conference", "").upper() != venue.upper():
        return False

    if year is not None and record.get("year") != year:
        return False

    if required_badges:
        badges = {badge.lower() for badge in record.get("badges", [])}
        if not badges.issuperset(badge.lower() for badge in required_badges):
            return False

    if title_patterns:
        title = record.get("title", "").lower()
        if not any(pattern.lower() in title for pattern in title_patterns):
            return False

    return True


# --------------------------------------------------------------------------- #
# Field extraction
# --------------------------------------------------------------------------- #


def normalise_title(title: str) -> str:
    """Collapse whitespace; ReproDB titles sometimes contain stray newlines."""
    return " ".join(title.split())


def make_example_id(record: dict[str, Any]) -> str:
    """Build a stable id such as ``eurosys2022-safepm``.

    The slug uses the part of the title before the first colon, which for
    systems papers is almost always the tool or system name.
    """
    title = normalise_title(record.get("title", ""))
    lead = title.split(":", 1)[0]
    slug = re.sub(r"[^a-z0-9]+", "-", lead.lower()).strip("-")
    venue = record.get("conference", "unknown").lower()
    return f"{venue}{record.get('year', '')}-{slug}"


def pick_repository_url(artifact_urls: Iterable[str]) -> str | None:
    """Return the first GitHub/GitLab repository URL in the artifact links."""
    for url in artifact_urls:
        if re.match(r"https?://(www\.)?(github|gitlab)\.com/[^/]+/[^/]+", url):
            return url.rstrip("/")
    return None


def extract_zenodo_record_id(record: dict[str, Any]) -> str | None:
    """Find the numeric Zenodo record id referenced by a ReproDB record."""
    candidates = [record.get("doi") or ""] + list(record.get("artifact_urls", []))
    for candidate in candidates:
        match = re.search(r"zenodo(?:\.org/records?/|\.)(\d+)", candidate)
        if match:
            return match.group(1)
    return None


def list_zenodo_files(files: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Describe every file of a Zenodo deposit, not just the main archive."""
    return [
        {
            "filename": entry.get("key"),
            "url": entry.get("links", {}).get("self"),
            "size_bytes": entry.get("size"),
        }
        for entry in files
    ]


def build_artifact_section(record: dict[str, Any], resolve_files: bool) -> dict[str, Any]:
    """Assemble the ``artifact`` block, optionally querying the Zenodo API."""
    record_id = extract_zenodo_record_id(record)
    artifact: dict[str, Any] = {
        "landing_page": f"https://zenodo.org/records/{record_id}" if record_id else None,
        "doi": record.get("doi") or None,
        "files": [],
    }

    if not (record_id and resolve_files):
        return artifact

    try:
        deposit = fetch_json(ZENODO_RECORD_API.format(record_id=record_id))
    except (urllib.error.URLError, json.JSONDecodeError, TimeoutError) as error:
        log.warning("Could not resolve Zenodo record %s: %s", record_id, error)
        return artifact

    artifact["files"] = list_zenodo_files(deposit.get("files", []))
    return artifact


def appendix_form(record: dict[str, Any]) -> str:
    """Say whether the artifact appendix is a separate file or part of the paper.

    Venues differ: EuroSys 2022 prints the appendix inside the paper, while
    other editions publish it as its own PDF that ReproDB links separately.
    """
    return "separate-file" if record.get("appendix_url") else "in-paper-or-unknown"


def build_example(record: dict[str, Any], resolve_files: bool) -> dict[str, Any]:
    """Convert one ReproDB record into a Triple-A example entry."""
    venue = record.get("conference", "")
    repository_url = pick_repository_url(record.get("artifact_urls", []))

    return {
        "id": make_example_id(record),
        "title": normalise_title(record.get("title", "")),
        "conference": {
            "name": VENUE_DISPLAY_NAMES.get(venue, venue),
            "year": record.get("year"),
            "category": record.get("category"),
        },
        "paper": {
            "doi": (record.get("paper_url") or "").replace("https://doi.org/", "") or None,
            "url": record.get("paper_url"),
            "local_pdf": None,  # curated by hand; publisher PDFs are not downloadable
            "appendix_url": record.get("appendix_url"),
            "appendix_form": appendix_form(record),
            "local_appendix": None,
        },
        "evaluation": {
            "badges": record.get("badges", []),
            "award": record.get("award"),
        },
        "artifact": build_artifact_section(record, resolve_files),
        "repository": {
            "url": repository_url,
            "ref": None,  # ReproDB does not record the evaluated tag/branch
            "tree_url": repository_url,
            "source_url": None,
            "local_path": None,
        },
    }


# --------------------------------------------------------------------------- #
# Hand-curated fields
# --------------------------------------------------------------------------- #


def read_curated_fields(path: Path) -> dict[str, dict[tuple[str, str], Any]]:
    """Collect hand-maintained values from a previously generated examples file.

    Regenerating from ReproDB would otherwise drop information that only a
    human knows, such as the evaluated git tag or the local PDF filename.
    """
    if not path.exists():
        return {}

    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as error:
        log.warning("Ignoring unreadable %s: %s", path, error)
        return {}

    curated: dict[str, dict[tuple[str, str], Any]] = {}
    for example in document.get("examples", []):
        values = {
            (section, field): example[section][field]
            for section, field in MANUAL_FIELDS
            if example.get(section, {}).get(field)
        }
        if values:
            curated[example.get("id", "")] = values
    return curated


def apply_curated_fields(
    example: dict[str, Any], curated: dict[str, dict[tuple[str, str], Any]]
) -> dict[str, Any]:
    """Restore hand-maintained values and keep ``tree_url`` consistent."""
    for (section, field), value in curated.get(example["id"], {}).items():
        example[section][field] = value

    repository = example["repository"]
    url, ref = repository.get("url"), repository.get("ref")
    repository["tree_url"] = f"{url}/tree/{ref}" if url and ref else url

    return example


# --------------------------------------------------------------------------- #
# Local copies of papers and artifacts
# --------------------------------------------------------------------------- #


def download_file(url: str, destination: Path) -> str:
    """Stream ``url`` to ``destination`` and return the SHA-256 of the content."""
    destination.parent.mkdir(parents=True, exist_ok=True)
    partial = destination.with_name(destination.name + ".part")
    digest = hashlib.sha256()

    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(request, timeout=HTTP_TIMEOUT_SECONDS) as response:
        with partial.open("wb") as handle:
            for chunk in iter(lambda: response.read(DOWNLOAD_CHUNK_BYTES), b""):
                handle.write(chunk)
                digest.update(chunk)

    partial.replace(destination)
    return digest.hexdigest()


def file_sha256(path: Path) -> str:
    """Compute the SHA-256 of an already present file."""
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(DOWNLOAD_CHUNK_BYTES), b""):
            digest.update(chunk)
    return digest.hexdigest()


def store_zenodo_files(example: dict[str, Any], base_dir: Path, download_dir: Path) -> None:
    """Mirror the whole Zenodo deposit and record where each file landed."""
    files = example.get("artifact", {}).get("files") or []
    if not files:
        log.warning("No Zenodo files listed for %s", example["id"])
        return

    destination_dir = download_dir / example["id"] / ZENODO_SUBDIR
    for entry in files:
        if not entry.get("url"):
            continue

        # Zenodo keys may contain slashes ("virtines/wasp-v1.0.1.zip"); keep the leaf.
        target = destination_dir / Path(entry["filename"]).name

        if target.exists():
            log.info("Already present: %s", target.name)
            checksum = file_sha256(target)
            if entry.get("sha256") and entry["sha256"] != checksum:
                log.warning("Checksum mismatch for %s; the local copy differs", target.name)
        else:
            log.info("Downloading %s", entry["url"])
            try:
                checksum = download_file(entry["url"], target)
            except (urllib.error.URLError, TimeoutError, OSError) as error:
                log.warning("Download failed for %s: %s", entry["filename"], error)
                continue

        entry["local_path"] = target.relative_to(base_dir).as_posix()
        entry["sha256"] = checksum


def repository_archive_url(url: str, ref: str | None) -> str | None:
    """Build the source-tarball URL for a GitHub repository."""
    match = re.match(r"https?://(?:www\.)?github\.com/([^/]+)/([^/]+)", url)
    if not match:
        return None

    owner, repo = match.group(1), match.group(2).removesuffix(".git")
    return GITHUB_ARCHIVE_URL.format(owner=owner, repo=repo, ref=ref or "HEAD")


def extract_tarball(archive: Path, destination: Path) -> None:
    """Extract a source tarball, dropping its single top-level directory."""
    destination.parent.mkdir(parents=True, exist_ok=True)

    with tempfile.TemporaryDirectory(dir=destination.parent) as staging:
        with tarfile.open(archive) as tar:
            tar.extractall(staging, filter="data")  # refuses absolute/escaping paths

        entries = list(Path(staging).iterdir())
        root = entries[0] if len(entries) == 1 and entries[0].is_dir() else Path(staging)
        root.rename(destination)


def fetch_repository_source(example: dict[str, Any], base_dir: Path, download_dir: Path) -> None:
    """Download the repository source at the evaluated ref into its own folder.

    A source tarball is used rather than ``git clone`` so the checkout carries
    no nested ``.git`` directory and no git proxy configuration is required.
    """
    repository = example["repository"]
    url = repository.get("url")
    if not url:
        log.warning("No repository URL for %s", example["id"])
        return

    target = download_dir / example["id"] / REPOSITORY_SUBDIR
    if target.exists():
        log.info("Repository source already present: %s", target)
        repository["local_path"] = target.relative_to(base_dir).as_posix()
        return

    ref = repository.get("ref")
    archive_url = repository_archive_url(url, ref)
    if not archive_url:
        log.warning("Only GitHub repositories can be mirrored; skipping %s", url)
        return

    archive_path = target.with_name("repo-source.tar.gz")
    log.info("Downloading repository source %s", archive_url)
    try:
        download_file(archive_url, archive_path)
        extract_tarball(archive_path, target)
    except (urllib.error.URLError, TimeoutError, OSError, tarfile.TarError) as error:
        log.warning("Repository download failed for %s: %s", url, error)
        shutil.rmtree(target, ignore_errors=True)
        return
    finally:
        archive_path.unlink(missing_ok=True)

    repository["source_url"] = archive_url
    repository["local_path"] = target.relative_to(base_dir).as_posix()


def store_appendix(example: dict[str, Any], base_dir: Path, download_dir: Path) -> None:
    """Download the artifact appendix when the venue publishes it separately."""
    paper = example["paper"]
    appendix_url = paper.get("appendix_url")
    if not appendix_url:
        return

    filename = Path(urllib.parse.urlparse(appendix_url).path).name or "appendix.pdf"
    target = download_dir / example["id"] / APPENDIX_SUBDIR / filename

    if not target.exists():
        log.info("Downloading appendix %s", appendix_url)
        try:
            download_file(appendix_url, target)
        except (urllib.error.URLError, TimeoutError, OSError) as error:
            log.warning("Appendix download failed for %s: %s", example["id"], error)
            return

    paper["local_appendix"] = target.relative_to(base_dir).as_posix()


def verify_paper_pdf(example: dict[str, Any], base_dir: Path) -> None:
    """Warn when the paper or its appendix is not actually present locally."""
    paper = example["paper"]

    local_pdf = paper.get("local_pdf")
    if not local_pdf:
        log.warning("No local PDF recorded for %s (%s)", example["id"], paper["url"])
    elif not (base_dir / local_pdf).exists():
        log.warning("Recorded PDF is missing on disk: %s", local_pdf)

    # Without a separate appendix the material is expected inside the paper itself.
    if paper.get("appendix_form") == "in-paper-or-unknown":
        log.info("%s: no separate appendix; expected within the paper", example["id"])


# --------------------------------------------------------------------------- #
# Entry point
# --------------------------------------------------------------------------- #


def parse_arguments(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate examples.json from ReproDB artifact-evaluation data.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--source",
        default=REPRODB_ARTIFACTS_URL,
        help="ReproDB artifacts.json URL or local file path.",
    )
    parser.add_argument("--venue", help="ReproDB venue key, e.g. EUROSYS, USENIXSEC, SOSP.")
    parser.add_argument("--year", type=int, help="Conference year to select.")
    parser.add_argument(
        "--title",
        action="append",
        default=[],
        dest="titles",
        help="Case-insensitive title substring; repeat for several papers.",
    )
    parser.add_argument(
        "--badge",
        action="append",
        default=[],
        dest="badges",
        help="Require this badge (available/functional/reproduced); repeatable.",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path(__file__).with_name("examples.json"),
        help="Destination file ('-' writes to stdout).",
    )
    parser.add_argument(
        "--download-dir",
        type=Path,
        default=Path(__file__).with_name("artifacts"),
        help="Directory holding one <example-id>/ folder per example.",
    )
    parser.add_argument(
        "--no-download",
        action="store_true",
        help="Only refresh the JSON index; do not fetch files or clone repositories.",
    )
    parser.add_argument(
        "--no-zenodo",
        action="store_true",
        help="Skip Zenodo API calls; the deposit file list is left empty.",
    )
    parser.add_argument(
        "--restore",
        action="store_true",
        help="Re-download the files of an existing examples.json; no ReproDB query.",
    )
    parser.add_argument("--log-level", default="info", help="debug, info, warning or error.")
    return parser.parse_args(argv)


def select_examples(args: argparse.Namespace) -> tuple[dict[str, Any], list[dict[str, Any]]] | None:
    """Return the output document and its examples, either restored or rebuilt."""
    if args.restore:
        if not args.output.exists():
            log.error("Nothing to restore: %s does not exist.", args.output)
            return None

        document = json.loads(args.output.read_text(encoding="utf-8"))
        log.info("Restoring %d examples from %s", len(document.get("examples", [])), args.output)
        return document, document.get("examples", [])

    records = load_reprodb_artifacts(args.source)
    selected = [
        record
        for record in records
        if matches_filters(record, args.venue, args.year, args.titles, args.badges)
    ]

    if not selected:
        log.error("No ReproDB record matched the given filters.")
        return None

    log.info("Selected %d of %d ReproDB records", len(selected), len(records))

    curated = read_curated_fields(args.output)
    examples = [
        apply_curated_fields(build_example(record, resolve_files=not args.no_zenodo), curated)
        for record in selected
    ]
    examples.sort(key=lambda example: example["id"])

    document = {
        "schema_version": "1.0",
        "description": (
            "Example papers with linked artifact packages, used as reference "
            "material for Triple-A (Agentic Artifact Appendices)."
        ),
        "sources": {
            "reprodb_artifacts": args.source,
        },
        "generated_by": Path(__file__).name,
        "generated_at": date.today().isoformat(),
        "examples": examples,
    }
    return document, examples


def main(argv: list[str] | None = None) -> int:
    args = parse_arguments(argv)
    logging.basicConfig(level=args.log_level.upper(), format="%(levelname)s: %(message)s")

    result = select_examples(args)
    if result is None:
        return 1
    document, examples = result

    # Papers and artifacts live next to the index file so the repo is self-contained.
    base_dir = args.output.parent if str(args.output) != "-" else Path(__file__).parent
    for example in examples:
        verify_paper_pdf(example, base_dir)
        if not args.no_download:
            store_appendix(example, base_dir, args.download_dir)
            store_zenodo_files(example, base_dir, args.download_dir)
            fetch_repository_source(example, base_dir, args.download_dir)

    document["examples"] = examples

    serialised = json.dumps(document, indent=2, ensure_ascii=False) + "\n"
    if str(args.output) == "-":
        sys.stdout.write(serialised)
    else:
        args.output.write_text(serialised, encoding="utf-8")
        log.info("Wrote %d examples to %s", len(examples), args.output)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
