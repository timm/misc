# CLAUDE.md — guidance for Claude Code in this repo

## what this is
A conference talk built as a **Beamer** deck via pandoc + tectonic.
One source of truth: `talk.md`. Everything else is generated.
Never hand-edit `talk.pdf`.

## build
- `make fast` -> talk.pdf (one pass: pandoc -> beamer -> tectonic)
- `make all`  -> talk.pdf + talk-handout.pdf
- images cached in `img/` on first build; `make distclean` refetches.
Needs `pandoc` and `tectonic` on PATH. Don't add dependencies.

## slide format
- Pandoc markdown, `--slide-level=2`: every `##` starts a slide.
- YAML front-matter sets title/theme/colors. Leave it.
- Image-right layout: `:::: {.columns}` / `::: {.column width=..}` divs.
- Raw LaTeX allowed sparingly (`\vspace`, `\centering`, `\small`).

## style (hard rules)
- Terse. Verdict-first. Fragments fine.
- Lines <= 85 chars.
- One idea per slide. 3-6 bullets max. If a slide is dense, split it.
- Slides are cues, not prose — no paragraphs on a slide.
- Minimum words. Cut adjectives. Aphorisms ok.
- No bloat: no extra files, no theme deps, no frameworks.

## when editing
- Edit `talk.md` only. Keep the arc intact unless asked.
- Match the existing voice. Don't pad. Don't "improve" by adding words.
