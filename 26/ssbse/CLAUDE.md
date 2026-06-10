# CLAUDE.md — guidance for Claude Code in this repo

## what this is
A conference talk built as a **Marp** deck. One source of truth: `talk.md`.
Everything else is generated. Never hand-edit `talk.pdf/.pptx/.html`.

## build
- `make html`  -> talk.html (fast, no Chrome)
- `make pdf`   -> talk.pdf  (needs Chrome/Chromium)
- `make pptx`  -> talk.pptx (needs Chrome/Chromium)
- `make watch` -> live preview at localhost
`npx` pulls marp-cli on demand. No global installs. Don't add dependencies.

## slide format
- Slides separated by a line containing only `---`.
- Front-matter at top sets `marp: true`, theme, paginate. Leave it.
- `#`/`##` = headline. Bullets below, sparse.

## style (hard rules)
- Terse. Verdict-first. Fragments fine.
- Lines <= 85 chars.
- One idea per slide. 3-6 bullets max. If a slide is dense, split it.
- Slides are cues, not prose — no paragraphs on a slide.
- Code fences 1-10 lines only.
- Minimum words. Cut adjectives. Aphorisms ok.
- No bloat: no extra files, no theme deps, no frameworks.

## when editing
- Edit `talk.md` only. Keep the arc intact unless asked.
- Match the existing voice. Don't pad. Don't "improve" by adding words.
