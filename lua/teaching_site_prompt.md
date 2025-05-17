# 🧠 Prompt: Generate a Fully Populated Teaching Website from a Python Script

Create a teaching website from a Python file (e.g., `kube.py`) plus optional `README.md` and `LICENSE`.

Site must **teach the code** through concepts, examples, and walkthroughs.  
**No placeholders. Everything must be filled in.**

---

## ✅ Global Rules

- **Prepend** the first paragraph of `README.md` (as HTML) to **every page**
- **Convert all Markdown to HTML**
- **Minimize LOC** in all output files — **tight prose, dense with meaning**
- Use **syntax-highlighted code**, **compact tables**, and **clear lists**
- All links must be **relative** and **offline-ready**

---

## 🧭 `index.html`

- Built from:
  - The module-level docstring of the `.py` file
  - Full contents of `README.md`
- End section: render `## Refs` as `<h3 id="author2023">Author,2023</h3>`
- Link in-text citations like `Author,2023` → `index.html#author2023`

### ➕ Install Instructions (insert near top)

Include this block:

```html
<h2>Installation</h2>
<p>This code runs with Python 3. No external dependencies required.</p>
<p>Sample data can be downloaded from <a href="https://github.com/timm/moot/tree/main/optimize">https://github.com/timm/moot/tree/main/optimize</a></p>
```

---

## 🎨 `style.css`

- Clean, minimal styling
- Support for headers, inline code, tables, and `<pre>`

---

## 🧱 `classes.html`

- Reflect on each class
- Show:
  - All attributes (including private, e.g. `_rows`)
  - Default values (if set)
  - Short, paraphrased purpose
- Group methods into **protocols** (e.g. `core`, `stats`, `distance`, `projection`)
  - Reuse protocol names across classes when possible
- Each method: one-line summary

---

## 📚 `theory.html`

- For each `eg__*` function:
  - Extract any concept a beginner might not know (e.g., entropy, memoization)
  - For each concept:
    - Linkable heading
    - Short definition
    - Motivating example
    - Code snippet
    - “Why it matters” (1 line)
- Add:
  - TOC at the top
  - 5–10 **review questions** at the end

---

## 🧪 `tut.html`

- For each `eg__*` function:
  - Walk through the code in commented chunks
  - Explain each step clearly and concisely
  - Link to relevant theory concepts
  - Add two practice tasks:
    - ✅ **Simple (1–10 min)** task
    - 🧠 **Harder (assignment-level)** task

---

## ⚖️ `license.html`

- Built from `LICENSE`
- Prepend first paragraph from `README.md` (as HTML)

---

## 📦 Output

- Must generate:
  - `index.html`, `classes.html`, `theory.html`, `tut.html`, `license.html`, `style.css`
- All files written in the **current directory**
- Output must be created by a **single shell script `here.sh`** using here-docs
- No external web requests required

---

## ❗️ Final Note

**Every page must be 100% complete.**  
No `[To be generated]`, empty sections, or skeletons.  
This is a **ready-to-use teaching artifact**.
