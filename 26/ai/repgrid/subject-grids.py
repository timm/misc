"""FOCUS-style repertory grids over CSC subjects (not departments).
Elements = subjects cited in the memo appendices, plus IDEAL (Pfaendtner).
Constructs = the nine provost constructs of grid 1, reworded to subject level.
Scores are preliminary: catalog + memo appendix analysis, to be verified with
lecturers. High-res PNG out (../ugrad-grid.png, ../grad-grid.png)."""

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap, BoundaryNorm
from scipy.cluster.hierarchy import linkage, dendrogram

plt.rcParams.update({
    "font.family": "serif",
    "font.serif": ["Charter"],
    "font.size": 9,
})

INK = "#333333"
MUTE = "#777777"
SERED = "#C80000"
RAMP = ["#FDECEA", "#F5BDB6", "#E88A7E", "#CC5246", "#8F221A"]
CMAP = ListedColormap(RAMP)
NORM = BoundaryNorm([0.5, 1.5, 2.5, 3.5, 4.5, 5.5], CMAP.N)

# grid-1 constructs, reworded from department level to subject level
CONSTRUCTS = [
    "C1  1=AI in syllabus … 5=no AI",
    "C2  1=fundamentals … 5=tool-chasing",
    "C3  1=no GPU needed … 5=GPU dependent",
    "C4  1=graded by artifact … 5=talk only",
    "C5  1=open + reproducible … 5=closed",
    "C6  1=teaches next-tool skill … 5=fixed tools",
    "C7  1=open to whole college … 5=majors only",
    "C8  1=AI-era current … 5=predates genAI",
    "C9  1=AI test/eval/security … 5=none",
]

# SE subjects (red in the memo appendices)
SE_UG = {"216", "326", "404", "408", "415", "418", "421", "433", "491a", "491b"}
SE_GR = {"510", "515", "517", "518", "519", "521", "591b"}

#         C1 C2 C3 C4 C5 C6 C7 C8 C9
UGRAD = {
    "IDEAL (Pfaendtner)":        [1, 1, 1, 1, 1, 1, 1, 1, 1],
    "216 software dev. fund.":   [4, 1, 1, 1, 2, 3, 3, 3, 4],
    "226 discrete math":         [5, 1, 1, 3, 3, 4, 2, 2, 5],
    "230 C and tools":           [5, 1, 1, 1, 2, 2, 3, 2, 5],
    "246 operating systems":     [5, 1, 1, 2, 2, 3, 4, 3, 4],
    "316 data struct. + alg.":   [5, 1, 1, 2, 2, 3, 3, 2, 4],
    "326 software engineering":  [4, 1, 1, 1, 2, 2, 3, 3, 3],
    "333 automata":              [5, 1, 1, 3, 3, 4, 3, 2, 5],
    "366 algorithms":            [5, 1, 1, 3, 2, 3, 3, 2, 4],
    "379 ethics in computing":   [2, 2, 1, 4, 2, 3, 1, 2, 4],
    "404 sw testing (prop.)":    [2, 1, 1, 1, 2, 2, 3, 1, 2],
    "405 computer security":     [4, 1, 1, 2, 2, 2, 3, 2, 3],
    "408 product management":    [4, 1, 1, 1, 2, 2, 2, 2, 4],
    "415 software security":     [4, 1, 1, 2, 2, 2, 3, 3, 2],
    "418 analysis + design":     [4, 1, 1, 2, 2, 2, 3, 3, 4],
    "421 genAI for SE":          [1, 2, 2, 1, 2, 1, 3, 1, 2],
    "422 ALDA":                  [2, 1, 2, 2, 2, 3, 3, 3, 3],
    "425 neural networks":       [1, 2, 4, 2, 2, 3, 4, 3, 4],
    "433 privacy":               [3, 2, 1, 2, 2, 3, 2, 2, 2],
    "474 network security":      [4, 1, 1, 2, 2, 2, 4, 2, 3],
    "491a gurus (prop. 413)":    [2, 1, 1, 1, 1, 1, 3, 1, 3],
    "491b SE for AI (prop. 426)":[1, 1, 1, 1, 1, 1, 3, 1, 2],
}

# grad scores; 510 and 591b checked against the live Fall'26 repos
# (~/gits/txt/se26f, ~/gits/txt/seai26f): both have tool talks (C6),
# open course repos (C5), artifact-graded projects (C4); 510 has an
# aiForSE night and a maintain-foreign-code project.
#         C1 C2 C3 C4 C5 C6 C7 C8 C9
GRAD = {
    "IDEAL (Pfaendtner)":          [1, 1, 1, 1, 1, 1, 1, 1, 1],
    "501 operating systems":       [5, 1, 1, 2, 2, 3, 4, 3, 4],
    "505 algorithms":              [5, 1, 1, 3, 2, 3, 3, 2, 4],
    "510 software engineering":    [3, 1, 1, 1, 1, 1, 3, 2, 3],
    "512 compilers":               [5, 1, 1, 2, 2, 3, 4, 2, 5],
    "515 software security":       [4, 1, 1, 2, 2, 2, 4, 3, 2],
    "517 OO design":               [4, 1, 1, 1, 2, 2, 4, 3, 4],
    "518 analysis+design (prop.)": [3, 1, 1, 2, 2, 2, 4, 2, 4],
    "519 DevOps":                  [3, 1, 1, 1, 1, 2, 4, 2, 3],
    "521 genAI for SE (prop.)":    [1, 2, 2, 1, 2, 1, 4, 1, 2],
    "522 ALDA":                    [2, 1, 2, 2, 2, 3, 3, 3, 3],
    "528 trustworthy+efficient AI":[1, 2, 2, 2, 2, 3, 4, 1, 1],
    "533 privacy":                 [3, 2, 1, 2, 2, 3, 3, 2, 2],
    "534 human-centered security": [3, 2, 1, 2, 2, 3, 3, 2, 3],
    "537 attacks + defenses":      [3, 1, 2, 1, 2, 2, 4, 2, 2],
    "572 optimization":            [2, 1, 2, 3, 2, 3, 4, 2, 4],
    "591b SE for AI":              [1, 1, 1, 1, 1, 1, 3, 1, 2],
}


def se_key(name):
    return name.split()[0]


def focus_grid(data, se_set, title, outfile):
    names = list(data.keys())
    X = np.array([data[n] for n in names], dtype=float)
    ideal = X[0]
    dists = np.abs(X - ideal).sum(axis=1).astype(int)

    zr = linkage(X, method="ward")
    zc = linkage(X.T, method="ward")

    nrow, ncol = X.shape
    hm_w, hm_h = ncol * 0.34, nrow * 0.21
    left, bottom, top_d, right_d = 2.05, 1.85, 0.45, 0.80
    fw = left + hm_w + right_d + 0.15
    fh = bottom + hm_h + top_d + 0.30

    fig = plt.figure(figsize=(fw, fh))
    ax = fig.add_axes((left / fw, bottom / fh, hm_w / fw, hm_h / fh))
    ax_t = fig.add_axes((left / fw, (bottom + hm_h) / fh, hm_w / fw, top_d / fh), sharex=ax)
    ax_r = fig.add_axes(((left + hm_w) / fw, bottom / fh, right_d / fw, hm_h / fh), sharey=ax)

    with plt.rc_context({"lines.linewidth": 0.9}):
        dc = dendrogram(zc, ax=ax_t, orientation="top",
                        link_color_func=lambda _: MUTE, no_labels=True)
        dr = dendrogram(zr, ax=ax_r, orientation="right",
                        link_color_func=lambda _: MUTE, no_labels=True)
    for a in (ax_t, ax_r):
        a.set_axis_off()

    col_order = dc["leaves"]
    row_order = dr["leaves"]
    ax.imshow(X[np.ix_(row_order, col_order)], cmap=CMAP, norm=NORM,
              aspect="auto", origin="lower",
              extent=(0, 10 * ncol, 0, 10 * nrow), interpolation="nearest")
    for i in range(1, ncol):
        ax.axvline(10 * i, color="white", lw=1.4)
    for j in range(1, nrow):
        ax.axhline(10 * j, color="white", lw=1.4)

    for j, ri in enumerate(row_order):
        for i, ci in enumerate(col_order):
            v = int(X[ri, ci])
            ax.text(10 * i + 5, 10 * j + 5, str(v), ha="center", va="center",
                    fontsize=7.8, color="white" if v >= 4 else INK)

    ax.set_xticks([10 * i + 5 for i in range(ncol)])
    ax.set_xticklabels([CONSTRUCTS[c] for c in col_order],
                       rotation=40, ha="right", fontsize=7.2, color=INK,
                       rotation_mode="anchor")
    ax.set_yticks([10 * j + 5 for j in range(nrow)])
    labs = ax.set_yticklabels(
        [f"{names[r]}   d={dists[r]}" for r in row_order], fontsize=8.2, color=INK)
    for lab, r in zip(labs, row_order):
        if r == 0:
            lab.set_fontweight("bold")
        elif se_key(names[r]) in se_set:
            lab.set_color(SERED)
    ax.tick_params(length=0)
    for s in ax.spines.values():
        s.set_visible(False)

    ax_t.set_title(title, fontsize=11, fontweight="bold", color=INK, pad=4)
    fig.savefig(outfile, format="png", dpi=300)
    plt.close(fig)

    order = np.argsort(dists)
    for i in order:
        print(f"{dists[i]:3d}  {names[i]}")
    print("wrote", outfile)


if __name__ == "__main__":
    focus_grid(UGRAD, SE_UG,
               "Undergraduate subjects vs the Provost's constructs",
               "../ugrad-grid.png")
    if GRAD:
        focus_grid(GRAD, SE_GR,
                   "Graduate subjects vs the Provost's constructs",
                   "../grad-grid.png")
