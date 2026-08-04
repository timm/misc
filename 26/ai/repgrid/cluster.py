"""FOCUS-style repertory grids: clustered heatmap, dendrograms above (constructs)
and right (elements). Grid 1 = provost constructs; Grid 1+ = augmented with two
elicited political constructs (marked †). Vector PDF out."""

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
    "pdf.fonttype": 42,
})

INK = "#333333"
MUTE = "#777777"
RAMP = ["#FDECEA", "#F5BDB6", "#E88A7E", "#CC5246", "#8F221A"]
CMAP = ListedColormap(RAMP)
NORM = BoundaryNorm([0.5, 1.5, 2.5, 3.5, 4.5, 5.5], CMAP.N)

G1_CONSTRUCTS = [
    "1=AI dosed early & often … 5=quarantined in electives",
    "1=fundamentals first … 5=tool-chasing",
    "1=small/open models … 5=frontier/GPU dependence",
    "1=industry projects … 5=lecture only",
    "1=open source … 5=closed/proprietary",
    "1=faculty upskilling … 5=none visible",
    "1=serves whole campus … 5=siloed to majors",
    "1=ships AI credentials … 5=stalled",
    "1=AI test/eval/security … 5=model-building only",
]
# IDEAL now clean all-1s (C8 judgement call removed)
G1 = {
    "IDEAL (Pfaendtner)": [1, 1, 1, 1, 1, 1, 1, 1, 1],
    "NCSU CSC":           [2, 2, 2, 1, 2, 2, 2, 2, 3],
    "Purdue":             [2, 1, 2, 2, 2, 2, 3, 2, 2],
    "Duke":               [2, 1, 2, 3, 2, 3, 1, 2, 3],
    "Georgia (UGA)":      [4, 1, 2, 2, 2, 1, 2, 2, 3],
    "Georgia Tech":       [4, 1, 3, 1, 2, 3, 1, 2, 3],
    "Michigan State":     [3, 1, 2, 1, 2, 3, 2, 3, 3],
    "Minnesota":          [4, 1, 2, 2, 2, 2, 1, 3, 3],
    "Virginia Tech":      [4, 1, 2, 2, 2, 2, 1, 3, 3],
    "Illinois":           [4, 1, 3, 2, 2, 2, 1, 3, 3],
    "Texas A&M":          [3, 1, 3, 2, 2, 2, 3, 3, 3],
    "Maryland":           [4, 1, 2, 3, 2, 2, 3, 2, 3],
    "Rutgers":            [4, 1, 2, 4, 2, 2, 2, 3, 3],
    "Arizona":            [2, 2, 3, 3, 2, 3, 2, 2, 4],
    "UC Davis":           [4, 1, 2, 2, 2, 3, 1, 4, 4],
}

# two constructs imported from triadic elicitation (grid 2), scored from dossiers
AUG_CONSTRUCTS = G1_CONSTRUCTS + [
    "1=ownership settled … 5=turf-locked †",
    "1=institutional AI bet … 5=no push †",
]
AUG_EXTRA = {   # (settled, bet)
    "IDEAL (Pfaendtner)": [1, 1],
    "NCSU CSC":           [5, 3],
    "Purdue":             [2, 1],
    "Duke":               [2, 2],
    "Georgia (UGA)":      [2, 1],
    "Georgia Tech":       [2, 1],
    "Michigan State":     [2, 3],
    "Minnesota":          [2, 2],
    "Virginia Tech":      [2, 2],
    "Illinois":           [2, 1],
    "Texas A&M":          [3, 2],
    "Maryland":           [2, 1],
    "Rutgers":            [3, 4],
    "Arizona":            [2, 3],
    "UC Davis":           [3, 3],
}
AUG = {k: G1[k] + AUG_EXTRA[k] for k in G1}


def focus_grid(data, constructs, title, outfile, bold_rows=("NCSU CSC",)):
    names = list(data.keys())
    X = np.array([data[n] for n in names], dtype=float)
    ideal = X[0]
    dists = np.abs(X - ideal).sum(axis=1).astype(int)

    zr = linkage(X, method="ward")
    zc = linkage(X.T, method="ward")

    nrow, ncol = X.shape
    cw = 0.30
    hm_w, hm_h = ncol * cw, nrow * cw
    left, bottom, top_d, right_d = 2.05, 1.75, 0.55, 0.80
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
    ax.set_xticklabels([constructs[c] for c in col_order],
                       rotation=40, ha="right", fontsize=7.2, color=INK,
                       rotation_mode="anchor")
    ax.set_yticks([10 * j + 5 for j in range(nrow)])
    labs = ax.set_yticklabels(
        [f"{names[r]}   d={dists[r]}" for r in row_order], fontsize=8.2, color=INK)
    for lab, r in zip(labs, row_order):
        if names[r] in bold_rows or r == 0:
            lab.set_fontweight("bold")
    ax.tick_params(length=0)
    for s in ax.spines.values():
        s.set_visible(False)

    ax_t.set_title(title, fontsize=11, fontweight="bold", color=INK, pad=4)
    fig.savefig(outfile, format="pdf")
    plt.close(fig)
    print("wrote", outfile)


focus_grid(G1, G1_CONSTRUCTS,
           "Grid 1 — the Provost’s constructs", "grid1-cluster.pdf")
focus_grid(AUG, AUG_CONSTRUCTS,
           "Grid 1+ — provost constructs + two elicited political constructs (†)",
           "grid1aug-cluster.pdf")
