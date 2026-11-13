#!/usr/bin/env python3
"""diet.py -- python twin of diet.pl; reads the same menus/*.pl fact files.

A day = breakfast + lunch + dinner, each at one venue.
Breakfast and lunch = starter + main. Dinner = starter + main + dessert.
Hard limits per day: BUDGET dollars, SUGAR_CAP grams, PROFILE kcal
(USDA 2020-2025, age ~25, moderately active).
Ranking: least cost first (cost matters most), then least sugar.

Run:  python3 diet.py [N]      # top N plans, default 5
"""
import re, sys, glob
from itertools import product

BUDGET    = 40.00
SUGAR_CAP = 60
PROFILE   = {"male_25": 2700, "female_25": 2100}
WHOAMI    = "male_25"

ITEM  = re.compile(r'item\((\w+),\s*(\w+),\s*(\w+),\s*([\d.]+),\s*(\d+),\s*(\d+)\)\.')
VENUE = re.compile(r'venue\((\w+),\s*"([^"]+)"\)\.')

def load(pattern="menus/*.pl"):
    venues, items = {}, {}
    for f in glob.glob(pattern):
        src = open(f).read()
        for v, name in VENUE.findall(src):
            venues[v] = name
        for v, item, course, price, sugar, cal in ITEM.findall(src):
            items.setdefault(v, {}).setdefault(course, []).append(
                (item, float(price), int(sugar), int(cal)))
    return venues, items

def best_meal(items, v, courses):
    """cheapest (then least sugar) combo of one item per course at venue v."""
    pools = [items.get(v, {}).get(c, []) for c in courses]
    if not all(pools):
        return None
    combos = ((sum(x[1] for x in xs), sum(x[2] for x in xs),
               sum(x[3] for x in xs), [x[0] for x in xs])
              for xs in product(*pools))
    return min(combos)            # (cost, sugar, cal, names)

def plans(venues, items):
    slot = {v: best_meal(items, v, ["starter", "main"]) for v in venues}
    din  = {v: best_meal(items, v, ["starter", "main", "dessert"]) for v in venues}
    cap  = PROFILE[WHOAMI]
    for b, l, d in product(venues, venues, venues):
        if not (b < l and d not in (b, l)):     # dedupe b/l swap; 3 venues
            continue
        B, L, D = slot[b], slot[l], din[d]
        if not (B and L and D):
            continue
        cost  = B[0] + L[0] + D[0]
        sugar = B[1] + L[1] + D[1]
        cal   = B[2] + L[2] + D[2]
        if cost <= BUDGET and sugar <= SUGAR_CAP and cal <= cap:
            yield cost, sugar, cal, (b, B), (l, L), (d, D)

def main(n=5):
    venues, items = load()
    found = sorted(plans(venues, items))      # ascending cost, then sugar
    print(f"{len(found)} plans fit budget ${BUDGET:.2f}, "
          f"sugar cap {SUGAR_CAP}g, {PROFILE[WHOAMI]} kcal ({WHOAMI}). Top {n}:\n")
    for cost, sugar, cal, *meals in found[:n]:
        print(f"=== total ${cost:.2f}, sugar {sugar}g, {cal} kcal ===")
        for slot_name, (v, (c, g, k, names)) in zip(
                ["breakfast", "lunch", "dinner"], meals):
            print(f"  {slot_name} at {venues[v]} (${c:.2f}, {g}g, {k}kcal): "
                  + ", then ".join(names))
        print()

if __name__ == "__main__":
    main(int(sys.argv[1]) if len(sys.argv) > 1 else 5)
