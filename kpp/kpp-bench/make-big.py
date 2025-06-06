import random, csv
with open("big.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerow([f"x{i}" for i in range(20)] + ["y+"])
    for _ in range(100_000):
        row = [round(random.uniform(0, 1), 4) for _ in range(20)]
        row.append(round(random.uniform(0, 1), 4))
        writer.writerow(row)
