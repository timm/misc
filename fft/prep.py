#!/usr/bin/env python3 -B
"""prep.py, convert raw fairness CSVs to moot format (4 datasets).
- keep only listed columns (drops leaky/id cols)
- numeric cols -> Uppercase-first (fft1 Num); label -> 'klass!' 0/1
- optional numeric age -> symbolic 'agegrp' (terciles)"""
def isnum(xs):
  n = ok = 0
  for x in xs[:800]:
    n += 1
    try: float(x); ok += 1
    except: pass
  return ok > 0.95*n

def prep(inf, outf, label, positive, keep, age=None):
  rows = [[c.strip().strip('"') for c in ln.split(",")]
          for ln in open(inf) if ln.strip()]
  head, body = rows[0], rows[1:]
  idx = {h: j for j, h in enumerate(head)}
  cols = list(zip(*body))
  agi  = idx[age] if age else None
  if agi is not None:
    av = sorted(float(v) for v in cols[agi])
    q1, q2 = av[len(av)//3], av[2*len(av)//3]
    agrp = lambda v: "lo" if float(v)<q1 else "mid" if float(v)<q2 else "hi"
  out_cols = [h for h in keep]                       # column order to emit
  nh = []
  for h in out_cols:
    if h == label:        nh.append("klass!")
    elif isnum(cols[idx[h]]): nh.append(h[0].upper()+h[1:])
    else:                 nh.append(h.lower())
  if age: nh.append("agegrp")
  with open(outf, "w") as f:
    f.write(",".join(nh)+"\n")
    for r in body:
      vals = [("1" if r[idx[h]] == positive else "0") if h == label
              else r[idx[h]] for h in out_cols]
      if age: vals.append(agrp(r[agi]))
      f.write(",".join(vals)+"\n")
  print("wrote %s  (%d rows, label %s=%s+)" % (outf, len(body), label, positive))

if __name__ == "__main__":
  H = "/Users/timm/tmp/"
  # adult: clean, age->agegrp ; protected gender/race/agegrp
  prep(H+"adultc.csv", H+"adult.csv", "Class-label", "1",
       keep=["age","workclass","education","educational-num","marital-status",
             "occupation","relationship","race","gender","hours-per-week",
             "capital-gain","capital-loss","Class-label"], age="age")
  # dutch: age->agegrp ; protected sex/country_birth/agegrp
  prep(H+"dutch.csv", H+"dutchf.csv", "occupation", "1",
       keep=["sex","age","household_position","household_size","citizenship",
             "country_birth","edu_level","economic_status","marital_status",
             "occupation"], age="age")
  # compas: keep pre-decision only (drop decile_score/is_recid/etc leaks)
  prep(H+"compasC.csv", H+"compas.csv", "two_year_recid", "1",
       keep=["age","priors_count","juv_fel_count","juv_misd_count",
             "juv_other_count","sex","race","age_cat","c_charge_degree",
             "two_year_recid"])
  # diabetes: protected race/gender/age (age already categorical bracket)
  prep(H+"diabetes-clean.csv", H+"diab.csv", "readmitted", "<30",
       keep=["race","gender","age","time_in_hospital","num_lab_procedures",
             "num_procedures","num_medications","number_diagnoses",
             "number_inpatient","number_emergency","readmitted"])
