#!/usr/bin/env python3 -B
"""
bayeseg.py: Naive Bayes demos.
"""
from pathlib import Path
from core  import *
from bayes import *

egclass1 = str(Path.home() / "gits/moot/classify/soybean.csv")
egclass2 = str(Path.home() / "gits/moot/classify/diabetes.csv")

def test_h(): print(__doc__)

def test_classify(file=egclass1):
  """Test-then-train; print confusion table."""
  print(file)
  cf = classify(csv(file))
  table(confused(cf), w=7)

if __name__ == "__main__": cli()
