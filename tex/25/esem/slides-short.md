
# 30-Minute Slide Deck (Short Version)

## Title
Industry Can Get Any Empirical Research It Wants  
Tim Menzies — NC State University  
PDF: http://timm.fyi/esem25.pdf

## 1. Origins & Context
- Early open-source culture (Portland 2004)
- Reproducibility nearly absent in SE research
- PROMISE founded to challenge “no one will share data”
- Contrast: MSR focused on collecting raw data; PROMISE focused on reusable curated datasets

## 2. Research Goal
Goal: Build **reliable, reproducible defect predictors** using static code attributes + pruning + open data.

Context:
- Bugs cluster strongly (Hamill & Goseva)
- Static metrics were controversial
- Needed empirical foundations + public datasets

## 3. 2007 Breakthrough (TSE’07)
Research Question:  
Can static attributes predict defect-prone modules?

Key Results:
- Pruning shows each project has different “best” metrics
- Combined weak signals outperform strong single metrics
- Multi-attribute learners beat classic 1990s methods
- Shared scripts + data → reproducible SE becomes mainstream

## 4. Nine Laws (Summary)
1. Different projects → different best metrics  
2. Weak signals combine into strong predictors  
3. Transfer learning surprisingly easy (Turkish toasters → satellites)  
4. Throw most data away (SE data highly redundant)  
5. Bigger ≠ better (simple models often win)  
6. Data quality matters less than expected  
7. Bad learners make good rankings (ranking > prediction)  
8. Science has “mud on the lens” (unstable conclusions)  
9. Many hard problems… aren’t (simple baselines competitive)

## 5. Industrial Impact
- Wan et al.: 90% practitioners open to adoption
- Telecom case: 87% defects predicted; 72% fewer inspections
- Samsung REMI: F1 ≈ 0.68
- Static analysis vs statistical methods: similar effectiveness, but predictors cheaper to adapt

## 6. Data Redundancy
Findings:
- Defect prediction: 97% of data ignorable
- Effort estimation: 91% ignorable
- Github issues: 80% ignorable
Interpretation:  
SE data live in low-dimensional manifolds → easy to model.

## 7. Transfer Learning Surprise
- Cross-project prediction works via nearest-neighbor matching
- Suggests software systems share structural regularities
- Reduces need for large domain-specific datasets

## 8. Modern Directions (2025)
- Interpretability (XAI for defect prediction)
- Multi-objective optimization
- Minimal-data active learning
- Semi-supervised learning
- Landscape analysis

## 9. Lessons for SE Community
- Use simple baselines before complex ones  
- Reproducibility needs open datasets + scripts  
- Evaluate stability: hyperparameters can flip conclusions  
- Ranking may matter more than accuracy  
- Focus on *how little data is enough*  

## 10. Challenge to Researchers
- Are we using the right metrics?
- Are our conclusions stable?
- Are our datasets modern enough?
- Are simpler methods being ignored?

## 11. References (Short List)
- Menzies et al. “Data Mining Static Code Attributes.” TSE 2007.  
- Menzies. “Retrospective: Data Mining Static Code Attributes.” TSE 2025.  
- Ostrand, Weyuker, Bell. “Where the Bugs Are.”  
- Hamill & Goseva-Popstojanova. Fault Trends in SE. TSE 2009.  
- Wan et al. Defect Prediction Perceptions. TSE 2018.  
- Rahman et al. Static Analysis vs Predictors. ICSE 2014.  
- Turhan et al. Cross-Project Defect Prediction.  

## End
Full PDF: http://timm.fyi/esem25.pdf
