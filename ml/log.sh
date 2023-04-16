# # PAUL's scores

pauborr(){
gawk -F, '{print $(NF-1),$NF}' paul.csv | sort | uniq -c
}
#  53 1.0 best
#  17 2.0 rest
#  22 3.0 rest
#  16 4.0 rest
#   4 5.0 rest
#   1 paul_score class
#
# 
#   weka.attributeSelection.WrapperSubsetEval -B weka.classifiers.bayes.NaiveBayes -F 5 -T 0.01 -R 1 -E DEFAULT --
#   Search:       weka.attributeSelection.BestFirst -D 1 -N 5
# 
#       number of folds (%)  attribute
#            2( 20 %)    1 monthly_PR_comments
#            5( 50 %)    2 monthly_PR_mergers
#            4( 40 %)    3 monthly_closed_PRs
#            4( 40 %)    4 monthly_closed_issues
#            1( 10 %)    5 monthly_commits
#           10(100 %)    6 monthly_contributors
#             0(  0 %)    7 monthly_forks
#            0(  0 %)    8 monthly_issue_comments
#            0(  0 %)    9 monthly_merged_PRs
#            2( 20 %)   10 monthly_open_PRs
#            1( 10 %)   11 monthly_open_issues
#            3( 30 %)   12 monthly_stargazer
#            1( 10 %)   13 monthly_watchers

# so running NB on just monthy contributors... ten way cross -valy
# === Detailed Accuracy By Class ===
# 
#                  TP Rate  FP Rate  Precision  Recall   F-Measure  MCC      ROC Area  PRC Area  Class
#                  0.340    0.102    0.750      0.340    0.468      0.290    0.614     0.667     best
#                  0.898    0.660    0.602      0.898    0.721      0.290    0.614     0.589     rest
# Weighted Avg.    0.634    0.396    0.672      0.634    0.601      0.290    0.614     0.626
# 
# === Confusion Matrix ===
# 
#   a  b   <-- classified as
#  18 35 |  a = best
#   6 53 |  b = rest
# 
#we are good at recongizing rest and of all the things we say are best 18/14 are best
#prec(best) = 75%
#reccall(rest) = 89%

#
paulplot() {
	gawk -F, '  { c=int($8); c=c>4?"5+":c; $NF=="best"? b[c]++: r[c]++}
         END{ 
	 print("monthlycontributrs","n", "%best", "%rest")
	 for(c in b)
	   print c , b[c]+r[c], int(100*b[c]/(b[c]+r[c]+0.001)), int(100*r[c]/(b[c] + r[c] + 0.0001))}' paul.csv 
 }
# paul summary
# monthlycontributrs  n   %best  %rest
# 0                   33  33     66
# 1                   29  41     58
# 2                   13  30     69
# 3                   15  53     46
# 4                   8   62     37
# 5+                  15  86     13

sinaboor(){
gawk -F, '$(NF-2) {print $(NF-2)}' sina.csv | sort | uniq -c
}
#  27 1.0
#  62 2.0
#  37 3.0
#   9 4.0
#   1 sina_score
#
sinaplot() {
	gawk -F, '  $(NF-2){ c=int($8); c=c>4?"5+":c; $(NF-2)>=2? b[c]++: r[c]++}
         END{ 
	 print("monthlycontributrs","n", "%best", "%rest")
 for(c in b)
	   print c , b[c]+r[c], int(100*b[c]/(b[c]+r[c]+0.001)), int(100*r[c]/(b[c] + r[c] + 0.0001))}' paul.csv 
   }

#monthlycontributrs  n   %best  %rest
#0                   32  90     9
#1                   27  88     11
#2                   12  91     8
#3                   14  57     42
#4                   8   99     0
#5+                  14  42     57
#
sinapprep() {
	gawk -F, '  BEGIN {OFS=","}
	             $(NF-2) && NR>1 {$(NF-2) = $(NF-2) >= 3 ?"rest": "best" }
		     $(NF-2){print $0}' sina.csv
   }

$1
# nb on sina three class leads to 

#Evaluator:    weka.attributeSelection.WrapperSubsetEval -B weka.classifiers.bayes.NaiveBayes -F 5 -T 0.01 -R 1 -E DEFAULT --
#Search:       weka.attributeSelection.BestFirst -D 1 -N 5
#Relation:     sinaprepped-weka.filters.unsupervised.attribute.Remove-R1-2,17-18
#Instance
#=== Attribute selection 10 fold cross-validation (stratified), seed: 1 ===
#
#number of folds (%)  attribute
#           2( 20 %)    1 monthly_PR_comments
#           0(  0 %)    2 monthly_PR_mergers
#           1( 10 %)    3 monthly_closed_PRs
#           7( 70 %)    4 monthly_closed_issues
#           1( 10 %)    5 monthly_commits
#           3( 30 %)    6 monthly_contributors
#           6( 60 %)    7 monthly_forks
#           2( 20 %)    8 monthly_issue_comments
#           0(  0 %)    9 monthly_merged_PRs
#           0(  0 %)   10 monthly_open_PRs
#           1( 10 %)   11 monthly_open_issues
#           9( 90 %)   12 monthly_stargazer
#           0(  0 %)   13 monthly_watchers

#  a  b   <-- classified as
# 33 56 |  a = best
#  7 39 |  b = rest
#
#  prec(best) = 83%
#  recall(rest)= 85%

notes() {
cat<<-'EOF'
sina biased one score minus to paul

mean sina=2
mean paul=1

aul we could do best and rest as best if score=1 else rest

sina  we had to do best=1,next=2, rest>2
EOF
}
