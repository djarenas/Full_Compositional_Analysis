# Full_Compositional_Analysis
The software is intended for statistical comparisons of compositions (proportions|percentages) that add up to a constant (1|100). For each group, it pulls the proportions you want to analyze, performs a centered log-rate transformation on the compositons, and performs a Mann-Whitney U test comparison between the two groups. It outputs the averages and standard deviations of the transformed compositions; it also outputs the p-values from the Mann-Whitney comparison of the means and the p-values from the Levene comparison of the variances. All code is in R.

This upload acts as a repository for the compositional analysis software used for the analysis of IHC data for the manuscript "Increased mTOR activation in idiopathic multicentric Castleman disease" by Arenas et al. (https://ashpublications.org/blood/article/135/19/1673/452765/Increased-mTOR-activation-in-idiopathic)


Two example input files are provided. They correspond to the data in Figure 1 for one for (https://ashpublications.org/blood/article/135/19/1673/452765/Increased-mTOR-activation-in-idiopathic). Each file has the percentages of weak, medium, strong, and negative staining for each individual. The file can contain multiple data for each subject (in our example, it has data on four different geographies of the lymph node).


As of 2021, Academic work that used this software:

Fajgenbaum, David C., et al. "Identifying and targeting pathogenic PI3K/AKT/mTOR signaling in IL-6 blockade–refractory idiopathic multicentric Castleman disease." The Journal of clinical investigation 129.10 (2019): 4451-4463.

Arenas, Daniel, et al. "Increased mTOR activation in idiopathic multicentric Castleman disease." Clinical Lymphoma Myeloma and Leukemia 19.10 (2019): e306.
