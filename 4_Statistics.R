################################################################################
# Statistics for bone collagen SI analysis
# Workshop May 2023 Kiel

# Angela Trentacoste 

######################################

#load packages. Install these first if you do not have them.
library("readxl")
library("tidyverse")
library("rstatix")
library("car")
library("ggpubr")


# Start by organizing your files
# What is the working directory?
getwd()
# Make sure this where your want to be working. 
# Otherwise set the working directory or make a new project.

# Load data
# Read in the the file we made in the last sessoin:
Coldata <- read_excel("Output/QCCollagenClean.xlsx")

# rename columns
Coldata <- Coldata %>%
  rename(d13C = d13C.m,
         d15N = d15N.m)
  
# Select taxa to investigate - I want to do the main domesticates
maindom <- c("Bos", "Ovis/Capra","Sus") # make a vector with the taxa
Coldata <- Coldata %>% 
  subset(TaxonGroup %in% maindom) %>% # subset the df to only include these taxa
  select(Site, TaxonGroup, d13C, d15N)  # select

### Pivot data
Coldatalong <- Coldata %>%  # change shape of data to long format
  pivot_longer(d13C:d15N, names_to = "Isotope", values_to = "SIvalue")

# seperate N and C SI data
Ndata <- subset(Coldatalong, Isotope == "d15N")
Cdata <- subset(Coldatalong, Isotope == "d13C")


#### Statistical tests -------------------------
# Extreme outliers?	if no, then...

# Shapiro-Wilk’s test	- are all groups normally distributed?
# Levene’s test - homogeneity 
# --- normal yes, 3+ group = Anova
# --- normal no, 3+ group = Kruskal-Wallis 
# --- normal yes, 2 groups = t-test
# --- normal no, 2 group = Mann-Whitney U test


# Here everything is for C SI data from the Tarquinia site - sub in the Ndata df to repeat for N isotopes
Col <- subset(Cdata, Site == "Tarquinia")

# .. Extreme outliers ------------
# Let's look at the data
ggboxplot(Col, x = "TaxonGroup", y = "SIvalue")

Col %>% 
  group_by(Site, TaxonGroup) %>%
  identify_outliers(SIvalue)

# .. Normality ------------------------------------------------------------------------
# If you have doubt about the normality of the data, 
# you can use the Kruskal-Wallis test, which is the non-parametric alternative to one-way ANOVA test.

### .... Model residuals -----------
# Build the linear model
model  <- lm(SIvalue ~ TaxonGroup, data = Col)
# Create a QQ plot of residuals
ggqqplot(residuals(model))
# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))
# Check position of points. If all the points fall approximately along the reference line, we can assume normality. 
# Check Shapiro-Wilk test. If p-value is not significant (p > 0.05), we can assume normality.

### .... Check separately -------------------
Col %>%
  group_by(TaxonGroup) %>%
  shapiro_test(SIvalue)
# p-value > 0.05 implyies that the distribution of the data is not significantly different from normal distribution. 
# In other words, we can assume the normality.
# If the score is (p > 0.05) for each group, we can assume normality.


# .. Variance --------------------------------------------------------------------------
# In a situation where the homogeneity of variance assumption is not met, you can compute the Welch one-way 
# ANOVA test using the function welch_anova_test()[rstatix package]. 
# This test does not require the assumption of equal variances.

# Homogeneity of variance
# The residuals versus fits plot can be used to check the homogeneity of variances.
plot(model, 1)

### .... Levene test -------------------
leveneTest(SIvalue ~ TaxonGroup, data = Col)
# If p-value is > 0.05 it is not significant.
# This means that, there is not significant difference between variances across groups. 
# Therefore, we can assume the homogeneity of variances in the different treatment groups.

### ANOVA -----------------------------------------------------------------------------
# We want to know if there is any significant difference between 
# the the SI values of the 3 main taxa
res.aov <- Col %>% anova_test(SIvalue ~ TaxonGroup)
res.aov

# ges = generalised effect size. Proportion of variance explain by group membership.
# Amount of dependancen in dep. variable explained by inde. variable
# How importance between 2 groups is?


### .. Kruskal Wallis ---------------------------------
res.kruskal <- Col %>% kruskal_test(SIvalue ~ TaxonGroup)
res.kruskal

# Effect size 
Col %>% kruskal_effsize(SIvalue ~ TaxonGroup)


# Wilcoxon rank-sum test / Mann-Whitney U-test ---------------------------------------
# The Wilcoxon rank sum test is a non-parametric alternative 
# to the independent two samples t-test for comparing two independent groups of samples, 
# in the situation where the data are not normally distributed.
# Synonymous: Mann-Whitney test, Mann-Whitney U test, Wilcoxon-Mann-Whitney test and two-sample Wilcoxon test.
# The Mann–Whitney U test / Wilcoxon rank-sum test is not the same as the Wilcoxon signed-rank test, 
# although both are nonparametric and involve summation of ranks. 
# The Mann–Whitney U test is applied to independent samples. 
# The Wilcoxon signed-rank test is applied to matched or dependent samples.

stat.test <- Col %>% 
  wilcox_test(SIvalue ~ TaxonGroup) %>%
  add_significance()
stat.test

# effect size
Col %>% wilcox_effsize(SIvalue ~ TaxonGroup)

######### Plot example
# we can use ggboxplot() from the ggpubr package to make a box plot 
# with fancy significance annotation

bxp <- ggboxplot(Col, x = "TaxonGroup", y = "SIvalue", # Make a box plot 
  ylab = "SI Value", xlab = "Taxon", add = "jitter") 
bxp # call the box plot 
stat.test <- stat.test %>% add_xy_position(x = "TaxonGroup")
bxp + stat_pvalue_manual(stat.test, tip.length = 0.01) + 
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))


### T-tests ################################
# Set parameters for equal vs unequal var based on result of Levene's test
# t_test(paired = FALSE, var.equal = FALSE)

Col %>% 
  t_test(SIvalue ~ TaxonGroup, var.equal = TRUE) %>%
  add_significance()

#effect size 
# Cohen's d Measure of Effect Size for t-tests
Col %>% 
  cohens_d(SIvalue ~ TaxonGroup)

### Here we have just looked at Tarquinia d13C values
# but using group_by() I can compare across all groups and Sites

Ndata %>% group_by(Site) %>%
  t_test(SIvalue ~ TaxonGroup, var.equal = TRUE) %>%
  add_significance()



