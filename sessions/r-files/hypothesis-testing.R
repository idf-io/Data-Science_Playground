# Title: HYPOTHESIS TESTING
# Author: Ian Dirk Fichtner
# Date: 2022-05-31

### PARAMETRIC TESTS
  # For normally distributed data

# Define datasets
weights.male <- rnorm(500, 75, 5)
weights.female <- rnorm(500, 65, 5)
weights.female.alt <- rnorm(500, 65, 7)

# Conditions for t-tests that must be fulfilled
# 1. Independent measuremnts/samples/realizations (inherent in the creation of dataset)
# 2. Constant variance (inherent in the creation of dataset)
# 3. Normal distribution of mesurements (inherent in the creation of dataset)
# Proof by QQ-plo
qqnorm(weights.male, pch = 1, frame = FALSE)
qqline(weights.male, col = "red")
qqnorm(weights.female, pch = 1, frame = FALSE)
qqline(weights.female, col = "red")
qqnorm(weights.female.alt, pch = 1, frame = FALSE)
qqline(weights.female.alt, col = "red")
# All scatterplots followed a 1:1 inclination indicating a normal distribution of the data

# Proof by Shapiro-Wilks test
shapiro.test(weights.male)
shapiro.test(weights.female)
shapiro.test(weights.female.alt)
# For arbitrary significance alpha=0.05: H0 (data belongs to a normal distribution) could not be discarded


# One-sample, unpaired test
t.test(weights.male, mu = 75)
# For arbitrary significance alpha=0.05: H0 cannot be discarded

# Two-sample, unpaired, two-sided test, same variance: STUDENT-TEST
t.test(weights.male, weights.female, var.equal = TRUE)
# For arbitrary significance alpha=0.05: H0 is be discarded and the alternative hypothesis accepted.

# Two-sample, upaired, one-sided upper-tail test
t.test(weights.male, weights.female, var.equal = TRUE, alternative = 'greater')
# For arbitrary significance alpha=0.05: H0 is be discarded and the alternative hypothesis accepted.

# Two-sample, unpaired, one-sided, different variances test: WELCH TEST
t.test(weights.male, weights.female, alternative = 'greater')
# For arbitrary significance alpha=0.05: H0 is be discarded and the alternative hypothesis accepted.

# Two-sample, paired, one-sided
# We assume the although randomly generated, the second dataset contains a change of the first one.
t.test(weights.male, weights.female, alternative = 'greater', paired = TRUE)
# For arbitrary significance alpha=0.05: H0 is be discarded and the alternative hypothesis accepted.



### NON-PARAMETRIC TESTS
  # For non-normal distributions

# Define the datasets
diabetic <- c(600, 610, 590, 590, 500, 500, 490, 750, 590, 490)
non.diabetic <- c(400, 300, 250, 310, 290, 210, 250, 240, 230, 230)

# Prove non-normal distribution
  # By QQ-Plot
qqnorm(diabetic, pch = 1, frame = FALSE)
qqline(diabetic, col = "red")
qqnorm(non.diabetic, pch = 1, frame = FALSE)
qqline(non.diabetic, col = "red")

# Wilcoxon Rank Sum Test/Mann-Whitney U Test
wilcox.test(non.diabetic, diabetic)
# For arbitrary significance alpha=0.05: H0 is be discarded and the alternative hypothesis accepted.
