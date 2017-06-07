##########################
# Oxaca for Productivity #
##########################

#Fit two alternative models using the pooled data for men and women:
# - M1: include a constant, education, a cubic in age, a female dummy
# - M2: include a constant, education, a cubic in age, a female dummy, and va

# Fit separate models (M3 and M4) for men and women that include a
# constant, education, and a cubic in age, and va.

load("modified_data.RData")
wage_df$age_sq = (wage_df$age)^2
wage_df$age_cubed = (wage_df$age)^3

###############
# Regressions #
###############
# reg of wage on education, cubic age, gender
m1 = lm(y~educ + female + age + age_sq + age_cubed, data = wage_df)
# reg of wage on education, cubic age, gender, and productivity
m2 = lm(y~educ + female + age + age_sq + age_cubed + va, data = wage_df)
# reg of wage on education, cubic age, and productivity for FEMALES
m4 = lm(y~educ + age + age_sq + age_cubed + va, data = wage_df[wage_df$female == 1, ])
# reg of wage on education, cubic age, and productivity for MALES
m3 = lm(y~educ + age + age_sq + age_cubed + va, data = wage_df[wage_df$female == 0, ])

library(memisc)
table2_reg = mtable("Model 1" = m1, "Model 2 (Prod.)" = m2,
                    "Model 2 (Female)" = m4, "Model 2 (Male)" = m3,
                    summary.stats = c('R-squared','F','p','N'))

save(m1, m2, m4, m3, table2_reg, file = "m-regressions.RData")

#######################
# Oxaca Decomposition #
#######################

# Begin by comparing models M1 and M2. How much of the gender gap is
# "explained" by the fact that women work at less productive firms?
wage_gap = as.numeric(colMeans(subset(wage_df, female == 1, select = "y")) 
                      - colMeans(subset(wage_df, female == 0, select = "y")))

beta_age = m1$coefficients["age"]
beta_age_sq = m1$coefficients["age_sq"]
beta_age_cubed = m1$coefficients["age_cubed"]
beta_prod = m2$coefficients["va"]

beta_female_prod = m4$coefficients["va"]
beta_male_prod = m3$coefficients["va"]

# mean female and male education
mean_female_prod = as.numeric(colMeans(subset(wage_df, female == 1, 
                                              select = "va")))
mean_male_prod = as.numeric(colMeans(subset(wage_df, female == 0, 
                                            select = "va")))

# effect of productivity
prod_comp = (mean_female_prod - mean_male_prod)*beta_prod 

# percentage explained by productivity of firms
prod_effect = prod_comp/wage_gap



# difference in wages after controlling for education, age, age squared, and age cubed
m1$coefficients["female"]

# difference in wages after controlling for education, age, age squared, aged cubed, and productivity
m2$coefficients["female"]


save(wage_gap, mean_female_prod, mean_male_prod, 
     beta_female_prod, beta_male_prod, 
     prod_comp, prod_effect, file = "m-oxaca.RData")

# Next, discuss the estimated effect of va on wages. Specifically, consider
# a causal wage model where part of the "residual" is due to an unobserved
# characteristic of workers that makes them more or less productive.

# What will happen in this model if more productive employers (with higher
# values of va) hire workers who have higher productivity?


# perform an Oaxaca style decomposition using models
# M3 and M4, there will be a component of the gender gap that arises because of
# the difference in "returns" to va.

prod_comp2 = (mean_female_prod - mean_male_prod)*beta_male_prod
prod_effect2 = prod_comp2/wage_gap

prod_comp3 = (mean_female_prod - mean_male_prod)*beta_female_prod
prod_effect3 = prod_comp3/wage_gap

# difference in coefficients: returns to productivity
diff_returns_prod = mean_female_prod*(beta_female_prod - beta_male_prod)

#### Re-Normalize
wage_df$normalized_va = wage_df$va - min(wage_df$va)

diff_returns_prod_norm = as.numeric(
  colMeans(
    subset(wage_df, 
           female == 1, 
           select = "normalized_va")))*(beta_female_prod - beta_male_prod)

save(prod_comp2, prod_effect2, prod_comp3, prod_effect3, diff_returns_prod, diff_returns_prod_norm,
     file = "m-oxaca2.RData")