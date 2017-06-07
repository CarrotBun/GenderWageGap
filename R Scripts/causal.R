load("c-regressions.RData")
load("m-regressions.RData")
load("modified_data.RData")

# Model C3: male wage on age, age-squared, dva
# Model C4: female wage on age, age-squared, dva

# M3: male wage on education, age, age-squared, age-cubed, va (firm productivity)
# M4: female wage on education, age, age-squared, age-cubed, va (firm productivity

male_data = subset(wage_df, female == 0, c("female", "y", "va", "educ", "age", "age_squared", "age_cubed"))
female_data = subset(wage_df, female == 1, c("female", "y", "va", "educ", "age", "age_squared", "age_cubed"))

# male
male_data$y_m_causal = male_data$y - c3$coefficients["dva"] * male_data$va
new_m3 = lm(y_m_causal ~ educ + age + age_squared + age_cubed, data = male_data)

# female
female_data$y_f_causal = female_data$y - c4$coefficients["dva"] * female_data$va
new_m4 = lm(y_f_causal ~ educ + age + age_squared + age_cubed, data = female_data)


# Then use these two models and their coefficients to re-do the Oaxaca style decompositions. 
# How much of the gender gap is due to differences in the mean value of va for males versus females? 
# How much to the different "returns" to working at a higher productivity firm for males versus females?

wage_gap = as.numeric(colMeans(subset(wage_df, female == 1, select = "y")) 
                      - colMeans(subset(wage_df, female == 0, select = "y")))

# mean female and male education
mean_female_prod = as.numeric(colMeans(subset(wage_df, female == 1, 
                                              select = "va")))
mean_male_prod = as.numeric(colMeans(subset(wage_df, female == 0, 
                                            select = "va")))

# percentage explained by productivity of firms
new_male_prod_comp = (mean_female_prod - mean_male_prod) * c3$coefficients["dva"]
new_male_prod_effect = new_male_prod_comp/wage_gap

new_female_prod_comp = (mean_female_prod - mean_male_prod) * c4$coefficients["dva"]
new_female_prod_effect = new_female_prod_comp/wage_gap

# difference in coefficients: returns to productivity using female mean
new_diff_returns_prod = mean_female_prod*(c4$coefficients["dva"] - c3$coefficients["dva"])
new_diff_returns_prod2 = mean_male_prod*(c4$coefficients["dva"] - c3$coefficients["dva"])

save(new_m3, new_m4, new_male_prod_comp, new_male_prod_effect, 
     new_female_prod_comp, new_female_prod_effect, 
     new_diff_returns_prod, new_diff_returns_prod2, file = "new-m-oxaca.RData")







