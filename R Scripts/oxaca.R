#######################
# Oxaca Decomposition #
#######################
load("modified_data.RData")
wage_df$age_sq = (wage_df$age)^2
wage_df$age_cubed = (wage_df$age)^3

###############
# Regressions #
###############
# pooled regression of wage on gender
s1 = lm(y~female, data = wage_df)

# pooled regression of wage on education, cubic age, and gender
s2 = lm(y~educ + female + age + age_sq + age_cubed, data = wage_df)

# regression of wage on education, and age cubed for females only
s4 = lm(y~educ + age + age_sq + age_cubed, data = wage_df[wage_df$female == 1, ])

# regression of wage on education, and age cubed for males only
s3 = lm(y~educ + age + age_sq + age_cubed, data = wage_df[wage_df$female == 0, ])

# table of regression
library(memisc)
table1_reg = mtable("Model 1" = s1, "Model 2 (Pooled)" = s2,
                    "Model 2 (Female)" = s4, "Model 2 (Male)" = s3,
                    summary.stats = c('R-squared','F','p','N'))

save(s1, s2, s3, s4, table1_reg, file = "s-regressions.RData")

#########
# Oxaca #
#########
# Then, use these two models to construct Oaxaca
# style decompositions of the mean log wage gap between males and females, as
# in Lecture 8. Comment on the contributions of the covariates (education and
# age) versus the coefficients on these variables.

# wage gap 
wage_gap = as.numeric(colMeans(subset(wage_df, female == 1, select = "y")) 
                      - colMeans(subset(wage_df, female == 0, select = "y")))

# mean female and male age
mean_female_age = as.numeric(colMeans(subset(wage_df, female == 1, select = "age")))
mean_male_age = as.numeric(colMeans(subset(wage_df, female == 0, select = "age")))

# mean female and male age squared
mean_female_age_sq = as.numeric(colMeans(subset(wage_df, female == 1, select = "age_sq")))
mean_male_age_sq = as.numeric(colMeans(subset(wage_df, female == 0, select = "age_sq")))

# mean female and male age cubed
mean_female_age_cubed = as.numeric(colMeans(subset(wage_df, female == 1, 
                                                   select = "age_cubed")))
mean_male_age_cubed = as.numeric(colMeans(subset(wage_df, female == 0, 
                                                 select = "age_cubed")))
# mean female and male education
mean_female_educ = as.numeric(colMeans(subset(wage_df, female == 1, 
                                                   select = "educ")))
mean_male_educ = as.numeric(colMeans(subset(wage_df, female == 0, 
                                                 select = "educ")))


#### using female betas ####
# beta coefficients of female age, female age squared, female age squared
beta_female_age = s4$coefficients["age"]
beta_female_age_sq = s4$coefficients["age_sq"]
beta_female_age_cubed = s4$coefficients["age_cubed"]

# age component of gap using female coefficients
age_comp = (as.numeric((mean_female_age - mean_male_age)*beta_female_age) 
            + as.numeric((mean_female_age_sq - mean_male_age_sq)*beta_female_age_sq) 
            + as.numeric((mean_female_age_cubed - mean_male_age_cubed)*beta_female_age_cubed))

# percent explained by age differences
abs(age_comp/wage_gap)*100

# education component of gap
educ_comp = as.numeric((mean_female_educ - mean_male_educ)*s4$coefficients["educ"])

# percent explained by educaiton differences
abs(educ_comp/wage_gap)

# percent unexplained
(1 - abs(age_comp/wage_gap) - abs(educ_comp/wage_gap))*100
  
#### using male betas ####
# beta coefficients of male age, female age squared, female age squared
beta_male_age = s3$coefficients["age"]
beta_male_age_sq = s3$coefficients["age_sq"]
beta_male_age_cubed = s3$coefficients["age_cubed"]

# age component of gap using male coefficients
age_comp2 = (as.numeric((mean_female_age - mean_male_age)*beta_male_age) 
            + as.numeric((mean_female_age_sq - mean_male_age_sq)*beta_male_age_sq) 
            + as.numeric((mean_female_age_cubed - mean_male_age_cubed)*beta_male_age_cubed))

# education component of gap using male coefficients
educ_comp2 = as.numeric((mean_female_educ - mean_male_educ)*s3$coefficients["educ"])

save(wage_gap, mean_female_age, mean_male_age, 
     mean_female_age_sq, mean_male_age_sq,
     mean_female_age_cubed, mean_male_age_cubed,
     mean_female_educ, mean_male_educ,
     beta_female_age, beta_female_age_sq,
     beta_female_age_cubed, age_comp, educ_comp,
     beta_male_age, beta_male_age_sq, beta_male_age_cubed, 
     age_comp2, educ_comp2, file = "s-oxaca.RData")

#########
# Plots #
#########
# relationship between wages and age for men and women who have 
# each of the 5 levels of education, and show the fit of your regression 
# models (Show 5 panels, one for each education group). For this
# exercise, use the predictions from models fit separately by age.

##########
library(ggplot2)
data = subset(wage_df, educ == 4, select = c("age", "y", "age", "age_sq", "age_cubed", "gender"))
plot1 = ggplot(data, aes(x = age, y = y, color = gender)) + 
  geom_point(shape = 1, size = 1) +
  labs(title = "Age & Wage for 4 Yrs. of Educ.", x = "Age", y = "Log Hourly Wage") +
  geom_smooth(method = "lm") +
  theme(text = element_text(size=10))

data = subset(wage_df, educ == 6, select = c("age", "y", "age_cubed", "gender"))
plot2 = ggplot(data, aes(x = age, y = y, color = gender)) + 
  geom_point(shape = 1, size = 1) +
  labs(title = "Age & Wage for 6 Yrs. of Educ.", x = "Age", y = "Log Hourly Wage") +
  geom_smooth(method = "lm")

data = subset(wage_df, educ == 9, select = c("age", "y", "age_cubed", "gender"))
plot3 = ggplot(data, aes(x = age, y = y, color = gender)) + 
  geom_point(shape = 1, size = 1) +
  labs(title = "Age & Wage for 9 Yrs. of Educ.", x = "Age", y = "Log Hourly Wage") +
  geom_smooth(method = "lm") 

data = subset(wage_df, educ == 12, select = c("age", "y", "age_cubed", "gender"))
plot4 = ggplot(data, aes(x = age, y = y, color = gender)) + 
  geom_point(shape = 1, size = 1) +
  labs(title = "Age & Wage for 12 Yrs. of Educ.", x = "Age", y = "Log Hourly Wage") +
  geom_smooth(method = "lm") 

data = subset(wage_df, educ == 16, select = c("age", "y", "age_cubed", "gender"))
plot5 = ggplot(data, aes(x = age, y = y, color = gender)) + 
  geom_point(shape = 1, size = 1) +
  labs(title = "Age & Wage for 16 Yrs. of Educ.", x = "Age", y = "Log Hourly Wage") +
  geom_smooth(method = "lm") 

save(plot1, plot2, plot3, plot4, plot5, file = "s-plots.RData")




