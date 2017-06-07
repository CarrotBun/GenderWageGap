# Data Exploring
load("modified_data.RData")
library(ggplot2)

# histogram of age
hist_age = ggplot(data = wage_df, aes(age)) +
  geom_histogram(colour = "black", binwidth = 2) +
  ggtitle("Distribution of Age")

# histogram of education
hist_educ = ggplot(data = wage_df, aes(educ)) +
  geom_histogram(colour = "black", binwidth = 1) +
  ggtitle("Distribution of Educaiton")

# store total number of observations (people)
N = nrow(wage_df)
f_N = nrow(subset(wage_df, female == 1, female))
m_N = nrow(subset(wage_df, female == 0, female))

### mean statistics ########################################
## for all workers ##
all_means = colMeans(wage_df[ ,c("educ", "age", "y", "va")])

## for female workers ##
female_means = colMeans(subset(wage_df, female == 1, select = c("educ", "age", "y", "va")))

## for male workers ##
male_means = colMeans(subset(wage_df, female == 0, select = c("educ", "age", "y", "va")))

## t-stat for educ ##
tstat_educ = t.test(subset(wage_df, female == 1, select = educ), subset(wage_df, female == 0, select = educ))$statistic

## t-stat for age ##
tstat_age = t.test(subset(wage_df, female == 1, select = age), subset(wage_df, female == 0, select = age))$statistic

## t-stat for wage ##
tstat_wage = t.test(subset(wage_df, female == 1, select = y), subset(wage_df, female == 0, select = y))$statistic

## t-stat for productivity ##
tstat_prod = t.test(subset(wage_df, female == 1, select = va), subset(wage_df, female == 0, select = va))$statistic

tstats = c(tstat_educ, tstat_age, tstat_wage, tstat_prod)

# table of means of education, age, hourly wage, productivity, and t stats based on gender
summary_table = cbind(all_means, female_means, male_means, tstats)

### education bins ############################################
unique(wage_df$educ)
# 4 years of education
f_fraction_educ_4 = sum(subset(wage_df, educ == 4, select = female))/f_N
m_fraction_educ_4 = nrow(subset(wage_df, educ == 4 & female == 0, select = female))/m_N

# 6 years of education
f_fraction_educ_6 = sum(subset(wage_df, educ == 6, select = female))/f_N
m_fraction_educ_6 = nrow(subset(wage_df, educ == 6 & female == 0, select = female))/m_N

# 9 years of education
f_fraction_educ_9 = sum(subset(wage_df, educ == 9, select = female))/f_N
m_fraction_educ_9 = nrow(subset(wage_df, educ == 9 & female == 0, select = female))/m_N

# 12 years of education
f_fraction_educ_12 = sum(subset(wage_df, educ == 12, select = female))/f_N
m_fraction_educ_12 = nrow(subset(wage_df, educ == 12 & female == 0, select = female))/m_N

# 16 years of education
f_fraction_educ_16 = sum(subset(wage_df, educ == 16, select = female))/f_N
m_fraction_educ_16 = nrow(subset(wage_df, educ == 16 & female == 0, select = female))/m_N

# summary table of gender fractions in each education bin
summary_table2 = cbind("4 years" = c("female fraction" = f_fraction_educ_4, 
                                     "male fraction" = m_fraction_educ_4), 
                       "6 years" = c(f_fraction_educ_6, m_fraction_educ_6), 
                       "9 years" = c(f_fraction_educ_9, m_fraction_educ_9), 
                       "12 years" = c(f_fraction_educ_12, m_fraction_educ_12), 
                       "16 years" = c(f_fraction_educ_16, m_fraction_educ_16))


# transposing table for bar plot
plot_table = as.data.frame(cbind("gender" = c("female", "male", "female", "male", "female", "male", 
                                              "female", "male", "female", "male"), 
                                 "years" = as.numeric(c(4, 4, 6, 6, 9, 9, 12, 12, 16, 16)),
                                 "fractions" = c(f_fraction_educ_4, m_fraction_educ_4,
                                                 f_fraction_educ_6, m_fraction_educ_6,
                                                 f_fraction_educ_9, m_fraction_educ_9,
                                                 f_fraction_educ_12, m_fraction_educ_12, 
                                                 f_fraction_educ_16, m_fraction_educ_16)))

# convert fractions from class of factors to numerics
plot_table$fractions = as.numeric(levels(plot_table$fractions))[plot_table$fractions]
plot_table$years = factor(plot_table$years, levels = c(4, 6, 9, 12, 16))

## plots #########################################
# bar plot of gender fractions for each education bin
bar_gender_educ = ggplot(plot_table, aes(as.factor(years), fractions)) +
  geom_bar(aes(fill = gender), position = "dodge", stat = "identity") +
  labs(title = "Distribution of Education for each Gender", x = "Years of Education", y = "Fractions")
bar_gender_educ 


# histogram of Hourly Wage grouped by gender
hist_wage = ggplot(wage_df, aes(x=y, fill=gender)) + 
  geom_density(alpha=.3) +
  labs(title = "Distribution of Log Hourly Wage", x = "Log Hourly Wage", y = "Density")

## summary for hourly wage ########################
# female wages are lower than males'
# female
f_wage_summary = summary(subset(wage_df, female == 1, select = y))
# male
m_wage_summary = summary(subset(wage_df, female == 0, select = y))

## plots ##########################################
# histogram of Productivity grouped by gender
hist_prod = ggplot(wage_df, aes(x=va, fill=gender)) + 
  geom_density(alpha=.3) +
  labs(title = "Distribution of Productivity", x = "Productivity", y = "Density")

## summary for productivity ########################
# female wages are lower than males'
# female
f_prod_summary = summary(subset(wage_df, female == 1, select = va))
# male
m_prod_summary = summary(subset(wage_df, female == 0, select = va))


save(hist_age, hist_educ, hist_wage, bar_gender_educ, 
     hist_wage, summary_table, summary_table2, f_wage_summary, m_wage_summary,
     hist_prod, f_prod_summary, m_prod_summary, file = "exploratory.RData")




