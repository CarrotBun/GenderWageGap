###########
# Splines #
###########
load("modified_data.RData")
library(splines)

# Using the data for men and women with exactly 12 years of education, 
# compare (for each gender) a cubic polynomial with a cubic spline 
# function with K knots, where K = 3,4,5....10. Use
# a simple 2-sample cross-validation method (by selecting a random 1/2 sample)
# to compare the MSE of the cubic and the spline with K knots.

wage_df$age_cubed = (wage_df$age)^3
wage_df$age_squared = (wage_df$age)^2
data_12edu = subset(wage_df, educ == 12)
data_12edu_f = subset(data_12edu, female == 1)
data_12edu_m = subset(data_12edu, female == 0)


###########
# Splines #
###########

# Females #########################################
set.seed(142)
female_train_indices = sample(1:nrow(data_12edu_f), size = .5 * nrow(data_12edu_f))
female_train = data_12edu_f[female_train_indices, 
                            c("y", "age", "age_squared", "age_cubed")]
female_test = data_12edu_f[-female_train_indices, 
                           c("y", "age", "age_squared", "age_cubed")]

# knot intervals for different amounts of knots in splines
knots = 3:10
for (j in 1:length(knots)){
  value = (max(female_train$age)-22)/knots[j]
  vec = vector()
  vec[1] = 22
  vec[knots[j]] = max(female_train$age)
  for (i in 2:(knots[j]-1)){
    vec[i] = value + vec[as.numeric(i-1)]
  }
  assign(paste("vec", sep = "_", knots[j]), vec) 
}

###########################
# empty matrices for each vector
for (i in 1:8){
  assign(paste("splines_matrix", sep = "_", knots[i]), matrix(NA, nrow = nrow(female_train), ncol = knots[(i)]))
}

##################### function #############################
# function to calculate data for k knots 
spline_data = function(vector, data){
  splines_matrix = matrix(NA, nrow = nrow(data), ncol = length(vector))
  for (i in 1:nrow(data)){
    for (j in 1:length(vector)){
      if((data[i, "age"]-vector[j]) > 0){
        splines_matrix[i,j] = (data[i, "age"] - vector[j])^3
      }
      else{
        splines_matrix[i,j] = 0
      }
    }  
  }
  splines_matrix
}
#############################################################

# regressions 
f_spline_3 = lm(y ~., data = cbind(female_train, spline_data(vec_3, female_train)))
f_spline_4 = lm(y ~., data = cbind(female_train, spline_data(vec_4, female_train)))
f_spline_5 = lm(y ~., data = cbind(female_train, spline_data(vec_5, female_train)))
f_spline_6 = lm(y ~., data = cbind(female_train, spline_data(vec_6, female_train)))
f_spline_7 = lm(y ~., data = cbind(female_train, spline_data(vec_7, female_train)))
f_spline_8 = lm(y ~., data = cbind(female_train, spline_data(vec_8, female_train)))
f_spline_9 = lm(y ~., data = cbind(female_train, spline_data(vec_9, female_train)))
f_spline_10 = lm(y ~., data = cbind(female_train, spline_data(vec_10, female_train)))



for (j in 1:length(knots)){
  value = (max(female_test$age)-22)/knots[j]
  vec = vector()
  vec[1] = 22
  vec[knots[j]] = max(female_test$age)
  for (i in 2:(knots[j]-1)){
    vec[i] = value + vec[as.numeric(i-1)]
  }
  assign(paste("tvec", sep = "_", knots[j]), vec) 
}

# MSE ##############
# empty MSE matrix
mse = matrix(NA, nrow = 8, ncol = 2)
mse[ ,1] = 3:10

################## MSE function ######################
mse_fun = function(model, vector, data){
  sum((predict(model, 
               cbind(data[ ,-1], spline_data(vector, data)))
       - data[ ,"y"])^2)
}
######################################################

# calucate MSE
mse[1, 2] = mse_fun(f_spline_3, tvec_3, female_test)
mse[2, 2] = mse_fun(f_spline_4, tvec_4, female_test)
mse[3, 2] = mse_fun(f_spline_5, tvec_5, female_test)
mse[4, 2] = mse_fun(f_spline_6, tvec_6, female_test)
mse[5, 2] = mse_fun(f_spline_7, tvec_7, female_test)
mse[6, 2] = mse_fun(f_spline_8, tvec_8, female_test)
mse[7, 2] = mse_fun(f_spline_9, tvec_9, female_test)
mse[8, 2] = mse_fun(f_spline_10, tvec_10, female_test)


# Males #########################################
set.seed(142)
male_train_indices = sample(1:nrow(data_12edu_m), size = .5 * nrow(data_12edu_m))
male_train = data_12edu_m[male_train_indices, 
                            c("y", "age", "age_squared", "age_cubed")]
male_test = data_12edu_m[-male_train_indices, 
                           c("y", "age", "age_squared", "age_cubed")]

# knot intervals for different amounts of knots in splines
knots = 3:10
for (j in 1:length(knots)){
  value = (max(male_train$age)-min(male_train$age))/knots[j]
  vec = vector()
  vec[1] = min(male_train$age)
  vec[knots[j]] = max(male_train$age)
  for (i in 2:(knots[j]-1)){
    vec[i] = value + vec[as.numeric(i-1)]
  }
  assign(paste("m_vec", sep = "_", knots[j]), vec) 
}

###########################
# empty matrices for each vector
for (i in 1:8){
  assign(paste("splines_matrix", sep = "_", knots[i]), matrix(NA, nrow = nrow(male_train), ncol = knots[(i)]))
}


# regressions 
m_spline_3 = lm(y ~., data = cbind(male_train, spline_data(m_vec_3, male_train)))
m_spline_4 = lm(y ~., data = cbind(male_train, spline_data(m_vec_4, male_train)))
m_spline_5 = lm(y ~., data = cbind(male_train, spline_data(m_vec_5, male_train)))
m_spline_6 = lm(y ~., data = cbind(male_train, spline_data(m_vec_6, male_train)))
m_spline_7 = lm(y ~., data = cbind(male_train, spline_data(m_vec_7, male_train)))
m_spline_8 = lm(y ~., data = cbind(male_train, spline_data(m_vec_8, male_train)))
m_spline_9 = lm(y ~., data = cbind(male_train, spline_data(m_vec_9, male_train)))
m_spline_10 = lm(y ~., data = cbind(male_train, spline_data(m_vec_10, male_train)))



for (j in 1:length(knots)){
  value = (max(male_test$age)-min(male_test$age))/knots[j]
  vec = vector()
  vec[1] = min(male_test$age)
  vec[knots[j]] = max(male_test$age)
  for (i in 2:(knots[j]-1)){
    vec[i] = value + vec[as.numeric(i-1)]
  }
  assign(paste("m_tvec", sep = "_", knots[j]), vec) 
}

# MSE ##############
# empty MSE matrix
m_mse = matrix(NA, nrow = 8, ncol = 2)
m_mse[ ,1] = 3:10

# calucate MSE
m_mse[1, 2] = mse_fun(m_spline_3, m_tvec_3, male_test)
m_mse[2, 2] = mse_fun(m_spline_4, m_tvec_4, male_test)
m_mse[3, 2] = mse_fun(m_spline_5, m_tvec_5, male_test)
m_mse[4, 2] = mse_fun(m_spline_6, m_tvec_6, male_test)
m_mse[5, 2] = mse_fun(m_spline_7, m_tvec_7, male_test)
m_mse[6, 2] = mse_fun(m_spline_8, m_tvec_8, male_test)
m_mse[7, 2] = mse_fun(m_spline_9, m_tvec_9, male_test)
m_mse[8, 2] = mse_fun(m_spline_10, m_tvec_10, male_test)


f_best_knots = mse[mse[ ,2] == min(mse[ ,2]), 1]
m_best_knots = m_mse[m_mse[ ,2] == min(m_mse[ ,2]), 1]

# composite MSE plot
library(ggplot2)
ggplot() +
  ylim(0, 900) +
  geom_point(aes(x = mse[ ,1], y = mse[ ,2], colour = "female")) +
  geom_point(aes(x = m_mse[ ,1], y = m_mse[ ,2], colour = "male")) +
  labs(title = "Composite MSE for Splines", x = "Number of Knots in Splines", y = "MSE") +
  scale_colour_manual(name = "Gender", values = c(female = "red", male = "blue"))

ggsave("composite-mse-plot.png", width = 5, height = 3, dpi = 100)


# table of MSEs
mse_table = as.data.frame(cbind(m_mse, mse[ ,2]))
colnames(mse_table) = c("Knots", "Male", "Females")

############
value = (max(female_test$age)-22)/3
vec = vector()
vec[1] = 22
vec[3] = max(female_test$age)
vec[2] = value + vec[1]
female_vec = vec

# best model is splines with 3 knots
female_predicted_y = predict(f_spline_3, cbind(female_test[ ,2:4], spline_data(female_vec, female_test))) 
#############

# cubic poly
cubic_poly = lm(y~., data = female_train)

female_cubic_predicted_y = predict(cubic_poly, female_test[ ,2:4])

# plot spline and cubic on 
f_plot = ggplot() +
  geom_point(aes(x = data_12edu_f$age, y = data_12edu_f$y), colour = "gray", shape = 1) +
  geom_line(aes(x = female_test$age, y = female_predicted_y, colour = "splines")) +
  geom_line(aes(x = female_test$age, y = female_cubic_predicted_y, colour = "cubic")) +
  scale_colour_manual(name = "Functions", values = c(splines = "purple", cubic = "green")) +
  labs(title = "Fitted Functions of Females' with HS Educ.", x = "Age", y = "Log Wage")

ggsave("f_plot.png", width = 5, height = 3, dpi = 140)
##############

value = (max(male_test$age)-22)/3
vec = vector()
vec[1] = 22
vec[3] = max(male_test$age)
vec[2] = value + vec[1]
male_vec = vec

# best model is splines with 3 knots
male_predicted_y = predict(m_spline_3, cbind(male_test[ ,2:4], spline_data(male_vec, male_test))) 
#############

# cubic poly
m_cubic_poly = lm(y~., data = male_train)

male_cubic_predicted_y = predict(m_cubic_poly, male_test[ ,2:4])

# plot spline and cubic on 
m_plot = ggplot() +
  geom_point(aes(x = data_12edu_m$age, y = data_12edu_m$y), colour = "gray") +
  geom_line(aes(x = male_test$age, y = male_predicted_y, colour = "splines")) +
  geom_line(aes(x = male_test$age, y = male_cubic_predicted_y, colour = "cubic")) +
  scale_colour_manual(name = "Functions", values = c(splines = "purple", cubic = "green")) +
  labs(title = "Fitted Functions of Males' with HS Educ.", x = "Age", y = "Log Wage")
ggsave("m_plot.png", width = 5, height = 3, dpi = 140)

##############


save(mse_table, f_best_knots, m_best_knots, f_plot, m_plot, file = "splines.RData")


