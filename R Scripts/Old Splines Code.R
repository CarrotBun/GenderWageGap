

# Females #########################################
# create train and test sets
set.seed(142)
train_indices = matrix(NA, nrow = .5*nrow(data_12edu_f), ncol = 10)
for(i in 1:10){
  train_indices[ ,i] = sample(1:nrow(data_12edu_f), size = .5*nrow(data_12edu_f))
}

# empty MSE matrix setup
mse = matrix(NA, nrow = 10, ncol = 8)
# empty mean MSE matrix setup
compact_mse = matrix(NA, nrow = 1, ncol = 8)
colnames(mse) = paste(k, "knots", sep = " ")

# 10 fold CV regression on train set with MSE from test set
for (j in 1:length(k)){ 
  # for each knot number run spline on the train set
  for (i in 1:ncol(train_indices)){
    # for each train set, run n knots
    # each column of train_indices is a n fold train set of indicies
    train = train_indices[ ,i]
    knots = k[j]
    # reg = smooth.spline(data_12edu_f[train, "age"], df = knots)
    # assign(paste("spline", sep = "_", i), value = reg) 
    pred = predict(smooth.spline(data_12edu_f[train, "age"], df = knots), data_12edu_f[-train_indices[ ,i], "age"])
    # test data set mse for each knot in a mse dataframe of 3 to 10 knots and 10 mse's for each knot
    mse[i, j] = mean((pred$y - data_12edu_f[-train_indices[ ,i], "y"])^2)
  } 
  # take the mean MSE across 10 fold
  compact_mse[1, j] = mean(mse[ ,j])
}

#########
# Plots #
#########


### age-wage plot ############
plot(data_12edu_f$y ~ data_12edu_f$age, col = "gray", lwd = 2, xlab = "age", ylab = "wage")
# linear
abline(lm(data_12edu_f$y ~ data_12edu_f$age), col = "grey")
# cubic
x = data_12edu_f[ , "age"]
lines(x, predict(cubic_poly, data = x), col = "black")
# splines
lines(data_12edu_f[ , "age"], predict(spline_10, newdata = data_12edu_f[ , "age"]), col = "red")
lines(smooth.spline(data_12edu_f[ ,"age"],data_12edu_f[ ,"y"],df = 10), col = "red")
lines(smooth.spline(data_12edu_f[ ,"age"],data_12edu_f[ ,"y"],df = 9), col = "blue")
lines(smooth.spline(data_12edu_f[ ,"age"],data_12edu_f[ ,"y"],df = 8), col = "green")
lines(smooth.spline(data_12edu_f[ ,"age"],data_12edu_f[ ,"y"],df = 7), col = "purple")
lines(smooth.spline(data_12edu_f[ ,"age"],data_12edu_f[ ,"y"],df = 6), col = "pink")
lines(smooth.spline(data_12edu_f[ ,"age"],data_12edu_f[ ,"y"],df = 5), col = "cyan")
lines(smooth.spline(data_12edu_f[ ,"age"],data_12edu_f[ ,"y"],df = 4), col = "royal blue")
lines(smooth.spline(data_12edu_f[ ,"age"],data_12edu_f[ ,"y"],df = 3), col = "orchid 4")





# Females #########################################

# create train and test sets
set.seed(142)
train_indices = matrix(NA, nrow = .5*nrow(data_12edu_m), ncol = 10)
for(i in 1:10){
  train_indices[ ,i] = sample(1:nrow(data_12edu_m), size = .5*nrow(data_12edu_m))
}


# empty MSE matrix setup
male_mse = matrix(NA, nrow = 10, ncol = 8)
# empty mean MSE matrix setup
male_compact_mse = matrix(NA, nrow = 1, ncol = 8)
colnames(male_mse) = paste(k, "knots", sep = " ")

# 10 fold CV regression on train set with MSE from test set
for (j in 1:length(k)){ 
  # for each knot number run spline on the train set
  for (i in 1:ncol(train_indices)){
    # for each train set, run n knots
    # each column of train_indices is the ith fold train set of indicies
    train = train_indices[ ,i]
    knots = k[j]
    # reg = smooth.spline(data_12edu_m[train, "age"], df = knots)
    # predict using model above but using the age variable of the test set for the ith fold
    pred = predict(smooth.spline(data_12edu_m[train, "age"], df = knots), data_12edu_m[-train_indices[ ,i], "age"])
    # test data set mse for jth knot in a mse dataframe of 3 to 10 knots
    male_mse[i, j] = mean((pred$y - data_12edu_m[-train_indices[ ,i], "y"])^2)
  } 
  # take the mean MSE across 10 fold
  male_compact_mse[1, j] = mean(male_mse[ ,j])
}

#########
# Plots #
#########


plot(data_12edu_m$y ~ data_12edu_m$age, col = "gray", lwd = 2, xlab = "age", ylab = "wage")
# linear
abline(lm(data_12edu_m$y ~ data_12edu_m$age), col = "grey")
# cubic

# splines
lines(smooth.spline(data_12edu_m[ ,"age"],data_12edu_m[ ,"y"],df = 10), col = "red")
lines(smooth.spline(data_12edu_m[ ,"age"],data_12edu_m[ ,"y"],df = 9), col = "blue")
lines(smooth.spline(data_12edu_m[ ,"age"],data_12edu_m[ ,"y"],df = 8), col = "green")
lines(smooth.spline(data_12edu_m[ ,"age"],data_12edu_m[ ,"y"],df = 7), col = "purple")
lines(smooth.spline(data_12edu_m[ ,"age"],data_12edu_m[ ,"y"],df = 6), col = "pink")
lines(smooth.spline(data_12edu_m[ ,"age"],data_12edu_m[ ,"y"],df = 5), col = "cyan")
lines(smooth.spline(data_12edu_m[ ,"age"],data_12edu_m[ ,"y"],df = 4), col = "royal blue")
lines(smooth.spline(data_12edu_m[ ,"age"],data_12edu_m[ ,"y"],df = 3), col = "orchid 4")




# MSE plot (averaged over 10 fold for each knot)
plot(3:10,compact_mse[1, ], 
     xlab = "number of knots", ylab = "MSE", main = "10-fold averaged Female MSE for Number of Knots")
library(ggplot2)

ggplot()+
  geom_point(aes(x = 3:10, y = compact_mse[1, ]), col = "red") +
  geom_point(aes(x = 3:10, y = male_compact_mse[1, ]), col = "blue")

