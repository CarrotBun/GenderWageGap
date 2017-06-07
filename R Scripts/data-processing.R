# Data Processing

# load data frame
wage_df = read.csv("wage-data.csv")

# create "female" and "male" labels
wage_df$gender = wage_df$female
for(i in 1:nrow(wage_df)){
  if(wage_df$female[i] == 1){
    wage_df$gender[i] = "female"
  }
  else{
    wage_df$gender[i] = "male"
  }
}

wage_df$age_cubed = (wage_df$age)^3
wage_df$age_squared = (wage_df$age)^2
save(wage_df, file = "modified_data.RData")
