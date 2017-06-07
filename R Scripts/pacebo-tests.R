# Placebo Tests
load("modified_data2.RData")

# male subset of ppl who didn't switch productive quartile
males = subset(wage_df2, 
               (female == 0 & va_prv_Q1 == 1 & va_Q1 == 1 | 
                  female == 0 & va_prv_Q2 == 1 & va_Q2 == 1 | 
                  female == 0 & va_prv_Q3 == 1 & va_Q3 == 1 | 
                  female == 0 & va_prv_Q4 == 1 & va_Q4 == 1),
               select = c("age", "age_squared", "dva", "dy_pre", "dy_post", "y", "va"))
# female subset of ppl who didn't switch productive quartile
females = subset(wage_df2, 
                 (female == 1 & va_prv_Q1 == 1 & va_Q1 == 1 | 
                    female == 1 & va_prv_Q2 == 1 & va_Q2 == 1 | 
                    female == 1 & va_prv_Q3 == 1 & va_Q3 == 1 | 
                    female == 1 & va_prv_Q4 == 1 & va_Q4 == 1),
                 select = c("age", "age_squared", "dva", "dy_pre", "dy_post", "y", "va"))


# male subset of ppl who did switch productive quartile
males_switched = subset(wage_df2, 
               (female == 0 & va_prv_Q1 == 1 & va_Q1 != 1 | 
                  female == 0 & va_prv_Q2 == 1 & va_Q2 != 1 | 
                  female == 0 & va_prv_Q3 == 1 & va_Q3 != 1 | 
                  female == 0 & va_prv_Q4 == 1 & va_Q4 != 1),
               select = c("age", "age_squared", "dva", "dy_pre", "dy_post", "y", "va"))

# female subset of ppl who did switch productive quartile
females_switched = subset(wage_df2, 
                 (female == 1 & va_prv_Q1 == 1 & va_Q1 != 1 | 
                    female == 1 & va_prv_Q2 == 1 & va_Q2 != 1 | 
                    female == 1 & va_prv_Q3 == 1 & va_Q3 != 1 | 
                    female == 1 & va_prv_Q4 == 1 & va_Q4 != 1),
                 select = c("age", "age_squared", "dva", "dy_pre", "dy_post", "y", "va"))

# males
p1a = lm(dy_pre ~ 0 + age + age_squared + dva,
        data = males)
p1b = lm(dy_post ~ 0 + age + age_squared + dva,
        data = males)
p1c = lm(dy_pre ~ 0 + age + age_squared + dva,
         data = males_switched)
p1d = lm(dy_post ~ 0 + age + age_squared + dva,
         data = males_switched)

# females
p2a = lm(dy_pre ~ 0 + age + age_squared + dva,
         data = females)
p2b = lm(dy_post ~ 0 + age + age_squared + dva,
         data = females)
p2c = lm(dy_pre ~ 0 + age + age_squared + dva,
         data = females_switched)
p2d = lm(dy_post ~ 0 + age + age_squared + dva,
         data = females_switched)

save(p1a, p1b, p1c, p1d, p2a, p2b, p2c, p2d, file = "p-regression.RData")












