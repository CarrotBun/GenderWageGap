load("modified_data.RData")

# - Model C1: include age, age-squared, and dva = va - va_previous, the change
# in va between the employer in period -1 and the employer in period 0
# - Model C2: include age, age-squared, a female dummy, and dva.
# - Model C3 and C4: fit models separately by gender that include age,
# age-squared, and dva.

cdata = wage_df[ ,c("dy", "dva", "female", "age", "age_squared", "age_cubed")]

c1 = lm(dy ~ 0 + age + age_squared + dva, 
        data = cdata[ ,c("age", "age_squared", "dva", "dy")])
c2 = lm(dy ~ 0 + age + age_squared + female + dva,
        data = cdata[ ,c("age", "age_squared", "female", "dva", "dy")])
c3 = lm(dy ~ 0 + age + age_squared + dva,
        data = subset(cdata, female == 0, c("age", "age_squared", "dva", "dy")))
c4 = lm(dy ~ 0 + age + age_squared + dva,
        data = subset(cdata, female == 1, c("age", "age_squared", "dva", "dy")))
 
save(c1, c2, c3, c4, file = "c-regressions.RData")
