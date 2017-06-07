# Gender Wage Gap
Wages of men and women from a Western European country (country undisclosed for privacy) to document and analyze the gender wage gap.  

# Motivations
Evaluate the importance of the education, experience, productivty in determining pay. Investigate the possibility that the correlation between employer productivity and wages is biased by unobserved worker characteristics, using information on how wages change as workers move between jobs at more and less productive employers.

# Models
- Regression of Wage on Gender and Age
- Oxaca Decomposition - Wage on Gender and Age
- Cubic and Splines - 12 years of education
- Oxaca Decomposition - Employer Productivity

# Data  
The csv data set contains 1 record per person for 23,144 observations (15,058 males and 8,086 females). The basic information pertains to the "reference year". There is also some information for the years before and after the reference year. The following variables provide information about the reference year:  
- year =calendar year, ranging from 2004 to 2008 
- age = age in the reference year, ranging from 22 to 59
- educ =years of education, which can be 4, 6, 9, 12, 16
- female
- y = log real hourly wage (after taxes, measured in Euros/hour) on the job
in the reference year.
- va = log of output per worker at the employer in the reference year (this
is measured in thousands of Euros per person)
