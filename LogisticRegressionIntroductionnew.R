
library(tidyverse)


# LOGISTIC REGRESSION  (Introduction)
#      * Used when the response variable is a binary categorical variable (1 or 0)
#      * Based on probability and odds (You will answer questions concerning probability or
#     odds)
#      * Formulas used for to process Logistic Regression
#      1) ln(P/(1 - P) =  B(0) + B(1)X(1) + B(2)X(2) + ... + B(k)X(k)
#      2) P =  (e^(B(0) + B(1)X(1) + B(2)X(2) + ... + B(k)X(k)) /
#              (1 + e^( B(0) + B(1)X(1) + B(2)X(2) + ... + B(k)X(k))

# Example 1

tribble(~EntranceExamScore,  ~GPA,   ~ADMIT,
        72,               3.2,       0,
        81,               3.4,       0,
        67,               2.7,       0,
        77,               2.8,       0,   #( 1 you git Admitted, 0 you do not)
        87,               3.3,       1,   # ( get Admitted)
        79,               3.8,       1,
        85,               2.7,       0,
        76,               2.5,       0,
        90,               3.1,       1,
        77,               2.6,       0,
        64,               2.4,       0,
        70,               2.75,      0,
        88,               3.0,       1,
        79,               3.55,      0,
        72,               2.8,       0,
        80,               3.3,       1,
        80,               2.4,       0,
        76,               3.0,       0,
        76,               2.75,      0,
        89,               2.8,       1,
        93,               3.3,       1,
        88,               3.2,       1,
        83,               3.5,       0
) -> ps
ps


ps


ps$ADMIT <- as.factor(ps$ADMIT)


ps




glm(ADMIT ~ EntranceExamScore + GPA, family = "binomial",  data = ps) -> logisticps
logisticps

summary(logisticps)


#   ln(P/(1 - P) = -59.1733 + .5536EntranceExamScore + 4.1010GPA  


# Example
#   Now use your model to find the probability that a student will be admitted if she has a 
#   GPA of 2.85 and an Entrance exam score of 83.

# ln(P/(1 - P) =   -59.1733 + .5536(83) + 4.1010(2.85)
# ln(P/(1 - P) = -1.53665
# e^(ln(P/(1 - P)) =e^(-1.53665)
#     P/(1 - P) = .2151005
# solve for P
#  P = .2151005(1 - P)
#  P =  .2151005 - .2151005P
#  1.2151005P = .2151005
#         P =  .2151005/ 1.2151005
#         P = .17538  (rounded to five digits)
# Hence the probability is approximately .18

# Find the odds that the student will be admitted.
# P/(1-P)  =  .18/.82 = 18/82 = 9/41 or 9 : 41 or 9 to 41


# Example 2

mtcars

library(tidyverse)
library(dplyr)

mtcars

# let's get a more concise table reporesentation.

as.tibble(mtcars)

as.tibble(mtcars) -> y
y

y

# Let us use Logistic Regression to find probabilities regarding the
# variable vs (which is coded as 1s or 0s)

# What is vs ?

?mtcars


# Let's change vs to categorical (two levels 0 and 1)


y$vs <- as.factor(y$vs)    

y

# Let's create a logistic regression model for a depoendent variable of vs
# and independent variables of mpg, hp (horsepower), and disp (displacement)


glm(vs ~ mpg + hp + disp, family = "binomial",  data = y) -> logisticy
logisticy

summary(logisticy)


#  ln(P/(1 - P) =  10.851538 -0.083254mpg -0.065013hp -0.006317disp



# Let's find the probability that a vehicle has a straight engine if mpg is
# 30, displacement is 125, and hp (horsepower) is 300.


#  ln(P/(1 - P) = 10.851538 -0.083254(30) -0.065013(300) -0.006317(125)

10.851538 -0.083254*(30) -0.065013*(300) -0.006317*(125)

#  ln(P/(1 - P) =  -11.93961

# e^ (ln(P/(1 - P)) = e^(-11.93961)

# P/(1 - P)  = .0000065267

# P  =   .0000065267 - .0000065267P

# 1.0000065267P  = .0000065267

# P =  .0000065267   (practically 0 !!)





