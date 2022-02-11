


# ANOVA (Analysis of Variance)  One Way

# Analysis of Variance (ANOVA) is an inferential method
# used to test the equality of three or more population means.

# Requirements to Perform a One-Way ANOVA Test
#   1) There must be k simple random samples, one from each of
# the k populations
#   2) The k samples must be independent of each other
#   3) The populations must be normally distributed
#   4) The populations must have the same variance

# Example
# three random samples from three populations

#    a        b          c

#    4        7          10
#    5        8          10
#    6        9          11
#    6        7          11
#    4        9          13

#  null hypothesis: The three population means are equal 
#  Mua = Mub = Muc
#  alternative hypothesis:  At least one of the means is
#  different from the others

# We will conduct the F test in order to determine if
# null hypothesis should be rejected or not.
#  n = 15, k = 3,  c = 5

# Step 1  find the mean for the entire data set.
#  (4+5+6+6+4+7+8+9+7+9+10+10+11+11+13) / 15 = 8

# Step 2  Find the sample mean for each sample
#  y(bar)a= 25/5 = 5   y(bar)b = 40/5 = 8   y(bar)c = 55/5 = 11

# Step 3  Find each sample variance
# sample variance for a :  ((4-5)^2 + (5-5)^2 + (6-5)^2 + (6-5)^2 + 
# (4-5)^2))/ (5-1) =  1
# sample variance for b :  ((7-8)^2 + (8-8)^2 + (9-8)^2 + (7-8)^2 + 
# (9-8)^2))/ (5-1) =  1
# sample variance for c :  ((10-11)^2 + (10-11)^2 + (11-11)^2 + 
# (11-11)^2 + (13-11)^2))/ (5-1) = 1.5

# Step 4  Compute the Sum of Squares due to Treatment SST
#         (the number of elements of the group multiplied by the
#          square of the difference between the group mean and 
#          the total mean)  Find the sum of each case
#         SST = 5(5-8)^2 + 5(8-8)^2 + 5(11-8)^2  = 90

#         Compute the Sum of Squares due to Errors   SSE
#         (degrees of freedom for each group multiplied by each
#          variance.  Find the sum of the cases)
#         
#         SSE = (5-1)1 + (5-1)1 + (5-1)(1.5)  =  14

# Step 5  Compute the Mean Square due to Treatment MST
#         MST =  SST/(k-1)   90/(3-1) = 45
#         Compute the Mean Square due to Error  MSE
#         MSE =  SSE/(n-k) = 14/(15-3) = 1.1667
# Step 6  Compute the F statistic  MST/MSE = 45/1.1667 = 38.57

#  Your results are typically organized in a table as follows:

#  Source of Variation      Sum of Squares    Degrees of Freedom   Mean Square   F-Test Statistic
#        Treatment                90                  2                45             38.57
#        Error                    14                  12               1.1667
#        Total                    104                 14
# Step 7  Find the F critical value by using the degrees of freedom
# the F distribution table.
#         If F-Test Statistic > F critical, you are to reject 
#         the null hypothesis.
#         If F-Test Statistic < F critical, you fail to reject 
#         the null hypothesis.

# Step 8  When you go to the standard F distribution table,
# using the degrees of freedom of 2 and  12 you find that the
# F critical = 3.89

# Since F-Test Statistic > F critical you reject the null hypothesis.

# Now use R to get the same result.

#    a        b          c

#    4        7          10
#    5        8          10
#    6        9          11
#    6        7          11
#    4        9          13

a<- c(4,5,6,6,4)
a

b<- c(7,8,9,7,9)
b

c<- c(10,10,11,11,13)
c

CombindGroups <- data.frame(cbind(a,b,c))
CombindGroups

StackedGroups <-stack(CombindGroups)
StackedGroups

aov(values~ind, data = StackedGroups) ->Are
Are
summary(Are)

# Since your p value is less than .05, we will reject the
# null hypothesis.

library(tidyverse)

# One Way ANOVA another example:

# Step 1

SlimMilk<- c(857,853,865,904,916,886,854,856)
SlimMilk

MixedMilk<-c(1006,991,1015,1035,1024,1013,1065,1002)
MixedMilk

WholeMilk <-c(879,938,841,818,870,874,881,836)
WholeMilk

# Step 2

CombindGroups <- data.frame(cbind(SlimMilk,MixedMilk,WholeMilk))
CombindGroups

# Step 3
StackedGroups <-stack(CombindGroups)
StackedGroups

# Step 4
aov(values~ind, data = StackedGroups) ->A
A
summary(A)

# Comment: We will reject the null hypothesis since P < .05


# Since the null hypothesis that all population means are equal
# has been rejected,
# lets determine what population groups have means that are
# significantly different.

TukeyHSD(A) # new

# We now further support our conclusion by looking at boxplots

boxplot(CombindGroups)

# Lets look at the data in a different format (We import the data from an external excel file)
library(tidyverse)

read_csv("MilkData2.csv") -> MD
MD


aov(Calcium~Milk, data = MD) ->AA
AA
summary(AA)

TukeyHSD(AA)

# boxplot base R method

boxplot(MD$Calcium~MD$Milk)

# boxplot tidyverse method

ggplot(data=MD) +
  geom_boxplot(mapping = aes(x = Milk, y = Calcium))



# Two Way ANOVA

# Two Way ANOVA compares population means across categories of two
# explanatory variables. Each null
# hypothesis states that the population means are 
# identical across categories of one categorical
# variable, controlling for the other one.

# Requirements to perform the the Two Way ANOVA
#     1. the populations are normal
#     2. the samples are independent
#     3. the populations have the same variance

# Hypothesis Testing for Two Way ANOVA
#   H(O): There is no interaction between the factors 
#   H(A): There is interaction between factors

#   H(O): There is no effect of factor A   
#   H(A): There is an effect of factor A

#   H(O): There is no effect of factor B 
#   H(A): There is effect of factor B

# We introduce another factor for the MilkData data.  
# The new factor is  Brand.


#                SkinMilk     MixedMilk         WholeMilk

#    BrandA     857 , 853     1006 , 991        879 , 938
#               865 , 904     1015 , 1035       841 , 818
#
#    BrandB     916 , 886     1024 , 1013       870 , 874
#               854 , 856     1065 , 1062       881 , 836

# We arrange or structure the data as follows for R input
# and analysis

library(tidyverse)

tribble(~Brand,   ~MilkType,    ~Calcium,
        "BrandA",   "SkinMilk",   857,
        "BrandA",   "SkinMilk",   853,
        "BrandA",   "SkinMilk",   865,
        "BrandA",   "SkinMilk",   904,
        "BrandA",   "MixedMilk",  1006,
        "BrandA",   "MixedMilk",  991,
        "BrandA",   "MixedMilk",  1015,
        "BrandA",   "MixedMilk",  1035,
        "BrandA",   "WholeMilk",  879,
        "BrandA",   "WholeMilk",  938,
        "BrandA",   "WholeMilk",  841,
        "BrandA",   "WholeMilk",  818,
        "BrandB",   "SkinMilk",   916,
        "BrandB",   "SkinMilk",   886,
        "BrandB",   "SkinMilk",   854,
        "BrandB",   "SkinMilk",   856,
        "BrandB",   "MixedMilk",  1024,
        "BrandB",   "MixedMilk",  1013,
        "BrandB",   "MixedMilk",  1065,
        "BrandB",   "MixedMilk",  1002,
        "BrandB",   "WholeMilk", 870,
        "BrandB",   "WholeMilk", 874,
        "BrandB",   "WholeMilk", 881,
        "BrandB",   "WholeMilk", 836
) -> Milk
Milk


as.factor(Milk$Brand) -> Milk$Brand

as.factor(Milk$MilkType) -> Milk$MilkType

str(Milk)


aov(Calcium ~ Brand + MilkType + Brand:MilkType, 
    data = Milk)-> aovMilk
aovMilk
summary(aovMilk)


interaction.plot(Milk$Brand, Milk$MilkType, Milk$Calcium, xlab = "Brand",
                 ylab = "Calcium")

# Interpreting the output table:
#   1) Note that the factor Brand is not significant, hence  
#      Calcium is not significantly impacted by variation in Brand
#      You cannot reject the null hypothesis that there is no effect
#      for the factor Brand

#   2) Note that the factor Milktype is significant, hence Calcium
#      is significantly impacted by variation in Milktype
#      You can reject the null hypothesis that there is no effect
#      for the factor MilkType

#   3) The interaction term is not significant. This indicates that
#      the relationship between Calcium and one of the factors does
#      not depend on its relationship with the other factor. 
#      The null hypothesis that there is no interaction can not be
#      rejected





# Example 2

tribble(~Weight,    ~pH,   ~Calluna,
        2.76, "pH3.5", "Present",
        2.39, "pH3.5", "Present",
        3.54, "pH3.5", "Present",
        3.71, "pH3.5", "Present",
        2.49, "pH3.5", "Present",
        4.10, "pH3.5",  "Absent",
        2.72, "pH3.5",  "Absent",
        2.28, "pH3.5",  "Absent",
        4.43, "pH3.5",  "Absent",
        3.31,  "pH3.5",  "Absent",
        3.21, "pH5.5", "Present",
        4.10, "pH5.5", "Present",
        3.04, "pH5.5", "Present",
        4.13, "pH5.5", "Present",
        5.21, "pH5.5", "Present",
        5.92, "pH5.5", "Absent",
        7.31, "pH5.5",  "Absent",
        6.10, "pH5.5",  "Absent",
        5.25, "pH5.5",  "Absent",
        7.45, "pH5.5",  "Absent"
        
) ->festuca
festuca


as.factor(festuca$pH) -> festuca$pH
as.factor(festuca$Calluna) -> festuca$Calluna

str(festuca)


aov(Weight ~ pH + Calluna + pH:Calluna, 
    data = festuca)-> aovfestuca
aovfestuca
summary(aovfestuca)


interaction.plot(festuca$pH, festuca$Calluna, festuca$Weight, xlab = "ph",
                 ylab = "weight")


#  Example 3
warpbreaks



as.factor(warpbreaks$wool) -> warpbreaks$wool
as.factor(warpbreaks$tension) -> warpbreaks$tension

str(warpbreaks)


aov(breaks ~ wool + tension + wool:tension, data = warpbreaks) -> wp
wp

summary(wp)


interaction.plot(warpbreaks$wool, warpbreaks$tension, warpbreaks$breaks, 
                 xlab = "wool", ylab = "breaks")
# note that the lines are non-parallel, hence significant interaction.

# Example 4
tribble( ~Gender,   ~Age,   ~Score,
         "boy",    "ten",     4,
         "boy",    "ten",     6,
         "boy",    "ten",     8,
         "girl",   "ten",     4,
         "girl",   "ten",     8,
         "girl",   "ten",     9,
         "boy",    "eleven",  6,
         "boy",    "eleven",  6,
         "boy",    "eleven",  9,
         "girl",   "eleven",  7,
         "girl",   "eleven",  10,
         "girl",   "eleven",  13,
         "boy",    "twelve",  8,
         "boy",    "twelve",  9,
         "boy",    "twelve",  13,
         "girl",   "twelve",  12,
         "girl",   "twelve",  14,
         "girl",   "twelve",  16 
)-> V

V   

as.factor(V$Gender) -> V$Gender

as.factor(V$Age) -> V$Age
str(V)

aov(Score ~ Gender + Age + Gender:Age, data = V) ->aovV
aovV

summary(aovV)


interaction.plot(V$Gender, V$Age, V$Score, xlab = "Gender",
                 ylab = "Score")
# The lines are close to being parallel, hence no interaction or very
# weak interaction


#  Classwork

tribble(~genotype,  ~gender,  ~activity,
        "FF", "Female", 3.34,
        "FF", "Female", 4.72,
        "FF", "Female", 3.39,
        "FO", "Female", 4.05,
        "FO", "Female", 5.06,
        "FO", "Female", 3.59,
        "OO", "Female", 4.12,
        "OO", "Female", 3.58,
        "OO", "Female", 4.09,
        "FF", "Male", 2.20,
        "FF", "Male", 2.60,
        "FF", "Male", 5.26,
        "FO", "Male", 2.72,
        "FO",  "Male", 3.28,
        "FO",  "Male", 3.43,
        "OO",  "Male", 3.12,
        "OO",  "Male", 3.74,
        "OO",  "Male", 4.60
        
) -> ac
ac



