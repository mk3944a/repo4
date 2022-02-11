library(ggplot2)
library(tidyverse)
library(dplyr)

# Data Transformation using dplyr

# Filtering  (selecting observations using their values)   filter

#Lets examine the diamonds data set
diamonds

?diamonds


# Choose only observations with a depth of 65.1

diamonds1<-filter(diamonds, depth==65.1)
diamonds1

# Choose only observations with a depth of 65.1 and an x value of 6.29
# (different column variables)
diamonds2<-filter(diamonds, depth==65.1 , x==6.29)
diamonds2

# Choose only observations with a depth of 65.1 or a depth of 62.3
# (same column variables)
diamonds3<-filter(diamonds,depth==65.1|depth==62.3)
diamonds3 

#Produce more than the default 10 rows.

print(diamonds3,  n=25)

#Choose only observations that have a depth that is greater than 50
diamonds4<-filter(diamonds, depth > 50)
diamonds4

#Choose only observations that have a color of E and a price that is 
#less than 338

diamonds5<-filter(diamonds, color == "E", price < 338 )
diamonds5

#Choose only observations that have a premium cut, a clarity of WS1,
#and a length that is greater than or equal to 3
 
diamonds6<-filter(diamonds, cut =="Premium" , clarity == "VVS1" , 
                  x >= 3)
diamonds6

#Choose only observations that have x values between 2 and 6. Print 30 rows.

diamonds77 <- filter(diamonds, x<6 & x>2)
diamonds77
print(diamonds77 , n=30)


# Arrange (changing the order of columns) 

#List the values of price from highest to lowest
diamonds7<- arrange(diamonds,  desc(price))
diamonds7


#List the values of price from lowest to highest. Print the first 20 rows
diamonds8 <- arrange (diamonds, price)
diamonds8
print(diamonds8, n=20)

#A comment about missing values
#Missing values are sorted at the end. The sort maybe ascending or
#descending


#Ascending
df<- tibble(x = c(5, 2, NA))
arrange(df, x)


#Descinding
ef<- tibble(x = c(5, 2, NA))
arrange(df, desc(x))


# Select  (Choosing columns)   

# From the diamonds data set choose only columns carat, cut, and
# price
diamonds

diamonds9 <- select(diamonds, carat, cut, price)
diamonds9

# Change the order of the selection to price, cut, and then carat

diamonds10 <- select(diamonds, price, cut, carat)
diamonds10


# Deselect the columns x,y, and z
diamonds11 <- select(diamonds,  -x, -y, -z)
diamonds11

#Mutate  (Adding columns to your data frame)

#Create a narrower data frame from diamonds
diamonds12<-select(diamonds, clarity,cut,price)
diamonds12

#Now add a variable column that decreases every price by 50
mutate(diamonds12,  dp = price-50)-> diamonds13
diamonds13

#Now add a varible that gives a ratio of dp to price
diamonds14<-mutate(diamonds13, ratio = (dp/price))
diamonds14

#Lets increase the output observations
print(diamonds14, n=50)


#Lets change the name of an observation item (We will capitalize Ideal
#of the cut column)

diamonds14

mutate(diamonds14, cut = recode(cut, "Ideal"= "IDEAL"))->
diamonds15
diamonds15




# Transforming a data frame using the pipping process
# pipe operator  %>%

#Exampke 1
#From the diamonds data frame choose the variables price, x and y

diamonds%>%
  select(price,x,y)


#Example 2
#From the diamonds data frame, create a data frame that shows the 
#variables price and carat, but for only price values in descending
#order.  Print the first 20 rows. 
diamonds%>%
  select(price,carat)%>%
  arrange(desc(price))%>%
  print(n=20)


#Example 3
#From the diamonds data frame, create a data frame that shows the 
#variables price, carat and cut, but for only price values in descending
#order that are less than or equal to 400.  Print the first 15 rows.

diamonds%>%
  select(price,carat,cut)%>%
  filter(price<=400)%>%
  arrange(desc(price))%>%
  print(n=15)



#Example 4
#From the diamonds data frame, create a data frame that shows the 
#all variables except x, y, and z, but for only price values in descending
#order that are between 18000 and 18500 and only for cuts that are Very Good.
# Print the first 25 observations
diamonds%>%
  select(-x, -y, -z)%>%
  filter(price < 18500, price >18000)%>%
  filter(cut == "Very Good")%>%
  arrange(desc(price))%>%
  print(n=25)


# lets compare methods for using dplyr functions to modify a data
#frame

# Example 1

# From the diamonds data frame choose only price values that are greater than 17000

# assigning to a variable
diamonds
k <-filter( diamonds,  price > 17000)
k

# Using the pipe process

diamonds %>%
  filter( price > 17000)


# Example 2

#From the diamonds data frame, create a data frame that shows the 
#variables price, carat and cut, but for only price values in descending
#order that are less than or equal to 400.  Print the first 15 rows.

#  Assigning Method

diamonds
A <- select(diamonds, price, carat, cut)
A
diamonds
B <- filter(A, price <= 400)
B
diamonds
C <- arrange(B, desc(price))
C  
print (C,  n = 15)


# Pipping Method

diamonds%>%
  select(price,carat,cut)%>%
  filter(price<=400)%>%
  arrange(desc(price))%>%
  print(n=15)




#Group by and Summarize

AA <-tribble(
  ~Name,      ~Gender,    ~PolParty,        ~Salary,  ~Age,
  "Ron",      "Male",       "Dem",           45000,     22,
  "Mary",     "Female",     "Rep",           53000,     25,
  "Juan",     "Male",       "Dem",           58000,     27,
  "Lindsy",   "Female",     "Rep",           50500,     30,
  "Abdul",    "Male",       "Dem",           61200,     32,
  "Leon",     "Male",       "Rep",           57200,     28,
  "Alice",    "Female",     "Dem",           60200,     25
)
AA

#Two ways to find the mean of a variable in a data frame

summary(AA)

summarise(AA, meansalary = mean(Salary))

# Now let's use the commands group by / summarize to find summary
#statistics for a levels of a categorical variable

# (I want the mean salaries for men and women separately)

AA %>%
  group_by(Gender) %>%
  summarise(MeanSalary = mean(Salary))


# (Now I want the mean salaries for men and women grouped by political party)

AA %>%
  group_by(Gender , PolParty) %>%
  summarise(MeanSalary = mean(Salary))


# Another Example


# For the mpg data frame let's find the average city miles per gallon
# for ford vehicles only.

mpg

mpg%>%
  select (manufacturer, cty) %>%
  filter (manufacturer == "ford")%>%
  print(n = 25)
  

mpg%>%
  select(manufacturer , cty)%>%
   group_by(manufacturer == "ford") %>%
  summarise(AverageCityMileage = mean(cty))



#Textbook Example
install.packages("nycflights13")
library(nycflights13)

flights
?flights
Viewflights


by_day <- group_by(flights , year , month , day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))
by_day




flights%>%
  select(year,month,day, dep_delay)%>%
  group_by(year, month , day)%>%
  summarize(delay = mean(dep_delay , na.rm = TRUE) ) 


q()
y






