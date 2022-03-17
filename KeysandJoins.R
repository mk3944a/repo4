
library(tidyverse)
library(nycflights13)
library(dplyr)
#Chapter 10 / Relational Data

# We will explore relations between pairs of data tables. In order to
# to do so we will use and analyze data tables from the nycfilghts13
# package and other standard data tables that we have analyzed so
# far

# The variables used to connect pairs of tables are called keys. A key
# is a variable (or set of variables) that uniquely identifies an
# observation of a table.

# Consider the data table planes.  Note that for the variable tailnum,
# all observations are different(unique). Hence the variable tailnum
# is a key.

planes

# Also, note that the variable model of the planes data set is not a
# key, for certain observational elements are repeated.

# We now consider a formal coding technique to determine if a 
# variable (or set of variables) is a key without eyeballing the 
# table.

# Example 1  We show formally that tailnum is a key for the data set
# planes

# We  formally show that tailnum is a key for the planes data 
# table by using  the following R coding chunk.
planes %>%
  count(tailnum)%>%
  filter(n>1)

# Tailnum is a key for the planes data table for the output shows
#  tibble: 0 X 2 ( The 0 tells us that you do not have more than
# 1 observation for each tailnumber designation,) All tailnumber
# entries are different


# Example 2  We show formally that the variable model is not a
# key for the data set planes. Consider the following R coding 
# sequence

planes %>%
  count(model)%>%
  filter(n>1)

# Note that the output shows that the entries for the variable model
# are not all different, for example the model 717-200 appears 88 
# times.  Also, the output message  A tibble 79 X 2 tells us that we
# have 79 model observations that have more than 1 designation.

# Example 3
# We now verify that the variable carrier is a key for the
# table airlines

airlines

airlines %>%
  count(carrier)%>%
  filter(n>1)

#Example 4
# Recall that a key can be a set or collection of variables.
# Confirm that the collection of variables(year, month, day, hour, 
# origin, humid) is a key for the table weather

weather


weather %>%
  count( origin, wind_gust)%>%
  filter(n>1)


weather %>%
  count(year, month, day, hour, origin, humid)%>%
  filter(n>1)

# There are two types of keys ;
  # A primary key uniquely identifies an observation in its own
  # table

  # A foreign key uniquely identifies an observation in another
  # table.
# The variable tailnum is a primary key for the data table planes,
# for it uniquely identifies each plane in the planes table, but
# tailnum is a foreign key for the table flights for it identifies
# a unique observation in the planes table.

planes

planes%>%
  count(tailnum)%>%
  filter(n>1)


flights

flights%>%
  count(tailnum)%>%
  filter(n>1)


# Mutating Joins
# We can combine variables from two tables by executing a Mutating
# Join

# In order to illustrate the different types of mutating joins, we 
# consider two basic data tables that we will call x  and  y.

# The two tables are created below,  Run each table construction to
# view the profile of each table.

x<- tribble(
  ~num,  ~color,
     1,  "red",
     2,  "red",
     3,  "blue"
)
x

y<- tribble(
  ~num,  ~color,
     1,  "blue",
     2,  "green",
     4,  "blue"
)
y

# Note that for each table, the key is the variable num.

# Example 5
# We will first combine the two tables x and y by executing an
# "inner join".  An inner join matches pairs of  observations from
# the tables whenever their keys are equal.


x%>%
  inner_join(y, by ="num")

#Note that you have observations for the key(num) elements 1  and  2.
# that both tables have in common.

# Example 6
# We will now combine the two tables x and y by 
# executing a "left join".  A left join keeps all of the
# observations in x.

x%>%
  left_join(y, by ="num")

# Example 7
# We will now combine the two tables x and y by 
# executing an "right join".  A right join keeps all of the
# observations in y.

x%>%
  right_join(y, by ="num")

# Example 8
# We will now combine the two tables x and y by 
# executing a "full join". A full join keeps all of the
# observations in x and y.

x%>%
  full_join(y, by ="num")


# Example 9
# Use R coding to show that the variable country is a key for the
# data sets table 4a and table 4b

table4a

table4a%>%
  count(country)%>%
  filter(n>1)

table4b

table4b%>%
  count(country)%>%
    filter(n>1)

# Now perform a left join on the tables 4a and 4b (Note that the
# key is country for both tables)

table4a%>%
  left_join(table4b ,  by ="country")



table4a%>%
  full_join(table4b ,  by ="country")



# Duplicate Keys

# We now perform joins involving tables with duplicate keys. 
# (Duplicate Keys: Observations across the table are unique, 
# but you have repetition in the key column)  

tablex <- tribble(
  ~key,  ~val_x,
     1,   "x1",
     2,   "x2",     # note that for tablex, you have unique observational rows, but duplication
     2,   "x3",     # in the key column
     1,   "x4"
)
tablex


tabley <- tribble(
  ~key,  ~val_y,
  1,   "y1",
  2,   "y2"
)
tabley

# We now execute a left join on the tables

tablex%>%
left_join(tabley, by = "key")

# Note that we can use alternative R coding to get the same left join result

left_join(tablex , tabley,  by = "key")


# What type of left join representation is obtained when both tables have duplicate keys ?
# (this will be a homework problem)


# Let's take a look at the data tables q and z  

q<- tribble(                   
  ~num,  ~color, ~direction,                 
     1,  "red",    "north",                    
     2,  "red",    "south",                
     3,  "blue",    "east"                   
)                                
q                       # We can join tables by only outputting  the observations that they have in    
                        # common.  Such a join is called a semi join.  The anti join will give you
z<- tribble(            # the opposite output.That is, the observations that they do not have in 
~num,  ~color,   ~direction,         # common
   1,  "blue",   "west",
   2,  "red",    "south",
   4,  "blue",   "north"
)
z

q%>%
  semi_join(z)

q%>%
  anti_join(z)

# Let's revisit the tables q and z  
q<- tribble(                   
  ~num,  ~color, ~direction,                 
  1,  "red",    "north",                    
  2,  "red",    "south",                
  3,  "blue",    "east"                   
)                                
q


z<- tribble(            
  ~num,  ~color,   ~direction,        
  1,  "blue",   "west",
  2,  "red",    "south",
  4,  "blue",   "north"
)
z

# Optional coding for joining or merging tables

intersect(q,z)  # we will produce a table that has the observations common to both  (semi_join)
union(q,z)   # we will produce a table that shows all observations (the intersect observation is
             # shown only once however)
setdiff(q,z) # observations in q that are not in z
setdiff(z,q) # observations in z that are not in q



q()
y



