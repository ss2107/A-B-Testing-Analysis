#libraries needed
require(dplyr)
require(rpart)
require(ggplot2)

setwd('C:\\Users\\Sarvesh\\Desktop\\udemy\\Projects\\AB Testing')

#read data
user = read.csv("user_table.csv")
test = read.csv("test_table.csv")

str(user)
str(test)

# Check the proportion of test and control groups
table(test$test)

# Are there any duplicates? 
length(unique(test$user_id)) == length(test$user_id)
# True, there are duplicate values.

length(user$user_id)
length(test$user_id)

# Is everyone in one table also present in the other table
length(user$user_id) - length(test$user_id)

# We have some userid's missing in the user table

data = merge(test,user, by = "user_id", all.x = TRUE)
data$date = as.Date(data$date)
summary(data)

# Check is Spain converts better than LatinAm?

data_conversion_country = data %>%
                          group_by(country) %>%
                          summarize( conversion = mean(conversion[test == 0])) %>% arrange (desc(conversion))

head(data_conversion_country)

# Yes, Spain has a higher conversion rate than other countries.

#a simple t-test here should work. We have collected ~0.5MM data and test/control split is ~50/50.
data_test = subset(data, country != "Spain") 
#nothing changed in Spain, so no point in keeping those users

t.test(data_test$conversion[data_test$test == 1], data_test$conversion[data_test$test == 0])
# Since the p-value is less than 0.05, we can reject the null hypothesis at 95% confidence interval.

mean(data_test$conversion[data_test$test == 1]) #4.3%
mean(data_test$conversion[data_test$test == 0]) #4.8%

# As per statisical test, the results for both test and control are significant

# The most likely reason for weird A/B test results are:
# 1) We didn't collect enough data.
# 2) Some bias has been introduced in the experiment so that test/control people are not really random.

# Firstly, let's plot day by day, to see if these weird results have been constantly happening or 
# they just started happening all of a sudden.

# For Test
data_test_by_day_control = data_test %>%
                           group_by(date) %>%
                           summarize(test = mean(conversion[test==1]))

# For Control
data_test_by_day_test = data_test %>%
                        group_by(date) %>%
                        summarize(control = mean(conversion[test==0]))

#Test_vs_Control
data_test_by_day = data_test %>%
                   group_by(date) %>%
                   summarize(test_vs_control = mean(conversion[test==1])/mean(conversion[test==0]))

qplot(date, test_vs_control, data= data_test_by_day, geom = "line")

# We can see that Test consistently performs worse than Control. Maybe there is a bias in 
# the experiment

# On a side note, we just ran it for 5 days. We should always run the test for at least 1 full week to
# capture weekly patterns, 2 weeks would be much better.

# Where is the bias? Ideally distribution in Test and Control should be the same.
# We can check using a decision tree. If the tree splits, then the randomization did not work

# we remove conversion. Doesn't matter now.
tree = rpart(test ~ .,data_test[,-8], 
             control = rpart.control(minbucket = nrow(data_test)/100, maxdepth = 2))

# we only look for segments representing at least 1% of the populations
tree

# The randomization is perfect for the countries on one side of the split
# (country=Bolivia, Chile, Colombia, Costa Rica, Ecuador, EL Salvador, Guatemala, Honduras, Mexico,
# Nicaragua, Panama, Paraguay, Peru, Venezuela). Indeed, in that leaf the test/control ratio is 0.498!

# However, Argentina and Uruguay together have 80% test and 20% control! So let's check the test results
# after controlling for country. That is, we check for each country how the test is doing:

# conversion rate (Control) by country
control_country = data_test %>%
                    group_by(country) %>%
                    summarize( test = mean(conversion[test==0]) )

# conversion rate (test) by country
test_country = data_test %>%
                    group_by(country) %>%
                    summarize( test = mean(conversion[test==1]) )

# Both Control and Test by country
data_test_country = data_test %>%
                    group_by(country) %>%
                    summarize( p_value = t.test( conversion[test==1],conversion[test==0])$p.value,
                    conversion_test = t.test( conversion[test==1],conversion[test==0])$estimate[1],
                    conversion_control = t.test( conversion[test==1],conversion[test==0])$estimate[2]
                     ) %>% arrange (p_value)

data_test_country

# After we control for country, the test clearly appears non significant. Not a great success given that the
# goal was to improve conversion rate, but at least we know that a localized translation didn't make things
# worse
