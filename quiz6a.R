#### Preamble ####
# Purpose: Simulation, Test and Visulization
# Author: Kelly Lyu
# Date: 13 February 2024
# Contact: kelly.lyu@mail.utoronto.ca
# Pre-requisites: None

### Simulation

# Load library
library(MASS) 
library(ggplot2)
install.packages("ggplot2")

# Set seed for reproducibility
set.seed(74)

# Define parameters
n_day <- 10
n_hour <- 8
mean_customer <- 6 
stad_dev <- 1 
correlation <- 0.6

# Simulate efficiencies for 10 days
efficiencies <- mvrnorm(n = n_day * n_hour, mu = c(mean_customer, mean_customer), 
                        Sigma = matrix(c(1, correlation, correlation, 1), ncol = 2) * stad_dev)

# Creating a data frame
efficiency_dataf <- data.frame(Day = rep(1:n_day, each = n_hour),
                            Hour = rep(9:16, times = n_day),
                            Employee1 = efficiencies[,1],
                            Employee2 = efficiencies[,2])

# Adjusting efficiencies to integers
efficiency_dataf$Employee1 <- round(efficiency_dataf$Employee1)
efficiency_dataf$Employee2 <- round(efficiency_dataf$Employee2)

# Print the first few rows
head(efficiency_dataf)

### Test

# Test 1: Test hour range (9 to 16)
test_hour_range <- all(efficiency_dataf$Hour >= 9 & efficiency_dataf$Hour <= 16)
print(test_hour_range)

# Test 2: Test if there are exactly two employees
test_two_employees <- ncol(efficiency_dataf) == 4 # Day, Hour, Employee1, Employee2
print(test_two_employees)

# Test 3: Test for correlation existence
correlation_coefficient <- cor(efficiency_dataf$Employee1, efficiency_dataf$Employee2)
test_correlation_exists <- correlation_coefficient > 0 # Assumes positive correlation
print(test_correlation_exists)

# Test 4: Test whether efficiency is an integer
test_efficiency_integer <- all(efficiency_dataf$Employee1 == round(efficiency_dataf$Employee1)) &
  all(efficiency_dataf$Employee2 == round(efficiency_dataf$Employee2))
print(test_efficiency_integer)

# Test 5: Test if the mean efficiency is within expected bounds
mean_efficiency_within_bounds <- all(mean(efficiency_dataf$Employee1) >= (mean_customer - stad_dev) &
                                       mean(efficiency_dataf$Employee1) <= (mean_customer + stad_dev)) &
  all(mean(efficiency_dataf$Employee2) >= (mean_customer - stad_dev) &
        mean(efficiency_dataf$Employee2) <= (mean_customer + stad_dev))
print(mean_efficiency_within_bounds)

### Visulization

# Plotting efficiencies for one day to visualize the correlation
ggplot(efficiency_dataf[efficiency_dataf$Day == 1, ], aes(x = Hour)) +
  geom_line(aes(y = Employee1, colour = "Employee 1")) +
  geom_line(aes(y = Employee2, colour = "Employee 2")) +
  labs(title = "Employee Efficiency on Day 1",
       x = "Hour of the Day",
       y = "Number of Customers Served") +
  scale_colour_manual("", 
                      breaks = c("Employee 1", "Employee 2"),
                      values = c("Employee 1" = "blue", "Employee 2" = "red")) +
  theme_minimal()


