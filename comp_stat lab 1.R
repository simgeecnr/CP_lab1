#### Question 2

### a)

# Definition of var

my_var <- function(x) {
  
# Define the number of observations in x 
  
  n <- length(x)
  if (n <= 1) {
    stop("Number of observations should be greater
         than 1")

  } else {
# Define the sum of squares of x    
    sum_sq <- sum(x^2)
    
# Define the sum of x    
    sum_x <- sum(x)
    variance_x <- (sum_sq - (1/n) * sum_x^2) / (n - 1)
    return(variance_x)
 }
}
###b)

# Define the given mean and variance
given_mean <- prod(10e8)
given_variance <- 1

# Calculate the standard deviation
std_dev <- sqrt(given_variance)

# Generate the vector x with 10,000 random numbers
set.seed(123)
n <- 10000
x <- rnorm(n, given_mean, std_dev)

# Print the random vector x
print(x)

###c)

# Create an empty vector to store the differences
differences <- numeric(length(x))

# Calculate the differences for each subset Xi
subset_x <- list()
for (i in 1:length(x)) {
  subset_x <- append(subset_x, list(x[1:i]))  # Extract subset Xi = {x1, ..., xi}
  
# Calculate variance using myvar function
myvar_result <- myvar(subset_x)
  
# Calculate variance using var function in R
var_result <- var(subset_x)
  
# Compute the difference Yi = myvar(Xi) - var(Xi)
differences[i] <- myvar_result - var_result
}

# Display the differences
print(differences)

