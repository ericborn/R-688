# Creates a sequence of five evenly spaced numbers
# starting from 5 ending on 25
x1 <- seq(5, 25, by=5)

# calculates the mean except the element in index 2
mean(x1[-2])

# Creates a datafrme of x1 and a vector of numbers
columns <- data.frame("one" = x1, "two" = c(2, 1, 4, 7, 8))

# creates a new list using the columns df and a logical vector
My.list <- list(columns, c(T, F, NA, T, F))

# lapply to sum element wise across rows of the dataframe
# function finds value in column 1 and column 2 then adds them
f.sum = function(x, output) {
  first = My.list[[1]][[1]]
  second = My.list[[1]][[2]]
  
  result = first + second
}

# using the newly created function across the rows with lapply
lapply(My.list[ 1 ], FUN=f.sum)

# here is a solution using regular apply
apply(My.list[[ 1 ]], 1, FUN=function(x) c(sum(x)))

# This also works, but is not accomplished using lapply
rowSums(My.list[[ 1 ]])
