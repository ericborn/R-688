# Creates a sequence of five evenly spaced numbers
# starting from 5 ending on 25
x1 <- seq(5, 25, by=5)

# calculates the mean except the element in index 2
mean(x1[-2])

# Creates a datafrme of x1 and a vector of numbers
columns <- data.frame("one" = x1, "two" = c(2, 1, 4, 7, 8))

# creates a new list using the columns df and a logical vector
my.list <- list(columns, c(T, F, NA, T, F))

my.list[[2]]$one


my.list[[1]]$two


# lapply
lapply(my.list[1], '[',sum)



lapply(my.list[[1]]$one, my.list[[1]]$two, sum)



lapply(my.list[1], '[', 1, )

