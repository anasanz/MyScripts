
# Design matrix examples (intro to wingbugs book)

mass <- c(6, 8, 5, 7, 9, 11)
pop <- factor(c(1,1,2,2,3,3))
region <- factor(c(1,1,1,1,2,2))
hab <- factor(c(1,2,3,1,2,3))
svl <- c(40, 45, 39, 50, 52, 57)

# Common mean
lm(mass ~ 1)
model.matrix(mass ~ 1)

# Effect of a single binary explanatory variable
lm(mass ~ region)
model.matrix(~ region)


