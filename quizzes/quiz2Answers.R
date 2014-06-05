cube <- function(x, n) {
        x^3
}

x <- 1:10
if(x > 5) {
        x <- 0
}

#What will be returned?
# z <- 10
# f(3)
f <- function(x) {
        g <- function(y) {
                y + z
        }
        z <- 4
        x + g(x)
}

# What is value of y after expression is evaluated?
x <- 5
y <- if(x < 3) {
        NA
} else {
        10
}