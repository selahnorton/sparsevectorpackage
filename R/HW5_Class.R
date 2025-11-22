# HW5 Class/Methods

#' Define a sparse numeric vector class
#'
#' Define a sparse numeric vector class, meaning that it stores non-zero numeric values in a vector, their given positions, and the length of the vector.
#'
#'
#' @slot value The non zero numbers in the vector
#' @slot pos The position/index of these non zeroes numbers in the vector
#' @slot length The total length of the vector
#' @return Sparse numeric vector
#' @examples
#' x <- new("sparse_numeric", value = c(4,3.2, 6.1), pos=c(2L, 3L, 5L), length = 50L)
#' @exportClass sparse_numeric
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#' Check the validity of the sparse numeric vector
#'
#' Ensure that the sparse numeric vector, only contains numbers, does not only contain zeroes, and has an appropriate set length
#'
#' @name sparse_numeric_validity

setValidity(
  Class = "sparse_numeric",
  method = function(object){
    if (!is.numeric(object@value)){
      return("error: value must be numeric")
    }
    if (length(object@pos) == 0) {
      return("error: all zero vector")
    }
    if (object@length < max(object@pos)){
      return("error: set length is not appropriate ")
    }
    TRUE
  }
)

# coerce numeric to sparse_numeric and vice versa

#' Convert numeric to sparse
#'
#' Convert numeric vector into sparse numeric vector format
#'
#' @param from Numeric vector
#' @return Sparse numeric vector
#' @examples
#' x <- c(0,0,0,0,3,0,0)
#' as(x, "sparse_numeric")
#'
#' @importFrom methods new
#' @name sparse_numeric_setas_s
setAs(
  from = "numeric",
  to = "sparse_numeric",
  def = function(from){
    # extract the numbers in the vector that are not 0
    num_pos <- which(from != 0)
    num_val <- from[num_pos]
    new("sparse_numeric",
        value = num_val,
        pos = as.integer(num_pos),
        length = length(from))
  }
)
#' Convert sparse to numeric
#'
#' Convert sparse numeric vector format into numeric vector
#'
#'
#' @param from Sparse Numeric vector
#' @return Numeric vector
#' @examples
#' x <- new("sparse_numeric", value = c(4,3.2, 6.1), pos=c(2L, 3L, 5L), length = 50L)
#' as(x, "numeric")
#'
#' @name sparse_numeric_setas_n

setAs(
  from = "sparse_numeric",
  to = "numeric",
  def = function(from){
    # make result the length of vector, filling the values of their given position
    result <- numeric(from@length)
    result[from@pos] <- from@value
    result
  }
)

# generic functions

#' Add two sparse vectors together
#'
#' Generic function that adds two sparse numeric vectors, into one resulting vector
#'
#'
#' @param x Sparse Numeric Vector
#' @param y Sparse Numeric Vector
#' @return Sparse numeric vector as a sum of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(1L, 7L), length = 10L)
#' sparse_add(x,y)
#' @exportMethod sparse_add

setGeneric(
  "sparse_add",
  def = function(x,y){
    standardGeneric("sparse_add")
  }
)

#' Subtract two sparse vectors together
#'
#' Generic function that subtracts two sparse numeric vectors, into one resulting vector
#'
#'
#' @param x Sparse Numeric Vector
#' @param y Sparse Numeric Vector
#' @return Sparse numeric vector as a difference of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(1L, 7L), length = 10L)
#' sparse_sub(x,y)
#' @exportMethod sparse_sub

setGeneric(
  "sparse_sub",
  def = function(x,y){
    standardGeneric("sparse_sub")
  }
)

#' Multiples two sparse vectors together
#'
#' Generic function that multiplies two sparse numeric vectors, into one resulting vector
#'
#'
#' @param x Sparse Numeric Vector
#' @param y Sparse Numeric Vector
#' @return Sparse numeric vector as a product of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 7L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(5L, 7L), length = 10L)
#' sparse_mult(x,y)
#' @exportMethod sparse_mult

setGeneric(
  "sparse_mult",
  def = function(x,y){
    standardGeneric("sparse_mult")
  }
)


#' Cross product of two sparse vectors
#'
#' Generic function that returns the cross product of two sparse numeric vectors, into one resulting number
#'
#'
#' @param x Sparse Numeric Vector
#' @param y Sparse Numeric Vector
#' @return Resulting cross product
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(1L, 7L), length = 10L)
#' sparse_crossprod(x,y)
#' @exportMethod sparse_crossprod


setGeneric(
  "sparse_crossprod",
  def = function(x,y){
    standardGeneric("sparse_crossprod")
  }
)

#' Division of two sparse vectors
#'
#' Generic function that returns the quotient of two sparse numeric vectors, into one resulting vector
#'
#'
#' @param x Sparse Numeric Vector
#' @param y Sparse Numeric Vector
#' @return Sparse numeric vector as a quotient of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 7L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(5L, 7L), length = 10L)
#' sparse_div(x,y)
#' @exportMethod sparse_div

setGeneric(
  "sparse_div",
  function(x, y) {
    standardGeneric("sparse_div")
  }
)

#' Norm of a sparse numeric vector
#'
#' Generic function that returns the norm of the entire vector
#'
#'
#' @param x Sparse Numeric Vector
#' @return Resulting norm
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' norm(x)
#' @exportMethod norm

setGeneric(
  "norm",
  function(x) {
    standardGeneric("norm")
  }
)

#' Standardizes a sparse numeric vector
#'
#' Generic function that standardizes the entire vector
#'
#'
#' @param x Sparse Numeric Vector
#' @return Standardized Sparse Numeric Vector
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' standardize(x)
#' @exportMethod standardize

setGeneric(
  "standardize",
  function(x){
    standardGeneric("standardize")
  }
)

# specific method functions

#' Add two sparse vectors together
#'
#' Method that adds two sparse numeric vectors, by merging them and performing addition on non-zeroes, returning a vector that is the sum
#'
#' @param x Sparse numeric vector
#' @param y Sparse numeric vector
#' @return Sparse numeric vector as a sum of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(1L, 7L), length = 10L)
#' sparse_add(x,y)
#' @importFrom methods new
#' @exportMethod sparse_add

setMethod(
  "sparse_add",
  c("sparse_numeric", "sparse_numeric"),
  function(x,y){
    # check to make sure they are the same length
    if (x@length!= y@length)
      stop("vectors are not the same length")
    # convert to data frames to enable merging
    df_x <- data.frame(pos = x@pos, value = x@value)
    df_y <- data.frame(pos = y@pos, value = y@value)
    # outer join
    df_both <- merge(df_x, df_y, by = "pos", all=TRUE)
    # replace na values with 0
    df_both[is.na(df_both)] <- 0
    # add values of the same position
    df_both$result <- df_both$value.x + df_both$value.y
    # get rid of elements that added to 0
    df_both <- df_both[df_both$result  != 0,]
    # result vector
    new("sparse_numeric",
        value = df_both$result,
        pos = as.integer(df_both$pos),
        length = (x@length))
  }
)

#' Subtracts two sparse vectors together
#'
#' Method that subtracts two sparse numeric vectors, by merging them and performing subtraction on non-zeroes, returning a vector that is the difference
#'
#' @param x Sparse numeric vector
#' @param y Sparse numeric vector
#' @return Sparse numeric vector as a difference of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(1L, 7L), length = 10L)
#' sparse_sub(x,y)
#' @importFrom methods new
#' @exportMethod sparse_sub

setMethod(
  "sparse_sub",
  c("sparse_numeric", "sparse_numeric"),
  function(x,y){
    # check to make sure they are the same length
    if (x@length!= y@length)
      stop("vectors are not the same length")
    # convert to data frames to enable merging
    df_x <- data.frame(pos = x@pos, value = x@value)
    df_y <- data.frame(pos = y@pos, value = y@value)
    # outer join
    df_both <- merge(df_x, df_y, by = "pos", all=TRUE)
    # replace na values with 0
    df_both[is.na(df_both)] <- 0
    # subtract values of the same position
    df_both$result <- df_both$value.x - df_both$value.y
    # get rid of elements that subtracted to 0
    df_both <- df_both[df_both$result  != 0,]
    # result vector
    new("sparse_numeric",
        value = df_both$result,
        pos = as.integer(df_both$pos),
        length = (x@length))
  }
)

#' Multiplies two sparse vectors together
#'
#' Method that multiplies two sparse numeric vectors, by merging them and performing multiplication on non-zeroes, returning a vector that is the product
#'
#' @param x Sparse numeric vector
#' @param y Sparse numeric vector
#' @return Sparse numeric vector as a product of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 7L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(5L, 7L), length = 10L)
#' sparse_mult(x,y)
#' @importFrom methods new
#' @exportMethod sparse_mult

setMethod(
  "sparse_mult",
  c("sparse_numeric", "sparse_numeric"),
  function(x,y){
    # check to make sure they are the same length
    if (x@length!= y@length)
      stop("vectors are not the same length")
    # convert to data frames to enable merging
    df_x <- data.frame(pos = x@pos, value = x@value)
    df_y <- data.frame(pos = y@pos, value = y@value)
    # outer join
    df_both <- merge(df_x, df_y, by = "pos", all=TRUE)
    # replace na values with 0
    df_both[is.na(df_both)] <- 0
    # multiply values of the same position
    df_both$result <- df_both$value.x * df_both$value.y
    df_both <- df_both[df_both$result != 0, ]
    # result vector
    new("sparse_numeric",
        value = df_both$result,
        pos = as.integer(df_both$pos),
        length = (x@length))
  }
)
#'
#' Cross product of two sparse vectors
#'
#' Method that computes the cross product of two sparse numeric vectors, by merging them and performing multiplication on non-zeroes and then summing them all up, returning one number that is the cross product
#'
#'
#' @param x Sparse Numeric Vector
#' @param y Sparse Numeric Vector
#' @return Resulting cross product
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(1L, 7L), length = 10L)
#' sparse_crossprod(x,y)
#' @exportMethod sparse_crossprod

setMethod(
  "sparse_crossprod",
  c("sparse_numeric", "sparse_numeric"),
  function(x,y){
    # check to make sure they are the same length
    if (x@length!= y@length)
      stop("vectors are not the same length")
    # convert to data frames to enable merging
    df_x <- data.frame(pos = x@pos, value = x@value)
    df_y <- data.frame(pos = y@pos, value = y@value)
    # join
    df_both <- merge(df_x, df_y, by = "pos")
    # multiply values of the same position
    df_both$result <- df_both$value.x * df_both$value.y
    # add up to get cross product
    result <- sum(df_both$result)
    # result vector
    result
  }
)

#' Divides one sparse vector by another
#'
#' Method that divides one sparse numeric vector by another, by merging them and performing division on non-zeroes, returning a vector that is the quotient
#'
#' @param x Sparse numeric vector
#' @param y Sparse numeric vector
#' @return Sparse numeric vector as a quotient of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 7L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(5L, 7L), length = 10L)
#' sparse_div(x,y)
#' @importFrom methods new
#' @exportMethod sparse_div

setMethod(
  "sparse_div",
  c("sparse_numeric", "sparse_numeric"),
  function(x,y){
    # check to make sure they are the same length
    if (x@length!= y@length)
      stop("vectors are not the same length")
    # convert to data frames to enable merging
    df_x <- data.frame(pos = x@pos, value = x@value)
    df_y <- data.frame(pos = y@pos, value = y@value)
    # join
    df_both <- merge(df_x, df_y, by = "pos")
    # divide values of the same position
    df_both$result <- df_both$value.x / df_both$value.y
    # result vector
    new("sparse_numeric",
        value = df_both$result,
        pos = as.integer(df_both$pos),
        length = (x@length))
  }
)

#' Calculates the mean of a sparse numeric vector
#'
#' Method that computes the mean of a sparse numeric vector, by summing up the non-zero elements and dividing by the length of the vector, returning a number that is the mean
#'
#'
#' @param x Sparse numeric vector
#' @return Resulting mean of the sparse numeric vector
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' mean(x)
#' @exportMethod mean


setMethod(
  "mean",
  "sparse_numeric",
  function(x){
    sum(x@value)/x@length
  }
)

#' Norm of a sparse numeric vector
#'
#' Method that computes the norm of the entire vector, by squaring each non-zero x value and then summing them together, and afterwards performing the squareroot, which results in the norm of the entire vector
#'
#'
#' @param x Sparse Numeric Vector
#' @return Resulting norm
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' norm(x)
#' @exportMethod norm
setMethod(
  "norm",
  "sparse_numeric",
  function(x){
    x_2 <- x@value^2
    sqrt(sum(x_2))
  }
)

#' Standardizes a sparse numeric vector
#'
#' Method that standardizes the entire vector, by taking each element of the vector and subtracting off the vector mean and dividing by the vector's standard deviation
#'
#'
#' @param x Sparse Numeric Vector
#' @return Standardized Sparse Numeric Vector
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' standardize(x)
#' @importFrom methods new
#' @exportMethod standardize


setMethod(
  "standardize",
  "sparse_numeric",
  function(x){
    mean <- mean(x)
    sd_1_2 <- (x@value - mean)^2
    sd_1 <- sum(sd_1_2)
    # can't just use x@value because this excludes the zeroes
    zeroes <- x@length - length(x@value)
    sd_2_2 <- (mean)^2
    sd_2 <- sd_1 + sd_2_2*zeroes
    sd <- sqrt(sd_2/x@length)
    stand <- (x@value - mean)/sd
    # result vector
    new("sparse_numeric",
        value = stand,
        pos = x@pos,
        length = (x@length))
  }
)

# methods for signs: +, -, *

#' Add two sparse vectors together using +
#'
#' Method that performs addition with + operator by calling sparse_add
#'
#' @param e1 Sparse numeric vector
#' @param e2 Sparse numeric vector
#'
#' @return Sparse numeric vector as a sum of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(1L, 7L), length = 10L)
#' z <- x + y
#' z
#'@exportMethod "+"

setMethod(
  "+",
  signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
  function(e1, e2){
    sparse_add(e1, e2)
  }
)


#' Subtract two sparse vectors together using -
#'
#' Method that performs subtraction with - operator by calling sparse_sub
#'
#' @param e1 Sparse numeric vector
#' @param e2 Sparse numeric vector
#'
#' @return Sparse numeric vector as a difference of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(1L, 7L), length = 10L)
#' z <- x - y
#' z
#' @exportMethod "-"

setMethod(
  "-",
  signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
  function(e1, e2){
    sparse_sub(e1, e2)
  }
)

#' Multiply two sparse vectors together using *
#'
#' Method that performs multiplication with * operator by calling sparse_mult
#'
#' @param e1 Sparse numeric vector
#' @param e2 Sparse numeric vector
#'
#' @return Sparse numeric vector as a product of the two parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 7L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(5L, 7L), length = 10L)
#' z <- x * y
#' z
#' @exportMethod "*"
setMethod(
  "*",
  signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
  function(e1, e2){
    sparse_mult(e1, e2)
  }
)

# method for show and plot

#' Show a sparse numeric vector
#'
#' Method that prints the information of a sparse numeric vector, including the non-zeros values, their positions, and the length of the vector
#'
#' @param object Sparse numeric vector
#'
#' @return Printed sparse numeric vector parameters
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 9L), length = 10L)
#' x

setMethod(
  "show",
  "sparse_numeric",
  function(object){
    cat("Values: ", object@value, "\n")
    cat("Positions of non-zero values: ", object@pos, "\n")
    cat("Vector length: ", object@length, "\n")
  }
)

#' Plot sparse numeric vectors
#'
#' Method that plots the information of two sparse numeric vectors, including the non-zeros values, their positions, and the length of the vector with the positions on the x-axis, and non-zeros values on the y-axis, each vector represented by a different color
#'
#' @param x Sparse numeric vector
#' @param y Sparse numeric vector
#'
#' @return Plotted sparse numeric vectors
#' @examples
#' x <- new("sparse_numeric", value = c(5,7), pos=c(5L, 7L), length = 10L)
#' y <- new("sparse_numeric", value = c(3,8), pos=c(5L, 7L), length = 10L)
#' plot(x,y)
#' @importFrom graphics points
#' @exportMethod plot


setMethod(
  "plot",
  c("sparse_numeric","sparse_numeric"),
  function(x,y){
    # check to make sure they are the same length
    if (x@length!= y@length)
      stop("vectors are not the same length")
    # convert to data frames to enable merging
    df_x <- data.frame(pos = x@pos, value = x@value)
    df_y <- data.frame(pos = y@pos, value = y@value)
    # outer join
    df_both <- merge(df_x, df_y, by = "pos")
    # find y range
    if (min(df_both$value.x) < min(df_both$value.y)){
      low_y <- min(df_both$value.x)
    } else {
      low_y <- min(df_both$value.y)
    }
    if (max(df_both$value.x) > max(df_both$value.y)){
      high_y <- max(df_both$value.x)
    } else {
      high_y <- max(df_both$value.y)
    }
    y_range <- c(low_y, high_y)
    plot(df_both$pos, df_both$value.x, col = "red", type="p", pch=16, ylim=y_range, xlab="position", ylab="value")
    points(df_both$pos, df_both$value.y, col = "blue", pch=16)
  }
)
