#1.
#(a) Assign all these weights as vector 'weight'.
weight <- c(60, 72, 34, 56, 87, 80, 89, 95, 76, 28, 48, 59)
#(b) Compute the mean of 'weight' and of the square of the weight.
mean(weight)
mean(weight^2)
#(c) What is the length of the weight?
length(weight)
#  (d) How many weights are larger than 55?
sum(weight>55)
#  (e) Show if each weight is larger than 55 and smaller than 85.
weight>55 & weight<85

# 2
#(a) Compute the sum of the ﬁrst and third column.
tmp <- matrix(rnorm(12), 3, 4)
tmp[1]+tmp[3]
#(b) Compute the product of the ﬁrst and third row.
tmp[1,]*tmp[3,]
#(c) Show the dimension of the matrix.
dim(tmp)
#(d) Use 'cat' function to output elements in the ﬁrst row that are larger than 0.5.
tmp.row1 = tmp[1,]
cat(tmp.row1[tmp.row1>0.5],'\n')

#3. How would you check whether two vectors are the same if they may contain missing (NA) values? (Use of the identical function is considered cheating!)
x = c(1,2,NA)
y = c(1,NA,2)
all(is.na(x)==is.na(y)) && all((x==y)[!is.na(x)])


#4. If x is a factor with n levels and y is a length n vector, what happens if you compute y[x]?
#It will output the original y vector.

#5
mydna <- paste(sample(c('a','t','c','g'),1000,replace=T),collapse='')
my_function <- function(x) {
  obj = gregexpr('cg',x,perl = T)
  print(length(obj[[1]]))
  print(gsub('cg','XY',x,perl = T))
}
my_function(mydna)
