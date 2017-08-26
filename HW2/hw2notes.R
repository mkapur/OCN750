type.two = function(a,h,r){
  ((a*r)/(1+a*h*r))
}
type.two(1,2,3)

qes = seq(0,10,1)

for (n in 1:length(qes)) {
  if (n == 1){
    plot(type.two(qes[n],0.05,x))
    curve(type.two(qes[n+1],0.05,x), add = TRUE)
  }
  else {
    curve(type.two(qes[n],0.05,x), add = TRUE)
  }
}

for (i in 2:10){
plot(type.two(0.05,qes[i],x), type = "l")
curve(type.two(0.05,qes[i+1],x), add = TRUE)
curve(type.two(qes[i],1,x), add = TRUE)
curve(type.two(qes[i+1],0.05,x), add = TRUE)}

curve(type.two(0.05,4,x), add = TRUE)
curve(type.two(0.05,5,x), add = TRUE)

plot(type.two(qes[2], 0.05,x), type = "l")
curve(type.two(3, 0.05,x), add = TRUE)
curve(type.two(0.05,4,x), add = TRUE)
curve(type.two(4,5,x), add = TRUE)

library(ggplot2)
#set up vector values for t (attack rate) and h (handling time)
#we will test these against r (prey density)
t = seq(0,10,0.1)
h = seq(0,10,0.1)
x = seq(0,10,0.1)
r = seq(0,10,1)

cbind(t,h)
fortest = function(){
  vart = 0
  varh = 0
  for (i in 1:length(t)){
    vart <<- type.two(t[i],0.05,x[i])
    varh <<- type.two(0.05,h[i],x[i])
  }
  vart
  varh
}


cbind(vart,varh,x)

ggplot(data.frame(x = c(0,100), aes(t,h,r)) +
         stat_function(fun = type.two, color = "red")
)

base = qplot(x, geom = "density") +
  stat_function(fun = type.two, color = "red")

#write the function
type.two = function(t,h,x){
  ((t*x)/(1+t*h*x))
}

t2 <- function(t,h,x){ 
  ((t*x)/(1+t*h*x))
} 

#The trick is to realize that the integrate function returns a list and you only want the 'value' part of that list so it can be Vectorize()-ed.

#Second you construct a matrix using that function:
#The "outer" function creates an array of your input values and applies whatever function to each 
#Essentially you get a matrix with the function applied to each array
mat <- outer( seq(.01, 10, length=100),
              seq(.01, 10, length=100),
              Vectorize( function(x,y) t2(t,h,r) ))

require(reshape2)
mmat <- melt(mat)
str(mmat) # to see the names in the melted matrix
g <- ggplot(mmat, aes(x=Var1, y=Var2, z=value) )
g <- g+stat_contour(aes(col = ..level..), breaks=seq(.1, .9, .1) )
g <- g + scale_colour_continuous(low = "#000000", high = "#000000") # make black
install.packages("directlabels", repos="http://r-forge.r-project.org", type="source")

require(directlabels)
direct.label(g)

require(lattice)
contourplot(mat, at=seq(.1,.9,.1))

####################### Functional Responses #####################################
#FROM https://github.com/mattbarbour34/comprehensive_exams/blob/master/functional_response_code.Rmd
a <- 0.1 # attack rate of predator (all functional response types)
w <- 0.1 # maximum attack rate of predator in Type 2 or 3 functional response
D <- w/a # half saturation constant. Only applicable to Type 2 or 3 functional response. Don't intuitively understand this.

type.two = function(a,w,d){
  ((a*w)/(1+a*w*d))
}

par(mfrow = c(1,2)) # plot 2 graphs side-by-side

# first plot: functional responses
curve(type.two(a[i],w,x), 0, 1, xlab = "Prey Density", ylab = "Prey Killed per Predator") # Type 2
curve(w[6] * x / (D + x), 0, 10, add = TRUE, lty = 2) # Type 2
#curve(w * x^2/ (D^2 + x^2), 0, 2, add = TRUE, lty = 3) # Type 3
```

