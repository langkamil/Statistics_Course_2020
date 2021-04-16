CTG = function(N, M, s, w){
  
  X = rep(1,N) #przygotowanie listy dla liczb losowych
  
  for (i in 1:N){
    Y = runif(M)  
    odch_stand = sd(Y) 
    X[i] = sum(Y-1/2) * sqrt(M) / (odch_stand * M)
  }
  
  return(X = X*sqrt(w) + s)
}


Box_Muller = function(N, s, w){
  X = rep(1,N)
  for (i in seq(1,N,2)) {
    U = runif(2)
    R = sqrt(-2*log(U[1]))
    teta = 2*pi*U[2]
    X[i] = R*cos(teta)
    X[i+1] = R*sin(teta)
  }
  return(X = X * sqrt(w) + s)
}

#ZADANIE 1

#GENEROWANE DLA (m,v) = (0,1)

c10 = CTG(10,100,0,1)
b10 = Box_Muller(10,0,1)
n10 = rnorm(10,0,1)

c30 = CTG(30,100,0,1)
b30 = Box_Muller(30,0,1)
n30 = rnorm(30,0,1)

c100 = CTG(100,100,0,1)
b100 = Box_Muller(100,0,1)
n100 = rnorm(100,0,sqrt(1))


#GENEROWANE DLA (m,v) = (125,5)

cc10 = CTG(10,100,120,5)
bb10 = Box_Muller(10,120,5)
nn10 = rnorm(10,120,sqrt(5))

cc30 = CTG(30,100,120,5)
bb30 = Box_Muller(30,120,5)
nn30 = rnorm(30,120,sqrt(5))

cc100 = CTG(100,100,120,5)
bb100 = Box_Muller(100,120,5)
nn100 = rnorm(100,120,sqrt(5))


#TEST SHAPIRO-WILKA

#(m,v) = (0,1)
shapiro.test(c10)
shapiro.test(c30)
shapiro.test(c100)

shapiro.test(b10)
shapiro.test(b30)
shapiro.test(b100)

shapiro.test(n10)
shapiro.test(n30)
shapiro.test(n100)

#(m,v) = (125,5)
shapiro.test(cc10)
shapiro.test(cc30)
shapiro.test(cc100)

shapiro.test(bb10)
shapiro.test(bb30)
shapiro.test(bb100)

shapiro.test(nn10)
shapiro.test(nn30)
shapiro.test(nn100)

#TEST KOLMOGOROWA-SMIRNOFFA

#(m,v) = (0,1)
ks.test(c10,'pnorm', mean = 0, sd = 1)
ks.test(c30,'pnorm', mean = 0, sd = 1)
ks.test(c100,'pnorm', mean = 0, sd = 1)

ks.test(b10,'pnorm', mean = 0, sd = 1)
ks.test(b30,'pnorm', mean = 0, sd = 1)
ks.test(b100,'pnorm', mean = 0, sd = 1)

ks.test(n10,'pnorm', mean = 0, sd = 1)
ks.test(n30,'pnorm', mean = 0, sd = 1)
ks.test(n100,'pnorm', mean = 0, sd = 1)


#(m,v) = (125,5)
ks.test(cc10,'pnorm',mean=120,sd = sqrt(5))
ks.test(cc30,'pnorm',mean=120,sd=sqrt(5))
ks.test(cc100,'pnorm',mean=120,sd=sqrt(5))


ks.test(bb10,'pnorm',mean=120,sd=sqrt(5))
ks.test(bb30,'pnorm',mean=120,sd=sqrt(5))
ks.test(bb100,'pnorm',mean=120,sd=sqrt(5))

ks.test(nn10,'pnorm',mean=120,sd=sqrt(5))
ks.test(nn30,'pnorm',mean=120,sd=sqrt(5))
ks.test(nn100,'pnorm',mean=120,sd=sqrt(5))


#HISTOGRAMY I Q-Q WYKRESY 

hist(c10, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(b10, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(n10, main = 'Histogram', xlab = '', ylab = 'czestosc')

hist(c30, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(b30, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(n30, main = 'Histogram', xlab = '', ylab = 'czestosc')

hist(c100, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(b100, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(n100, main = 'Histogram', xlab = '', ylab = 'czestosc')

hist(cc10, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(bb10, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(nn10, main = 'Histogram', xlab = '', ylab = 'czestosc')

hist(cc30, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(bb30, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(nn30, main = 'Histogram', xlab = '', ylab = 'czestosc')

hist(cc100, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(bb100, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(nn100, main = 'Histogram', xlab = '', ylab = 'czestosc')


library('car')
qqPlot(c10, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(b10, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(n10, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')

qqPlot(c30, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(b30, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(n30, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')

qqPlot(c100, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(b100, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(n100, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')

qqPlot(cc10, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(bb10, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(nn10, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')

qqPlot(cc30, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(bb30, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(nn30, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')

qqPlot(cc100, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(bb100, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(nn100, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')


#ZADANIE 2

#iris
iris

shapiro.test(iris$Sepal.Length)
shapiro.test(iris$Sepal.Width)

ks.test(iris$Sepal.Length,'pnorm',mean=mean(iris$Sepal.Length),sd=sd(iris$Sepal.Length))
ks.test(iris$Sepal.Width,'pnorm',mean=mean(iris$Sepal.Width),sd=sd(iris$Sepal.Width))


#Chicken Weights By Feed Type
chickwts

library('dplyr')

#grupowanie 
chickwts %>%
group_keys(feed)

casein = chickwts %>% filter(feed == 'casein')  
horsebean = chickwts %>% filter(feed == 'horsebean')  
linseed = chickwts %>% filter(feed == 'linseed')  
meatmeal = chickwts %>% filter(feed == 'meatmeal')  
soybean = chickwts %>% filter(feed == 'soybean')  
sunflower = chickwts %>% filter(feed == 'sunflower')  

#analiza calosciowa
shapiro.test(chickwts$weight)
ks.test(chickwts$weight,'pnorm',mean = mean(chickwts$weight),sd = sd(chickwts$weight))

#analiza wg klasyfikacji

shapiro.test(casein$weight)
ks.test(casein$weight,'pnorm',mean=mean(casein$weight),sd=sd(casein$weight))  

shapiro.test(horsebean$weight)
ks.test(horsebean$weight,'pnorm',mean=mean(horsebean$weight),sd=sd(horsebean$weight)) 

shapiro.test(linseed$weight)
ks.test(linseed$weight,'pnorm',mean=mean(linseed$weight),sd=sd(linseed$weight))  

shapiro.test(meatmeal$weight)
ks.test(meatmeal$weight,'pnorm',mean=mean(meatmeal$weight),sd=sd(meatmeal$weight))  

shapiro.test(soybean$weight)
ks.test(soybean$weight,'pnorm',mean=mean(soybean$weight),sd=sd(soybean$weight))  

shapiro.test(sunflower$weight)
ks.test(sunflower$weight,'pnorm',mean=mean(sunflower$weight),sd=sd(sunflower$weight))  


#HISTOGRAMY 

#iris
hist(iris$Sepal.Length, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(iris$Sepal.Width, main = 'Histogram', xlab = '', ylab = 'czestosc')

#chickwts
hist(chickwts$weight, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(casein$weight, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(horsebean$weight, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(linseed$weight, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(meatmeal$weight, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(soybean$weight, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(sunflower$weight, main = 'Histogram', xlab = '', ylab = 'czestosc')

#Q-Q

qqPlot(iris$Sepal.Length, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(iris$Sepal.Width, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(chickwts$weight, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(casein$weight, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(horsebean$weight, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(linseed$weight, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(meatmeal$weight, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(soybean$weight, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(sunflower$weight, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')



#ZADANIE 3

#N = 10
w05 = rweibull(10, shape = 0.5, scale = 1)
w1 = rweibull(10, shape = 1, scale = 1)
w2 = rweibull(10, shape = 2, scale = 1)
w34 = rweibull(10, shape = 3.4, scale = 1)

#N = 30 
ww05 = rweibull(30, shape = 0.5, scale = 1)
ww1 = rweibull(30, shape = 1, scale = 1)
ww2 = rweibull(30, shape = 2, scale = 1)
ww34 = rweibull(30, shape = 3.4, scale = 1)

#N = 100
www05 = rweibull(100, shape = 0.5, scale = 1)
www1 = rweibull(100, shape = 1, scale = 1)
www2 = rweibull(100, shape = 2, scale = 1)
www34 = rweibull(100, shape = 3.4, scale = 1)

#test 1
ks.test(w05, 'pnorm', mean=mean(w05), sd=sd(w05))
ks.test(w1, 'pnorm', mean=mean(w1), sd=sd(w1))
ks.test(w2, 'pnorm', mean=mean(w2), sd=sd(w2))
ks.test(w34, 'pnorm', mean=mean(w34), sd=sd(w34))

#test 2

ks.test(ww05, 'pnorm', mean=mean(ww05), sd=sd(ww05))
ks.test(ww1, 'pnorm', mean=mean(ww1), sd=sd(ww1))
ks.test(ww2, 'pnorm', mean=mean(ww2), sd=sd(ww2))
ks.test(ww34, 'pnorm', mean=mean(ww34), sd=sd(ww34))

#test 3
ks.test(www05, 'pnorm', mean=mean(www05), sd=sd(www05))
ks.test(www1, 'pnorm', mean=mean(www1), sd=sd(www1))
ks.test(www2, 'pnorm', mean=mean(www2), sd=sd(www2))
ks.test(www34, 'pnorm', mean=mean(www34), sd=sd(www34))

#histogramy i wykresy q-q 

hist(w05, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(w1, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(w2, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(w34, main = 'Histogram', xlab = '', ylab = 'czestosc')

hist(ww05, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(ww1, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(ww2, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(ww34, main = 'Histogram', xlab = '', ylab = 'czestosc')

hist(www05, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(www1, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(www2, main = 'Histogram', xlab = '', ylab = 'czestosc')
hist(www34, main = 'Histogram', xlab = '', ylab = 'czestosc')

qqPlot(w05, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(w1, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(w2, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(w34, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
 

qqPlot(ww05, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(ww1, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(ww2, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(ww34, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')

qqPlot(www05, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(www1, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(www2, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
qqPlot(www34, distribution = 'norm', xlab = 'kwantyle teoretyczne' , ylab = 'kawantyle z próby')
