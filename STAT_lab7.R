#ZADANIE 1

kujaw <- c(175,168,168,190,156,181,182,175,174,179)
zawisza <- c(185,169,173,173,188,186,175,174,179,180)

#TESTOWANIE

shapiro.test(kujaw)
shapiro.test(zawisza)

var.test(kujaw,zawisza)
t.test(kujaw,zawisza)


#ZADANIE 2
data(mtcars)
require(tidyverse)
man <- mtcars$mpg[mtcars$am=="1"] 
aut <- mtcars$mpg[mtcars$am=="0"]

#TESTY
shapiro.test(man)
shapiro.test(aut)
var.test(man,aut)
t.test(man,aut)

#ZADANIE 3
data(chickwts)
chick <- chickwts$weight
mt <- chickwts$weight[chickwts$feed=="meatmeal"]
cs <- chickwts$weight[chickwts$feed=="casein"]
sy <- chickwts$weight[chickwts$feed=="soybean"]
su <- chickwts$weight[chickwts$feed=="sunflower"]
li <- chickwts$weight[chickwts$feed=="linseed"]
ho <- chickwts$weight[chickwts$feed=="horsebean"]

shapiro.test(mt)
shapiro.test(cs)
shapiro.test(sy)
shapiro.test(su)
shapiro.test(li)
shapiro.test(ho)

var.test(mt,cs)
t.test(mt,cs)

var.test(mt,su)
t.test(mt,su)

var.test(cs,su)
t.test(cs,su)

var.test(cs,li)
t.test(cs,li)

var.test(su,li)
t.test(su,li)

var.test(mt,li)
t.test(mt,li)




