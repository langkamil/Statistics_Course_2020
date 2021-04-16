#ZADANIE 1
CTG = function(N, M, s, w){
  
  X = rep(1,N) #przygotowanie listy dla liczb losowych
  
  for (i in 1:N){
    
    Y = runif(M)  #losowanie M liczb z rozkladu jendostajnego na przedziale (0,1)
    odch_stand = sd(Y) #odchylenie standardowe niezbedne do dalszych obliczen
    X[i] = sum(Y-1/2) * sqrt(M) / (odch_stand * M) #Na mocy CTG (1/2 to srednia z (0,1))
        }

  return(X = X*sqrt(w) + s) #ze wzoru na standaryzowanie zmiennych losowych o rozkladzie normalnym  
}


CTG(1000, 100, 0, 1)
rnorm(1000)



#ZADANIE 2 

#na podstawie https://pl.wikipedia.org/wiki/Transformacja_Boxa-Mullera
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


#ZADANIE 3

c1 = CTG(10,100,0,1)
bm1 = Box_Muller(10,0,1)
n1 = rnorm(10)

c2 = CTG(100,100,0,1)
bm2 = Box_Muller(100,0,1)
n2 = rnorm(100)

c3 = CTG(10000,100,0,1)
bm3 = Box_Muller(10000,0,1)
n3 = rnorm(10000)


# histogramy dla N=10 dla trzech metod
hist(c1, main = "Histogram CTG N=10")
hist(bm1, main = "Histogram Box-Muller N=10")
hist(n1, main = "Histogram rnorm N=10")

#histogramy dla N=100 dla trzech metod
hist(c2,main = "Histogram CTG N=100")
hist(bm2, main = "Histogram Box-Muller N=100")
hist(n2,main = "Histogram rnorm N=100")

#histogramy dla N=10000 dla trzech metod
hist(c3,main = "Histogram CTG N=10000")
hist(bm3,main = "Histogram Box-Muller N=10000")
hist(n3,main = "Histogram rnorm N=10000")

#srednia
porownanie_srednie = data.frame(wartosc_sredniej = seq(1,2), CTG_N_10 = seq(1:2), BM_N_10 = seq(1:2),
                                rnorm_N_10 = seq(1:2),CTG_N_100 =seq(1:2), BM_N_100= seq(1:2), rnorm_N_100=seq(1:2),
                                CTG_N_10000 = seq(1:2), BM_N_10000 =seq(1:2), rnorm_N_10000 =seq(1:2)) 

porownanie_srednie$wartosc_sredniej[1] = 'teoretyczna'
porownanie_srednie$wartosc_sredniej[2] = 'wyznaczona'

porownanie_srednie$CTG_N_10[1] = 0
porownanie_srednie$BM_N_10[1] = 0
porownanie_srednie$rnorm_N_10[1] = 0
porownanie_srednie$CTG_N_100[1] = 0
porownanie_srednie$BM_N_100[1] = 0
porownanie_srednie$rnorm_N_100[1] = 0
porownanie_srednie$CTG_N_10000[1] = 0
porownanie_srednie$BM_N_10000[1] = 0
porownanie_srednie$rnorm_N_10000[1] = 0

porownanie_srednie$CTG_N_10[2] = mean(c1)
porownanie_srednie$BM_N_10[2] = mean(bm1)
porownanie_srednie$rnorm_N_10[2] = mean(n1)
porownanie_srednie$CTG_N_100[2] = mean(c2)
porownanie_srednie$BM_N_100[2] = mean(bm2)
porownanie_srednie$rnorm_N_100[2] = mean(n2)
porownanie_srednie$CTG_N_10000[2] = mean(c3)
porownanie_srednie$BM_N_10000[2] = mean(bm3)
porownanie_srednie$rnorm_N_10000[2] = mean(n3)

show(porownanie_srednie)


#wariancja 
porownanie_wariancja = data.frame(wartosc_wariancji = seq(1,2), CTG_N_10 = seq(1:2), BM_N_10 = seq(1:2),
                                rnorm_N_10 = seq(1:2),CTG_N_100 =seq(1:2), BM_N_100= seq(1:2), rnorm_N_100=seq(1:2),
                                CTG_N_10000 = seq(1:2), BM_N_10000 =seq(1:2), rnorm_N_10000 =seq(1:2)) 

porownanie_wariancja$wartosc_wariancji[1] = 'teoretyczna'
porownanie_wariancja$wartosc_wariancji[2] = 'wyznaczona'


porownanie_wariancja$CTG_N_10[2] = var(c1)
porownanie_wariancja$BM_N_10[2] = var(bm1)
porownanie_wariancja$rnorm_N_10[2] = var(n1)
porownanie_wariancja$CTG_N_100[2] = var(c2)
porownanie_wariancja$BM_N_100[2] = var(bm2)
porownanie_wariancja$rnorm_N_100[2] = var(n2)
porownanie_wariancja$CTG_N_10000[2] = var(c3)
porownanie_wariancja$BM_N_10000[2] = var(bm3)
porownanie_wariancja$rnorm_N_10000[2] = var(n3)

show(porownanie_wariancja)

#ZADANIE 4

procent = function(X, a, b){ 
  
  licznik = 0 
  
  for (i in 1:length(X)){
    if (X[i] >= a && X[i] <= b){
      licznik = licznik + 1
    }
  } 
  return(licznik*100/length(X))
}



#regula 3 sigm dla ctg

sigmy = data.frame( przedzial = seq(1:3), wartosc_teoretyczna = seq(1:3),
                        CTG_N_10 = seq(1:3), BM_N_10 = seq(1:3),
                        rnorm_N_10 = seq(1:3),CTG_N_100 =seq(1:3),
                        BM_N_100= seq(1:3), rnorm_N_100=seq(1:3),
                        CTG_N_10000 = seq(1:3), BM_N_10000 =seq(1:3), rnorm_N_10000 =seq(1:3))

sigmy$przedzial[1] = '[miu - sigma, miu + sigma]'
sigmy$przedzial[2] = '[miu - 2sigma, miu + 2sigma]'
sigmy$przedzial[3] = '[miu - 3sigma, miu + 3sigma]'

sigmy$wartosc_teoretyczna[1] = 68.27
sigmy$wartosc_teoretyczna[2] = 95.45
sigmy$wartosc_teoretyczna[3] =  99.73

sigmy$CTG_N_10[1] = procent(c1, mean(c1) - sd(c1), mean(c1)+sd(c1))
sigmy$CTG_N_10[2] = procent(c1, mean(c1) - 2*sd(c1), mean(c1) +2*sd(c1))
sigmy$CTG_N_10[3] = procent(c1,mean(c1) - 3*sd(c1), mean(c1)+ 3*sd(c1))

sigmy$CTG_N_100[1] = procent(c2, mean(c2) - sd(c2), mean(c2)+sd(c2))
sigmy$CTG_N_100[2] = procent(c2, mean(c2) - 2*sd(c2), mean(c2) +2*sd(c2))
sigmy$CTG_N_100[3] = procent(c2,mean(c2) - 3*sd(c2), mean(c2)+ 3*sd(c2))

sigmy$CTG_N_10000[1] = procent(c3, mean(c3) - sd(c3), mean(c3)+sd(c3))
sigmy$CTG_N_10000[2] = procent(c3, mean(c3) - 2*sd(c3), mean(c3) +2*sd(c3))
sigmy$CTG_N_10000[3] = procent(c3,mean(c3) - 3*sd(c3), mean(c3)+ 3*sd(c3))


sigmy$BM_N_10[1] = procent(bm1, mean(bm1) - sd(bm1), mean(bm1)+sd(bm1))
sigmy$BM_N_10[2] = procent(bm1, mean(bm1) - 2*sd(bm1), mean(bm1) +2*sd(bm1))
sigmy$BM_N_10[3] = procent(bm1,mean(bm1) - 3*sd(bm1), mean(bm1)+ 3*sd(bm1))

sigmy$BM_N_100[1] = procent(bm2, mean(bm2) - sd(bm2), mean(bm2)+sd(bm2))
sigmy$BM_N_100[2] = procent(bm2, mean(bm2) - 2*sd(bm2), mean(bm2) +2*sd(bm2))
sigmy$BM_N_100[3] = procent(bm2,mean(bm2) - 3*sd(bm2), mean(bm2)+ 3*sd(bm2))

sigmy$BM_N_10000[1] = procent(bm3, mean(bm3) - sd(bm3), mean(bm3)+sd(bm3))
sigmy$BM_N_10000[2] = procent(bm3, mean(bm3) - 2*sd(bm3), mean(bm3) +2*sd(bm3))
sigmy$BM_N_10000[3] = procent(bm3, mean(bm3) - 3*sd(bm3), mean(bm3)+ 3*sd(bm3))


sigmy$rnorm_N_10[1] = procent(n1, mean(n1) - sd(n1), mean(n1)+sd(n1))
sigmy$rnorm_N_10[2] = procent(n1, mean(n1) - 2*sd(n1), mean(n1) +2*sd(n1))
sigmy$CTG_N_10[3] = procent(n1,mean(n1) - 3*sd(n1), mean(n1)+ 3*sd(n1))

sigmy$rnorm_N_100[1] = procent(n2, mean(n2) - sd(n2), mean(n2)+sd(n2))
sigmy$rnorm_N_100[2] = procent(n2, mean(n2) - 2*sd(n2), mean(n2) +2*sd(n2))
sigmy$rnorm_N_100[3] = procent(n2,mean(n2) - 3*sd(n2), mean(n2)+ 3*sd(n2))

sigmy$rnorm_N_10000[1] = procent(n3, mean(n3) - sd(n3), mean(n3)+sd(n3))
sigmy$rnorm_N_10000[2] = procent(n3, mean(n3) - 2*sd(n3), mean(n3) +2*sd(n3))
sigmy$rnorm_N_10000[3] = procent(n3,mean(n3) - 3*sd(n3), mean(n3)+ 3*sd(n3))

sigmy

