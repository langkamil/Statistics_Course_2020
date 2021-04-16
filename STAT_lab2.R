#KAMIL LANGOWSKI
#176553


#tabela dla losowania
zliczone = data.frame(nr_serii = seq(1:1000), tarcza_1_trafienia = seq(1:1000),
                      tarcza_2_trafienia = seq(1:1000), tarcza_3_trafienia = seq(1:1000),
                      suma_punktow = seq(1:1000), wylosowany_punkt_1 = seq(1:1000), wylosowany_punkt_2 = seq(1:1000))
#tabela podsumowujaca
podsumowanie = data.frame(Prawd. = seq(1:4), seria.1 = seq(1:4), seria.2 = seq(1:4)
                          , seria.3 = seq(1:4))

podsumowanie$Prawd.[1] = "P(pkt >= 3)"
podsumowanie$Prawd.[2] = "P(traf. tarcza1)"
podsumowanie$Prawd.[3] = "P(traf. tarcza2)"
podsumowanie$Prawd.[4] = "P(traf. tarcza3)"

w = 0
x = 0
y = 0
z = 0
 
#losowanie
for (k in 3:1){
  
licznik = 0

for (i in 1:1000){
  
  punkt_1 = 0
  punkt_2 = 0 
  punkt_3 = 0
  suma_punktów = 0
  
  N = runif(2,0,1)
  
  for (j in 1:2){ 
    #uderzenie w tarcze 1
    if (N[j] <= 1/9) {
      
      punkt_3 = punkt_3 + 3
      
    }
    #uderzenie w tarcze 2
    else if (N[j] > 1/9 && N[j] <= 4/9) {
      punkt_2 = punkt_2 + 2
    }
    #uderzenie w tarcze 3
    else if (N[j] > 4/9 && N[j] <= 1){ 
      punkt_1 = punkt_1 + 1
      
    }
  }
    
    suma_punktów = punkt_1 + punkt_2 + punkt_3
    
  
    zliczone$tarcza_1_trafienia[i] = punkt_3/3
    zliczone$tarcza_2_trafienia[i] = punkt_2/2
    zliczone$tarcza_3_trafienia[i] = punkt_1
    
    zliczone$wylosowany_punkt_1[i] = N[1]
    zliczone$wylosowany_punkt_2[i] = N[2]
    zliczone$suma_punktow[i] = suma_punktów
    
    #uzyskane co najmniej 3 pkt w jednej serii rzutow
    if (zliczone$suma_punktow[i]  >= 3) {
      licznik = licznik + 1
    }

}
#prawd. uzyskania co najmniej 3 pkt
w[k] = round(licznik/1000,digits = 4)

#prawd trafienia w 1 tarcze
x[k] = round(sum(zliczone$tarcza_1_trafienia)/2000, digits = 4)

#prawd trafienia w 2 tarcze
y[k] = round(sum(zliczone$tarcza_2_trafienia)/2000, digits = 4)

#prawd trafienia w 3 tarcze
z[k] = round(sum(zliczone$tarcza_3_trafienia)/2000, digits = 4)
}


zliczone

podsumowanie$seria.1[1] = w[1]
podsumowanie$seria.1[2] = x[1]
podsumowanie$seria.1[3] = y[1]
podsumowanie$seria.1[4] = z[1]

podsumowanie$seria.2[1] = w[2]
podsumowanie$seria.2[2] = x[2]
podsumowanie$seria.2[3] = y[2]
podsumowanie$seria.2[4] = z[2]

podsumowanie$seria.3[1] = w[3]
podsumowanie$seria.3[2] = x[3]
podsumowanie$seria.3[3] = y[3]
podsumowanie$seria.3[4] = z[3]

print(podsumowanie)

hist(zliczone$suma_punktow, xlab = "suma_pkt", ylab = "czestosc", xlim = c(2,6))

