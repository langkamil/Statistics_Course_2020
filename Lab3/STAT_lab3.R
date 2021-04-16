#Wczytujemy dane
z31 = read.table("C:\\Users\\Kamil\\Documents\\z31.txt",header = TRUE, sep = "," )
z32 = read.table("C:\\Users\\Kamil\\Documents\\Matematyka sem. V\\Statystyka I\\Laboratorium\\L3\\z32.txt", header = TRUE, sep = "," )
z33 = read.table("C:\\Users\\Kamil\\Documents\\Matematyka sem. V\\Statystyka I\\Laboratorium\\L3\\z33.txt", header = TRUE, sep = "," )
z34 = read.table("C:\\Users\\Kamil\\Documents\\Matematyka sem. V\\Statystyka I\\Laboratorium\\L3\\z34.txt", header = TRUE, sep = "," )

#Liczymy za pomoca wbudowanych funkcji
z31_p = cor(z31$Number.of.people.who.drowned.by.falling.into.a.pool,z31$films.Nicolas.Cage.appeared.in) 
z32_p = cor(z32$Number.of.people.who.died.by.becoming.tangled.in.their.bedsheets, z32$Per.capita.cheese.consumption)
z33_p = cor(z33$Uranium.stored.at.US.nuclear.power.plants, z33$Math.doctorates.awarded)
z34_p = cor(z34$X, z34$Y)

z31_s = cor(z31$Number.of.people.who.drowned.by.falling.into.a.pool,z31$films.Nicolas.Cage.appeared.in, method = "spearman") 
z32_s = cor(z32$Number.of.people.who.died.by.becoming.tangled.in.their.bedsheets, z32$Per.capita.cheese.consumption, method = "spearman")
z33_s = cor(z33$Uranium.stored.at.US.nuclear.power.plants, z33$Math.doctorates.awarded, method = "spearman")
z34_s = cor(z34$X, z34$Y, method = "spearman")


#funkcja do liczenia sredniej arytmetycznej
sr = function(v) { 
  
  suma = 0 
  n = 0
  wynik = 0
  
  for (k in 1:15) { 
    
    if (is.na(v[k])) {
      
      n = k - 1       
      break
 
    }
    
    else {
      
      suma = suma + v[k]
      print (suma)
      
    }
  }
  
  wynik = suma/n
  return (wynik)
}

#funkcja do liczenia wspolczynnika korelacji Pearsona
pearson = function(u,v) {
  
 sr_u = sr(u)
 sr_v = sr(v)
 r = 0
 r_licznik = 0 
 r_mianownik_u = 0 
 r_mianownik_v = 0 
 
  for (i in 1:15) {
    
    if (is.na(u[i]) || is.na(v[i])){
      
      break
    }
    
    else {
      
      r_licznik = r_licznik + (u[i] - sr_u) * (v[i] - sr_v)
      r_mianownik_u = r_mianownik_u + (u[i] - sr_u)^2
      r_mianownik_v = r_mianownik_v + (v[i] - sr_v)^2 
      
    }
  }
 
  r = r_licznik / sqrt(r_mianownik_v * r_mianownik_u )
  return(r)
}




z31_p_w = pearson(z31$Number.of.people.who.drowned.by.falling.into.a.pool,z31$films.Nicolas.Cage.appeared.in)
z32_p_w = pearson(z32$Number.of.people.who.died.by.becoming.tangled.in.their.bedsheets,z32$Per.capita.cheese.consumption)
z33_p_w = pearson(z33$Uranium.stored.at.US.nuclear.power.plants,z33$Math.doctorates.awarded)
z34_p_w = pearson(z34$X,z34$Y)


rangax1 = rank(z31$Number.of.people.who.drowned.by.falling.into.a.pool)
rangay1 = rank(z31$films.Nicolas.Cage.appeared.in)
n1 = nrow(z31)
z31_s_w = 1-(6*sum((rangax1-rangay1)^2))/(n1*(n1^2-1))


#z32
rangax2 = rank(z32$Number.of.people.who.died.by.becoming.tangled.in.their.bedsheets)
rangay2 = rank(z32$Per.capita.cheese.consumption)
n2 = nrow(z32)
z32_s_w = 1-(6*sum((rangax2-rangay2)^2))/(n2*(n2^2-1))


#z33
rangax3 = rank(z33$Uranium.stored.at.US.nuclear.power.plants)
rangay3 = rank(z33$Math.doctorates.awarded)
n3 = nrow(z33)
z33_s_w = 1-(6*sum((rangax3-rangay3)^2))/(n3*(n3^2-1))


#z34
rangax4 = rank(z34$X)
rangay4 = rank(z34$Y)
n4 = nrow(z34)
z34_s_w = 1-(6*sum((rangax4-rangay4)^2)) / (n4*(n4^2-1))




#Porownanie 

porownanie = data.frame(Dane = seq(1:4), pearson_wlasna = seq(1:4), pearson_wbud = seq(1:4), spearman_wlasna = seq(1:4), spearman_wbud = seq(1:4))
porownanie$Dane[1] = "z31"
porownanie$Dane[2] = "z32"
porownanie$Dane[3] = "z33"
porownanie$Dane[4] = "z34"

porownanie$pearson_wlasna[1] = z31_p_w
porownanie$pearson_wlasna[2] = z32_p_w
porownanie$pearson_wlasna[3] = z33_p_w
porownanie$pearson_wlasna[4] = z34_p_w
  
porownanie$pearson_wbud[1] =   z31_p
porownanie$pearson_wbud[2] =   z32_p
porownanie$pearson_wbud[3] =   z33_p
porownanie$pearson_wbud[4] =   z34_p
  
porownanie$spearman_wlasna[1] =   z31_s_w
porownanie$spearman_wlasna[2] =   z32_s_w
porownanie$spearman_wlasna[3] =   z33_s_w
porownanie$spearman_wlasna[4] =   z34_s_w
  
porownanie$spearman_wbud[1] =   z31_s
porownanie$spearman_wbud[2] =   z32_s
porownanie$spearman_wbud[3] =   z33_s
porownanie$spearman_wbud[4] =   z34_s

porownanie  

