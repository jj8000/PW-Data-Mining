# Data Mining - zadanie - grupowanie
# Wykonał: Jerzy Jakubowski

library(cluster)
library(factoextra)
library(dplyr)
library(dbscan)
library(fpc)

set.seed(52)

# Wczytanie danych

data = read.csv("Pokemon.csv")

# Opis danych ze źródła

#: ID for each pokemon
# Name: Name of each pokemon
# Type 1: Each pokemon has a type, this determines weakness/resistance to attacks
# Type 2: Some pokemon are dual type and have 2
# Total: sum of all stats that come after this, a general guide to how strong a pokemon is
# HP: hit points, or health, defines how much damage a pokemon can withstand before fainting
# Attack: the base modifier for normal attacks (eg. Scratch, Punch)
# Defense: the base damage resistance against normal attacks
# SP Atk: special attack, the base modifier for special attacks (e.g. fire blast, bubble beam)
# SP Def: the base damage resistance against special attacks
# Speed: determines which pokemon attacks first each round

# Sprawdzenie atrybutów i brakujących danych:
summary(data)
str(data)

# Wybranie atrybutów numerycznych
# Uwaga - nie uwzględniono cechy "Total" - zgodnie z opisem, jest ona sumą innych cech

data_num <- data[, c("HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed", "Generation")]

#Skalowanie danych 

data_num_scaled <- as.data.frame(scale(data_num, center = F))
summary(data_num)
summary(data_num_scaled)

# 25% danych - ilość wierszy

n = nrow(data) * 0.25
sample1 = data_num_scaled[sample(seq_len(nrow(data)), size = n, replace = FALSE), ]


# Metoda "łokcia"

wss1 <- 0
for (i in 1:20)
{
  km.out1 <- kmeans(sample1, centers = i, nstart=20)
  wss1[i] <- km.out1$tot.withinss
  
}

plot(1:20, wss1, type = "b",  xlab = "Liczba grup", ylab = "Suma błędu kwadratowego wewnątrz grup")

# przyjęto 8 grup

sample2 = data_num_scaled[sample(seq_len(nrow(data)), size = n, replace = FALSE), ]
sample3 = data_num_scaled[sample(seq_len(nrow(data)), size = n, replace = FALSE), ]

wss2 <- 0
for (i in 1:20)
{
  km.out2 <- kmeans(sample2, centers = i, nstart=20)
  wss2[i] <- km.out2$tot.withinss
}

plot(1:20, wss2, type = "b",  xlab = "Liczba grup", ylab = "Suma błędu kwadratowego wewnątrz grup")


wss3 <- 0
for (i in 1:20)
{
  km.out3 <- kmeans(sample3, centers = i, nstart=20)
  wss3[i] <- km.out3$tot.withinss
}

plot(1:20, wss3, type = "b",  xlab = "Liczba grup", ylab = "Suma błędu kwadratowego wewnątrz grup")


# Przyjeto 8 grup na podstawie oceny 3 wykresów

# Przeprowadzenie grupowania poszczególnymi algorytmami


kmHW = kmeans(data_num_scaled, centers = 8, nstart = 40)
kmL = kmeans(data_num_scaled, centers = 8, nstart = 40, algorithm = "Lloyd")
kmF = kmeans(data_num_scaled, centers = 8, nstart = 40, algorithm = "Forgy")
kmM = kmeans(data_num_scaled, centers = 8, nstart = 40, algorithm = "MacQueen")


avg_silhouette_score <- function(km) {
  sil = silhouette(km$cluster, dist = dist(data_num_scaled))
  fsil = (fviz_silhouette(sil))
  fsildf = fsil$data
  mean(fsildf$sil_width)
}

km_list = list(kmHW, kmL, kmF, kmM)
sil_list = lapply(km_list, avg_silhouette_score)
# 
# > sil_list
# [[1]]
# [1] 0.1807023
# 
# [[2]]
# [1] 0.1825338
# 
# [[3]]
# [1] 0.1772347
# 
# [[4]]
# [1] 0.1840208

# Zgodnie z indeksem Silhouette najlepsze grupowanie osiągnięto algorytmem MacQueen,
# choć wyniki są zbliżone

# Przypisanie do grup:

data$Cluster = kmM$cluster

cluster_summary <- data %>% group_by(Cluster) %>%   summarise(
  avg_total = mean(Total),
  avg_hp = mean(HP),
  avg_attack = mean(Attack),
  avg_defense = mean(Defense),
  avg_sp_atk = mean(Sp..Atk),
  avg_sp_def = mean(Sp..Def),
  avg_speed = mean(Speed)
)


# Uśredniona charakterystyka każdej z grup:

cluster_summary

# Cluster count avg_total avg_hp avg_attack avg_defense avg_sp_atk avg_sp_def avg_speed
# <int> <int>     <dbl>  <dbl>      <dbl>       <dbl>      <dbl>      <dbl>     <dbl>
#   1       1    34      472.  129.        70.9        65.9       76         81.5      48.4
# 2       2    50      512.   66.8       67.4       114.        85.0      123.       55.1
# 3       3   121      485.   77.5       93.2        75.5       77.1       73.3      88.2
# 4       4   134      317.   53.2       57.1        54.9       49.2       52.3      50.0
# 5       5   143      470.   71.5       82.4        69.5       81.7       79.0      85.4
# 6       6    92      627.   88.4      120.         90.8      129.        97.4     101. 
# 7       7   157      296.   48.5       52.4        50.3       47.7       47.5      49.2
# 8       8    69      495.   75.2      108.        122.        64.4       72.6      52.4


# część 2 - algorytm DBSCAN

# Wyznaczenie parametru eps

kNNdistplot(sample2, k = 8)
abline(h = 0.6, col='darkgoldenrod')
abline(h = 0.7, col='coral')
abline(h = 0.65, col='darkgreen')

# na podstawie wykresu określono par. eps na ~0,65

# dla k = 7: 

kNNdistplot(sample2, k = 7)
abline(h = 0.6, col='darkgoldenrod')
abline(h = 0.7, col='coral')
abline(h = 0.65, col='darkgreen')

kNNdistplot(sample2, k = 9)
abline(h = 0.6, col='darkgoldenrod')
abline(h = 0.7, col='coral')
abline(h = 0.65, col='darkgreen')

kNNdistplot(sample2, k = 10)
abline(h = 0.6, col='darkgoldenrod')
abline(h = 0.7, col='coral')
abline(h = 0.65, col='darkgreen')

# kształty wykresu są bardzo zbliżone, przyjęto eps równe 0,65.

# Przeprowadzenie grupowania przy pomocy DBSCAN dla różnych zestawów parametrów:

dbs1 = dbscan::dbscan(data_num_scaled, eps = 0.60, minPts = 12)
dbs2 = dbscan::dbscan(data_num_scaled, eps = 0.65, minPts = 12)
dbs3 = dbscan::dbscan(data_num_scaled, eps = 0.65, minPts = 20)
dbs4 = dbscan::dbscan(data_num_scaled, eps = 0.70, minPts = 12)

# Wyznaczenie indeksu Silhouette

dbs_list = list(dbs1, dbs2, dbs3, dbs4)
sil_list2 = lapply(dbs_list, avg_silhouette_score)

# cluster size ave.sil.width
# 0       0  106         -0.07
# 1       1  694          0.30
# cluster size ave.sil.width
# 0       0   63         -0.08
# 1       1  737          0.33
# cluster size ave.sil.width
# 0       0   88         -0.08
# 1       1  712          0.30
# cluster size ave.sil.width
# 0       0   47         -0.09
# 1       1  753          0.34

# W każdym przypadku uzyskano tylko dwie grupy. W związku z tym przeprowadzono iterację 
# po nast. wartościach eps i minPts:

eps_values <- seq(0.5, 2, by = 0.1)
minPts_values <- seq(8, 20, by = 1)

dbs_results = data.frame(eps = numeric(0), minPts = numeric(0), clusters = numeric(0), avg_silhouette = numeric(0))

for (i in seq_along(eps_values)) {
  for (j in seq_along(minPts_values)) {
    dbs_ij <- dbscan::dbscan(data_num_scaled, eps = eps_values[i], minPts = minPts_values[j])
    no_of_clusters = length(unique(dbs_ij$cluster))
    silhouette <- cluster.stats(dist(data_num_scaled), dbs_ij$cluster)$avg.silwidth
    dbs_results <- rbind(dbs_results, data.frame(eps = eps_values[i], minPts = minPts_values[j], 
                                                 clusters = no_of_clusters, avg_silhouette = silhouette))
    }
  }


# Wyniki posegregowane wg ilosci grup


head(dbs_results[order(dbs_results$clusters, decreasing = T), ], 20)

#       eps minPts clusters avg_silhouette
# 2  0.5      9        4   -0.045446711
# 1  0.5      8        3    0.009949176
# 3  0.5     10        2    0.158015489
# 4  0.5     11        2    0.153958047
# 5  0.5     12        2    0.150376342
# 6  0.5     13        2    0.148518972
# 7  0.5     14        2    0.148617924
# 8  0.5     15        2    0.147659490
# 9  0.5     16        2    0.144725150
# 10 0.5     17        2    0.140070497
# 11 0.5     18        2    0.136837843
# 12 0.5     19        2    0.134030129
# 13 0.5     20        2    0.133377715
# 14 0.6      8        2    0.263113550
# 15 0.6      9        2    0.259365443
# 16 0.6     10        2    0.256389210
# 17 0.6     11        2    0.250461662
# 18 0.6     12        2    0.249215662
# 19 0.6     13        2    0.241746370
# 20 0.6     14        2    0.238924022

# Wyniki posegregowane wg Silhouette


head(dbs_results[order(dbs_results$avg_silhouette, decreasing = T), ], 20)

#       eps minPts clusters avg_silhouette
# 157 1.7      8        2      0.5901580
# 158 1.7      9        2      0.5901580
# 159 1.7     10        2      0.5901580
# 160 1.7     11        2      0.5901580
# 161 1.7     12        2      0.5901580
# 162 1.7     13        2      0.5901580
# 163 1.7     14        2      0.5901580
# 164 1.7     15        2      0.5901580
# 165 1.7     16        2      0.5901580
# 166 1.7     17        2      0.5901580
# 167 1.7     18        2      0.5901580
# 168 1.7     19        2      0.5901580
# 169 1.7     20        2      0.5901580
# 118 1.4      8        2      0.5732102
# 119 1.4      9        2      0.5732102
# 120 1.4     10        2      0.5732102
# 121 1.4     11        2      0.5732102
# 122 1.4     12        2      0.5732102
# 123 1.4     13        2      0.5732102
# 124 1.4     14        2      0.5732102

# Jak widać, DBSCAN właściwie w ogóle nie dzieli populacji na większą ilość grup. Jednocześnie
# dla większej ilości grup indeks Silhouette jest najniższy. Optymalna Wartosć eps nie pokrywa 
# się też z odczytaną z wykresu.


# dla eps = 1,7 i minPts = 8

dbs_opt = dbscan::dbscan(data_num_scaled, eps = 1.7, minPts = 8)

# Przyporządkowanie do grup:


data$ClusterDBS = dbs_opt$cluster

cluster_summary <- data %>% group_by(ClusterDBS) %>%   summarise(
  count = n(),
  avg_total = mean(Total),
  avg_hp = mean(HP),
  avg_attack = mean(Attack),
  avg_defense = mean(Defense),
  avg_sp_atk = mean(Sp..Atk),
  avg_sp_def = mean(Sp..Def),
  avg_speed = mean(Speed)
)

cluster_summary

# Jak widać, nawet przy optymalnych parametrach, DBS wydziela tylko jeden rekord do oddzielnej grupy. 
# Wyróżnia się on b. wysoką wartością Sp..Def, a niska Sp..Atk.

# Trudno o porównanie metod ze wzgledu na całkowicie odmienne wyniki.

