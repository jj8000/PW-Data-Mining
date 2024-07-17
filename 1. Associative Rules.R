# Data mining - reguły asocjacyjne
# Wykonał: Jerzy Jakubowski, gr. 4

# Cel eksperymentów: Wykrycie interesujących reguł asocjacyjnych

# Potencjalne wykorzystanir odkrytych reguł, na podstawie opisu danych (sklep internetowy): 

# - sugerowanie klientom produktów czesto kupowanych razem na podstawie zawartości koszyka/historii przeglądania; 
#   także łączenie takich produktów w pakiety promocyjne oraz odpowiednie rozmieszcznie ich na stronie sklepu

# - dbanie o stany magazynowe - zwłaszcza popularnych produktów; ponadto wszystkie produkty czesto kupowane razem 
#   powinny być w miarę możliwości zawsze dostępne w danym momencie. Dodatkowo, z racji na częste zakupy hurtem, 
#   produktów często kupowane w ten sposób powinny być dostępne w odpowiednio dużych ilościach
# 
# - wycofywanie/ograniczanie ilości produktów, na który jest mały popyt
# 
# - planowanie dostaw
# 
# - planowanie reklam / kampanii marketingowych


# Potencjalnie interesujące reguły:
# 
# - reguły o wysokim wsparciu i zaufaniu - produkty, których te reguły dotyczą, powinny być priorytetem pod
# kątem odpowiedniego zaopatrzenia w nie sklepu

# - reguły o niskim wsparciu i wysokim wsp. podniesienia - produkty powinny być dostępne jednocześnie. Ponadto tego
#  typu produkty jako niszowe mogą być dostępne na specjalne zamówienie, jeżeli jest to uzasadnione 
# (np. ze względu na koszty magazynowania)

# - reguły o wysokim współczynniku podniesienia - produkty zawarte w takich regułach powinny być zawsze dostępne 
#   jednocześnie oraz sugerowane klientom łącznie

# Najlepsze reguły to te o najwyższych wartościach ww. parametrów lub, w przypadku reguł dot. produktów niszowych,
# o wsparciu nie wyższym od pewnej wartości.

library(arules)
library(readxl)
library(dplyr)

#Wczytanie danych

online_retail = read_excel("online_retail_II.xlsx")

summary(online_retail)
str(online_retail)

# Przygotowanie i eksploracja danych

# Dodatkowe informacje o danych ze źródła:

# InvoiceNo: Invoice number. Nominal. A 6-digit integral number uniquely assigned to each transaction. 
# If this code starts with the letter 'c', it indicates a cancellation. 
# StockCode: Product (item) code. Nominal. A 5-digit integral number uniquely assigned to each distinct product. 
# Description: Product (item) name. Nominal. 
# Quantity: The quantities of each product (item) per transaction. Numeric.	
# InvoiceDate: Invice date and time. Numeric. The day and time when a transaction was generated. 
# UnitPrice: Unit price. Numeric. Product price per unit in sterling (Â£). 
# CustomerID: Customer number. Nominal. A 5-digit integral number uniquely assigned to each customer. 
# Country: Country name. Nominal. The name of the country where a customer resides.

# Wartości brakujące
sum(is.na(online_retail)) # 110855

# Zdecydowana większość braków występuje w ID klienta. Tę kolumnę można usunąć, 
# numer klienta jest w przypadku odkrywania reguł zbędną informacją

online_retail <- subset(online_retail, select = -`Customer ID`)

# Po usunięciu:
sum(is.na(online_retail)) # 2928
sum(is.na(online_retail$Description)) # 2928

# Wszystkie pozostałe braki w kol. Description, w < 1% obserwacji. Obserwacje usunięto.
online_retail <- online_retail[!is.na(online_retail$Description), ]
sum(is.na(online_retail)) # 0

# Występują 3 pozycje z ujemnymi cenami, opisane jako "Adjust bad debt". Usunięto.

online_retail <- online_retail[online_retail$Price >=0, ]

# Zgodnie z opisem danych, faktury o numerach zaczynających się na "C" zostały anulowane. Dane usunięto.

online_retail <- online_retail[!grepl("^C", online_retail$Invoice), ]

# Usunięto koszty pocztowe

online_retail <- online_retail[!(grepl("POST", online_retail$Description) | grepl("POST", online_retail$StockCode)), ]

# Kolumny 'StockCode' i 'Description' - jedna z nich może być zbędna, gdyż są to zapewne 
# dwa różne sposoby na opisanie unikalnego przedmiotu. Sprawdzenie poniżej:

length(unique(online_retail$StockCode))
length(unique(online_retail$Description))

# Powyższe wartości nie są równe. Znalezienie przypadków z wielokrotnymi opisami tego samego kodu poniżej.

multi_stockcodes <- online_retail %>% group_by(StockCode) %>% filter(n_distinct(Description) > 1) 
multi_stockcodes

# Dalsze przekształcenia - złączono opisy w celu łatwiejszej inspekcji:

multi_stockcodes <- multi_stockcodes %>%  summarise(c_Descriptions = paste(unique(Description), collapse = ', '))
multi_stockcodes

multi_descriptions <- online_retail %>% group_by(Description) %>% filter(n_distinct(StockCode) > 1) %>%  
  summarise(c_StockCodes = paste(unique(StockCode), collapse = ', ')); multi_descriptions

# Wnioski: 1. niektóre duplikaty wynikają z rozbieżnych (lecz podobnych) opisów, np: PINK SPOTTY BOWL, PINK POLKADOT BOWL
#          2. Wszelkie anomalie (zwroty, uszkodzenia, korekty, pomyłki - zapisano małymi literami, lub 
#             co najwyżej zaczynając od wielkich. W związku z tym opisy ujednolicono, a wiersze z anomaliami usunięto.
#          3. W przypadku rozbieżnych kodów przypisanych do tego samego opisu, prawie wszytskie spowodowane są niekonsekwentnym
#             stosowaniem małych/wielkich liter (np. 84558A, 84558a).

# Na podstawie powyższych wniosków dodkonano przekształceń:

# Usunięcie wierszy w których opis zawiera małe litery lub nie zawiera żadnych liter (np. opis jako "???")

online_retail <- online_retail[!grepl(".*[a-z].*", online_retail$Description), ]
online_retail <- online_retail[grepl("[A-Z]", online_retail$Description), ]

# Zamiana liter na wielkie w kolumnie z kodami towarów

online_retail$StockCode <- toupper(online_retail$StockCode)

# Ponowne sprawdzenie

multi_stockcodes <- online_retail %>% group_by(StockCode) %>% filter(n_distinct(Description) > 1) %>%  
  summarise(c_Descriptions = paste(unique(Description), collapse = ', '))

multi_descriptions <- online_retail %>% group_by(Description) %>% filter(n_distinct(StockCode) > 1) %>%  
  summarise(c_StockCodes = paste(unique(StockCode), collapse = ', ')); multi_descriptions

print(multi_descriptions, n = Inf)

# Zauważono kolejny specyficzny przypadek - opis braków jako "MISSING", "MIA". Usunięto.

online_retail <- online_retail[!online_retail$Description %in% c("MISSING", "MIA"), ]

# Ujednolicenie nazw - przyjęcie najczęściej występującej

uniform_descriptions <- online_retail %>% group_by(StockCode) %>% 
  summarise(Description_uniform = names(sort(table(Description), decreasing = TRUE))[1]) %>%
  ungroup()

online_retail = inner_join(online_retail, uniform_descriptions, by = "StockCode")

online_retail[online_retail$Description != online_retail$Description_uniform, ]
  
online_retail[, "Description"] <- online_retail[, "Description_uniform"]
online_retail <- subset(online_retail, select = -Description_uniform)


# Ponowne sprawdzenie

length(unique(online_retail$StockCode))
length(unique(online_retail$Description))

multi_stockcodes <- online_retail %>% group_by(StockCode) %>% filter(n_distinct(Description) > 1) %>%  
  summarise(c_Descriptions = paste(unique(Description), collapse = ', ')); multi_stockcodes


multi_descriptions <- online_retail %>% group_by(Description) %>% filter(n_distinct(StockCode) > 1) %>%  
  summarise(c_StockCodes = paste(unique(StockCode), collapse = ', ')); multi_descriptions

print(multi_descriptions, n = Inf)

# Zgodnie z powyższym, pozostały nieliczne przypadki, w których ten sam przedmiot opisany jest różnymi kodami. 
# Wszystkie wiersze multi_descriptions dotyczą jednak faktycznych przedmiotów. W związku z tym kolumnę StockCode usunięto.

online_retail <- subset(online_retail, select = -StockCode)

# Przekształcenie do postaci transakcyjnej

split_data <- split(unlist(online_retail[, "Description"]), unlist(online_retail[, "Invoice"]))
str(split_data)

transactions <- as(split_data, "transactions")
summary(transactions)

apriori_parameters2 = new("APparameter", "confidence" = 0.5, "support" = 0.01, "minlen"= 2,
                          "target" = "rules") 

rules = apriori(transactions, apriori_parameters) #Stworzono 408 reguł

summary(rules)

# set of 408 rules
# 
# rule length distribution (lhs + rhs):sizes
# 2   3   4 
# 188 208  12 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   2.000   3.000   2.569   3.000   4.000 
# 
# summary of quality measures:
#   support          confidence        coverage            lift            count      
# Min.   :0.01002   Min.   :0.5000   Min.   :0.01089   Min.   : 3.198   Min.   :206.0  
# 1st Qu.:0.01083   1st Qu.:0.5420   1st Qu.:0.01755   1st Qu.: 9.240   1st Qu.:222.8  
# Median :0.01211   Median :0.6015   Median :0.01989   Median :12.801   Median :249.0  
# Mean   :0.01365   Mean   :0.6244   Mean   :0.02248   Mean   :16.613   Mean   :280.6  
# 3rd Qu.:0.01427   3rd Qu.:0.6924   3rd Qu.:0.02433   3rd Qu.:21.237   3rd Qu.:293.5  
# Max.   :0.03871   Max.   :0.9196   Max.   :0.06497   Max.   :63.358   Max.   :796.0  

# mining info:
#   data ntransactions support confidence                                                         call
# transactions         20564    0.01        0.5 apriori(data = transactions, parameter = apriori_parameters)


# usunięcie reguł zbędnych (zawartych w innych o co najmniej takim samym zaufaniu):
rules <- rules[is.redundant(rules) == FALSE] # 2 reguły usunięto

summary(rules)
# set of 406 rules
# 
# rule length distribution (lhs + rhs):sizes
# 2   3   4 
# 188 206  12 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   2.000   3.000   2.567   3.000   4.000 
# 
# summary of quality measures:
#   support          confidence        coverage            lift            count      
# Min.   :0.01002   Min.   :0.5000   Min.   :0.01089   Min.   : 3.198   Min.   :206.0  
# 1st Qu.:0.01081   1st Qu.:0.5419   1st Qu.:0.01755   1st Qu.: 9.228   1st Qu.:222.2  
# Median :0.01208   Median :0.6008   Median :0.01989   Median :12.782   Median :248.5  
# Mean   :0.01365   Mean   :0.6243   Mean   :0.02249   Mean   :16.596   Mean   :280.7  
# 3rd Qu.:0.01432   3rd Qu.:0.6936   3rd Qu.:0.02435   3rd Qu.:21.254   3rd Qu.:294.5  
# Max.   :0.03871   Max.   :0.9196   Max.   :0.06497   Max.   :63.358   Max.   :796.0  
# 
# mining info:
#   data ntransactions support confidence                                                         call
# transactions         20564    0.01        0.5 apriori(data = transactions, parameter = apriori_parameters)

# Wyznaczenie najbardziej interesujących reguł, zgodnie z założeniami:

# Reguły o ponadprzeciętnnych wsparciu i zaufaniu:

mean(rules@quality$support) -> avg_supp
mean(rules@quality$confidence) -> avg_conf

rules[quality(rules)$confidence > avg_conf & quality(rules)$support > avg_supp] # 48 

# lhs                                      rhs                                     support confidence   coverage      lift count
# [1]  {PAINTED METAL PEARS ASSORTED}        => {ASSORTED COLOUR BIRD ORNAMENT}      0.01366466  0.7336815 0.01862478 10.677584   281
# [2]  {SET/10 BLUE SPOTTY PARTY CANDLES}    => {SET/10 PINK SPOTTY PARTY CANDLES}   0.01497763  0.7777778 0.01925695 33.743085   308
# [3]  {SET/10 PINK SPOTTY PARTY CANDLES}    => {SET/10 BLUE SPOTTY PARTY CANDLES}   0.01497763  0.6497890 0.02304999 33.743085   308
# [4]  {SET/6 RED SPOTTY PAPER CUPS}         => {SET/6 RED SPOTTY PAPER PLATES}      0.01522077  0.7279070 0.02091033 31.646256   313
# [5]  {SET/6 RED SPOTTY PAPER PLATES}       => {SET/6 RED SPOTTY PAPER CUPS}        0.01522077  0.6617336 0.02300136 31.646256   313
# [6]  {FELTCRAFT CUSHION RABBIT}            => {FELTCRAFT CUSHION BUTTERFLY}        0.01419957  0.6320346 0.02246645 27.190712   292
# [7]  {KEY FOB , BACK DOOR}                 => {KEY FOB , SHED}                     0.01371329  0.7175573 0.01911107 30.869973   282
# [8]  {SET/6 RED SPOTTY PAPER PLATES}       => {SET/20 RED SPOTTY PAPER NAPKINS}    0.01585295  0.6892178 0.02300136 12.031472   326
# [9]  {ROSES REGENCY TEACUP AND SAUCER}     => {GREEN REGENCY TEACUP AND SAUCER}    0.01434546  0.7468354 0.01920833 40.522227   29
# [...]
# [41] {72 SWEETHEART FAIRY CAKE CASES,                                                                                              
#   PACK OF 60 PINK PAISLEY CAKE CASES}  => {60 TEATIME FAIRY CAKE CASES}        0.01405369  0.7118227 0.01974324 10.956528   289
# [42] {72 SWEETHEART FAIRY CAKE CASES,                                                                                              
#   PACK OF 60 PINK PAISLEY CAKE CASES}  => {PACK OF 72 RETRO SPOT CAKE CASES}   0.01439409  0.7290640 0.01974324  8.090919   296
# [43] {60 TEATIME FAIRY CAKE CASES,                                                                                                 
#   72 SWEETHEART FAIRY CAKE CASES}      => {PACK OF 72 RETRO SPOT CAKE CASES}   0.01638786  0.7139831 0.02295273  7.923555   337
# [44] {72 SWEETHEART FAIRY CAKE CASES,                                                                                              
#   PACK OF 72 RETRO SPOT CAKE CASES}    => {60 TEATIME FAIRY CAKE CASES}        0.01638786  0.6505792 0.02518965 10.013855   337
# [45] {SWEETHEART CERAMIC TRINKET BOX,                                                                                              
#   WHITE HANGING HEART T-LIGHT HOLDER}  => {STRAWBERRY CERAMIC TRINKET BOX}     0.01381054  0.8452381 0.01633923 10.598461   284
# [46] {CHARLOTTE BAG , PINK/WHITE SPOTS,                                                                                            
#   CHARLOTTE BAG SUKI DESIGN}           => {RED SPOTTY CHARLOTTE BAG}           0.01590158  0.7500000 0.02120210 14.915861   327
# [47] {CHARLOTTE BAG SUKI DESIGN,                                                                                                   
#   RED SPOTTY CHARLOTTE BAG}            => {CHARLOTTE BAG , PINK/WHITE SPOTS}   0.01590158  0.6972281 0.02280685 16.594675   327
# [48] {60 TEATIME FAIRY CAKE CASES,                                                                                                 
#   PACK OF 60 PINK PAISLEY CAKE CASES}  => {PACK OF 72 RETRO SPOT CAKE CASES}   0.01988913  0.6897133 0.02883680  7.654217   409

# Powyższe reguły interesujące są z uwagi na potencjalne zastosowanie w łączeniu zawartych w nich produktów w pakiety promocyjne,
# dostępność na stanie, oraz sugestie dla klientów

# Reguły o stosunkowo niskim wsparciu i wysokim wsp. podniesienia:

mean(rules@quality$lift) -> avg_lift
supp_001 = quantile(rules@quality$support, 0.1)



inspect(rules[quality(rules)$lift > avg_lift & quality(rules)$support < supp_001]) # 18 reguł

# lhs                                      rhs                                      support confidence   coverage     lift count
# [1]  {EDWARDIAN PARASOL RED}               => {EDWARDIAN PARASOL NATURAL}           0.01016339  0.5110024 0.01988913 16.73289   209
# [2]  {RED SPOT CERAMIC DRAWER KNOB}        => {BLUE STRIPE CERAMIC DRAWER KNOB}     0.01001751  0.5613079 0.01784672 29.52106   206
# [3]  {BLUE STRIPE CERAMIC DRAWER KNOB}     => {RED SPOT CERAMIC DRAWER KNOB}        0.01001751  0.5268542 0.01901381 29.52106   206
# [4]  {WHITE SPOT RED CERAMIC DRAWER KNOB}  => {WHITE SPOT BLUE CERAMIC DRAWER KNOB} 0.01011476  0.5036320 0.02008364 25.19875   208
# [5]  {WHITE SPOT BLUE CERAMIC DRAWER KNOB} => {WHITE SPOT RED CERAMIC DRAWER KNOB}  0.01011476  0.5060827 0.01998638 25.19875   208
# [6]  {WHITE SPOT RED CERAMIC DRAWER KNOB}  => {RED STRIPE CERAMIC DRAWER KNOB}      0.01006613  0.5012107 0.02008364 26.15963   207
# [7]  {RED STRIPE CERAMIC DRAWER KNOB}      => {WHITE SPOT RED CERAMIC DRAWER KNOB}  0.01006613  0.5253807 0.01915970 26.15963   207
# [8]  {BLUE STRIPE CERAMIC DRAWER KNOB}     => {BLUE SPOT CERAMIC DRAWER KNOB}       0.01011476  0.5319693 0.01901381 27.14496   208
# [9]  {BLUE SPOT CERAMIC DRAWER KNOB}       => {BLUE STRIPE CERAMIC DRAWER KNOB}     0.01011476  0.5161290 0.01959735 27.14496   208
# [10] {BLUE SPOT CERAMIC DRAWER KNOB}       => {RED STRIPE CERAMIC DRAWER KNOB}      0.01011476  0.5161290 0.01959735 26.93827   208
# [11] {RED STRIPE CERAMIC DRAWER KNOB}      => {BLUE SPOT CERAMIC DRAWER KNOB}       0.01011476  0.5279188 0.01915970 26.93827   208
# [12] {POPPY'S PLAYHOUSE KITCHEN,                                                                                                   
#       POPPY'S PLAYHOUSE LIVINGROOM}        => {POPPY'S PLAYHOUSE BEDROOM}           0.01001751  0.8803419 0.01137911 56.92877   206
# [13] {POPPY'S PLAYHOUSE BEDROOM,                                                                                                   
#         POPPY'S PLAYHOUSE LIVINGROOM}        => {POPPY'S PLAYHOUSE KITCHEN}           0.01001751  0.9196429 0.01089282 55.29689   206
# [14] {POPPY'S PLAYHOUSE BEDROOM,                                                                                                   
#       POPPY'S PLAYHOUSE KITCHEN}           => {POPPY'S PLAYHOUSE LIVINGROOM}        0.01001751  0.7518248 0.01332426 59.23573   206
# [15] {JUMBO STORAGE BAG SUKI,                                                                                                      
#       RED SPOTTY CHARLOTTE BAG}            => {CHARLOTTE BAG SUKI DESIGN}           0.01001751  0.7383513 0.01356740 17.04091   206
# [16] {CHARLOTTE BAG , PINK/WHITE SPOTS,                                                                                            
#       CHARLOTTE BAG SUKI DESIGN,                                                                                                   
#       STRAWBERRY CHARLOTTE BAG}            => {RED SPOTTY CHARLOTTE BAG}            0.01011476  0.8739496 0.01157362 17.38095   208
# [17] {CHARLOTTE BAG SUKI DESIGN,                                                                                                   
#       RED SPOTTY CHARLOTTE BAG,                                                                                                    
#       STRAWBERRY CHARLOTTE BAG}            => {CHARLOTTE BAG , PINK/WHITE SPOTS}    0.01011476  0.8125000 0.01244894 19.33825   208
# [18] {CHARLOTTE BAG , PINK/WHITE SPOTS,                                                                                            
#       CHARLOTTE BAG SUKI DESIGN,                                                                                                   
#       RED SPOTTY CHARLOTTE BAG}            => {STRAWBERRY CHARLOTTE BAG}            0.01011476  0.6360856 0.01590158 19.29272   208


# Produkty z następników powyższych reguły powinny zawsze być dostępne jednocześnie z poprzednikami. Jednocześnie zakup zgodny z 
# taką regułą nie jest zbyt częsty (zapasy nie muszą być znaczne)


# Reguły o wysokim wsp. podniesienia:

rules[quality(rules)$lift > quantile(rules@quality$lift, 0.75)] -> high_lift_rules 
high_lift_rules = sort(high_lift_rules, by = "support")
inspect(high_lift_rules)

# 102 reguły

# lhs                                      rhs                                      support confidence   coverage     lift count
# [1]   {CHILDS GARDEN TROWEL BLUE}           => {CHILDS GARDEN TROWEL PINK}           0.01035791  0.8287938 0.01249757 63.35805   213
# [2]   {CHILDS GARDEN TROWEL PINK}           => {CHILDS GARDEN TROWEL BLUE}           0.01035791  0.7918216 0.01308111 63.35805   213
# [3]   {KEY FOB , FRONT  DOOR}               => {KEY FOB , BACK DOOR}                 0.01035791  0.7830882 0.01322700 40.97564   213
# [4]   {KEY FOB , BACK DOOR}                 => {KEY FOB , FRONT  DOOR}               0.01035791  0.5419847 0.01911107 40.97564   213
# [5]   {PINK REGENCY TEACUP AND SAUCER}      => {ROSES REGENCY TEACUP AND SAUCER}     0.01040654  0.7955390 0.01308111 41.41637   214
# [6]   {ROSES REGENCY TEACUP AND SAUCER}     => {PINK REGENCY TEACUP AND SAUCER}      0.01040654  0.5417722 0.01920833 41.41637   214
# [7]   {PINK REGENCY TEACUP AND SAUCER}      => {GREEN REGENCY TEACUP AND SAUCER}     0.01133048  0.8661710 0.01308111 46.99720   233
# [8]   {GREEN REGENCY TEACUP AND SAUCER}     => {PINK REGENCY TEACUP AND SAUCER}      0.01133048  0.6147757 0.01843027 46.99720   233
# [9]   {GREEN 3 PIECE MINI DOTS CUTLERY SET} => {RED 3 PIECE MINI DOTS CUTLERY SET}   0.01278934  0.7265193 0.01760358 21.68381   263
# [10]  {POPPY'S PLAYHOUSE LIVINGROOM}        => {POPPY'S PLAYHOUSE KITCHEN}           0.01137911  0.8965517 0.01269208 53.90845   234
# [11]  {POPPY'S PLAYHOUSE KITCHEN}           => {POPPY'S PLAYHOUSE LIVINGROOM}        0.01137911  0.6842105 0.01663101 53.90845   234
#[...]


# Dla następników ważna jest dostępność. Mniej istotne jest ich reklamowanie/sugerowanie wspólnie z poprzednikami, gdyż produkty
# w tych regułach kupowane są razem nieprzypadkowo (czyli skutkiem przemyślenia zakupu).



# Kolejny eksperyment - poszukiwanie dłuższych reguł

apriori_parameters2 = new("APparameter", "confidence" = 0.9, "support" = 0.005, "minlen"= 4,
                          "target" = "rules") 

rules2 = apriori(transactions, apriori_parameters2) #Stworzono 408 reguł

summary(rules2)

# Przy zmienionych paraetrach poszukiwano reguł dłuższych - aby uzyskać większą ich liczbę naturalnie
# konieczne było zmniejszenie wsparcia. Min. zaufanie natomiast można było zwiększyć, gdyż reguły długie 
#charakteryzują się większą jego wartością. Dlatego nawet dla 0,9 dostajemy 88 reguł:

# set of 88 rules
# 
# rule length distribution (lhs + rhs):sizes
# 4  5 
# 72 16 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.000   4.000   4.000   4.182   4.000   5.000 
# 
# summary of quality measures:
#   support           confidence        coverage             lift           count      
# Min.   :0.005009   Min.   :0.9000   Min.   :0.005057   Min.   :10.01   Min.   :103.0  
# 1st Qu.:0.005203   1st Qu.:0.9105   1st Qu.:0.005677   1st Qu.:18.03   1st Qu.:107.0  
# Median :0.005544   Median :0.9218   Median :0.006030   Median :41.20   Median :114.0  
# Mean   :0.005825   Mean   :0.9293   Mean   :0.006270   Mean   :38.57   Mean   :119.8  
# 3rd Qu.:0.006176   3rd Qu.:0.9389   3rd Qu.:0.006638   3rd Qu.:48.77   3rd Qu.:127.0  
# Max.   :0.009288   Max.   :1.0000   Max.   :0.010212   Max.   :93.81   Max.   :191.0  
# 
# mining info:
#   data ntransactions support confidence                                                          call
# transactions         20564   0.005        0.9 apriori(data = transactions, parameter = apriori_parameters2)

inspect(sort(rules2, by = "lift"))

# [1]  {CHILDS GARDEN FORK PINK,                                                                                                     
#   CHILDS GARDEN TROWEL BLUE,                                                                                                   
#   CHILDS GARDEN TROWEL PINK}            => {CHILDS GARDEN FORK BLUE}          0.007586073  0.9397590 0.008072359 93.81167   156
# [2]  {CHILDS GARDEN FORK BLUE,                                                                                                     
#   CHILDS GARDEN TROWEL BLUE,                                                                                                   
#   CHILDS GARDEN TROWEL PINK}            => {CHILDS GARDEN FORK PINK}          0.007586073  0.9512195 0.007975102 89.31908   156
# [3]  {CHILDRENS GARDEN GLOVES PINK,                                                                                                
#   CHILDS GARDEN TROWEL BLUE,                                                                                                   
#   CHILDS GARDEN TROWEL PINK}            => {CHILDRENS GARDEN GLOVES BLUE}     0.005738183  0.9291339 0.006175841 86.84868   118
# [4]  {CHILDS GARDEN FORK BLUE,                                                                                                     
#   CHILDS GARDEN FORK PINK,                                                                                                     
#   CHILDS GARDEN TROWEL PINK}            => {CHILDS GARDEN TROWEL BLUE}        0.007586073  0.9873418 0.007683330 79.00271   156
# [5]  {CHILDRENS GARDEN GLOVES PINK,                                                                                                
#   CHILDS GARDEN FORK BLUE,                                                                                                     
#   CHILDS GARDEN TROWEL PINK}            => {CHILDS GARDEN TROWEL BLUE}        0.005008753  0.9809524 0.005106011 78.49146   103
# [6]  {CHILDRENS GARDEN GLOVES BLUE,                                                                                                
#   CHILDRENS GARDEN GLOVES PINK,                                                                                                
#   CHILDS GARDEN TROWEL PINK}            => {CHILDS GARDEN TROWEL BLUE}        0.005738183  0.9672131 0.005932698 77.39210   118
# [7]  {CHILDRENS GARDEN GLOVES BLUE,                                                                                                
#   CHILDRENS GARDEN GLOVES PINK,                                                                                                
#   CHILDS GARDEN TROWEL BLUE}            => {CHILDS GARDEN TROWEL PINK}        0.005738183  1.0000000 0.005738183 76.44610   118
# [8]  {CHILDRENS GARDEN GLOVES PINK,                                                                                                
#   CHILDS GARDEN FORK PINK,                                                                                                     
#   CHILDS GARDEN TROWEL BLUE}            => {CHILDS GARDEN TROWEL PINK}        0.005106011  0.9905660 0.005154639 75.72491   105
# [9]  {CHILDRENS GARDEN GLOVES PINK,                                                                                                
#   CHILDS GARDEN FORK BLUE,                                                                                                     
#   CHILDS GARDEN TROWEL BLUE}            => {CHILDS GARDEN TROWEL PINK}        0.005008753  0.9903846 0.005057382 75.71104   103
# [...]

# Dla powyższych reguł ważna jest dostępność wszystkich elementów. Te towary są ze sobą mocno powiązane,
# więc z pewnością warto rozważyć oferowanie wspólnie jako zrestaw.


#Poszukiwanie reguł o największym wsparciu

apriori_parameters3 = new("APparameter", "confidence" = 0.01, "support" = 0.02, "minlen"= 2,
                          "target" = "rules") 

rules3 = apriori(transactions, apriori_parameters3) #Stworzono 408 reguł

summary(rules3) 

# set of 120 rules
# 
# rule length distribution (lhs + rhs):sizes
# 2 
# 120 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2       2       2       2       2       2 
# 
# summary of quality measures:
#   support          confidence        coverage            lift            count      
# Min.   :0.02013   Min.   :0.1258   Min.   :0.03127   Min.   : 1.317   Min.   :414.0  
# 1st Qu.:0.02120   1st Qu.:0.2849   1st Qu.:0.04989   1st Qu.: 3.545   1st Qu.:436.0  
# Median :0.02295   Median :0.4097   Median :0.05587   Median : 6.811   Median :472.0  
# Mean   :0.02460   Mean   :0.4095   Mean   :0.07147   Mean   : 7.011   Mean   :505.9  
# 3rd Qu.:0.02650   3rd Qu.:0.5299   3rd Qu.:0.09011   3rd Qu.: 8.471   3rd Qu.:545.0  
# Max.   :0.03871   Max.   :0.7952   Max.   :0.16125   Max.   :21.172   Max.   :796.0  
# 
# mining info:
#   data ntransactions support confidence                                                          call
# transactions         20564    0.02       0.01 apriori(data = transactions, parameter = apriori_parameters3)



# Otrzymano 120 reguł o największym wsparciu. Zgodnie z oczekiwaniami są to reguły krótkie (tylko dwuelementowe),
# a wśród nich znajduja się reguły o najmniejszym zaufaniu.
# Interesujący jest jednak duży rozstrzał wartości zaufania (~0,12 do ~0,8), jest on też stosunkowo równomierny.

inspect(sort(rules3, by = "confidence"))

#       lhs                                      rhs                                   support    confidence coverage   lift    
# [1]   {SWEETHEART CERAMIC TRINKET BOX}      => {STRAWBERRY CERAMIC TRINKET BOX}      0.03870842 0.7952048  0.04867730  9.971092
# [2]   {RED HANGING HEART T-LIGHT HOLDER}    => {WHITE HANGING HEART T-LIGHT HOLDER}  0.03749271 0.7232645  0.05183816  4.485287
# [3]   {BLUE 3 PIECE MINI DOTS CUTLERY SET}  => {PINK 3 PIECE MINI DOTS CUTLERY SET}  0.02076444 0.6640747  0.03126824 21.172141
# [4]   {PINK 3 PIECE MINI DOTS CUTLERY SET}  => {BLUE 3 PIECE MINI DOTS CUTLERY SET}  0.02076444 0.6620155  0.03136549 21.172141
# [5]   {CHARLOTTE BAG , PINK/WHITE SPOTS}    => {RED SPOTTY CHARLOTTE BAG}            0.02776697 0.6608796  0.04201517 13.143451
# [6]   {BLUE 3 PIECE MINI DOTS CUTLERY SET}  => {RED 3 PIECE MINI DOTS CUTLERY SET}   0.02032678 0.6500778  0.03126824 19.402321
# [7]   {WOODEN PICTURE FRAME WHITE FINISH}   => {WOODEN FRAME ANTIQUE WHITE}          0.02942035 0.6388596  0.04605135 11.524130
# [8]   {JUMBO BAG STRAWBERRY}                => {JUMBO BAG RED RETROSPOT}             0.03248395 0.6319773  0.05140051  6.524087
# [9]   {SINGLE HEART ZINC T-LIGHT HOLDER}    => {HANGING HEART ZINC T-LIGHT HOLDER}   0.02022953 0.6265060  0.03228944 13.633301
# [10]  {STRAWBERRY CHARLOTTE BAG}            => {RED SPOTTY CHARLOTTE BAG}            0.02061856 0.6253687  0.03297024 12.437217
# [11]  {PACK OF 60 DINOSAUR CAKE CASES}      => {PACK OF 72 RETRO SPOT CAKE CASES}    0.02139662 0.6162465  0.03472087  6.838906
# [12]  {PACK OF 60 PINK PAISLEY CAKE CASES}  => {PACK OF 72 RETRO SPOT CAKE CASES}    0.03433184 0.6112554  0.05616612  6.783517
# [...]

# Powyższe reguły charakteryzują się wysokim zaufaniem, więc można rozważyć oferowanie ich w parach (zwłaszcza dotyczy to
# par z mniejszym wsp. podniesienia - te kupowane są razem mniej "automatycznie"). 

























# # Dyskretyzacja dat miesiącami
# 
# online_retail$InvoiceDate <- format(online_retail$InvoiceDate, "%Y-%m")
# colnames(online_retail)[colnames(online_retail) == 'InvoiceDate'] <- 'InvoiceMonth'
# 
# # Dodanie kolumny z ceną całkowitą(ilość * cena), usunięcie kolumny Price
# 
# online_retail$Total_Price = online_retail$Quantity * online_retail$Price
# online_retail <- subset(online_retail, select = -Price)
# colnames(online_retail)[colnames(online_retail) == 'Total_Price'] <- 'TotalPrice'
# 
# # Dyskretyzacja kwot
# 
# invoice_total <- online_retail %>% group_by(Invoice) %>% summarise(InvoiceTotal = sum(TotalPrice))
# 
# online_retail <- left_join(online_retail, invoice_total, by = "Invoice")
# 
# online_retail[["InvoiceTotal"]] <- ordered(cut(online_retail[["InvoiceTotal"]], c(0, 5, 10, 20, 50 , 100, 500, Inf), 
#                                         include.lowest = T))
# summary(online_retail[["InvoiceTotal"]])  
# 
# online_retail <- subset(online_retail, select = -TotalPrice)
# 
# # Usunięto ilości towarów
# 
# online_retail <- subset(online_retail, select = -Quantity)
# 
# # Stworzenie postaci transakcyjnej
# 
# retail_transactions <- online_retail %>%
#   group_by(Invoice, InvoiceMonth, Country, InvoiceTotal) %>%
#   summarise(
#     items = toString(Description)
#   ) %>%
#   ungroup()
# 
# transactions_list <- retail_transactions %>%
#   mutate(items = strsplit(items, ", ")) %>%
#   unnest_longer(items)
# 
# transactions_df <- transactions_list %>%
#   group_by(Invoice, InvoiceMonth, Country, InvoiceTotal, items) %>%
#   summarise() %>%
#   ungroup()
# 
# transactions_wide <- transactions_df %>%
#   mutate(value = 1) %>%
#   spread(items, value, fill = 0)
# 
# transactions_wide <- transactions_wide %>%
#   select(-Invoice)
# 
# transactions <- as(transactions_wide, "transactions")
# 
# inspect(head(transactions))
# 
# 
# retail_transactions <- online_retail %>%
#   group_by(Invoice) %>%
#   summarise(
#     Items = list(Description),
#     Value = first(InvoiceTotal),
#     InvoiceMonth = first(InvoiceMonth),
#     Country = unique(Country)
#   )
# 
# 
# for(i in 1:ncol(retail_transactions)) retail_transactions[[i]] <- as.factor(retail_transactions[[i]])
# 
# transactions <- as(retail_transactions, "transactions")
# inspect(head(transactions))
