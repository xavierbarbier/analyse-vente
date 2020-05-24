install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")

library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)

#Importation data frame
library(readr)
customers <- read_csv("data/P3/customers.csv")
View(customers)

products <- read_csv("data/P3/products.csv")
View(products)

transactions <- read_csv("data/P3/transactions.csv")
View(transactions)

#CUSTOMERS
## Table NA'a customer
table(is.na(customers$client_id)) #ok mais c_ & ct_ SUPRESSION 
table(is.na(customers$sex)) #ok
table(is.na(customers$birth)) #ok

##Suppression CT
customers<-customers[customers$client_id != "ct_1", ]
customers<-customers[customers$client_id != "ct_0", ]

##Table sexe
table(customers$sex) #ok

#PRODUCTS

##recherche de NA's
table(is.na(products$id_prod))
table(is.na(products$categ))

## Recherche NaN's
is_numeric_prices<-apply(products['price'], 1, is.numeric)
table(is_numeric_prices)

##Suppression T_0
products<-products[products$id_prod != "T_0", ]

#TRANSACTIONS

##Suppression CT
transactions<-transactions[transactions$client_id != "ct_1", ]
transactions<-transactions[transactions$client_id != "ct_0", ]

##Nombre produit NA
na_id_prod<- is.na(transactions$id_prod)
table(na_id_prod)

##Nombre date NA
na_date<- is.na(transactions$date)
table(na_date)

## Rangement Na's transactions
a<-transactions[is.na(transactions$date),]

## conservation non NA's uniquement
transactions<-transactions[!is.na(transactions$date),]

##Nombre session NA
na_session_id<- is.na(transactions$session_id)
table(na_session_id)

#Nombre clients NA
na_client_id<- is.na(transactions$client_id)
table(na_client_id)

## liste produit ID
list_prod<-as.list(unique(products$id_prod))

## liste vide produits inconnus
empty_list <- vector(mode = "list")

# Fonction détection id_prod inconnus
list_id_prod = function(id_prod){
  if(! id_prod %in% list_prod){
    print(sprintf(' - "%s" n\'est pas un code valide, A VERIFIER.',id_prod))
    empty_list<<-append(empty_list,id_prod)
  } 
  return (id_prod)
}

## Application fonction 
transactions['id_prod'] = apply(transactions['id_prod'], 1, list_id_prod) #0_2245

## Imputation  0_2245 non disponible pour imputation par moyenne -> moyenne categ 0
### Calcul moyenne prix categ 0
round(mean(products$price[products$categ == 1]),1) #25.5

## Imputation 
product_0_2245<-data.frame("0_2245",round(mean(products$price[products$categ == 1]),1) ,0)
names(product_0_2245)<-c("id_prod","price","categ")
products<-rbind(products,product_0_2245)

# liste produit ID
list_client<-unique(customers$client_id)

# Fonction suppression client_id inconnus
check_client_id = function(client_id){
  if(! client_id %in% list_client){
    print(sprintf(' - "%s" n\'est pas un code valide A VERIFIER',client_id))
    return(NA)
  } 
  return (client_id)
}

# Vérification Nombre produit NA
na_id_prod<- is.na(transactions$id_prod)
table(na_id_prod)

# Application fonctions
transactions['client_id'] = apply(transactions['client_id'], 1, check_client_id)

## Vérification Nombre produit NA
na_id_prod<- is.na(transactions$id_prod)
table(na_id_prod)

## Rangement Na's transactions
b<-transactions[is.na(transactions$id_prod),]

## conservation non NA's uniquement
transactions<-transactions[!is.na(transactions$id_prod),]

##Nombre clients NA
na_client_id<- is.na(transactions$client_id)
table(na_client_id)

## Séparation date
date<-strsplit(as.character(transactions$date), " ") 

# Création data Frame avec date & horaires en variables
tempo_date<-data.frame(matrix(unlist(date), nrow=length(date), byrow=T))

# dupliquer variables pour eviter factor
tempo_date$date<-as.Date(tempo_date$X1)
tempo_date$horaire<-as.character(tempo_date$X2)

# Ajout date, horaire à transactions
transactions$date<-as.Date(as.character(tempo_date$date))
transactions$horaire<-tempo_date$X2

##Nombre date NA
na_date<- is.na(tempo_date$date)
table(na_date)

##Nombre horaire NA
na_date<- is.na(tempo_date$horaire)
table(na_date)


#retirer df inutilisées
remove(a)
remove(b)
remove(date)
remove(list_prod)
remove(list_client)
remove(na_date)
remove(na_client_id)
remove(na_id_prod)
remove(na_session_id)
remove(is_numeric_prices)
remove(empty_list)
remove(product_0_2245)


