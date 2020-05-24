install.packages('reshape2') # if not installed already
library(reshape2)

# Stype Graphique

style<- theme_bw() +
  theme(panel.background=element_rect(fill="#F0F0F0")) +
  theme(plot.background=element_rect(fill="#F0F0F0")) +
  theme(panel.border=element_rect(colour="#F0F0F0")) +
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.ticks=element_blank())+
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=20))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

#Merge infos products & customers vers transactions
## Extraction heure
first = function(str){
  str = str[[1]]
  parts = strsplit(str,':')[[1]]
  first_part = parts[1]
  if(length(parts) >= 2)
    print(sprintf(' - Il y a plusieurs parties dans "%s", ne gardons que %s.',paste(parts,collapse=""),first_part))  
  return(first_part)
}

tempo_date['horaire'] = apply(tempo_date['horaire'], 1, first)

## Ajout heure à transactions
transactions$heure<-tempo_date$horaire

## jours de la semaine
transactions$jour<-weekdays(transactions$date)

## Préparation merge price
price<-select(products,id_prod,price)

## Merge price
transactions<-merge(transactions,price,by="id_prod")

## préparation merge categ
categ<-select(products,id_prod,categ)

##Merge categ
transactions<-merge(transactions,categ,by="id_prod")

## préparation merge sex
sex<-select(customers,sex,client_id)

##Merge sex
transactions<-merge(transactions,sex,by="client_id")

## préparation merge age
customers$age<-2020-customers$birth
age<-select(customers,age,client_id)

#Merge age
transactions<-merge(transactions,age,by="client_id")

#Ajout du mois
transactions$mois<-month(transactions$date)

#Fréquence achats par utilisateur 
freq<-as.data.frame(table(transactions$client_id))
summary(freq) # c_1609 = 12855 + c_6714 + c_3454 + c_4958 = outliers = mise de coté pour la suite

# CA clients pro
ca_pro = sum(transactions$price[transactions$client_id == "c_1609"])+sum(transactions$price[transactions$client_id == "c_6714"])+
  sum(transactions$price[transactions$client_id == "c_3454"])+sum(transactions$price[transactions$client_id == "c_4958"])
# = 410085.4

# Ratio pro CA
ratio_pro = (ca_pro/sum(transactions$price) )*100 #7.483493%

# correctif = suppression client pro

transactions<-transactions[transactions$client_id != "c_1609",]
transactions<-transactions[transactions$client_id != "c_6714",]
transactions<-transactions[transactions$client_id != "c_3454",]
transactions<-transactions[transactions$client_id != "c_4958",]


#CA

##Selection variables
annuel<-select(transactions,date,price)
##Regroupement et somme par date
annuel<-annuel%>%
  group_by(date)%>%
  summarise_all(funs(sum))

## Graphique CA
ggplot(annuel, aes(x=date, y=price)) +
  geom_line(aes(colour="#f8766d"),size=1.6) + ggtitle("Evolution CA quotidien sur l'année")+
  xlab("Période")+ylab("CA quotidien") +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")
  


#OCTOBRE outliers
date1<-as.Date("2021-10-2")
date2<-as.Date("2021-10-27")

pb_date<-transactions %>% filter(date >= date1 & date <= date2)

## proportion et CA par categ
categ_pb_date<-table(pb_date$categ)
categ<-table(transactions$categ)

prop.table(categ_pb_date)
prop.table(categ)

## Calcul somme par catégorie octobre
ca_pb_date<-select(pb_date,categ,price)
ca_pb_date<-ca_pb_date%>%
  group_by(categ)%>%
  summarise_all(funs(sum))

## Calcul somme par catégorie sur l'année
ca_categ<-select(transactions,categ,price)
ca_categ<-ca_categ%>%
  group_by(categ)%>%
  summarise_all(funs(sum))


#Calcul ration et préparation pie chart
ca_pb_date$ratio<-ca_pb_date$price/sum(ca_pb_date$price)
ca_pb_date$ypos = cumsum(ca_pb_date$ratio)- 0.5*ca_pb_date$ratio

ca_categ$ratio<-ca_categ$price/sum(ca_categ$price)
ca_categ$ypos = cumsum(ca_categ$ratio)- 0.5*ca_categ$ratio 


# Graphique pie chart CA par catégorie année + octobre
ggplot(ca_categ, aes(x="", y=ratio, fill=categ)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +theme_void() +
  geom_text(aes(y = ypos, label=paste("Categ",categ,"\n",round(ratio*100,0), "%")), color = "white", size=4)+
  ggtitle("Répartition CA selon catégorie sur année")+ 
  theme(legend.position="none")

ggplot(ca_pb_date, aes(x="", y=ratio, fill=categ)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +theme_void() +
  geom_text(aes(y = ypos, label=paste("Categ",categ,"\n",round(ratio*100,0), "%")), color = "white", size=4)+
  ggtitle("Répartition CA selon catégorie sur octobre")+ 
  theme(legend.position="none")


## Nombre id_prod categ 1 
length(unique(products$id_prod[products$categ==1])) #739 refs

#CORRECTIF Retrait OCTOBRE outliers
date1<-as.Date("2021-10-1")
date2<-as.Date("2021-10-31")

transactions<-transactions %>% filter(date < date1 | date > date2)

##CA mensuel
transactions$mois<-month(transactions$date)
mensuel<-select(transactions,price,mois)
mensuel<-mensuel%>%group_by(mois)%>%
  summarise_all(funs(sum))

## CA mensuel moyen
mean(mensuel$price) #483 267

## Graphique CA mensuel
ggplot(mensuel, aes(x=as.factor(mois), y=price, group=1)) +
  geom_line(aes(colour="#f8766d"),size=1.6) + ggtitle("Evolution CA mensuel sur l'année")+
  xlab("Période")+ylab("CA mensuel cumulé") +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) 
  
##CA journalier
jour<-select(transactions,price,jour)
jour<-jour%>%group_by(jour)%>%
  summarise_all(funs(sum))

## réorder par jour
jour$jour<- factor(jour$jour, levels=c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
jour$jour<-jour[order(jour$jour),]

## Graphique CA par jour
ggplot(jour, aes(x=jour$jour, y=jour$price, fill = jour$jour)) +
  geom_bar(stat = "identity") + ggtitle("Evolution CA par jour de la semaine")+
  theme(legend.position="none") +
  xlab("jour de la semaine")+ylab("CA cumulé")+ scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

## CA mensuel moyen
mean(jour$price) #782838


##CA par sexe
ca_sex<-select(transactions,sex,price)
ca_sex<-ca_sex%>%group_by(sex)%>%
  summarise_all(funs(sum))

##Calcul ratio et préparation pie chart
ca_sex$ratio<-ca_sex$price/sum(ca_sex$price)
ca_sex$ypos = cumsum(ca_sex$ratio)- 0.5*ca_sex$ratio

## Graphique CA par sexe
ggplot(ca_sex, aes(x="", y=ca_sex$ratio, fill=ca_sex$sex)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +theme_void() +
  geom_text(aes(y = ypos,label = ca_sex$sex), color = "white", size=6)+ 
  theme(legend.position="none") +ggtitle("Répartition CA selon Sexe")

##Panier moyen
panier<-select(transactions,session_id,price)
panier<-panier%>%
  group_by(session_id)%>%
  summarise_all(funs(sum))

summary(panier$price)

## CA annuel moyen

ca_clients<-select(transactions,client_id,price)
ca_clients<-ca_clients%>%
  group_by(client_id)%>%
  summarise_all(funs(sum))

summary(ca_clients$price)

#CLIENTS

#nombre
nrow(customers)#8621

## Calcul age
customers$age<-2022-customers$birth
summary((customers$age)) #ok

## Graphique Répartition clients selon sexe
ggplot(customers, aes(x=as.factor(sex), fill=as.factor(sex) )) +  
  geom_bar( ) + theme(legend.position="none") +
  ggtitle("Répartition clients selon sexe")+
  xlab("Sexe")+
  ylab("Nombre")+ scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

#Fréquence achats par utilisateur 
freq<-as.data.frame(table(transactions$client_id))
summary(freq) 

#nombres client achat unique + ratio
nrow(freq[freq$Freq ==1,]) #46 
nrow(freq[freq$Freq ==1,])/nrow(customers) #0.005335808


## Préparation merge age fréquence
age<-select(customers,age,client_id)
names(freq)[1]<-"client_id"

##Merge price
freq<-merge(freq,age,by="client_id")

## Préparation merge sexe client
sex<-select(customers,sex,client_id)

##Merge sex
freq<-merge(freq,sex,by="client_id")

## Merge infos sex, age, id_client par panier/session
panier<-select(transactions,session_id,price)
panier<-panier%>%
  group_by(session_id)%>%
  summarise_all(funs(sum))

sessions<-select(transactions,session_id,client_id)
sessions<-unique(sessions)

panier<-merge(sessions,panier, by="session_id")

panier<-merge(panier,sex, by="client_id")

panier<-merge(panier,age, by="client_id")

## graphique distribution fréquence des achats
ggplot(freq[freq$Freq <1000,], aes(x=Freq)) + geom_histogram(fill="#f8766d")+
  ggtitle ("Distribution fréquence des achats") +
  xlab("Fréquence des achats")+
  ylab("Fréquence")+ scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

#Tendance centrale fréquence achat
summary(freq$Freq[freq$Freq <1000])

# Graphique distribution age
ggplot(customers, aes(x=age)) + 
  geom_histogram(fill="#f8766d")+
  ggtitle ("Distribution age des clients") +
  xlab("Age")+
  ylab("Fréquence") + scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

#CONCENTRATION AGE CLIENT

##Lorenz
dep = customers$age
lorenz = cumsum(sort(dep)) / sum(dep)
lorenz = c(0,lorenz) # La courbe de Lorenz commence à 0

n = length(lorenz)
lorenz_df = data.frame(x=((0:(n-1))/n), y=lorenz)

## Gini
n = length(lorenz)
aire_ss_courbe = sum(lorenz[1:(n-1)])/n # aire sous la courbe de Lorenz. La dernière valeur ne participe pas à l'aire, d'où "[:-1]"
S = 0.5 - aire_ss_courbe # aire entre la 1e bissectrice et la courbe de Lorenz
gini = 2*S
gini # 0.2209791

## Graphique Lorenz concentration age clients
ggplot(lorenz_df, aes(x,y)) + geom_area(shape=1)+
  ggtitle("Lorenz concentration age clients")+geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  geom_text(x=0.2, y=0.75, label="gini = 0.22",size=6)+ scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")


# Graphique Répartition clients selon sexe
ggplot(customers, aes(x=as.factor(sex), fill=as.factor(sex) )) +  
  geom_bar( ) + theme(legend.position="none") +
  ggtitle("Répartition clients selon sexe")+
  xlab("Sexe")+
  ylab("Nombre")+ scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

# Graphique Distribution age des clients selon de sexe
ggplot(customers, aes(x=age, fill = sex)) + 
  geom_histogram(color="#e9ecef")+
  ggtitle ("Distribution age des clients selon de sexe") +
  xlab("Age")+
  ylab("Fréquence") +
  facet_wrap(~sex)+ 
  theme(legend.position="none")+ scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

## graphique distribution fréquence des achats selon sexe
ggplot(freq[freq$Freq <1000,], aes(x=Freq, fill = sex)) + geom_histogram(color="#e9ecef")+
  ggtitle ("Distribution fréquence des achats selon sexe") +
  xlab("Fréquence des achats")+
  ylab("Fréquence")  +facet_wrap(~sex)+ 
  theme(legend.position="none")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")
  

# Graphique Montant du panier moyen selon de sexe
ggplot(panier, aes(y=price, fill = sex)) +
  geom_boxplot()+facet_wrap(~sex)+
  ggtitle ("Montant du panier moyen selon de sexe") +
  xlab("")+
  ylab("Montant du panier") + 
  theme(legend.position="none")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")
  


# PRODUITS
## Nb produits
nrow(products) #3287

## Tendances centrales Prix
summary(products$price)

# Graphique distribution prix
ggplot(price, aes(x=price)) + 
  geom_histogram(fill="#f8766d")+
  ggtitle ("Distribution prix produits") +
  xlab("Prix")+
  ylab("Fréquence") +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

## CONCENTRATION PRIX (Lorenz)
##Selection variables
concentration<-select(transactions,id_prod,price)
##Regroupement et somme par produit
concentration<-concentration%>%
  group_by(id_prod,)%>%
  summarise_all(funs(sum))

##Lorenz
dep = concentration$price
lorenz = cumsum(sort(dep)) / sum(dep)
lorenz = c(0,lorenz) # La courbe de Lorenz commence à 0

n = length(lorenz)

## Gini
n = length(lorenz)
aire_ss_courbe = sum(lorenz[1:(n-1)])/n # aire sous la courbe de Lorenz. La dernière valeur ne participe pas à l'aire, d'où "[:-1]"
S = 0.5 - aire_ss_courbe # aire entre la 1e bissectrice et la courbe de Lorenz
gini = 2*S
gini #0.7389

## Graphique Lorenz concentration des prix
lorenz_df = data.frame(x=((0:(n-1))/n), y=lorenz)
ggplot(lorenz_df, aes(x,y)) + geom_area(shape=1)+
  ggtitle("Lorenz concentration des prix")+geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  geom_text(x=0.2, y=0.75, label="gini = 0.73",size=6)+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

# Graphique selon catégorie
ggplot(products, aes(x=as.factor(categ), y=price, fill=as.factor(categ))) +
  geom_boxplot() +
  theme(legend.position="none") +
  ggtitle("Répartition prix selon catégorie") +
  xlab("Catégorie")+
  ylab("Prix")+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

## Prix par catégorie
price_categ<-select(products,categ,id_prod)
price_categ<-merge(price_categ,price,by="id_prod")

# Graphique distribution prix par catégorie
ggplot(price_categ, aes(x=price, fill = as.factor(categ))) + 
  geom_histogram(color="#e9ecef")+
  ggtitle ("Distribution prix produits") +
  xlab("Prix")+
  ylab("Fréquence") +
  facet_wrap(~categ) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

##Lorenz prix categ 0
categ_0<-price_categ[price_categ$categ == 0,]

dep = categ_0$price
lorenz = cumsum(sort(dep)) / sum(dep)
lorenz = c(0,lorenz) # La courbe de Lorenz commence à 0

n = length(lorenz)

## Gini
n = length(lorenz)
aire_ss_courbe = sum(lorenz[1:(n-1)])/n # aire sous la courbe de Lorenz. La dernière valeur ne participe pas à l'aire, d'où "[:-1]"
S = 0.5 - aire_ss_courbe # aire entre la 1e bissectrice et la courbe de Lorenz
gini = 2*S
gini #0.35

## Graphique Lorenz concentration des prix
df0 <- data.frame(x=((0:(n-1))/n), y=lorenz)
lorenz_df = data.frame(x=((0:(n-1))/n), y=lorenz)

ggplot(lorenz_df, aes(x,y)) + geom_point(shape=1)+
  ggtitle("Lorenz concentration des prix categ 0")+geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  geom_text(x=0.2, y=0.75, label="gini = 0.35",size=6)+
  

##Lorenz prix categ 1
categ_1<-price_categ[price_categ$categ == 1,]

dep = categ_1$price
lorenz = cumsum(sort(dep)) / sum(dep)
lorenz = c(0,lorenz) # La courbe de Lorenz commence à 0

n = length(lorenz)

## Gini
n = length(lorenz)
aire_ss_courbe = sum(lorenz[1:(n-1)])/n # aire sous la courbe de Lorenz. La dernière valeur ne participe pas à l'aire, d'où "[:-1]"
S = 0.5 - aire_ss_courbe # aire entre la 1e bissectrice et la courbe de Lorenz
gini = 2*S
gini #0.33

## Graphique Lorenz concentration des prix
df1 <- data.frame(x=((0:(n-1))/n), y=lorenz)
lorenz_df = data.frame(x=((0:(n-1))/n), y=lorenz)
ggplot(lorenz_df, aes(x,y)) + geom_point(shape=1)+
  ggtitle("Lorenz concentration des prix categ 1")+geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  geom_text(x=0.2, y=0.75, label="gini = 0.33",size=6)

##Lorenz prix categ 2
categ_2<-price_categ[price_categ$categ == 2,]

dep = categ_2$price
lorenz = cumsum(sort(dep)) / sum(dep)
lorenz = c(0,lorenz) # La courbe de Lorenz commence à 0

n = length(lorenz)

## Gini
n = length(lorenz)
aire_ss_courbe = sum(lorenz[1:(n-1)])/n # aire sous la courbe de Lorenz. La dernière valeur ne participe pas à l'aire, d'où "[:-1]"
S = 0.5 - aire_ss_courbe # aire entre la 1e bissectrice et la courbe de Lorenz
gini = 2*S
gini #0.25

## Graphique Lorenz concentration des prix
df2 <- data.frame(x=((0:(n-1))/n), y=lorenz)
lorenz_df = data.frame(x=((0:(n-1))/n), y=lorenz)
ggplot(lorenz_df, aes(x,y)) + geom_point(shape=1)+
  ggtitle("Lorenz concentration des prix categ 2")+geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  geom_text(x=0.2, y=0.75, label="gini = 0.25",size=6)

## Lorenz regroupé
ggplot() + geom_line(data=df0, aes(x=x, y = y), color= "orange") + 
  geom_line(data=df1, aes(x=x,y=y), color = "blue") + 
  geom_line(data=df2, aes(x=x, y), color = "green")+
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+
  ggtitle ("Indices Gine des 3 catégories")


##Fréquence achats par produits
freq_id_prod<-as.data.frame(table(transactions$id_prod))
summary(freq_id_prod)

#CONCENTRATION FREQUENCE VENTES

##Lorenz
dep = freq_id_prod$Freq
lorenz = cumsum(sort(dep)) / sum(dep)
lorenz = c(0,lorenz) # La courbe de Lorenz commence à 0

n = length(lorenz)
lorenz_df = data.frame(x=((0:(n-1))/n), y=lorenz)

## Gini
n = length(lorenz)
aire_ss_courbe = sum(lorenz[1:(n-1)])/n # aire sous la courbe de Lorenz. La dernière valeur ne participe pas à l'aire, d'où "[:-1]"
S = 0.5 - aire_ss_courbe # aire entre la 1e bissectrice et la courbe de Lorenz
gini = 2*S
gini #0.6926361


## Graphique Lorenz concentration des fréquences de ventes
ggplot(lorenz_df, aes(x,y)) + geom_point(shape=1)+
  ggtitle("Lorenz concentration des fréquences de ventes")+geom_abline(intercept = 0, slope = 1, color = "red", size = 1)+
  geom_text(x=0.2, y=0.75, label="gini = 0.69",size=6)+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

## Relation prix ventes et fréquence achat
names(freq_id_prod)[1]<-"id_prod"

## Merge id_prod
freq_id_prod<-merge(freq_id_prod,price, by="id_prod")

## Merge categ
freq_id_prod<-merge(freq_id_prod,products, by="id_prod")

## Renommage correct price + supression doublon
names(freq_id_prod)[3]<-c("price")
freq_id_prod<-freq_id_prod[-4]

## graphique Relation prix ventes et fréquence achat
ggplot(freq_id_prod, aes(x=price, y=Freq, color = as.factor(categ))) + 
  geom_point()+ ggtitle("Relation prix ventes et fréquence achat")+
  xlab("Prix de vente")+ylab("Fréquence de vente") + 
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position=c(250, 500)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")


# Relation prix ventes et age
ggplot(transactions, aes(x=age, y=price, color = as.factor(categ))) + 
  geom_point()+ ggtitle("Relation age client et prix produit")+
  xlab("Age client")+ylab("Prix produit")+ 
  theme(legend.position="none")+ scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

# TOP 10 produit + vendu
top_id_prod<-freq_id_prod[order(freq_id_prod$Freq,decreasing = TRUE),]
names(top_id_prod)[1]<-"id_prod"
top_id_prod[1:10,]




####################"
#1
sex_categ<-table(transactions$sex,transactions$categ)
sex_categ

##Heatmap sex / categ

X = "sex"
Y = "categ"

c = as.data.frame.matrix(table(transactions[,c(X,Y)]))
cont = c

tx = rowSums(cont)
ty = colSums(cont)

cont[,"total"] = tx
cont["total",(1:(ncol(cont)-1))] = ty
cont["total","total"] = nrow(transactions)
cont

indep = tx %*% t(ty) / nrow(transactions)

mesure = (c-indep)^2/indep
xi_n = sum(mesure)

contributions = mesure/xi_n

cont_reshaped = as.data.frame(table(transactions[,c(X,Y)]))
cont_reshaped["contrib"] = NA
for(i in 1:nrow(cont_reshaped)){
  x = cont_reshaped[i,X]
  y = cont_reshaped[i,Y]
  cont_reshaped[i,"contrib"] = contributions[x,y]
}


ggplot(cont_reshaped, aes_string(X,Y)) +    
  geom_tile(aes(fill = contrib)) +
  geom_text(aes(label=round(contrib,2)), size=4) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_flip()+ggtitle("Relation entre le sexe et la catégorie")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

chisq.test(sex_categ)$expected# ok > 5
chisq.test(sex_categ) #p-value < 2.2e-16 m= ++


eta_squared = function(x,y){
  moyenne_y = mean(y)
  classes = c()
  for(classe in unique(x)){
    yi_classe = y[x==classe]
    classes = c(classes,
                list(
                  list('ni'= length(yi_classe),
                       'moyenne_classe' = mean(yi_classe))))
  }
  SCT = 0
  for(yj in y){SCT = SCT + (yj-moyenne_y)^2}
  SCE = 0
  for(c in classes){
    SCE = SCE + c$ni*(c$moyenne_classe-moyenne_y)^2
  }
  return(SCE/SCT)
}

eta_squared(transactions[,"sex"],transactions[,"categ"]) #5.906479e-06

#2
ca_age<-select(transactions,client_id,price)
ca_age<-ca_age%>%
  group_by(client_id)%>%
  summarise_all(funs(sum))

ca_age<-merge(ca_age,age,by="client_id")

# Relation age et CA cummulé
p<-ggplot(ca_age, aes(x=age, y=price)) + 
  geom_point(aes(colour="#f8766d"),size=1.6)+ ggtitle("Relation age client et CA annuel")+
  xlab("Age client")+ylab("CA annuel")+
  geom_smooth(method=lm , color="red", se=FALSE)

lm_eq <- function(ca_age){
  
  m <- lm(price ~ age, ca_age);
  
  eq <- substitute("p<0.05 &"  (r)^2~"="~r2, 
                   
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        
                        b = format(unname(coef(m)[2]), digits = 2),
                        
                        r2 = format(summary(m)$r.squared, digits = 3)))
  
  as.character(as.expression(eq));
  
}

p1 <- p + geom_text(x = 80, y = 2200, label = lm_eq(ca_age), parse = TRUE, size = 5)

p1+scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

## Intercept & coef
summary(lm(ca_age$price~ca_age$age)) # coef =  -4.6268 R2 = 0.03263

##Calcul décenie
ca_age$diz<-round(ca_age$age,digits = -1)
ca_age<-select(ca_age,diz,price)
ca_age<-ca_age%>%
  group_by(diz)%>%
  summarise_all(funs(sum))


# Relation age et CA cummulé
p<-ggplot(ca_age, aes(x=diz, y=price)) + 
  geom_point(aes(colour="#f8766d"),size=2)+ ggtitle("Relation décennie client et CA annuel")+
  xlab("Dec client")+ylab("CA annuel")+
  geom_smooth(method=lm , color="red", se=FALSE)

lm_eq <- function(ca_age){
  
  m <- lm(price ~ diz, ca_age);
  
  eq <- substitute("p<0.05 &" (r)^2~"="~r2, 
                    
                    list(a = format(unname(coef(m)[1]), digits = 2),
                         
                         b = format(unname(coef(m)[2]), digits = 2),
                         
                         r2 = format(summary(m)$r.squared, digits = 3)))
  
  as.character(as.expression(eq));
  
}

p1 <- p + geom_text(x = 80, y = 1220000, label = lm_eq(ca_age), parse = TRUE, size = 5)

p1+scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")



## Intercept & coef
summary(lm(ca_age$price~ca_age$diz)) # p = 0.007692 coef =  -16308 R2 = 0.67

# Relation age et frequence achat
ggplot(freq[freq$Freq < 1000,], aes(x=age, y=Freq)) + 
  geom_point(aes(colour="#f8766d"),size=2)+ ggtitle("Relation age client et fréquence achat")+
  xlab("Age client")+ylab("Fréquence achat")+
  geom_smooth(method=lm , color="red", se=FALSE)+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

## Intercept & coef
summary(lm(freq$Freq~freq$age)) # coef =  -5.964 R2 = 4.193e-05 p=0.001096

##Calcul décenie
freq$diz<-round(freq$age,digits = -1)
freq<-select(freq,diz,Freq)
freq<-freq%>%
  group_by(diz)%>%
  summarise_all(funs(sum))

# Relation age et frequence achat
p<-ggplot(freq, aes(x=diz, y=Freq)) + 
  geom_point(aes(colour="#f8766d"),size=2)+ ggtitle("Relation décennie client et fréquence achat")+
  xlab("Diz client")+ylab("Fréquence achat")+
  geom_smooth(method=lm , color="red", se=FALSE)

lm_eq <- function(freq){
  
  m <- lm(Freq ~ diz, freq);
  
  eq <- substitute("p>0.05 &" (r)^2~"="~r2, 
                    
                    list(a = format(unname(coef(m)[1]), digits = 2),
                         
                         b = format(unname(coef(m)[2]), digits = 2),
                         
                         r2 = format(summary(m)$r.squared, digits = 3)))
  
  as.character(as.expression(eq));
  
}

p1 <- p + geom_text(x = 80, y = 100000, label = lm_eq(freq), parse = TRUE, size = 5)

p1+scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")


## Intercept & coef
summary(lm(freq$Freq~freq$diz))  # p = 0.1643 coef = -754.6 r2 = 0.2948

# Selection, regroupement, merge préparation Relation age et panier moyen
panier_nb<-select(transactions,session_id)
panier_nb$nb_articles<-c(1)
panier_nb<-panier_nb%>%
  group_by(session_id)%>%
  summarise_all(funs(sum))

panier_nb<-merge(panier_nb,sessions,by="session_id")

panier_nb<-merge(panier_nb,age,by="client_id")


# Relation age et panier moyen
ggplot(panier_nb, aes(x=age, y=nb_articles)) + 
  geom_point(aes(colour="#f8766d"),size=1.6)+ ggtitle("Relation age client et nombre d’articles par panier)")+
  xlab("Age client")+ylab("Nombre d’articles par panier")+
  geom_smooth(method=lm , color="red", se=FALSE)+
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")


## Intercept & coef
summary(lm(panier_nb$nb_articles~panier_nb$age)) # p < 2.2e-16 R =  -0.0163882 R2 = 0.03157

##Calcul décenie
panier_nb$diz<-round(panier_nb$age,digits = -1)
panier_nb<-select(panier_nb,diz,nb_articles)
panier_nb<-panier_nb%>%
  group_by(diz)%>%
  summarise_all(funs(mean))

p <-ggplot(panier_nb, aes(x=diz, y=nb_articles)) + 
  geom_point(aes(colour="#f8766d"),size=2)+ ggtitle("Relation décennie client et nombre d’articles par panier)")+
  xlab("Age client")+ylab("Nombre d’articles par panier")+
  geom_smooth(method=lm , color="red", se=FALSE)


lm_eq <- function(panier_nb){
  
  m <- lm(nb_articles ~ diz, panier_nb);
  
  eq <- substitute("p>0.05 &" (r)^2~"="~r2, 
                   
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        
                        b = format(unname(coef(m)[2]), digits = 2),
                        
                        r2 = format(summary(m)$r.squared, digits = 3)))
  
  as.character(as.expression(eq));
  
}

p1 <- p + geom_text(x = 80, y = 2.25, label = lm_eq(panier_nb), parse = TRUE, size = 5)

p1+scale_color_fivethirtyeight() +
  theme_fivethirtyeight()+scale_fill_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

## Intercept & coef
summary(lm(panier_nb$nb_articles~panier_nb$diz)) # p-value=  0.05202 R =  -0.0152180 R2 = 0.03157


## age et catégorie
transactions$diz<-round(transactions$age,digits = -1)

age_categ<-table(transactions$categ,transactions$diz)
age_categ

X = "diz"
Y = "categ"

c = as.data.frame.matrix(table(transactions[,c(X,Y)]))
cont = c

tx = rowSums(cont)
ty = colSums(cont)

cont[,"total"] = tx
cont["total",(1:(ncol(cont)-1))] = ty
cont["total","total"] = nrow(transactions)
cont

indep = tx %*% t(ty) / nrow(transactions)

mesure = (c-indep)^2/indep
xi_n = sum(mesure)

contributions = mesure/xi_n

cont_reshaped = as.data.frame(table(transactions[,c(X,Y)]))
cont_reshaped["contrib"] = NA
for(i in 1:nrow(cont_reshaped)){
  x = cont_reshaped[i,X]
  y = cont_reshaped[i,Y]
  cont_reshaped[i,"contrib"] = contributions[x,y]
}

ggplot(cont_reshaped, aes_string(X,Y)) +    
  geom_tile(aes(fill = contrib)) +
  geom_text(aes(label=round(contrib,2)), size=4) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_flip()+ggtitle("Relation age et catégorie")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")

chisq.test(age_categ)$expected# ok > 5
chisq.test(age_categ) #p-value < 2.2e-16 m= ++

eta_squared(transactions[,"age"],transactions[,"categ"])




## groupe age et catégorie 

## itération groupes (groupe 1 : -30 ans / groupe 2 : 30-50 ans / groupe 3 : 50+)
transactions$groupe_age<-c("")


for (i in 1:dim(transactions)[1]) {
  if (transactions$age[i] < 30) {
    transactions$groupe_age[i] <- "Groupe 1"
  } 
  if (transactions$age[i] >= 30 & transactions$age[i] < 50) {
    transactions$groupe_age[i] <- "Groupe 2"
  }
  if (transactions$age[i] >= 50) {
    transactions$groupe_age[i] <- "Groupe 3"
  }
  
}

#heatmap groupe / categ
X = "groupe_age"
Y = "categ"

c = as.data.frame.matrix(table(transactions[,c(X,Y)]))
cont = c

tx = rowSums(cont)
ty = colSums(cont)

cont[,"total"] = tx
cont["total",(1:(ncol(cont)-1))] = ty
cont["total","total"] = nrow(transactions)
cont

indep = tx %*% t(ty) / nrow(transactions)

mesure = (c-indep)^2/indep
xi_n = sum(mesure)

contributions = mesure/xi_n

cont_reshaped = as.data.frame(table(transactions[,c(X,Y)]))
cont_reshaped["contrib"] = NA
for(i in 1:nrow(cont_reshaped)){
  x = cont_reshaped[i,X]
  y = cont_reshaped[i,Y]
  cont_reshaped[i,"contrib"] = contributions[x,y]
}

ggplot(cont_reshaped, aes_string(X,Y)) +    
  geom_tile(aes(fill = contrib)) +
  geom_text(aes(label=round(contrib,2)), size=4) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_flip()+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), axis.title.x = element_blank(),axis.title.y = element_blank())+
  theme(legend.position="none") +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold"))+
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  geom_hline(yintercept=0,size=1.2,colour="#535353")+ggtitle("Relation groupe age et catégorie")


group_categ<-table(transactions$categ,transactions$groupe_age)
group_categ


chisq.test(group_categ)$expected# ok > 5
chisq.test(group_categ) #p-value < 2.2e-16 m= ++


eta_squared(transactions[,"groupe_age"],transactions[,"categ"]) # eta2 = 0.2036181

