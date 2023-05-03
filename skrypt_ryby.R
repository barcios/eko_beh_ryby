library(dplyr)
library(car)
library(ggplot2)
library(multcomp)
library(ggpubr)
library(ordinal)
library(palmerpenguins)
library(emmeans)
library(FSA)
library(ggsci)
library(paletteer)
library(ggthemes)
library(stringr)
library(viridis)
library(hrbrthemes)
library(lubridate)

#install.packages("dplyr","car","ggplot2","ggplot","multcomp","ggpubr","ordinal","stringr")

setwd("C:/Users/barci/Desktop/ryby/")
dane <- read.table("ryby_eksploratywnosc.csv", sep=",", encoding = "UTF-8", header = TRUE, fill = TRUE)

#usuwam kolumnę czas_stoper
dane=dane[,c(2:7)]


# PRZELICZANIE CZASU - pakiet lubridate -----------------------------------


#zmieniam dane na format czasu wg dokumentacji funkcji
czas_hms <- hms(dane$czas_jednostkowy)                                                      

#przeliczam na sekundy
seconds(czas_hms) 

#wstawiam kolumnę do tabeli dane
dane$czas_sek <- as.numeric(seconds(czas_hms))


#sumaryczny czas spedzony w czystej wodzie dla każdej ryby
dane=dane[,c(2,4,5,6,7)]

czas_trwania_wizyt_s <- aggregate(dane$czas_sek, by=list(dane$ryba, dane$typ), FUN=sum)

colnames(czas_trwania_wizyt_s)[1]  <- "ryba"
colnames(czas_trwania_wizyt_s)[2]  <- "typ"
#colnames(czas_trwania_wizyt_s)[3]  <- "dzien"
colnames(czas_trwania_wizyt_s)[3]  <- "czas_suma"

View(czas_trwania_wizyt_s)
  

# PRZEPLYNIETY DYSTANS ----------------------------------------------------

dane <- read.table("ryby_eksploratywnosc.csv", sep=",", encoding = "UTF-8", header = TRUE, fill = TRUE)

#obliczam przepłyniety dystans dla każdej ryby tzn. liczba przemieszczeń *10
dane$dystans <- dane$suma_przemieszczen*10

#podsumowanie danych w nowej ramce
dystans_podsumowanie <- dane[,c(3,4,5,6,8)]

#usuwam zbedne wiersze
dystans_podsumowanie <- na.omit(dystans_podsumowanie)

#wykres dystans pokonany przez poszczególne ryby w zależności od środowiska
dystans_typ_ryba_wykres <- ggplot(dystans_podsumowanie, aes(x=ryba, y=dystans, fill=typ)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  theme_classic() + 
  labs(x="numer osobnika", y="dystans [cm]", fill="rodzaj środowiska")+
  ggtitle("Przepłynięty dystans w zależności od rodzaju środowiska")

dystans_typ_ryba_wykres + 
  font("xy.text", size = 14, color = "black") +
  font("xlab", size = 15, color = "black") +
  font("ylab", size = 15, color = "black") +
  font("title", size=15, color="black") +
  theme(legend.position="bottom") +
  scale_x_continuous(n.breaks=15)+
  theme(axis.title.x = element_text(margin=margin(t=15)),
        axis.title.y = element_text(margin=margin(r=15)))


# PIASKOWNICA -------------------------------------------------------------


#usuwam części tysięczne
#czas_jednostkowy_ms <- str_split_fixed(dane$czas_jednostkowy, "\\.", n=2)

#łącze tabele
#dane <- cbind(dane, czas_jednostkowy_ms)

#nazywam nowe kolumny
#colnames(dane)[7]  <- "czas_hms"
#colnames(dane)[8]  <- "tysieczne"

#czas z formatu HH:MM:SS na sekundy
#dane$sekundy <- lubridate::period_to_seconds(hms(dane$czas_hms))

#łączę sekundy z częściami tysięcznymi
#dane$czas2 <- paste(dane$sekundy,dane$tysieczne,sep=".")

#obliczam czas spedzony w konkretnym sektorze, od n+1 odejmuję n, dodatkowo wszystko zamieniam na wartość bezwzględną
#dane$czas_spedzony <- abs(ave(dane$sekundy, FUN = function(x) c(diff(x),0)))


dane %>%
  group_by(ryba, dzien, typ) %>%
  mutate(abs(ave(dane$sekundy, FUN = function(x) c(diff(x),0))))

#podsumowanie czasu trwania wizyt z uwzglednieniem poszczególnych gatunków
group_by(dane) %>% 
  summarise(sekundy, ryba
    # count = n(),
    # mean = mean(czas_jednostkowy, na.rm = TRUE),
    # sd = sd(czas_jednostkowy, na.rm = TRUE),
    # median = median(czas_jednostkowy, na.rm = TRUE),
    # IQR = IQR(czas_jednostkowy, na.rm = TRUE),
    # MIN = min(czas_jednostkowy, na.rm = TRUE),
    # MAX = max(czas_jednostkowy, na.rm = TRUE)
  )
