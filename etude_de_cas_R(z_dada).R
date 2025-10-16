#tâche commerciale: l'objectif principal était de concevoir une stratégie marketing
#pour convertir les utilisateurs occationnels de Cyclistic en menbres annuels,
#les membres annuels étant jugés beaucoup plus rentables que les utilisateurs occationnels.



#J'ai initialisé en installant et chargeant la librairie tidyverse.
install.packages("tidyverse")
library(tidyverse)

#J'ai importé les deux jeux de données trimestriels(Divvy 2019 Q1 et divvy 2020 Q1) dans R.
q1_2019 <- read_csv("Divvy_Trips_2019_Q1 - Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1 - Divvy_Trips_2020_Q1.csv")

#J'ai utilisé la fonction rename() pour uniformiser le nom des colonnes du fichier 2019,
#afin qu'elles correspondent au format du fichier 2020.
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
))

#J'ai converti des variables clés comme l'identifiant du trajet(ride_id) en chaîne de caractères,
#pour assurer une cohérence des types de données avant de combiner les tables.
q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

#J'ai combiné les deux trimestres en une seule table nommée all_trips à l'aide de la fonction bind_rows(),
#créant ainsi une base de données consolidée pour l'analyse globale.
all_trips <- bind_rows(q1_2019, q1_2020)#, q3_2019)#, q4_2019, q1_2020)

#J'ai retiré les colonnes non pertinentes ou contenant des informations personnelles identifiables,
#comme la latitude/longitude de fin, l'année de naissance et le genre.
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  "tripduration"))
colnames(all_trips)

#J'ai normalisé la désignation des utilisateurs, 
#remplaçant les termes originaux par les catégories claires(member et casual) pour facliter l'agrégation.
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

#J'ai extrait des composants clé de la date de début du trajet, notamment le jour de la semaine (day_of_week),
#essentiel pour identifier les habitudes de trajet (loisir vs domicile-travail)
table(all_trips$member_casual)
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#J'ai calculé la durée trajet (ride_lenght) en seconde à partir des heures de début et de fin.
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#J'ai calculé des statistiques descriptives (moyenne, médiane, minimum, maximum) de la durée de trajet,
#regroupées par type d'utilisateur pour comparer les habitudes des deux groupes.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#J'ai approfondi en calculant la durée moyenne du trajet, 
#cette fois détaillée par type d'utilisateur et par jour de la semaine.
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#J'ai régroupé les données par utilisateur et jour de la semaine pour calculer deux indicateurs essentiels,
#le nombre de trajets(number_of_rides) et la durée moyenne (average_duration)
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()	
            ,average_duration = mean(ride_length)) %>% 
arrange(member_casual, weekday)	

#J'ai généré une visualisation (graphiques à barres), comparant le volume de trajets (axe Y),
#en fonction des jours de la semaine (axe X), avec une catégorisation par type d'utilisateur (couleur)
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge")

#J'ai calculé la moyenne de la durée du trajet par type d'utilisateur et par jour de la semaine.
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#J'ai exporté le tableau des durées moyennes de trajet dans un fichier CSV.
write.csv(counts, file = 'avg_ride_length.csv')


#Conclusion : à la lumière de tout ce qui précède, je constate que
#les membres annuels utilisent les vélos pour des trajets courts et fréquents en semaine tandis que
#les utilisateurs occasionnels font des trajets nettement plus longs, principalement le week-end.

#Le succès de l'entreprise repose sur l'adaptation d'une stratégie de conversion qui cible spécifiquement 
#des habitudes de loisir. j'ai non seulement extrait ces chiffres, mes j'ai également créé les visualisations
#nécessaires pour prouver cette recommandation à l'équipe de direction. 
#Ce projet valide ma capacité à transformer des données brutes en une stratégie commerciale exploitable.












