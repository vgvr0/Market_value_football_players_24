# Carga de librerías
library(Boruta)
library(caret)
library(corrplot)
library(cowplot)
library(doParallel)
library(dplyr)
library(dummies) 
library(gam)
library(ggplot2)
library(gridExtra)
library(klaR)
library(lubridate)
library(MASS)
library(mlbench)
library(missForest)
library(MXM)
library(naniar)
library(parallel)
#library(plyr) similar a dplyr
library(psych)
library(randomForest)
library(reshape2)
library(RColorBrewer)
library(sas7bdat)
library(VIM)

# Directorio de trabajo 
#setwd("")

# Número de núcleos para paralelizar partes del código
cl <- makeCluster(detectCores() - 1) 
registerDoParallel(cl) 

# Carga de datos
df <- read.csv("2_datos_con_dummies_y_missings_eliminados.csv")
dput(names(df))
str(df)

# Lista con todas las variables del dataset sin dummies

lista <- c("overall", "potential", "value_eur_m", "wage_eur_m", "age", 
           "height_cm", "weight_kg", "league_level", "club_position", 
           "weak_foot", "skill_moves", "international_reputation", "work_rate", 
           "release_clause_eur_m", "player_traits", "pace", "shooting", 
           "passing", "dribbling", "defending", "physic", "attacking_crossing", 
           "attacking_finishing", "attacking_heading_accuracy", "attacking_short_passing", 
           "attacking_volleys", "skill_dribbling", "skill_curve", "skill_fk_accuracy", 
           "skill_long_passing", "skill_ball_control", "movement_acceleration", 
           "movement_sprint_speed", "movement_agility", "movement_reactions", 
           "movement_balance", "power_shot_power", "power_jumping", "power_stamina", 
           "power_strength", "power_long_shots", "mentality_aggression", 
           "mentality_interceptions", "mentality_positioning", "mentality_vision", 
           "mentality_penalties", "mentality_composure", "defending_marking_awareness", 
           "defending_standing_tackle", "defending_sliding_tackle", "goalkeeping_diving", 
           "goalkeeping_handling", "goalkeeping_kicking", "goalkeeping_positioning", 
           "goalkeeping_reflexes", "years_remaining", "years_in_club", "preferred_foot", "category", "is_internacional")

base<-df[,lista]

# Selección de las columnas numéricas (NO TARGET)
listconti <- c("overall", "potential", "wage_eur_m", "age", 
              "height_cm", "weight_kg", "league_level", 
              "weak_foot", "skill_moves", "international_reputation",  
              "release_clause_eur_m", "pace", "shooting", 
              "passing", "dribbling", "defending", "physic", "attacking_crossing", 
              "attacking_finishing", "attacking_heading_accuracy", "attacking_short_passing", 
              "attacking_volleys", "skill_dribbling", "skill_curve", "skill_fk_accuracy", 
              "skill_long_passing", "skill_ball_control", "movement_acceleration", 
              "movement_sprint_speed", "movement_agility", "movement_reactions", 
              "movement_balance", "power_shot_power", "power_jumping", "power_stamina", 
              "power_strength", "power_long_shots", "mentality_aggression", 
              "mentality_interceptions", "mentality_positioning", "mentality_vision", 
              "mentality_penalties", "mentality_composure", "defending_marking_awareness", 
              "defending_standing_tackle", "defending_sliding_tackle", "goalkeeping_diving", 
              "goalkeeping_handling", "goalkeeping_kicking", "goalkeeping_positioning", 
              "goalkeeping_reflexes", "years_remaining", "years_in_club")

listclass <- c("category", "preferred_foot", "is_internacional")
vardep <- c("value_eur_m")

# 61 variables

base <- base[,c(listconti,listclass,vardep)]

# Estandarización
means <-apply(base[,listconti],2,mean,na.rm=TRUE)
sds<-sapply(base[,listconti],sd,na.rm=TRUE)

base2<-scale(base[,listconti], center = means, scale = sds)
base<-data.frame(cbind(base2,base[,c(listclass,vardep)]))

str(base)

basebis<-dummy.data.frame(base, listclass, sep = ".")#nuevo dataframe con las dummys

# Matriz para algunas selección de variables:
nombres1 <- c("overall", "potential", "wage_eur_m", "age", 
              "height_cm", "weight_kg", "league_level", 
              "weak_foot", "skill_moves", "international_reputation",  
              "release_clause_eur_m", "pace", "shooting", 
              "passing", "dribbling", "defending", "physic", "attacking_crossing", 
              "attacking_finishing", "attacking_heading_accuracy", "attacking_short_passing", 
              "attacking_volleys", "skill_dribbling", "skill_curve", "skill_fk_accuracy", 
              "skill_long_passing", "skill_ball_control", "movement_acceleration", 
              "movement_sprint_speed", "movement_agility", "movement_reactions", 
              "movement_balance", "power_shot_power", "power_jumping", "power_stamina", 
              "power_strength", "power_long_shots", "mentality_aggression", 
              "mentality_interceptions", "mentality_positioning", "mentality_vision", 
              "mentality_penalties", "mentality_composure", "defending_marking_awareness", 
              "defending_standing_tackle", "defending_sliding_tackle", "goalkeeping_diving", 
              "goalkeeping_handling", "goalkeeping_kicking", "goalkeeping_positioning", 
              "goalkeeping_reflexes", "years_remaining", "years_in_club", "category.Defensa", 
              "category.Delantero", "category.Medio", "preferred_foot.Left", "preferred_foot.Right", 
              "is_internacional.0", "is_internacional.1")

length(nombres1) #60 variables
vardep <- vardep <- c("value_eur_m")

archivo1 <- basebis 
y<-archivo1[,vardep]
x<-archivo1[,nombres1]

# Semilla 1
set.seed(123456)

# Selección de variables: método 1
filtro <- sbf(x, y, sbfControl = sbfControl(functions = rfSBF,method = "cv", verbose = FALSE))
a1 <- dput(filtro$optVariables)
length(a1) #52

variables_a1 <- c("overall", "potential", "wage_eur_m", "age", "weight_kg", "league_level", 
                  "weak_foot", "skill_moves", "international_reputation", "release_clause_eur_m", 
                  "pace", "shooting", "passing", "dribbling", "defending", "physic", 
                  "attacking_crossing", "attacking_finishing", "attacking_heading_accuracy", 
                  "attacking_short_passing", "attacking_volleys", "skill_dribbling", 
                  "skill_curve", "skill_fk_accuracy", "skill_long_passing", "skill_ball_control", 
                  "movement_acceleration", "movement_sprint_speed", "movement_agility", 
                  "movement_reactions", "movement_balance", "power_shot_power", 
                  "power_jumping", "power_stamina", "power_strength", "power_long_shots", 
                  "mentality_aggression", "mentality_interceptions", "mentality_positioning", 
                  "mentality_vision", "mentality_penalties", "mentality_composure", 
                  "defending_marking_awareness", "defending_standing_tackle", "defending_sliding_tackle", 
                  "years_remaining", "years_in_club", "category.Defensa", "category.Delantero", 
                  "category.Medio", "is_internacional.0", "is_internacional.1")

# Selección de variables: método 2
control_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
results <- rfe(x, y, sizes = c(1:8), rfeControl = control_rfe)

selecrfe <- results$optVariables
a2 <- dput(results$optVariables)
length(a2) #6

variables_a2 <- c("release_clause_eur_m", "overall", "potential", "age", "wage_eur_m", "movement_reactions")


# Selección de variables: método 3
full <- glm(value_eur_m~., data=base, family = gaussian(link = "identity"))
null <- glm(value_eur_m~1, data=base, family = gaussian(link = "identity"))

selec1 <- stepAIC(null, scope=list(upper=full), direction="both", family = gaussian(link = "identity"), trace=FALSE)
vec <- (names(selec1[[1]]))
length(vec) #28

a3 <- dput(vec)
variables_a3 <- c("(Intercept)", "release_clause_eur_m", "international_reputation", 
                  "power_stamina", "attacking_volleys", "weight_kg", "potential", 
                  "overall", "age", "is_internacional", "movement_balance", "categoryDelantero", 
                  "category.Medio", "mentality_interceptions", "skill_ball_control", 
                  "years_remaining", "years_in_club", "mentality_penalties", "power_jumping", 
                  "height_cm", "movement_acceleration", "attacking_heading_accuracy", 
                  "power_strength", "skill_dribbling", "dribbling", "league_level", 
                  "skill_fk_accuracy", "skill_curve")

# Selección de variables: método 4 stepAIC
#ponemos k=log(n) en stepAIC, en este caso n = 15129 observaciones, k = 9.62

full<-glm(value_eur_m~., data=base, family = gaussian(link = "identity"))
null<-glm(value_eur_m~1, data=base, family = gaussian(link = "identity"))

selec1<-stepAIC(null, scope=list(upper=full),
                direction="both", family = gaussian(link = "identity"), trace = FALSE, k = 9.62)

vec<-(names(selec1[[1]]))
length(vec) #15
a4 <- dput(vec)
variables_a4 <- c("release_clause_eur_m", "international_reputation", 
                  "power_stamina", "weight_kg", "potential", "overall", "age", 
                  "is_internacional.1", "movement_balance", "mentality_interceptions", 
                  "years_remaining", "years_in_club", "category.Delantero", "category.Medio")

# Selección de variables: método 5. Boruta

# También vale como Filter
out.boruta <- Boruta(value_eur_m~., data = base)
print(out.boruta)
summary(out.boruta)
sal<-data.frame(out.boruta$finalDecision)

sal2<-sal[which(sal$out.boruta.finalDecision=="Confirmed"),,drop=FALSE]
a5 <- dput(row.names(sal2))
length(a5) #31

variables_a5 <- c("overall", "potential", "wage_eur_m", "age", "international_reputation", 
                                                       "release_clause_eur_m", "pace", "shooting", "passing", "dribbling", 
                                                       "defending", "physic", "attacking_crossing", "attacking_short_passing", 
                                                       "skill_dribbling", "skill_curve", "skill_fk_accuracy", "skill_long_passing", 
                                                       "skill_ball_control", "movement_acceleration", "movement_sprint_speed", 
                                                       "movement_reactions", "power_stamina", "mentality_interceptions", 
                                                       "mentality_positioning", "mentality_vision", "mentality_composure", 
                                                       "defending_marking_awareness", "defending_standing_tackle", "defending_sliding_tackle", 
                                                       "years_remaining")
  
# Selección de variables: método 6. MXM
mmpc1 <- MMPC(vardep, archivo1, max_k = 2, hash = TRUE,test = "testIndFisher")
mmpc1@selectedVars
a6 <- dput(names(archivo1[,c(mmpc1@selectedVars)]))

length(a6) #10
variables_a6 <- c("overall", "wage_eur_m", "skill_moves", "international_reputation", 
                  "release_clause_eur_m", "movement_agility", "power_stamina", 
                  "years_remaining", "is_internacional.0", "is_internacional.1")

# Selección de variables: método 7
SES1 <- SES(vardep, archivo1, max_k = 3, hash = TRUE,
            test = "testIndFisher")

SES1@selectedVars

dput(names(archivo1[,c(SES1@selectedVars)]))
a7 <-dput(names(archivo1[,c(SES1@selectedVars)]))
length(a7) #6

variables_a7 <- c("overall", "international_reputation", "release_clause_eur_m", 
                  "attacking_volleys", "years_remaining", "is_internacional.1")

# Selección de variables: método 8
source("funcion steprepetido.R")

lista <- steprepetido(data=archivo1, vardep = c("value_eur_m"),
                    listconti = nombres1, sinicio=12345, sfinal = 12385, porcen = 0.8, criterio="AIC")


tabla <- lista[[1]]
a8 <- dput(lista[[2]][[1]])
length(a8) #27

variables_a8 <- c("release_clause_eur_m", "international_reputation", "attacking_volleys", 
                  "potential", "overall", "age", "is_internacional.1", "weight_kg", 
                  "movement_balance", "category.Delantero", "mentality_interceptions", 
                  "years_remaining", "height_cm", "skill_ball_control", "power_jumping", 
                  "movement_acceleration", "physic", "attacking_heading_accuracy", 
                  "mentality_aggression", "weak_foot", "years_in_club", "skill_dribbling", 
                  "dribbling", "mentality_penalties", "league_level", "category.Defensa", 
                  "mentality_composure")
  
# Selección de variables: método 9. STEP REPETIDO BIC
lista <- steprepetido(data=archivo1, vardep=c("value_eur_m"),
                      listconti = nombres1,
                      sinicio=12345, sfinal=12385, porcen = 0.8, criterio="BIC")


tabla<-lista[[1]]
dput(lista[[2]][[1]]) 
variables_9_1 <- c("release_clause_eur_m", "international_reputation", "power_stamina", 
                   "potential", "overall", "age", "weight_kg", "movement_balance", 
                   "category.Delantero", "mentality_interceptions", "is_internacional.0")

length(variables_9_1) #11

dput(lista[[2]][[2]])
variables_9_2 <- c("release_clause_eur_m", "international_reputation", "power_stamina", 
                   "defending_sliding_tackle", "age", "is_internacional.1", "years_remaining", 
                   "years_in_club", "category.Delantero")

length(variables_9_2) #9

# COMPROBADO LO ANTERIOR, EJECUTAR DESDE AQUÍ PARA NO TARDAR UN SIGLO
# VALIDACIÓN CRUZADA

source("cruzadas avnnet y lin.R")
data <- basebis

# Variables elegidas en método AIC
medias1<-cruzadalin(data=data,
                    vardep="value_eur_m",listconti=c("release_clause_eur_m", "international_reputation", 
                                                     "power_stamina", "attacking_volleys", "weight_kg", "potential", 
                                                     "overall", "age", "is_internacional.1", "movement_balance", "category.Delantero", 
                                                     "category.Medio", "mentality_interceptions", "skill_ball_control", 
                                                     "years_remaining", "years_in_club", "mentality_penalties", "power_jumping", 
                                                     "height_cm", "movement_acceleration", "attacking_heading_accuracy", 
                                                     "power_strength", "skill_dribbling", "dribbling", "league_level", 
                                                     "skill_fk_accuracy", "skill_curve"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias1$modelo="STEPAIC"

# intercept      RMSE  Rsquared       MAE     RMSESD  RsquaredSD       MAESD
# 1      TRUE 0.7436055 0.9912201 0.3043772 0.05348571 0.001029595 0.008124472

# Variables elegidas en método BIC
medias2<-cruzadalin(data=data,
                    vardep="value_eur_m",listconti=c("release_clause_eur_m", "international_reputation", 
                                                             "power_stamina", "weight_kg", "potential", "overall", "age", 
                                                             "is_internacional.1", "movement_balance", "mentality_interceptions", 
                                                             "years_remaining", "years_in_club", "category.Delantero", "category.Medio"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias2$modelo="STEPBIC"
# intercept     RMSE  Rsquared       MAE     RMSESD  RsquaredSD       MAESD
# 1      TRUE 0.745583 0.9911734 0.3024513 0.05373192 0.001039662 0.008219088

medias3<-cruzadalin(data=data,
                    vardep="value_eur_m",listconti=c("release_clause_eur_m", "international_reputation", "attacking_volleys", 
                                                              "potential", "overall", "age", "is_internacional.1", "weight_kg", 
                                                              "movement_balance", "category.Delantero", "mentality_interceptions", 
                                                              "years_remaining", "height_cm", "skill_ball_control", "power_jumping", 
                                                              "movement_acceleration", "physic", "attacking_heading_accuracy", 
                                                              "mentality_aggression", "weak_foot", "years_in_club", "skill_dribbling", 
                                                              "dribbling", "mentality_penalties", "league_level", "category.Defensa", 
                                                              "mentality_composure"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias3$modelo="STEPrep1"
# intercept      RMSE  Rsquared       MAE    RMSESD  RsquaredSD       MAESD
# 1      TRUE 0.7436124 0.9912198 0.3040198 0.0534674 0.001029419 0.008176405

medias4<-cruzadalin(data=data,
                    vardep="value_eur_m",listconti=c("release_clause_eur_m", "international_reputation", 
                                                             "power_stamina", "weight_kg", "potential", "overall", "age", 
                                                             "is_internacional.1", "movement_balance", "mentality_interceptions", 
                                                             "years_remaining", "years_in_club", "category.Delantero", "category.Medio"),
                    listclass=c(""),grupos=4,sinicio=1234,repe=25)
medias4$modelo="STEPrep2"
# intercept     RMSE  Rsquared       MAE     RMSESD  RsquaredSD       MAESD
# 1      TRUE 0.745583 0.9911734 0.3024513 0.05373192 0.001039662 0.008219088

medias5<-cruzadalin(data=data,
                   vardep="value_eur_m",listconti=c("overall", "potential", "wage_eur_m", "age", "weight_kg", "league_level", 
                                                     "weak_foot", "skill_moves", "international_reputation", "release_clause_eur_m", 
                                                     "pace", "shooting", "passing", "dribbling", "defending", "physic", 
                                                     "attacking_crossing", "attacking_finishing", "attacking_heading_accuracy", 
                                                     "attacking_short_passing", "attacking_volleys", "skill_dribbling", 
                                                     "skill_curve", "skill_fk_accuracy", "skill_long_passing", "skill_ball_control", 
                                                     "movement_acceleration", "movement_sprint_speed", "movement_agility", 
                                                     "movement_reactions", "movement_balance", "power_shot_power", 
                                                     "power_jumping", "power_stamina", "power_strength", "power_long_shots", 
                                                     "mentality_aggression", "mentality_interceptions", "mentality_positioning", 
                                                     "mentality_vision", "mentality_penalties", "mentality_composure", 
                                                     "defending_marking_awareness", "defending_standing_tackle", "defending_sliding_tackle", 
                                                     "years_remaining", "years_in_club", "category.Defensa", "category.Delantero", 
                                                     "category.Medio", "is_internacional.0", "is_internacional.1"),
                   listclass=c(""),grupos=4,sinicio=1234,repe=25)

medias5$modelo="SBF"
# intercept      RMSE  Rsquared       MAE     RMSESD  RsquaredSD       MAESD
# 1      TRUE 0.7457822 0.9911675 0.3059785 0.05343494 0.001039633 0.007890434
medias6<-cruzadalin(data=data,
                    vardep="value_eur_m",listconti=c("release_clause_eur_m", "overall", "potential", "age", "wage_eur_m", "movement_reactions"),
                    listclass=c(""), grupos=4, sinicio=1234, repe=25)

medias6$modelo="RFE"
# intercept      RMSE  Rsquared       MAE     RMSESD  RsquaredSD       MAESD
# 1      TRUE 0.7710036 0.9905551 0.3045512 0.05873291 0.001201108 0.009003541
medias7<-cruzadalin(data=data,
                    vardep="value_eur_m",listconti=c("overall", "potential", "wage_eur_m", "age", "international_reputation", 
                                                     "release_clause_eur_m", "pace", "shooting", "passing", "dribbling", 
                                                     "defending", "physic", "attacking_crossing", "attacking_short_passing", 
                                                     "skill_dribbling", "skill_curve", "skill_fk_accuracy", "skill_long_passing", 
                                                     "skill_ball_control", "movement_acceleration", "movement_sprint_speed", 
                                                     "movement_reactions", "power_stamina", "mentality_interceptions", 
                                                     "mentality_positioning", "mentality_vision", "mentality_composure", 
                                                     "defending_marking_awareness", "defending_standing_tackle", "defending_sliding_tackle", 
                                                     "years_remaining"),
                    listclass=c(""),grupos=5,sinicio=1234,repe=25)

medias7$modelo="Boruta"
# intercept      RMSE  Rsquared       MAE     RMSESD  RsquaredSD      MAESD
# 1      TRUE 0.7467337 0.9911047 0.3024539 0.06089944 0.001436395 0.01040839

medias8<-cruzadalin(data=data,
                    vardep="value_eur_m",listconti=c("overall", "wage_eur_m", "skill_moves", "international_reputation", 
                                                     "release_clause_eur_m", "movement_agility", "power_stamina", 
                                                     "years_remaining", "is_internacional.0", "is_internacional.1"),
                    listclass=c(""),grupos=5,sinicio=1234,repe=25)

medias8$modelo="MXM"
# intercept      RMSE  Rsquared       MAE     RMSESD  RsquaredSD     MAESD
# 1      TRUE 0.7495236 0.9910333 0.3024811 0.06100133 0.001456637 0.0103531
# Hasta aquí muy rápido
# Union para el boxplot

union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8)
par(cex.axis=0.9, las = 2)
boxplot(data=union1,col="cyan",error~modelo)


union1$error2<-sqrt(union1$error)
par(cex.axis=0.9, las = 2)
boxplot(data=union1,col="cyan",error2~modelo)

# Test con los diferentes conjuntos de variables
# con StepAIC (500 obs / parametro)
# h = 20

stepAIC <- c("release_clause_eur_m", "international_reputation", 
  "power_stamina", "attacking_volleys", "weight_kg", "potential", 
  "overall", "age", "is_internacional.1", "movement_balance", "category.Delantero", 
  "category.Medio", "mentality_interceptions", "skill_ball_control", 
  "years_remaining", "years_in_club", "mentality_penalties", "power_jumping", 
  "height_cm", "movement_acceleration", "attacking_heading_accuracy", 
  "power_strength", "skill_dribbling", "dribbling", "league_level", 
  "skill_fk_accuracy", "skill_curve")

#value_eur_m~release_clause_eur_m+international_reputation+power_stamina+attacking_volleys+weight_kg+potential+overall+age+is_internacional.1+movement_balance+category.Delantero+category.Medio+mentality_interceptions+skill_ball_control+years_remaining+years_in_club+mentality_penalties+power_jumping+height_cm+movement_acceleration+attacking_heading_accuracy+power_strength+skill_dribbling+dribbling+league_level+skill_fk_accuracy+skill_curve

control <- trainControl(method = "cv", number = 7, savePredictions = "all")
avnnetgrid <- expand.grid(size = c(20),
                          decay = c(0.01, 0.1), bag = FALSE)

redavnnet <- train(value_eur_m ~ release_clause_eur_m + international_reputation + power_stamina + attacking_volleys + weight_kg + potential + overall + age + is_internacional.1 + movement_balance + category.Delantero + category.Medio + mentality_interceptions + skill_ball_control + years_remaining + years_in_club + mentality_penalties + power_jumping + height_cm + movement_acceleration + attacking_heading_accuracy + power_strength + skill_dribbling + dribbling + league_level + skill_fk_accuracy + skill_curve,
                   data = data, method = "avNNet", linout = TRUE, maxit = 100,
                   trControl = control, tuneGrid = avnnetgrid, repeats = 5, 
                   allowParallel = TRUE)
redavnnet


# Resampling: Cross-Validated (7 fold) 
# Summary of sample sizes: 12968, 12969, 12968, 12967, 12968, 12967, ... 
# Resampling results across tuning parameters:
# 
#   size  decay  RMSE      Rsquared   MAE      
#   17    0.001  1.236849  0.9770822  0.3691273
#   17    0.010  1.128893  0.9800349  0.3483474
#   17    0.100  1.219989  0.9772936  0.3704919
#   20    0.001  1.157392  0.9778660  0.3685027
#   20    0.010  1.193045  0.9779097  0.3494402
#   20    0.100  1.151851  0.9791794  0.3545829
#   23    0.001  1.147541  0.9810943  0.3520308
#   23    0.010  1.130553  0.9800053  0.3614498
#   23    0.100  1.136573  0.9801561  0.3582076
#   27    0.001  1.194860  0.9776985  0.3804899
#   27    0.010  1.114979  0.9801026  0.3676952
#   27    0.100  1.169854  0.9782067  0.3721630
# 
# Tuning parameter 'bag' was held constant at a value of FALSE
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were size = 33, decay = 0.001 and bag = FALSE.

medias9 <- cruzadaavnnet(data = data,
                         vardep = "value_eur_m",
                         listconti = c("release_clause_eur_m", "international_reputation", 
                                       "power_stamina", "attacking_volleys", "weight_kg", "potential", 
                                       "overall", "age", "is_internacional.1", "movement_balance", "category.Delantero", 
                                       "category.Medio", "mentality_interceptions", "skill_ball_control", 
                                       "years_remaining", "years_in_club", "mentality_penalties", "power_jumping", 
                                       "height_cm", "movement_acceleration", "attacking_heading_accuracy", 
                                       "power_strength", "skill_dribbling", "dribbling", "league_level", 
                                       "skill_fk_accuracy", "skill_curve"),
                         listclass = c(""), 
                         grupos = 10, 
                         sinicio = 1235, 
                         repe = 25, 
                         repeticiones = 5, 
                         itera = 100,
                         size = c(20), 
                         decay = c(0.01))

# Con tamaño 20 y decay 0.01
# size decay   bag     RMSE  Rsquared       MAE    RMSESD RsquaredSD      MAESD
# 1   20  0.01 FALSE 1.229625 0.9761021 0.4014488 0.5918416 0.01756561 0.04332425

medias9$modelo="zr_STEPAIC" # 20 nodos y 27 variables seleccionadas del método AIC

union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9)

par(cex.axis=1.0, las=2)
boxplot(data=union1,col="cyan",error~modelo)

# con stepBIC (14 variables)
# El numero de nodos optimo es 34

paste(c("release_clause_eur_m", "international_reputation", 
        "power_stamina", "weight_kg", "potential", "overall", "age", 
        "is_internacional.1", "movement_balance", "mentality_interceptions", 
        "years_remaining", "years_in_club", "category.Delantero", "category.Medio"),collapse = "+")

value_eur_m~release_clause_eur_m+international_reputation+power_stamina+weight_kg+potential+overall+age+is_internacional.1+movement_balance+mentality_interceptions+years_remaining+years_in_club+category.Delantero+category.Medio

control<-trainControl(method = "cv", number = 5, savePredictions = "all") 
set.seed(123)
avnnetgrid <-expand.grid(size=c(28,32,34),
                         decay=c(0.01,0.1),bag=FALSE)

redavnnet<- train(value_eur_m~release_clause_eur_m+international_reputation+power_stamina+weight_kg+potential+overall+age+is_internacional.1+movement_balance+mentality_interceptions+years_remaining+years_in_club+category.Delantero+category.Medio,
                  data=data,method="avNNet",linout = TRUE,maxit=100,
                  trControl=control,tuneGrid=avnnetgrid, repeats=5)

redavnnet

# 15129 samples
# 14 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 12103, 12103, 12103, 12105, 12102 
# Resampling results across tuning parameters:
#   
#   size  decay  RMSE       Rsquared   MAE      
# 28    0.001  0.8686732  0.9883076  0.2763148
# 28    0.010  0.8787242  0.9878734  0.2713265
# 28    0.100  0.9394300  0.9859383  0.2734086
# 30    0.001  0.8814992  0.9877642  0.2861368
# 30    0.010  0.8787462  0.9884073  0.2676507
# 30    0.100  0.8466426  0.9888089  0.2685555
# 33    0.001  0.8638832  0.9879644  0.2723132
# 33    0.010  0.8882455  0.9875489  0.2707596
# 33    0.100  0.8305828  0.9892712  0.2568360
# 34    0.001  0.9235949  0.9866821  0.2668244
# 34    0.010  0.8509376  0.9885076  0.2734488
# 34    0.100  0.8368521  0.9892319  0.2530016
# 35    0.001  0.8575092  0.9885932  0.2653762
# 35    0.010  0.8301164  0.9891227  0.2712058
# 35    0.100  0.8632437  0.9884798  0.2703303


medias10<-cruzadaavnnet(data=data,
                        vardep="value_eur_m", c("release_clause_eur_m", "international_reputation", 
                                                "power_stamina", "weight_kg", "potential", "overall", "age", 
                                                "is_internacional.1", "movement_balance", "mentality_interceptions", 
                                                "years_remaining", "years_in_club", "category.Delantero", "category.Medio"),
                        listclass=c(""),grupos=10,sinicio=1235,repe=25,repeticiones=5,itera=100,
                        size=c(33),decay=c(0.01))

medias10$modelo="zr_STEPBIC" #33 nodos y variables seleccionadas con STEPBIC

# size decay   bag      RMSE  Rsquared      MAE    RMSESD RsquaredSD      MAESD
# 1   33   0.1 FALSE 0.9827395 0.9850804 0.316356 0.4863367 0.01087616 0.03164791

union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9,medias10)

par(cex.axis=1.0, las=2)
boxplot(data=union1,col="cyan",error~modelo)


# Red con STEP REPETIDO1

# Variables de steprep1 (9 variables)
# Número de nodos 51

paste(c("release_clause_eur_m", "international_reputation", "power_stamina", 
        "defending_sliding_tackle", "age", "is_internacional.1", "years_remaining", 
        "years_in_club", "category.Delantero"),collapse = "+")

value_eur_m~release_clause_eur_m + international_reputation + power_stamina + defending_sliding_tackle + age + is_internacional.1 + years_remaining + years_in_club + category.Delantero

control<-trainControl(method = "cv",number=5,savePredictions = "all") 
set.seed(123)
avnnetgrid <-expand.grid(size=c(49),
                         decay=c(0.01,0.1),bag=FALSE)
redavnnet<- train(value_eur_m~release_clause_eur_m + international_reputation + power_stamina + defending_sliding_tackle + age + is_internacional.1 + years_remaining + years_in_club + category.Delantero,
                  data=data,method="avNNet",linout = TRUE,maxit=100,
                  trControl=control,tuneGrid=avnnetgrid, repeats=5)

redavnnet

# 15129 samples
# 9 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 12103, 12103, 12103, 12105, 12102 
# Resampling results across tuning parameters:
#   
# size  decay  RMSE       Rsquared   MAE      
# 47    0.001  0.9173642  0.9870806  0.3197883
# 47    0.010  0.8878302  0.9880077  0.3130992
# 47    0.100  0.9226121  0.9869393  0.3193348
# 49    0.001  0.8682853  0.9884530  0.3122436
# 49    0.010  0.8744016  0.9880053  0.3120502
# 49    0.100  0.8920008  0.9876894  0.3197015
# 51    0.001  0.8669497  0.9885350  0.3110278
# 51    0.010  0.8751612  0.9880865  0.3061546
# 51    0.100  0.8917000  0.9876773  0.3103530
# 
# Tuning parameter 'bag' was held constant at a value of FALSE
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were size = 51, decay = 0.001 and bag = FALSE.

medias11<-cruzadaavnnet(data=data,
                        vardep="value_eur_m",listconti=c("release_clause_eur_m", "international_reputation", "power_stamina", 
                                                         "defending_sliding_tackle", "age", "is_internacional.1", "years_remaining", 
                                                         "years_in_club", "category.Delantero"),
                        listclass=c(""),grupos=10,sinicio=1235,repe=25,repeticiones=5,itera=100,
                        size=c(51),decay=c(0.01))

medias11$modelo="zr_SETPrep1"#51 nodos y variables seleccionadas con STEPrep1

union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9, medias11)

par(cex.axis=1.0, las=2)
boxplot(data=union1,col="cyan",error~modelo)

# SELECCIÓN DE VARIABLES CON RF 
# PRUEBA CON RFOREST 30 AUNQUE SE PUEDE PROBAR CON BAGGING 

rfgrid<-expand.grid(mtry=c(30))

control<-trainControl(method = "cv",number = 5,savePredictions = "all") 

rf<- train(value_eur_m~.,data=data,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = T,ntree=1200,nodesize=10,replace=TRUE,
           importance=TRUE)

rf
# CON IMPORTANCIA DE VARIABLES RANDOM FOREST

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$IncNodePurity),]
tabla

# tabla<-tabla[order(-tabla$"%IncMSE"),]
# tabla

barplot(tabla$IncNodePurity,names.arg=rownames(tabla))

lista<-dput(rownames(tabla))

vardep<-"value_eur_m"

vacio2<-data.frame()

#Dejo comentado porque solo es para el gráfico
for (i in (2:20))
{
  varis<-lista[1:i]
  data2<-data[,c(varis,vardep)]
  rfgrid<-expand.grid(mtry=c(i))

  control<-trainControl(method = "cv",number=5,savePredictions = "all")

  rf<- train(value_eur_m~.,data=data2,
             method="rf",trControl=control,tuneGrid=rfgrid,
             linout = T,ntree=300,nodesize=10,replace=TRUE,
             importance=TRUE)

  a<-rf$results$MAE
  vacio <- data.frame(Variables = i, MAE = a)
  vacio2<-rbind(vacio,vacio2)

}
vacio2 <- arrange(vacio2, Variables)

ggplot(vacio2,aes(y=MAE, x=Variables))+geom_point()+geom_line()+
  scale_x_continuous(breaks = vacio2$Variables)+
  scale_y_continuous(breaks = vacio2$MAE) +  labs(title="RANDOM FOREST")


selecrandomforest<-lista[1:7]

dput(selecrandomforest)

# release_clause_eur_m        71.47823342  5.300338e+05
# overall                     28.21689722  2.059854e+05
# potential                   24.38681375  7.243883e+04
# wage_eur_m                  12.66219001  5.753354e+04
# movement_reactions          10.79225757  4.401533e+04
# skill_ball_control           7.24879656  7.986305e+03
# age                         15.50655442  3.659775e+03
# dribbling                    2.48139615  2.837130e+03
# movement_sprint_speed        0.14850823  2.155152e+03
# attacking_finishing          1.99305653  1.472433e+03
# attacking_short_passing      4.04805855  1.368844e+03
# skill_dribbling              2.02849976  9.294926e+02
# pace                        -1.03695530  8.313058e+02
# mentality_positioning        0.76375866  7.716361e+02
# international_reputation     1.71755816  7.049665e+02
# mentality_composure          5.39114181  6.480652e+02
# shooting                     1.75195196  6.300259e+02
# movement_acceleration        1.50334564  5.917670e+02
# skill_long_passing           3.76120571  5.687084e+02
# power_shot_power             1.27040321  5.315719e+02
# years_remaining              4.08340320  5.001679e+02
# defending_sliding_tackle     2.03969792  4.816922e+02
# movement_agility            -1.77013956  4.112183e+02
# power_stamina                5.22520187  3.614607e+02
# defending_marking_awareness  2.26096005  3.157170e+02
# goalkeeping_diving          -0.98658616  2.778378e+02
# height_cm                    2.79387576  2.726568e+02
# skill_fk_accuracy            2.76088684  2.548992e+02
# defending_standing_tackle    3.81813789  2.525836e+02
# passing                      3.57995951  2.460104e+02
# defending                    4.37542214  2.313474e+02
# mentality_interceptions      3.42562738  2.224515e+02
# attacking_heading_accuracy   2.56479881  2.191522e+02
# physic                       0.65369217  2.117475e+02
# power_jumping                2.37095375  2.116375e+02
# attacking_crossing           3.19710178  2.075376e+02
# goalkeeping_handling        -0.64241081  2.045059e+02
# mentality_penalties          0.73679590  2.011886e+02
# movement_balance             1.68320054  1.997846e+02
# mentality_vision             2.92824411  1.952934e+02
# attacking_volleys            0.96790027  1.948163e+02
# goalkeeping_kicking         -1.31073161  1.945144e+02
# goalkeeping_reflexes        -0.79650829  1.839437e+02
# power_long_shots             3.83292145  1.793039e+02
# weight_kg                    1.80802476  1.786964e+02
# weak_foot                    1.58879248  1.703217e+02
# power_strength               2.64062303  1.636469e+02
# mentality_aggression         1.94471017  1.627831e+02
# years_in_club               -1.23569337  1.596232e+02
# skill_curve                  2.67516332  1.580573e+02
# goalkeeping_positioning      0.66082695  1.321738e+02
# skill_moves                 -0.07071613  1.054955e+02
# is_internacional.1           1.51120782  4.641138e+01
# is_internacional.0           2.08338113  3.243959e+01
# preferred_foot.Right        -0.13419155  1.704140e+01
# category.Medio               0.36427061  1.404676e+01
# category.Delantero          -0.97007640  1.255207e+01
# preferred_foot.Left         -0.18205662  1.231115e+01
# category.Defensa             0.06416935  1.096760e+01
# league_level                 0.03999268  5.821629e+00

variables_rf <- c("release_clause_eur_m", "overall", "potential", "wage_eur_m", "movement_reactions", "skill_ball_control", "age")
paste(c("release_clause_eur_m", "overall", "potential", "wage_eur_m", "movement_reactions", "skill_ball_control", "dribbling"), collapse = " + ")

# Lineal con random_forest seleccion de variables
medias12 <-cruzadalin(data=data,
                    vardep="value_eur_m",listconti=variables_rf,
                    listclass=c(""), grupos=10, sinicio=1234, repe=25)

medias12$modelo="rf_lineal"

# intercept      RMSE  Rsquared       MAE     RMSESD  RsquaredSD      MAESD
# 1      TRUE 0.7695829 0.9905504 0.3050161 0.06350201 0.001565473 0.01081121

union1 <-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias12)
par(cex.axis=0.9, las=2)
boxplot(data=union1,col="cyan",error~modelo)

# Red con random_forest selección de variables
# Tenemos 7, por lo que 55 nodos 

control<-trainControl(method = "cv", number = 5, savePredictions = "all") 
set.seed(123)
avnnetgrid <-expand.grid(size=c(45,53),
                         decay=c(0.01,0.1,0.001),bag=FALSE)

redavnnet<- train(value_eur_m~release_clause_eur_m + overall + potential + wage_eur_m + movement_reactions + skill_ball_control + age,
                  data=data,method="avNNet",linout = TRUE,maxit=100,
                  trControl=control,tuneGrid=avnnetgrid, repeats=5)

redavnnet

# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 12103, 12103, 12103, 12105, 12102 
# Resampling results across tuning parameters:
#   
# size  decay  RMSE       Rsquared   MAE      
# 50    0.001  0.8240752  0.9894396  0.2975340
# 50    0.010  0.8241754  0.9894730  0.2954904
# 50    0.100  0.8315770  0.9893029  0.2920147
# 53    0.001  0.8136922  0.9898322  0.2947727
# 53    0.010  0.7722893  0.9908583  0.2872407
# 53    0.100  0.7984537  0.9900008  0.2902439
# 55    0.001  0.7895871  0.9905337  0.2905865
# 55    0.010  0.7785420  0.9905775  0.2843074
# 55    0.100  0.8076598  0.9899902  0.2889998
# 
# Tuning parameter 'bag' was held constant at a value of FALSE
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were size = 53, decay = 0.01 and bag = FALSE.

medias13<-cruzadaavnnet(data=data,
                        vardep="value_eur_m",listconti=variables_rf,
                        listclass=c(""),grupos=4,sinicio=1234,repe=25, size=c(53),decay=c(0.1),repeticiones=5,itera=1000)


medias13$modelo="zrf_red"

# size decay   bag      RMSE  Rsquared      MAE    RMSESD  RsquaredSD      MAESD
# 1   53  0.01 FALSE 0.7414321 0.9911114 0.164906 0.3217185 0.007714562 0.01499751

union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9, medias13)
par(cex.axis=1.0, las=2)
boxplot(data=union1,col="cyan",error~modelo)

# Comprobación de los nodos:

# Red con random_forest selección de variables
# Tenemos 7, por lo que 55 nodos 
variables_rf <- c("release_clause_eur_m", "overall", "potential", "wage_eur_m", "movement_reactions", "skill_ball_control", "age")

control<-trainControl(method = "cv", number = 5, savePredictions = "all") 
set.seed(123)
avnnetgrid <-expand.grid(size=c(5,10,15,20,30,40,50),
                         decay=c(0.01,0.1),bag=FALSE)

redavnnet<- train(value_eur_m~release_clause_eur_m + overall + potential + wage_eur_m + movement_reactions + skill_ball_control + age,
                  data=data,method="avNNet",linout = TRUE,maxit=100,
                  trControl=control,tuneGrid=avnnetgrid, repeats=5)

redavnnet

# TUNEO DEL PARÁMETRO maxit con la mejor red
# Validación cruzada con avNNet

control<-trainControl(method = "cv",
                      number=5, savePredictions = "all") 

set.seed(123)
nnetgrid <- expand.grid(size=c(5, 20, 40, 50), decay=c(0.1,0.01, 0.01/2), bag=F)

completo<-data.frame()
listaiter<-c(10, 20, 50, 100, 500, 1000, 1200)

# Cambiar variables por el modelo elegido en este caso el mejor era con las 7 de rf (modelo rf_red)
for (iter in listaiter)
{
  rednnet<- train(value_eur_m~release_clause_eur_m + overall + potential + wage_eur_m + movement_reactions + skill_ball_control + dribbling,
                  data=data,
                  method="avNNet",linout = TRUE,maxit=iter,
                  trControl=control,repeats=5, tuneGrid=nnetgrid,trace=F)
  rednnet$results$itera<-iter
  completo<-rbind(completo,rednnet$results)
  
}

completo<-completo[order(completo$RMSE),]

ggplot(completo, aes(x=factor(itera), y=RMSE, 
                     color=factor(decay),pch=factor(size))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# Llega un momento en que se ve que se sobreajusta, es necesario early stopping
# size decay   bag      RMSE  Rsquared       MAE     RMSESD   RsquaredSD       MAESD itera
# 1   10  0.01 FALSE 0.6000679 0.9943066 0.1892569 0.13713812 0.0016728044 0.013391603  1500
# 2   10  0.10 FALSE 0.5800655 0.9947278 0.1929375 0.04773237 0.0004135345 0.005895104  1500
# 3   20  0.01 FALSE 0.6030690 0.9943352 0.1670892 0.10165305 0.0012523884 0.015866184  1500
# 4   20  0.10 FALSE 0.6564546 0.9936167 0.1722719 0.09905182 0.0008988284 0.008085325  1500
# 5   30  0.01 FALSE 0.6425461 0.9937049 0.1591170 0.16897165 0.0021690293 0.014575135  1500
# 6   30  0.10 FALSE 0.7154135 0.9923849 0.1655306 0.17267599 0.0021882349 0.008994776  1500
# 7   53  0.01 FALSE 0.8463020 0.9890927 0.1658147 0.33762533 0.0069161524 0.018127941  1500
# 8   53  0.10 FALSE 0.8151749 0.9897772 0.1680668 0.29919137 0.0056591160 0.016485926  1500

# Añadimos la caja de la red:

medias17<-cruzadaavnnet(data=data,
                        vardep="value_eur_m",listconti=variables_rf,
                        listclass=c(""),grupos=10,sinicio=1234,repe=25,repeticiones=5,itera=1000,
                        size=c(5),decay=c(0.01))

medias17$modelo="Red_cv10"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9, medias10, medias11, medias12, medias13, medias1, medias17)

par(cex.axis=1.0, las = 2)#las=2 si lo ponemos aqui caben los nombres de las variables
boxplot(data=union1,col="cyan",error~modelo)


union1$error2<-sqrt(union1$error)

par(cex.axis=1.0, las = 2)
boxplot(data=union1,col="cyan",error2~modelo)


medias18<-cruzadaavnnet(data=data,
                        vardep="value_eur_m",listconti=variables_rf,
                        listclass=c(""),grupos=5,sinicio=1234,repe=25,repeticiones=5,itera=1000,
                        size=c(5),decay=c(0.01))

medias18$modelo="Red_cv5"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9, medias10, medias11, medias12, medias13, medias1, medias17, medias18)

par(cex.axis=1.0, las = 2)#las=2 si lo ponemos aqui caben los nombres de las variables
boxplot(data=union1,col="cyan",error~modelo)


union1$error2<-sqrt(union1$error)

par(cex.axis=1.0, las = 2)
boxplot(data=union1,col="cyan",error2~modelo)

# otra de prueba
medias16<-cruzadaavnnet(data=data,
                        vardep="value_eur_m",listconti=variables_rf,
                        listclass=c(""),grupos=4,sinicio=1234,repe=25,repeticiones=5,itera=500,
                        size=c(50),decay=c(0.1))

medias16$modelo="Red2"


union1<-rbind(medias1,medias2,medias3,medias4,medias5,medias6,medias7,medias8,medias9, medias10, medias11, medias12, medias13, medias15, medias16)

par(cex.axis=1.0, las = 2)#las=2 si lo ponemos aqui caben los nombres de las variables
boxplot(data=union1,col="cyan",error~modelo)


union1$error2<-sqrt(union1$error)

par(cex.axis=1.0, las = 2)
boxplot(data=union1,col="cyan",error2~modelo)

##############################################################################
#---------------------------COMPARACIÓN R Y SAS------------------------------#
data#datos limpios
basebis
# Hay que tener en cuenta que la evaluación más precisa de la performance 
# se obtiene con los resultados de cv repetida y boxplot. 
# 
# 
# 1) Crearemos archivo train-test  en R y crearemos los archivos sas correspondientes.
# 2) Construiremos el mejor modelo con train en SAS y el mejor modelo en R
# 3) Predeciremos test y evaluaremos y compararemos la performance.

# Obviamente los mismos sets de variables con regresión tendrán las mismas performance en R y SAS.
# Pero las redes neuronales no.

# 1) Crearemos archivo train-test  en R y crearemos los archivos sas correspondientes.
# Dividiremos en train test el archivo, 70%ag train, 30% test

##################################################3
##############################################################################
#---------------------------COMPARACIÓN R Y SAS------------------------------#
# Establecer la semilla para reproducibilidad
set.seed(12345)

sample_size <- floor(0.3 * nrow(basebis))
test_indices <- sample(seq_len(nrow(basebis)), size = sample_size)
test_data <- basebis[test_indices, ]
train_data <- basebis[-test_indices, ]

rf_sec <- c("release_clause_eur_m", "overall", "potential", "wage_eur_m", "movement_reactions", "skill_ball_control", "age", "value_eur_m")

test_selected_rf <- test_data[, rf_sec]
train_selected_rf <- train_data[, rf_sec]

# Guarda los conjuntos de prueba y entrenamiento en archivos CSV
write.csv(test_selected_rf, file = "rf_train.csv", row.names = FALSE)
write.csv(train_selected_rf, file = "rf_test.csv", row.names = FALSE)

st_sec <- c("release_clause_eur_m", "international_reputation", "power_stamina", 
            "defending_sliding_tackle", "age", "is_internacional.1", "years_remaining", 
            "years_in_club", "category.Delantero", "value_eur_m")

test_selected_st <- test_data[, st_sec]
train_selected_st <- train_data[, st_sec]

write.csv(test_selected_st, file = "st_train.csv", row.names = FALSE)
write.csv(train_selected_st, file = "st_test.csv", row.names = FALSE)


# ******************
# FUNCIÓN PREDICT
# ******************

# En principio el mejor modelo obtenido con R es con las variables rf

# 1) Construimos la red con los datos train
# 2) Evaluamos la red construida sobre datos test con la función predict.
set.seed(136919)
test <- test_selected_rf
train <- train_selected_rf

library(caret)
control<-trainControl(method = "none") 
nnetgrid <-  expand.grid(size=c(5),decay=c(0.01),bag=F)
rednnet<- train(value_eur_m~release_clause_eur_m + overall + potential + wage_eur_m + movement_reactions + skill_ball_control + age,
                data=train,method="avNNet",linout = TRUE,
                trControl=control,repeats=5,tuneGrid=nnetgrid,maxit=1000,trace=T)

# Se aplica la función predict:

predicciones<-predict(rednnet,test)
# Se añaden las predicciones al archivo test y se calcula el error:

comple<-cbind(test,predicciones)

comple$error<-(comple$value_eur_m-comple$predicciones)^2

MSE<-mean(comple$error); MSE #0.261676
RMSE<-sqrt(MSE); RMSE #0.5115428

# Predicciones con la semilla 2
set.seed(123456)
test <- test_selected_rf
train <- train_selected_rf

library(caret)
control<-trainControl(method = "none") 
nnetgrid <-  expand.grid(size=c(5),decay=c(0.01),bag=F)
rednnet<- train(value_eur_m~release_clause_eur_m + overall + potential + wage_eur_m + movement_reactions + skill_ball_control + age,
                data=train,method="avNNet",linout = TRUE,
                trControl=control,repeats=5,tuneGrid=nnetgrid,maxit=1000,trace=T)

predicciones<-predict(rednnet,test)
comple<-cbind(test,predicciones)
comple$error<-(comple$value_eur_m-comple$predicciones)^2
MSE<-mean(comple$error); MSE #0.2992231
RMSE<-sqrt(MSE); RMSE #0.5470129


