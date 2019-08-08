# Pesquisa Reproduzível: Analisando um monitor de passos de uma pessoa. 
### Carregando e pré-processando os dados

library(data.table)
Originais <- fread("activity.csv")
Originais$date <- as.Date(Originais$date, "%Y-%m-%d")

summary(Originais)
head(Originais)
length(Originais$steps)

## Qual é o número total médio de passos dados por dia?

Passos <- Originais[, lapply(.SD, sum), by = date, .SDcols = "steps"]

par(lend = "square")

plot(Passos$date, Passos$steps, 
     type = "h", lwd = 10, col = "black", 
     xlab = "date", ylab = "number of steps",
     main = "number of steps per day")

lines(Passos$date, Passos$steps, type = "h", lwd = 7, col = "magenta")

mean(Passos$steps, na.rm = T);median(Passos$steps, na.rm = T)

## Qual é o padrão de atividade média diária?

Passos2 <- Originais[!is.na(Originais$steps)][,lapply(.SD, mean), by = interval, .SDcols = "steps"]

plot(Passos2$interval, Passos2$steps, type = "l", 
     xlab = "interval", ylab = "steps", 
     main = "steps by interval")

Passos2[which.max(Passos2$steps)]

## Inserindo valores ausentes

length(Originais$steps[is.na(Originais$steps)])

Originais$steps[is.na(Originais$steps)] <- with(Originais, ave(steps, interval, 
                                                               FUN = function(x) median(x, na.rm = T)))[is.na(Originais$steps)] 
#median substitute missing values 
#Replot "Steps Per Day"
Passos3 <- Originais[, lapply(.SD, sum), by = date, .SDcols = "steps"]

par(lend = "square")

plot(Passos3$date, Passos3$steps, 
     type = "h", lwd = 8, col = "black", 
     xlab = "date", ylab = "number of steps",
     main = "number of steps per day")

lines(Passos3$date, Passos3$steps, type = "h", lwd = 7, col = "blue")

mean(Passos3$steps, na.rm = T);
median(Passos3$steps, na.rm = T)

library(chron)
library(plyr)
Originais$day <- revalue(as.factor(is.weekend(Originais$date)), 
                         c("FALSE" = "WEEKDAY", "TRUE"="WEEKEND"))

library(reshape2) 
PassosMerge <- suppressWarnings(melt(Originais, c("interval", "day"))) 
PassosSoma  <- dcast(PassosMerge, day + interval ~ variable, mean)[,1:3] 

library(lattice)
xyplot(steps ~ interval | day, data = PassosSoma, layout = c(1,2), type = "l")

