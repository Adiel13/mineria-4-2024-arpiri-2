install.packages("arules")
library(arules)
install.packages("genero")
library(genero)

data <- read.csv('C:/Users/kevin/OneDrive/Documentos/data.csv', sep = ";", fileEncoding = "latin1")

data$cui <- format(data$cui, scientific =FALSE)

data$nota[data$nota == "SDE"] <- -1
data$final[data$final == "SDE"] <- -1
data$final[data$final == "NSP"] <- -1

data$nombre1 <- sapply(strsplit(data$nombre, " "), `[`, 1)
data$nombre2 <- sapply(strsplit(data$nombre, " "), `[`, 2)

genero("LUIS")
genero("EMILY")

data$genero <- genero(data$nombre1)

subset(data, is.na(data$genero))

data$genero <- ifelse(is.na(data$genero), genero(data$nombre2), data$genero)

data[77, "genero"] <- "male"
data[113, "genero"] <- "male"
data[119, "genero"] <- "female"
data[120, "genero"] <- "male"
data[179, "genero"] <- "female"
data[185, "genero"] <- "male"
data[202, "genero"] <- "male"
data[225, "genero"] <- "male"
data[250, "genero"] <- "male"
data[276, "genero"] <- "female"
data[363, "genero"] <- "female"
data[473, "genero"] <- "female"
data[487, "genero"] <- "male"
data[566, "genero"] <- "male"

data$genero <- ifelse(data$genero == "male", 1, 2)

data$anio_carne <- substr(data$carne, start=1, stop=4)

subset(data, anio_carne > 8000)

data$anio_carne <- ifelse(data$anio_carne > 8000, as.numeric(substr(data$anio_carne, 1,2))+1900, data$anio_carne)

data$edad <- as.integer(data$anio) - as.integer(data$anio_carne) +18

data$municipio <- substr(data$cui, nchar(data$cui) -1, nchar(data$cui))
data$departamento <- substr(data$cui, nchar(data$cui) -3, nchar(data$cui)-2)

data_apriori <- data[, c("lab", "zona", "final", "nota", "anio", "sem", "genero", "edad", "municipio", "departamento")]

reglas <- apriori(data_apriori, parameter = list(support=0.2, confidence = 0.5))

inspect(reglas[0:100])

inspect(reglas[100:200])

data_apriori2 <- subset(data_apriori, genero ==1)
data_apriori3 <- subset(data_apriori, genero ==2)

reglas2 <- apriori(data_apriori2, parameter = list(support=0.2, confidence = 0.5))

data_apriori2 <- data_apriori2[, -7]

inspect(reglas2[0:100])

data_apriori3 <- data_apriori3[, -7]
reglas3 <- apriori(data_apriori3, parameter = list(support=0.2, confidence = 0.5))
inspect(reglas3[0:100])
inspect(reglas3[100:200])

