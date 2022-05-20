####
# Paquets
#### 
library(tidyverse)


####
# Dades
#### 
dades <- read.csv("Enquesta_TFG.csv")
dades <- dades[, -1]
dades

colnames <- c(paste0(rep("CNN", 8), "_", 1:8),
              paste0(rep("Inception", 8), "_", 1:8),
              paste0(rep("cGAN", 8), "_", 1:8))

colnames(dades) <- colnames
dades

glimpse(dades)
dades <- dades %>% mutate(across(where(is.character), as_factor))
glimpse(dades)

dades


####
# Anàlisi 
#### 
dades <- dades %>%
  mutate(
    CNN_1 = ifelse(CNN_1 == "Falsa", 1, 0),
    CNN_2 = ifelse(CNN_2 == "Falsa", 1, 0),
    CNN_3 = ifelse(CNN_3 == "Falsa", 1, 0),
    CNN_4 = ifelse(CNN_4 == "Falsa", 1, 0),
    CNN_5 = ifelse(CNN_5 == "Real", 1, 0),
    CNN_6 = ifelse(CNN_6 == "Real", 1, 0),
    CNN_7 = ifelse(CNN_7 == "Real", 1, 0),
    CNN_8 = ifelse(CNN_8 == "Real", 1, 0),
    Inception_1 = ifelse(Inception_1 == "Falsa", 1, 0),
    Inception_2 = ifelse(Inception_2 == "Falsa", 1, 0),
    Inception_3 = ifelse(Inception_3 == "Falsa", 1, 0),
    Inception_4 = ifelse(Inception_4 == "Falsa", 1, 0),
    Inception_5 = ifelse(Inception_5 == "Real", 1, 0),
    Inception_6 = ifelse(Inception_6 == "Real", 1, 0),
    Inception_7 = ifelse(Inception_7 == "Real", 1, 0),
    Inception_8 = ifelse(Inception_8 == "Real", 1, 0),
    cGAN_1 = ifelse(cGAN_1 == "Falsa", 1, 0),
    cGAN_2 = ifelse(cGAN_2 == "Falsa", 1, 0),
    cGAN_3 = ifelse(cGAN_3 == "Falsa", 1, 0),
    cGAN_4 = ifelse(cGAN_4 == "Falsa", 1, 0),
    cGAN_5 = ifelse(cGAN_5 == "Real", 1, 0),
    cGAN_6 = ifelse(cGAN_6 == "Real", 1, 0),
    cGAN_7 = ifelse(cGAN_7 == "Real", 1, 0),
    cGAN_8 = ifelse(cGAN_8 == "Real", 1, 0),
  )

dades

# CNN 
n <- nrow(dades) * 8

p_CNN <- sum(dades[, 1:8]) / n

# CNN + Inception-ResNet-v2 
p_Inception <- sum(dades[, 9:16]) / n

# cGAN 
p_cGAN <- sum(dades[, 17:24]) / n  

# Resultats (p = probabilitat encertar)
var_CNN <- (p_CNN * (1 - p_CNN)) / n 
var_Inception <- (p_Inception * (1 - p_Inception)) / n 
var_cGAN <- (p_cGAN * (1 - p_cGAN)) / n

data.frame(p = c(p_CNN,
                 p_Inception,
                 p_cGAN),
           Low = c(p_CNN - 1.96 * sqrt(var_CNN),
                   p_Inception - 1.96 * sqrt(var_Inception),
                   p_cGAN - 1.96 * sqrt(var_cGAN)),
           Upp = c(p_CNN + 1.96 * sqrt(var_CNN),
                   p_Inception + 1.96 * sqrt(var_Inception),
                   p_cGAN + 1.96 * sqrt(var_cGAN)))

# Resultats (p = probabilitat fallar) 
p_CNN <- 1 - p_CNN
p_Inception <- 1 - p_Inception
p_cGAN <- 1 - p_cGAN 

var_CNN <- (p_CNN * (1 - p_CNN)) / n 
var_Inception <- (p_Inception * (1 - p_Inception)) / n 
var_cGAN <- (p_cGAN * (1 - p_cGAN)) / n

data.frame(p = c(p_CNN,
                 p_Inception,
                 p_cGAN),
           Low = c(p_CNN - 1.96 * sqrt(var_CNN),
                   p_Inception - 1.96 * sqrt(var_Inception),
                   p_cGAN - 1.96 * sqrt(var_cGAN)),
           Upp = c(p_CNN + 1.96 * sqrt(var_CNN),
                   p_Inception + 1.96 * sqrt(var_Inception),
                   p_cGAN + 1.96 * sqrt(var_cGAN)))




