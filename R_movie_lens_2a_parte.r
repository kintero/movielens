# Segmentación de películas: el dataset Movielens
# Pedro Concejero  pedro.concejerocerezo@telefonica.com
# mayo 2015
# para su presentación en grupo R Madrid 12 de mayo 2015

# Con código en parte obtenido de aquí:
# https://github.com/ddehghan/machine_learning_class/blob/master/movielens/movie_lens.r

# Inicialización
# Comprueba si están y si no instala librerías
# Este capítulo supone que están instaladas las librerías de la primera 

if ("directlabels" %in% row.names(installed.packages())  == FALSE) install.packages("directlabels", dependencies = T)
if ("ggplot2" %in% row.names(installed.packages())  == FALSE) install.packages("ggplot2", dependencies = T)



# limpia tu espacio de trabajo

rm(list = ls())
gc()

setwd("C:/Users/pedroc/Desktop/movielens/ml-20m") # pon tu directorio trabajo

load(file = "ratings8500.rda")
load(file = "sparse8500.rda")
load(file = "ratings.per.movie.8500.rda")


######################################
# TERCERA PARTE DE ESTE EJERCICIO: ANALISIS FACTORIAL
# Métodos numéricos son mucho menos eficientes que svd para matrix sparse
# escogeremos 500 películas de entre las más populares y variables

summary(ratings.per.movie.8500)

cond <- {ratings.per.movie.8500$sd_rating > 1}

kk <- ratings.per.movie.8500[cond, ]
movies500 <- head(kk[with(kk,order(n_ratings,
                                   decreasing = T)),],
                            n = 500)

ratings500 <- ratings8500[which(ratings8500$movieId %in% movies500$movieId), ]

# problema de este enfoque: tenemos películas relativamente antiguas
# que son las que más ratings contienen

# matriz sparse de estos ratings
library(Matrix)
users <- unique(ratings500$userId)
movies <- unique(ratings500$movieId)

movie_indices <- data.frame(movies)
movie_indices$movie_codes <- seq_along(movie_indices$movies)
head(movie_indices)
summary(movie_indices)

# esto para luego poder poner bien los títulos de pelis en gráficos de salida

titles500 <- merge(movie_indices,
                   movies500[, c("movieId",
                                 "title")],
                   by.x = "movies",
                   by.y = "movieId")

user_indices <- data.frame(users)
user_indices$user_codes <- seq_along(user_indices$users)
head(user_indices)
summary(user_indices)

ratings <- data.frame(userId = ratings500$userId,
                      movieId = ratings500$movieId,
                      rating = ratings500$rating)

ratings2 <- merge(ratings[, c("userId",
                              "movieId",
                              "rating")],
                  movie_indices,
                  by.x = "movieId",
                  by.y = "movies")

ratings3 <- merge(ratings2,
                  user_indices,
                  by.x = "userId",
                  by.y = "users")

m.matrix = sparseMatrix(ratings3$user_codes,
                        ratings3$movie_codes,
                        x = ratings3$rating, 
                        dims = c(length(unique(ratings3$user_codes)),
                                 length(unique(ratings3$movie_codes))))


# calculamos matriz de correlaciones con función a medida para sparse
# a partir de
# http://stackoverflow.com/questions/5888287/running-cor-or-any-variant-over-a-sparse-matrix-in-r


sparse.cor3 <- function(x){
#  memory.limit(size=10000)
  n <- nrow(x)
  
  cMeans <- colMeans(x)
  cSums <- colSums(x)
  
  # Calculate the population covariance matrix.
  # There's no need to divide by (n-1) as the std. dev is also calculated the same way.
  # The code is optimized to minize use of memory and expensive operations
  covmat <- tcrossprod(cMeans, (-2*cSums+n*cMeans))
  crossp <- as.matrix(crossprod(x))
  covmat <- covmat+crossp
  
  sdvec <- sqrt(diag(covmat)) # standard deviations of columns
  covmat/crossprod(t(sdvec)) # correlation matrix
}

system.time(corx <- sparse.cor3(m.matrix))

# USAREMOS RCOMMANDER CON EXCELENTE PLUGIN "FactoMineR"

#library(Rcmdr)
#library(RcmdrPlugin.FactoMineR)
#library(FactoMineR)
# library(Factoshiny) Parece no funcionar muy bien

FA <- factanal(covmat = corx,
               factors = 15,
               rotation = "varimax")

# podemos extraer el patrón factorial
print(FA, digits = 2, cutoff = .3, sort = TRUE)

# y los pesos factoriales para combinaciones de factores
# Factor 1 y 2
load <- FA$loadings[ ,1:2]
pesos <- as.data.frame(load)
pesos[abs(pesos) < .25] <- 0
pesos$title <- titles500$title

pesos2 <- pesos[which(pesos$Factor1 > 0 | pesos$Factor2 > 0),]
pesos22 <- pesos2[sample(nrow(pesos2), nrow(pesos2)/2),]

pdf("fact1&2.pdf",
    width = 10,
    height = 10,
    pointsize = 4)

g <- ggplot(pesos22, aes(x = Factor1 , y = Factor2 )) + geom_point(shape = 1) 
g <- g + scale_y_continuous(trans=log2_trans()) + scale_x_continuous(trans=log2_trans())
g <- g + geom_text(aes(label = pesos22$title, 
                       size = 0.5,
                       angle = 30),
                   hjust = 0,
                   vjust = 0)
g <- g + theme(legend.position="none")
print(g)
dev.off()

# Factor 3 y 4
load <- FA$loadings[ ,3:4]
pesos <- as.data.frame(load)
pesos[abs(pesos) < .25] <- 0
pesos$title <- titles500$title

pesos2 <- pesos[which(pesos$Factor3 > 0 | pesos$Factor4 > 0),]
pesos22 <- pesos2[sample(nrow(pesos2), nrow(pesos2)/2),]

pdf("fact3&4.pdf",
    width = 10,
    height = 10,
    pointsize = 4)

g <- ggplot(pesos22, aes(x = Factor3 , y = Factor4 )) + geom_point(shape = 1) 
g <- g + scale_y_continuous(trans=log2_trans()) + scale_x_continuous(trans=log2_trans())
g <- g + geom_text(aes(label = pesos22$title, 
                       size = 0.5,
                       angle = 30),
                   hjust = 0,
                   vjust = 0)
g <- g + theme(legend.position="none")
print(g)
dev.off()

# Factor 5 y 6
load <- FA$loadings[ ,5:6]
pesos <- as.data.frame(load)
pesos[abs(pesos) < .25] <- 0
pesos$title <- titles500$title

pesos2 <- pesos[which(pesos$Factor5 > 0 | pesos$Factor6 > 0),]
pesos22 <- pesos2[sample(nrow(pesos2), nrow(pesos2)/2),]

pdf("fact5&6.pdf",
    width = 10,
    height = 10,
    pointsize = 4)

g <- ggplot(pesos22, aes(x = Factor5 , y = Factor6 )) + geom_point(shape = 1) 
g <- g + scale_y_continuous(trans=log2_trans()) + scale_x_continuous(trans=log2_trans())
g <- g + geom_text(aes(label = pesos22$title, 
                       size = 0.5,
                       angle = 30),
                   hjust = 0,
                   vjust = 0)
g <- g + theme(legend.position="none")
print(g)
dev.off()


# Factor 7 y 8
load <- FA$loadings[ ,7:8]
pesos <- as.data.frame(load)
pesos[abs(pesos) < .25] <- 0
pesos$title <- titles500$title

pesos2 <- pesos[which(pesos$Factor7 > 0 | pesos$Factor8 > 0),]
pesos22 <- pesos2[sample(nrow(pesos2), nrow(pesos2)/2),]

pdf("fact7&8.pdf",
    width = 10,
    height = 10,
    pointsize = 4)

g <- ggplot(pesos22, aes(x = Factor7 , y = Factor8 )) + geom_point(shape = 1) 
g <- g + scale_y_continuous(trans=log2_trans()) + scale_x_continuous(trans=log2_trans())
g <- g + geom_text(aes(label = pesos22$title, 
                       size = 0.5,
                       angle = 30),
                   hjust = 0,
                   vjust = 0)
g <- g + theme(legend.position="none")
print(g)
dev.off()

# Probemos a aumentar el número de factores de salida y lo sacamos a tabla

FA30 <- factanal(covmat = corx,
               factors = 30,
               rotation = "varimax")

# podemos extraer el patrón factorial
print(FA30, digits = 2, cutoff = .25, sort = TRUE)

load <- FA30$loadings
pesos <- as.data.frame(load[,1:30])
pesos$title <- titles500$title
write.table(pesos,
            file = "FA30pesos.txt",
            sep = "\t",
            row.names = F)

################
# links y tags

links <- read.table(file = "links.csv",
                    header = T,
                    sep = ",",
                    fileEncoding = "UTF-8",
                    quote = '"',
                    comment = "")

tags <- read.table(file = "tags.csv",
                   header = T,
                   sep = ",",
                   fileEncoding = "UTF-8",
                   quote = '"',
                   comment = "")

