# Segmentación de películas: el dataset Movielens
# Pedro Concejero  pedro.concejerocerezo@telefonica.com
# mayo 2015
# para su presentación en grupo R Madrid 12 de mayo 2015

# Con código en parte obtenido de aquí:
# https://github.com/ddehghan/machine_learning_class/blob/master/movielens/movie_lens.r

###########################
# NOTA IMPORTANTE
# Este script carga el dataset movielens 20m en memoria y realiza operaciones matriciales con él
# Requiere >8 GB RAM según se ha probado en un sistema windows 7 y en un ubuntu 13
# Por debajo de esa memoria el PC puede quedar inestable o muy lento

# Inicialización
# Comprueba si están y si no instala librerías

if ("data.table" %in% row.names(installed.packages())  == FALSE) install.packages("data.table", dependencies = T)
if ("scales" %in% row.names(installed.packages())  == FALSE) install.packages("scales", dependencies = T)
if ("Matrix" %in% row.names(installed.packages())  == FALSE) install.packages("Matrix", dependencies = T)
if ("irlba" %in% row.names(installed.packages())  == FALSE) install.packages("irlba", dependencies = T)
if ("rARPACK" %in% row.names(installed.packages())  == FALSE) install.packages("rARPACK", dependencies = T)
if ("ggplot2" %in% row.names(installed.packages())  == FALSE) install.packages("ggplot2", dependencies = T)



# limpia tu espacio de trabajo

rm(list = ls())
gc()

setwd("C:/Users/pedroc/Desktop/movielens") # pon tu directorio trabajo

# movielens 20 Mdata

url <- "http://files.grouplens.org/datasets/movielens/ml-20m.zip"

destfile <- "ml-20m.zip"
download.file(url, destfile, mode =  "wb")

unzip(destfile, list = T)
unzip(destfile) # cuidado que los pone en un subdir ml-20m/

setwd("C:/Users/pedroc/Desktop/movielens/ml-20m")
files <- list.files(pattern = glob2rx("*.csv"))
files

library(Matrix)  # para manejar matrices sparse

m.raw = scan(file = "ratings.csv", 
             what = list(user=0,movie=0,rating=0,timestamp=0),
             skip = 1, # original file has header
             sep = ",", # CAVEAT about separator
             flush = TRUE)

# How can we know number of different movies and raters?

num_rows <- length(unique(m.raw$user))
num_rows

num_cols <- length(unique(m.raw$movie))
num_cols

# NOW WE HAVE THE PROBLEM OF NON-SEQUENTIAL MOVIE CODES

# Extract required meta information from the data set.

users <- unique(m.raw$user)
movies <- unique(m.raw$movie)

number_of_rows = length(users)
number_of_columns = length(movies)

# As the user IDs and movie IDs are not continueous integers
# (there are missing numbers inbetween), 
# a proper mapping is required. It will be used 
# when inserting data into the matrix. 

movie_indices <- data.frame(movies)
movie_indices$codes <- seq_along(movie_indices$movies)
head(movie_indices)
summary(movie_indices)

user_indices <- data.frame(users)
user_indices$codes <- seq_along(user_indices$users)
head(user_indices)
summary(user_indices)

ratings <- data.frame(userId = m.raw$user,
                         movieId = m.raw$movie,
                         rating = m.raw$rating)

ratings2 <- merge(ratings[, c("userId",
                              "movieId",
                              "rating")],
                  movie_indices,
                  by.x = "movieId",
                  by.y = "movies")

m.matrix = sparseMatrix(ratings2$userId,
                        ratings2$codes,
                        x = ratings2$rating, 
                        dims = c(number_of_rows,
                                 number_of_columns))


#####################################
# calculamos la SVD con 50 componentes

library(rARPACK)

m.svd = svds(m.matrix, 50)
summary (m.svd)

# representación gráfica de los valores singulares, los 20 primeros
plot(1:20, m.svd$d[1:20])

# Todos los 50 valores
plot(1:length(m.svd$d), 
     m.svd$d,
     main = "Valores singulares movielens 20M",
     xlab = "N/orden de valor (1..50)",
     ylab = "",
     type = "l")

dim(diag(m.svd$d))

d <- m.svd$d
u <- m.svd$u
v <- m.svd$v

save.image(file = "movielens_sparse_svd_with_all.rda")

######################################
# SEGUNDA PARTE DE ESTE EJERCICIO: SVD O PCA SOBRE SELECCION DE PELICULAS

setwd("C:/Users/pedroc/Desktop/movielens/ml-20m")
load("movielens_sparse_svd_with_all.rda")


# Este es enfoque habitual en recomendadores
# Pero vamos a utilizar un enfoque diferente, más habitual en psicometría

# Veamos primero la distribución de ratings por película
# data.table es aquí muy útil

movies = read.table(file = "movies.csv", 
                    header = T,
                    sep = ",",
                    fileEncoding = "UTF-8",
                    quote = '"',
                    comment = "",
                    colClasses = c("integer",
                                   "character",
                                   "character"))
# Mucho cuidado que película 130640 hace que se pare lectura
# La elimino a mano de la tabla csv original

dim(movies)
max(movies$movieId)  # debe ser 131262


library(data.table)
ratings2 <- as.data.table(ratings2)

ratings.per.movie <- ratings2[,.(n_ratings = .N,
                                 mean_rating = mean(rating),
                                 sd_rating = sd(rating)),
                                 by = movieId]

ratings.per.movie <- merge(as.data.frame(ratings.per.movie),
                           movies,
                           by.x = "movieId",
                           by.y = "movieId",
                           all.x = T,
                           all.y = F)

# Veamos los top25 por número de ratings acumulados
head(ratings.per.movie[order(ratings.per.movie$n_ratings,
                             decreasing = T),],
     n = 25)

# Veamos los last25 por número de ratings acumulados
tail(ratings.per.movie[order(ratings.per.movie$n_ratings,
                             decreasing = T),],
     n = 25)

# Veamos cuántas películas hay con dos o menos ratings
nrow(ratings.per.movie[ratings.per.movie$n_ratings <= 2,])
# Nada menos que 6015!

# Intentamos un gráfico para representar estos valores básicos
# par(mar = c(5, 12, 4, 4) + 0.1) #Dejamos tanto espacio como sea posible a izda (2º valor)

library(scales)

plot(ratings.per.movie$n_ratings[order(ratings.per.movie$n_ratings,
                                       decreasing = T)],
#     axes=F, 
     ylim = c(0,max(ratings.per.movie$n_ratings)), 
     xlab = "movieId", 
     ylab = "n ratings",
     type = "l",
     col  = "black", 
     main = "")

par(new=T)

plot(ratings.per.movie$mean_rating[order(ratings.per.movie$n_ratings,
                                         decreasing = T)],
     ylim = c(0,max(ratings.per.movie$mean_rating)), 
     axes = F,
     type = "p",
     col  =  alpha("red", 0.9), 
     main = "",
     cex = 0.2,
     xlab = "",
     ylab = "")

axis(side=4, at = pretty(range(ratings.per.movie$mean_rating)))
mtext("Promedio votos usuarios", side=4, line=3)

par(new=T)

plot(ratings.per.movie$sd_rating[order(ratings.per.movie$n_ratings,
                                       decreasing = T)],
     #     axes=F, 
     ylim=c(0,max(ratings.per.movie$sd_rating, na.rm = T)), 
     axes = F,
     type = "p",
     col  =  alpha("green", 0.8),
     main = "",
     cex = 0.2,
     xlab = "",
     ylab = "")

legend("topright",
       legend = c("Promedio votos","Desv tip votos"),
       text.col = c("red","green"),
       pch = c(16,15),
       col = c("red","green"))

# Veamos algún ranking, de las mejores y de las peores
# las peores muy a menudo con muy pocos votos

ratings.per.movie.order <- ratings.per.movie[order(ratings.per.movie$n_ratings,
                                                             decreasing = T), ]

ratings.per.movie.order[which(
  ratings.per.movie.order$mean_rating == min(ratings.per.movie.order$mean_rating)),]

# y lo mismo pasa con las mejores, los 5 es porque tienen muy pocos votos

ratings.per.movie.order[which(
  ratings.per.movie.order$mean_rating == max(ratings.per.movie.order$mean_rating)),]

# si queremos un ranking correcto tendremos que poner como condición un mínimo de votos

zz <- ratings.per.movie.order[ratings.per.movie.order$n_ratings > 100,]
zz[zz$mean_rating == min(zz$mean_rating),]
zz[zz$mean_rating == max(zz$mean_rating),]



##########################
# Selección de películas por n_ratings y sd

# Veamos qué pasa si nos quedamos con las 10000 top por n_ratings
# Tenemos películas con solo 60 ratings
ratings.per.movie.order <- ratings.per.movie[order(ratings.per.movie$n_ratings,
                                                             decreasing = T),]
ratings.per.movie.order[10000,]

# Y veamos con 12000; sólo 30 ratings
ratings.per.movie.order[12000,]

# Para t4ener un mínimo de 100 ratings nos tenemos que quedar con las primeras 8500
ratings.per.movie.order[8500,]

##########################
# Probemos un poco a lo bestia: hagamos PCA de las 8500 top

kk <- ratings.per.movie.order[1:8500,]
str(kk)
dim(kk)
head(kk)

# Extraemos los ratings de las movieId en las top8500

kkk <- ratings2[ratings2$movieId %in% kk$movieId,]
str(kkk)
dim(kkk)
head(kkk)

# Ya que tenemos data.table agregemos la media de rating y calculemos la diferencia
# de tal manera que tengamos una variable (x) de input a svd/pca y que esté centrada en 0

kkk <- kkk[, media := mean(rating, na.rm = T), by = codes]
kkk[335000:355200,]
# debemos comprobar que
# la media se ha calculado correctamente por peli (p.ej. min media rating = 0.83) ver antes
summary(kkk)
# y ahora le añadimos nuestra x que es el rating menos la media por peli
kkk <- kkk[, x := rating - media]

# no perdemos tantos ratings; tenemos el 98.5%
dim(kkk)[1]/dim(ratings)[1]

# Nos montamos una sparse con estos ratings
movies.kkk <- unique(kkk$movieId)
movies.kkk.codes <- seq_along(movies.kkk)

movies.kkk <- as.data.frame(cbind(movieId = movies.kkk, 
                            movie_codes = movies.kkk.codes))

kkkk <- merge(as.data.frame(kkk),
              movies.kkk,
              by.x = "movieId",
              by.y = "movieId",
              all.x = T,
              all.y = F)

# comprobamos que los códigos de usuarios (filas de matriz) y movies (cols de matriz)
# son correlativos

range(kkkk$movie_codes); length(unique(kkkk$movie_codes))
range(kkkk$userId); length(unique(kkkk$userId))

m.8500 = sparseMatrix(kkkk$userId,
                      kkkk$movie_codes,
                      x = kkkk$x, 
                      dims = c(length(unique(kkkk$userId)),
                               length(unique(kkkk$movie_codes))))
dim(m.8500)

################# 
# svd con irlba

library(irlba)

n_comp <- 50
system.time({
#  xt.x <- crossprod(m.8500)
#  x.means <- colMeans(x)
#  xt.x <- (xt.x - m * tcrossprod(x.means)) / (m-1)
  svd.0 <- irlba(m.8500, 
                 nu = 0, 
                 nv = n_comp, 
                 tol = 1e-10)
})
# Lleva 400 segundos aprox, un poco más de 6 min

str(svd.0)

plot(svd.0$d)
# Todos los 50 valores
plot(1:length(svd.0$d), 
     svd.0$d,
     main = "Valores singulares movielens 20M",
     xlab = "N/orden de valor (1..50)",
     ylab = "",
     type = "l")

# el primer valor singular está disparado


save.image("movielens_sparse_svd_8500.rda")

# Loadings are eigenvectors normalized to respective eigenvalues: A value = V * (s/sqrt(m -1))
# Loadings are the correlations between variables and components.
m <- length(unique(kkkk$userId))
V <- svd.0$v
S <- diag(1, nrow = 50, ncol = 50) * d

A <- (V %*% S) / sqrt(m-1)

# Tenemos que añadir los movie_codes a data frame de ratings
ratings.per.movie.8500 <- merge(kk,
                                movies.kkk,
                                by.x = "movieId",
                                by.y = "movieId")

loadings <- as.data.frame(
              cbind(movie_codes = as.integer(rownames(as.data.frame(A))),
              A))

ratings.per.movie.8500 <- merge(loadings,
                  ratings.per.movie.8500,
                  by.x = "movie_codes",
                  by.y = "movie_codes")

######################################
# Exploramos los componentes (factores aprendidos) encontrados
head(ratings.per.movie.8500[order(ratings.per.movie.8500$V2, decreasing = T),])
head(ratings.per.movie.8500[order(ratings.per.movie.8500$V3, decreasing = T),])
head(ratings.per.movie.8500[order(ratings.per.movie.8500$V4, decreasing = T),])
head(ratings.per.movie.8500[order(ratings.per.movie.8500$V5, decreasing = T),])



######################################
# INTENTEMOS AFINAR MAS, solo 20 componentes
# y además probamos la arpack

library(rARPACK)

m.svd.85 = svds(m.8500, 20)
summary(m.svd.85)

plot(1:length(m.svd.85$d), m.svd.85$d)
plot(1:20, m.svd.85$d[1:20])

# OJO NOS QUITAMOS EL PRIMER COMPONENTE QUE ESTÁ DISPARADO

S <- diag(m.svd.85$d[2:20])
U <- m.svd.85$u
V <- m.svd.85$v[,2:20]

A <- (V %*% S) / sqrt(m-1)
dim(A)
head(A)

loadings <- as.data.frame(
  cbind(movie_codes = as.integer(rownames(as.data.frame(A))),
        A))

ratings.per.movie.8500.19 <- merge(loadings,
                                   ratings.per.movie.8500[,c("movie_codes",
                                                             "movieId",
                                                             "n_ratings",
                                                             "mean_rating",
                                                             "sd_rating",
                                                             "title",
                                                             "genres")],
                                   by.x = "movie_codes",
                                   by.y = "movie_codes")

######################################
# Exploramos los componentes (factores aprendidos) encontrados
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V2, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V3, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V4, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V5, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V6, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V7, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V8, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V9, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V10, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V11, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V12, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V13, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V14, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V15, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V16, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V17, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V18, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V19, decreasing = T),], n = 10)
head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V20, decreasing = T),], n = 10)

# Representación gráfica de componentes 3 y 5

library("ggplot2")

# compos 3 y 5
c3 <- head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V3, decreasing = T),], 
           n = 10)
cc3 <- c3[,c("title", "V3", "V5")]

c5 <- head(ratings.per.movie.8500.19[order(ratings.per.movie.8500.19$V5, decreasing = T),], 
           n = 10)
cc5 <- c5[,c("title", "V3", "V5")]

loads <- rbind(cc3, cc5)


pdf("PCA20-c3-c5.pdf",
    width = 10,
    height = 10,
    pointsize = 4)

g <- ggplot(loads, aes(x = V3 , y = V5 )) + geom_point(shape = 1) 
#g <- g + scale_y_continuous(trans=log2_trans()) + scale_x_continuous(trans=log2_trans())
g <- g + geom_text(aes(label = loads$title, 
                       size = 0.5,
                       angle = 30),
                   hjust = 1,
                   vjust = 1)
g <- g + theme(legend.position="none")
print(g)
dev.off()



# Salvamos todo el espacio de trabajo
# Ojo que son aprox. 1.2GB en disco
save.image(file = "movielens_sparse_8500.rda")

# Lo que necesitamos para la siguiente parte del ejercicio son solo tres objetos
ratings8500 <- kkkk
sparse8500 <- m.8500

save(ratings8500, file = "ratings8500.rda")
save(sparse8500, file = "sparse8500.rda")
save(ratings.per.movie.8500, file = "ratings.per.movie.8500.rda")


######################################
# TERCERA PARTE DE ESTE EJERCICIO: ANALISIS FACTORIAL
# en script separado
