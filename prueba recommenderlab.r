# Prueba de la librería recommenderlab

library(recommenderlab)
# viñeta aquí http://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf

data(MovieLense)  # sic,nombre de datos en librería -aunque oficialmente es MovieLens

MovieLense

## visualize part of the matrix
image(MovieLense[1:100,1:100])

## number of ratings per user
hist(rowCounts(MovieLense))
## number of ratings per movie
hist(colCounts(MovieLense))
## mean rating (averaged over users)
mean(rowMeans(MovieLense))


# Ejemplo de entrenamiento de modelo de recomendador

train <- MovieLense[1:900]

dim(train) #Hemos seleccionado los primeros 900 raters -filas

u <- MovieLense[901] #escogemos el siguiente al último seleccionado

u #1 x 1664 rating matrix of class ‘realRatingMatrix’ with 124 ratings

as(u, "matrix") # para mostrar los elementos de matriz normalmente


# Creating a recommender
# A recommender is created using the creator functionRecommender(). 
# Available recommendation methods are stored in a registry. 
# The registry can be queried. Here we are only interested
# in methods for real-valued rating data.

recommenderRegistry$get_entries(dataType = "realRatingMatrix")

#IBCF item-based collaborative filtering (real data)
# PCA based on PCA approximation (real data)
# POPULAR based on item popularity (real data)
# RANDOM
# SVD based on SVD approximation (real data)
# UBCF based on user-based collaborative filtering (real data)

# user based collaborative filtering (UBCF)

r <- Recommender(train, method = "UBCF") 
r  # objeto de tipo "Recommender"

# The model can be obtained from a recommender usinggetModel().
names(getModel(r))

recom <- predict(r, u, n = 5)
recom

as(recom, "list")

# probamos y evaluamos varios de estos algoritmos con opciones
# ver opciones más adelante

scheme <- evaluationScheme(train, 
                           method = "cross", 
                           k = 4,
                           given = 10, 
                           goodRating=3)

algorithms <- list(`random items` = list(name = "RANDOM", param = NULL),
                   `popular items` = list(name = "POPULAR", param = NULL),
                   `user-based CF` = list(name = "UBCF",
                                          param = list(method = "Cosine", nn = 50)),
                   `item-based CF` = list(name = "IBCF",
                                          param = list(method = "Cosine", k = 50)))

results <- evaluate(scheme, algorithms,
                    n = c(1, 3, 5, 10, 15, 20, 50))

plot(results, annotate = c(1, 3), legend = "right")



#################
# Ejemplo con método POPULAR

r2 <- Recommender(train, method = "POPULAR") 
r2  # objeto de tipo "Recommender"

# Recommendations as ‘topNList’ for 1 users.
getModel(r2)$topN
as(getModel(r2)$topN, "list")

# la documentación base no es suficientemente buena, aquí algunas opciones
# de recommender
# uu.rec=Recommender(data=r[1:5], 
#                    method="UBCF", 
#                    param=list(normalize="Z-score", 
#                               method="pearson", 
#                               nn=50, 
#                               minRating=3, 
#                               sample=F)

