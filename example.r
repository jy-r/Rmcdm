require(Rmcdm)

#WSA
#-------------------------------
matrix <- structure(list(edu = c(4, 2, 3, 4, 4, 3), eng = c(1, 2, 2, 3, 
                                                            2, 3), pc = c(5, 4, 5, 2, 2, 3), prax = c(1, 0, 3, 2, 1, 3), 
                         int = c(85, 80, 85, 70, 65, 90)), .Names = c("edu", "eng", 
                                                                      "pc", "prax", "int"), row.names = c(NA, -6L), class = "data.frame")
weights <- structure(list(X1 = c(0.2, 0.333333333333333, 0.133333333333333, 
                                 0.0666666666666667, 0.266666666666667)), .Names = "X1", row.names = c(NA, 
                                                                                                       -5L), class = "data.frame")
min = rep(0,5)

WSA(matrix, weights, min)


#Electre I.
#---------------------------------
matrix <- structure(list(dist = c(6, 3, 10), food = c(10, 5, 3), atm = c(4, 
                                                                         4, 5), serv = c(3, 4, 1)), .Names = c("dist", "food", "atm", 
                                                                                                               "serv"), row.names = c(NA, -3L), class = "data.frame")

vmax <- matrix %>% summarise_all(max)
matrix[,14]<-vmax[14]-matrix[,14]
matrix[,15]<-vmax[15]-matrix[,15]
matrix[,16]<-vmax[16]-matrix[,16]

weights <- structure(list(X1 = c(0.1, 0.45, 0.25, 0.2)), .Names = "X1", row.names = c(NA, 
                                                                                      -4L), class = "data.frame")
electre1(matrix, weights, c=0.5, d=0.9)


#Topsin 
#---------------------------------

matrix <- structure(list(price = c(240, 160, 200, 160, 50, 0, 220), power = c(900,300, 100, 100, 100, 0, 100), 
                         ser = c(1, 1, 0, 0, 1, 1, 1), pos = c(1, 1, 1, 0, 1, 0, 1), safty = c(1, 1, 1, 0, 1, 0, 1), app = c(5, 7, 8, 4, 8, 6, 7)),
                    .Names = c("price", "power", "ser", "pos",  "safty", "app"), row.names = c(NA, -7L), class = "data.frame")

vmax <- matrix %>% summarise_all(max)
matrix[,14]<-vmax[14]-matrix[,14]
matrix[,15]<-vmax[15]-matrix[,15]
matrix[,16]<-vmax[16]-matrix[,16]


weights <- structure(list(X1 = c(0.206896551724138, 0.172413793103448, 0.0689655172413793, 
                                 0.137931034482759, 0.137931034482759, 0.275862068965517)), .Names = "X1", row.names = c(NA, 
                                                                                                                         -6L), class = "data.frame")

topsin(matrix, weights)
  