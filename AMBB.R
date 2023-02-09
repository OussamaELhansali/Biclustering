# On définit une fonction qui nous retourne les row-seeds pour chaque matrice
Row_seed <- function(mat) {
  row_sums <- rowSums(mat)
  max_sum <- max(row_sums)
  return(which(row_sums == max_sum))
}


#  On définit une fonction qui nous retourne les column-seeds pour chaque matrice
Column_seed <- function(mat) {
  Column_sums <- colSums(mat)
  max_sum <- max(Column_sums)
  return(which(Column_sums == max_sum))
}


# On définit une fonction qui prend en entrées deux lignes et nous retournes le nombre de différences entre eux
row_diff <- function(row, seed){
  c=0
  for (i in 1: length(seed)){
    if (seed[i] != row[i]){
      c = c+1
    } 
  }
  return(c)
}

# On définit la fonction qui nous consruit la matrice de différences pour toutes les lignes selon le seed 

Row_difference <- function(E, seed){
  c = rep(0,length(seed))
  for (i in 1:dim(E)[1]){
    c[i] = row_diff(row = E[i,] , seed)
  }
  return(c)
}

# On définit une cette fonction qui prend en entrée une matrice, un row threshold et un seed, et nous retourne retient que 
# les lignes ayant une differnce-value inférieur au threshold
Row_cluster <- function(E, row_threshold,i){
  seed = E[Row_seed(E)[i],]
  row_DV = Row_difference(E,seed = seed)
  list <- c()
  for(i in 1:length(row_DV)){
    if ( row_DV[i]>= row_threshold){
      list <- c(list,i)
    }
  }
  if (length(list) == 0 ){
    return(E) 
  }
  return(E[-list,])
}
# On définit une cette fonction qui prend en entrée une matrice, un  column-threshold et un col-seed, et nous retourne retient que 
# les colonnes ayant une differnce-value inférieur au threshold
Col_cluster <- function(E, col_threshold,i){
  seed = E[,Row_seed(t(E))[i]]
  #seed = E[,5]
  col_DV = Row_difference(t(E),seed)
  list <-c()
  for(i in 1:length(col_DV)){
    if ( col_DV[i]> col_threshold){
      list <- c(list,i)
    }
  }
  if (length(list) == 0 ){
    return(E) 
  }
  return(E[,-list])
}

# On définit une fonction qui prend une liste de matrice et nous retourne la matrice ayant la somme des valeurs maximale
# dans notre cas on va l'utiliser pour retenir le bicluster ayant le nombre maximal des 1 
max_sum_matrix <- function(matrix_list) {
  max_sum <- -Inf
  max_matrix <- matrix_list[[1]]
  
  for (matrix in matrix_list) {
    sum <- sum(matrix)
    if (sum > max_sum) {
      max_sum <- sum
      max_matrix <- matrix
    }
  }
  
  return(max_matrix)
}


# AMBB2 fonction qui fait le biclustering , elle prend en entrée une matrice binaire, un row-threshold et un col-threshold et retourne
#le bicluster maximale
AMBB2 <- function(E, row_threshold, col_threshold){
  row_seeds = Row_seed(E)# la liste des row-seeds
  Bicluster = list()#on initialise notre liste de bicluster par une liste vide
  #Biclusters = list()
  for (i in 1:length(row_seeds)){ #on parcour les row-seeds 
    row_matrix = Row_cluster(E,row_threshold,i) #on construit le row-cluster selon la valeur du row-threshold
    col_seeds = Row_seed(t(row_matrix))#la liste des col-seeds du le row-cluster
    for (j in 1:length(col_seeds)){#on parcours les col-seeds
      col_matrix = Col_cluster(row_matrix, col_threshold,j) #on construit le Col-cluster selon chaque seed
      if (!is.null(dim(col_matrix))){ # on vérifie si la matrice col-matrixest bien définie
        
        if (!any(0 %in% col_matrix)){ # on vérifie si elle contient encore des 0 
          Bicluster <- append(Bicluster, list(col_matrix)) #si elle ne contient pas des 0 on la rajoute à notre liste de Biclusters
        }
        else{
          Bicluster <- append(Bicluster,list((AMBB2(col_matrix,1,1)))) #sinon on refait le même processus pour la col-matrix
        }
        
      }
      
    }
    
    
    
  }
  return(max_sum_matrix(Bicluster)) #On retourn à la fin le Bicluster ayant le nombre maximal des 1
  
  
  
}

#Pour l'affichage du bicluster on définit la fonction permute

# Cette fonction prend une matrice et deux indice de deux lignes et les permute
permute_rows <- function(M, row1, row2){
  rows <- rownames(M)
  rows[c(row1, row2)] <- rows[c(row2, row1)]
  rownames(M) <- rows
  M[c(row1, row2),] <- M[c(row2, row1),]
  return(M)
}
# Cette fonction prend une matrice et deux indice de deux colonnes et les permute
permute_cols <- function(M2, col1, col2){
  cols <- colnames(M2)
  cols[c(col1, col2)] <- cols[c(col2, col1)]
  colnames(M2) <- cols
  M2[, c(col1, col2)] <- M2[, c(col2, col1)]
  return(M2)
}

#Cette fonction prend en entrée une matrice binaire et le bicluster donée par l'algorithme et réarange les lignes
#et les colonnes pour faire apparaitre le Bicluster 
permute <- function(M,Bicluster){
  r= rownames(Bicluster)
  c= colnames(Bicluster)
  for (i in 1:length(r)){
    M <- permute_rows(M,i,as.numeric(r[i]))
  }
  for (j in 1:length(c)){
    M <- permute_cols(M,j,as.numeric(c[j]))
  }
  
  return(M)
  
}














