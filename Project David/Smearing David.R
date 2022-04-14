# smearing
gendata_smr <- function(data, palt = 0.5, n_gen){

  if(palt < 0 | palt > 1){stop("palt must be bound between between 0 and 1")}
  
  index_gen <- sample.int(n = nrow(data), size = n_gen, replace = TRUE)
  gen_data <- data[index_gen,]
  
  if(palt != 0){
    for(i in 1:nrow(gen_data)){
      
      for(j in 1:ncol(gen_data)){
        
        if(runif(1) <= palt){gen_data[i,j] <- sample(x = data[,j], size = 1)}
      }
    }
  }
  return(gen_data)
}

smeared <- gendata_smr(testdat, 1.5, 50)

View(gendata_smr(testdat, 1, 100))

?Position

hoi <- apply(testdat, c(1,2), function(i) Position(i,testdat))



testdat <- rbind(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5),rep(6,5),rep(7,5),rep(8,5),rep(9,5),rep(10,5))



2 > 1
