
GramSchmidt <- function(espaco,ortonormalizar){
  if(!is.list(espaco) || length(espaco) < 1)return(NULL);
  espaco_ortogonal <- list()
  espaco_ortogonal[length(espaco_ortogonal)+1] <- espaco[1]
  
  for(i in 2:length(espaco)){
    w <- vector()
    w <- espaco[[i]]
    for(j in 1:length(espaco_ortogonal)){
      w <- w - ((sum(w * espaco_ortogonal[[j]]))/(sum(espaco_ortogonal[[j]] * espaco_ortogonal[[j]])))*espaco_ortogonal[[j]]
    }
    espaco_ortogonal[[length(espaco_ortogonal)+1]] <- w
  }
  if(as.logical(ortonormalizar)){
    for(i in 1:length(espaco_ortogonal)){
      espaco_ortogonal[[i]] <- espaco_ortogonal[[i]]/sqrt(sum(espaco_ortogonal[[i]] * espaco_ortogonal[[i]])) 
    }
  }
  return(espaco_ortogonal)
}

#espaco_vetorial <- list(
 # c(1,1,0),
#  c(1,0,1),
#  c(0,1,1)
#)

#cat(str(GramSchmidt(espaco_vetorial,FALSE)))

espaco_vetorial <- list(
  c(2,0),
  c(1,1)
)
cat(str(GramSchmidt(espaco_vetorial,TRUE)))
