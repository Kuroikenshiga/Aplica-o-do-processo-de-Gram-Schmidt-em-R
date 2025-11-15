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

espaco_vetorial <- list(
  c(1,1),
  c(1,2)
  
  
)

plot(0, 0, xlim = c(-2, 2), ylim = c(-1, 3), type = "n",
     xlab = "Eixo X", ylab = "Eixo Y",main = "Base original", asp = 1)

abline(h=0, v=0, col="black")

arrows(0, 0, espaco_vetorial[[1]][1], espaco_vetorial[[1]][2], col="red", lwd=2, angle=15)
arrows(0, 0, espaco_vetorial[[2]][1], espaco_vetorial[[2]][2], col="blue", lwd=2, angle=15)
cat(str(espaco_vetorial,TRUE))

espaco_ortogonalizado <- GramSchmidt(espaco_vetorial,FALSE)
plot(0, 0, xlim = c(-2, 2), ylim = c(-1, 3), type = "n",
     xlab = "Eixo X", ylab = "Eixo Y",main = "Base ortogonalizada", asp = 1)

abline(h=0, v=0, col="black")

arrows(0, 0, espaco_ortogonalizado[[1]][1], espaco_ortogonalizado[[1]][2], col="red", lwd=2, angle=15)
arrows(0, 0, espaco_ortogonalizado[[2]][1], espaco_ortogonalizado[[2]][2], col="blue", lwd=2, angle=15)
cat(str(espaco_ortogonalizado,TRUE))

espaco_ortonormalizado <- GramSchmidt(espaco_vetorial,TRUE)
plot(0, 0, xlim = c(-2, 2), ylim = c(-1, 3), type = "n",
     xlab = "Eixo X", ylab = "Eixo Y",main = "Base ortonormalizada", asp = 1)

abline(h=0, v=0, col="black")

arrows(0, 0, espaco_ortonormalizado[[1]][1], espaco_ortonormalizado[[1]][2], col="red", lwd=2, angle=15)
arrows(0, 0, espaco_ortonormalizado[[2]][1], espaco_ortonormalizado[[2]][2], col="blue", lwd=2, angle=15)
cat(str(espaco_ortonormalizado,TRUE))




