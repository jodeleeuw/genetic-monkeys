library(stringr)

target <- "cognitive science"

pop.size <- 200
mutation.rate <- 0.01
n.generations <- 40
fitness.cap <- -2

possible.keys <- c(letters, " ")

create.first.generation <- function(pop.size, target){
  population <- matrix(sample(possible.keys, pop.size*nchar(target), replace=T), byrow=T, nrow=pop.size)
}

fitness <- function(population, target){
  fit.val <- rep(0, nrow(population))
  for(r in 1:nrow(population)){
    fit.val[r] <- sum(population[r,] == str_split(target, "", simplify=T))
  }
  return(fit.val)
}

crossover.mutation <- function(parent.1, parent.2){
  child <- parent.1
  for(i in 1:length(parent.1)){
    if(rbinom(1,1,0.5)==1){
      child[i] <- parent.2[i]
    }
  }
  return(child)
}

random.mutation <- function(parent, mutation.rate){
  for(i in 1:length(parent)){
    if(rbinom(1,1,mutation.rate)==1){
      parent[i] <- sample(possible.keys, 1)
    }
  }
  return(parent)
}

next.generation <- function(population, mutation.rate, target){
  next.pop <- population
  pop.fitness <- fitness(population, target)
  fit.prob <- sapply(pop.fitness, function(x){ if(x >= max(pop.fitness) + fitness.cap) { return(x) } else { return(0) }})
  for(i in 1:nrow(next.pop)){
    parent.1 <- population[sample(1:length(fit.prob), 1, prob = fit.prob),]
    parent.2 <- population[sample(1:length(fit.prob), 1, prob = fit.prob),]
    child <- crossover.mutation(parent.1, parent.2)
    mutated.child <- random.mutation(child, mutation.rate)
    next.pop[i, ] <- mutated.child
  }
  return(next.pop)
}

mean.fitness <- function(population, target){
  fit.vals <- fitness(population, target)
  return(mean(fit.vals))
}

max.fitness <- function(population, target){
  fit.vals <- fitness(population, target)
  return(max(fit.vals))
}

fit.tracker <- rep(0, n.generations)
pop <- create.first.generation(pop.size, target)
for(i in 1:n.generations){
  pop <- next.generation(pop, mutation.rate, target)
  fit.tracker[i] <- max.fitness(pop, target)
}

plot(fit.tracker)

                  