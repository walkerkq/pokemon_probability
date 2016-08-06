library(ggplot2)
library(ggrepel)

#### sources 
# http://www.pokemongodb.net/2016/07/average-pokemon-cp-gain-per-power-up.html
# https://pokeassistant.com/main/pokemonstats
# http://www.serebii.net/pokemongo/evolution.shtml
setwd("/Users/kaylinwalker/R/pokemon_probability")

poke <- read.csv("poke_all.csv", stringsAsFactors=F)
poke$Rarity <- round(poke$Rarity/100,4)
poke$Capture <- round(poke$Capture/100,4)
poke$Flee <- round(poke$Flee/100,4)

######## CATCH OR EVOLVE?

# ease of catching
poke$Catch.Ease <- NA
for(h in seq_along(poke[,1])){     
     throw1 <- poke$Capture[h]
     throw2 <- (1-throw1) * (1-poke$Flee[h])^1 * throw1
     throw3 <- (1-sum(throw1, throw2)) * (1-poke$Flee[h])^2 * throw1
     throw4 <- (1-sum(throw1, throw2, throw3)) * (1-poke$Flee[h])^3 * throw1
     throw5 <- (1-sum(throw1, throw2, throw3, throw4)) * (1-poke$Flee[h])^4 * throw1
     poke$Catch.Ease[h] <- poke$Rarity[h] * sum(throw1, throw2, throw3, throw4, throw5)
}


# ease of evolving
candy <- poke[poke$Stage==1 & poke$Total.Stages>1, c(1,14,15,20)]
poke$Evolve.Ease.100 <- NA
poke$Evolve.Ease.1000 <- NA
poke$Evolve.Ease.10000 <- NA
poke$Candy.Prob <- NA
for(g in seq_along(poke[,1])){
     if(poke$Stage[g]>1){
          poke$Candy.Prob[g] <- candy[candy$Pokemon==poke$Family[g], 4]
          catches.needed <- ceiling(poke$Candy.it.Took[g]/4) +1
          poke$Evolve.Ease.100[g] <- 1 - pbinom(catches.needed, size=100, prob=poke$Candy.Prob[g])
          poke$Evolve.Ease.1000[g] <- 1 - pbinom(catches.needed, size=1000, prob=poke$Candy.Prob[g])
          poke$Evolve.Ease.10000[g] <- 1 - pbinom(catches.needed, size=10000, prob=poke$Candy.Prob[g])
     }
}
#write.csv(poke, "poke_prob.csv", row.names=F)


# how many encounters does it take to get to 0.99% prob??

prob_test <- function(poke1, threshold){
     prob.df <- data.frame()
     j <- 0
     while(max.prob < threshold) {
          if(j <= 1000) { j <- j +10 }
          candies <- ceiling(poke1$Candy.it.Took/4)
          prob <- 1 - pbinom((candies + 1), size=j, prob=poke1$Candy.Prob)
          prob.1 <- data.frame(pokemon=poke1$Pokemon,encounters=j, prob)
          prob.df <- rbind(prob.df, prob.1)
          max.prob <- max(prob.df$prob)
          if(j >= 1000) { j <- j + 100 }
     }
     return(prob.df)
}

all.probs <- data.frame()
for(p in poke$Pokemon[poke$Stage>1 & poke$Family!="Eevee"]){
     probs <- prob_test(poke[poke$Pokemon==p,], 0.99)
     p99 <- max(probs$encounters)
     p75 <- max(probs$encounters[probs$prob <= 0.75])
     p50 <- max(probs$encounters[probs$prob <= 0.5])
     p25 <- max(probs$encounters[probs$prob <= 0.25])
     probs0 <- data.frame(pokemon=probs$pokemon[1], p99, p75, p50, p25)
     all.probs <- rbind(all.probs, probs0)
}
#for(r in c(2:5)) all.probs[,r] <- all.probs[,r] - 1 
#write.csv(all.probs, "pokemon_encounters.csv", row.names=F)

probplot <- merge(all.probs, poke[,c(1,20)], by.y="Pokemon", by.x="pokemon", all.x=T)
probplot$Catches <- probplot$Catch.Ease * probplot$p99
#write.csv(probplot[,c(1,2,6,7)], "pokemon_verdict.csv", row.names=F)



