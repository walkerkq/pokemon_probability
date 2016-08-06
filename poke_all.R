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
poke$Evolve.Ease <- NA
poke$Candy.Prob <- NA
for(g in seq_along(poke[,1])){
     if(poke$Stage[g]>1){
          poke$Candy.Prob[g] <- candy[candy$Pokemon==poke$Family[g], 4]
          poke$Evolve.Ease[g] <- poke$Catch.Ease * poke$Candy.Prob[g]  ^ ceiling(poke$Candy.it.Took[g]/4)
     }
}

# ease of double evolving
poke$Double.Evolve.Ease <- NA
poke$Final.Evolve.Ease <- poke$Evolve.Ease
for(h in seq_along(poke[,1])){
     if(poke$Stage[h]==3){
          subset <- poke[poke$Family==poke$Family[h], ]
          subset <- subset[order(subset$Stage), ]
          poke$Double.Evolve.Ease[h] <- subset$Evolve.Ease[2] * subset$Evolve.Ease[3]
          poke$Final.Evolve.Ease[h] <- subset$Evolve.Ease[2] * subset$Evolve.Ease[3]
     }
}

#write.csv(poke, "poke_prob.csv", row.names=F)


