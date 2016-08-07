library(ggplot2)

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

# candies prob
candy <- poke[poke$Stage==1 & poke$Total.Stages>1, c(1,14,15,20)]
poke$Candy.Prob <- NA
for(g in seq_along(poke[,1])){
     if(poke$Stage[g]>1){
          poke$Candy.Prob[g] <- candy[candy$Pokemon==poke$Family[g], 4]
     }
}

#how many encounters does it take to get to 0.99% prob of catch vs. evolve?

prob_test <- function(poke1, threshold, n, type){
     max.prob <- 0
     prob.df <- data.frame()
     j <- 0
     if(type=="evolve"){
          prob1 <- poke1$Candy.Prob
     } else if(type=="catch"){
          prob1 <- poke1$Catch.Ease
     }
     while(max.prob < threshold) {
          if(j >= 100000) { 
               if(max.prob==0) {
                    prob.1 <- data.frame(pokemon=poke1$Pokemon, encounters=Inf, prob=0)
                    prob.df <- rbind(prob.df, prob.1)
                    break
               } else { j <- j + 2500 }
          } else if(j < 100) {
               j <- j + 1
          } else if(j < 1000) {
               j <- j + 10
          } else if(j < 10000) {
               j <- j + 100
          } else if(j < 100000) {
               j <- j + 1000
          } 
          prob <- 1 - pbinom(n, size=j, prob=prob1)
          prob.1 <- data.frame(pokemon=poke1$Pokemon, encounters=j, prob)
          prob.df <- rbind(prob.df, prob.1)
          max.prob <- max(prob.df$prob)
     }
     return(prob.df)
}


poke$p99c <- NA; poke$p75c <- NA; poke$p50c <- NA
poke$p99e <- NA; poke$p75e <- NA; poke$p50e <- NA

for(p in seq_along(poke$Pokemon)){
     # catch cumu probabilities
     if(poke$Catch.Ease[p]>0){
          probs.c <- prob_test(poke[p,], 0.99, 1, "catch")
          poke$p99c[p] <- max(probs.c$encounters)
          poke$p75c[p] <- max(probs.c$encounters[probs.c$prob <= 0.75])
          poke$p50c[p] <- max(probs.c$encounters[probs.c$prob <= 0.5])
     }
}

for(p in seq_along(poke$Pokemon)){
     # evolve cumu probabilities
     if(!is.na(poke$Candy.it.Took[p])){
          n1 <- ceiling(poke$Candy.it.Took[p]/4) + 1
          probs.e <- prob_test(poke[p,], 0.99, n1, "evolve")
          poke$p99e[p] <- max(probs.e$encounters)
          if(probs.e$encounters[1]!=Inf){
               poke$p75e[p] <- max(probs.e$encounters[probs.e$prob <= 0.75])
               poke$p50e[p] <- max(probs.e$encounters[probs.e$prob <= 0.5])
          } else {
               poke$p75e[p] <- Inf
               poke$p50e[p] <- Inf
          }
     }
}

#write.csv(poke, "poke1.csv", row.names=F)
compare <- poke[!is.na(poke$p99e), ]

# get some residuals to help with formatting the plot
fit <- lm(p99e ~ p99c, compare) 
resid <- data.frame(Pokemon=compare$Pokemon, p99e=compare$p99e, Residual=fit$residuals)
resid <- resid[order(-resid$Residual), ]
resid <- resid[!duplicated(resid), ]
compare <- merge(compare, resid, by=c("Pokemon","p99e"))

for(x in seq_along(compare$Pokemon)){
     if(compare$Residual[x] > 100000) { 
          compare$Group[x] <- "Catch"
     } else if(compare$Residual[x] < -50000) { 
          compare$Group[x] <- "Evolve"
     } else { compare$Group[x] <- "Either" }
}
compare$Group <- factor(compare$Group, levels=c("Catch", "Evolve", "Either"))


myColors <- c("#a6cee3", "#1f77b4", "#b2df8a")
ggplot(compare, aes(p99c/1000, p99e/1000)) + geom_point(aes(color=Group)) + 
     geom_abline(intercept=0, slope=1, color="black") + xlim(c(0,700))  +
     geom_label_repel(data=subset(compare, Residual > 200000 | Residual < -125000 | (Residual >= -30000 & Residual <= 75000)), 
                      aes(label=Pokemon, fill=Group), size=3.5, color="white", fontface="bold", 
                      label.padding=unit(0.15, "lines"),
                      label.size=0.1,
                      point.padding=unit(0.1, "lines")) +
     labs(title="Maximum Wild Encounters Needed to Catch or Evolve to a Pokémon") +
     xlab("Encounters Needed for 99% Probability of Catch (1000s)") + 
     ylab("Encounters Needed for 99% Probability of Evolving (1000s)") +
     scale_color_manual(values=myColors) + scale_fill_manual(values=myColors) +
     theme_classic(base_size=12) +
     theme(axis.line.x = element_line(color="black"),
           axis.line.y = element_line(color="black"),
           axis.title.y=element_text(margin=margin(0,10,0,0)),
           axis.title.x=element_text(margin=margin(10,0,0,0))) 

compare <- compare[order(compare$Residual), ]
compareE <- compare[1:5, ]
compareE <- compareE[order(-compareE$Residual), ]
compareE$Pokemon <- factor(compareE$Pokemon, levels=compareE$Pokemon)
compareC <- compare[65:69, ]
compareC$Pokemon <- factor(compareC$Pokemon, levels=compareC$Pokemon)

ggplot(compareE, aes(Pokemon, abs(Residual))) + geom_bar(stat="identity", fill="#1f77b4") + coord_flip() + 
     theme_classic() + xlab("") + ylab("")

ggplot(compareC, aes(Pokemon, Residual*-1)) + geom_bar(stat="identity", fill="#a6cee3") + coord_flip() + 
     theme_classic() + xlab("") + ylab("")
