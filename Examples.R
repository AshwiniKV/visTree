# Examples 
setwd("C:/Users/user/ownCloud")
blsdata<-read.csv("finalcode - Obesity Research\\Documentation\\blsdata.csv", header = T)
names(blsdata)
library(partykit)
names(blsdata)[c(4, 6, 12, 19, 21, 22, 23, 24, 25)]<-c("skcal", "srvgssb", "edeq15", "freqff","rest.eating", "disinhibition", "hunger", "liking", "wanting")
newblsdata<-blsdata[,c(7,21, 22,23, 24, 25, 26)]

# Continuous - CTree
potentialtree<-ctree(kcal24h0~., data = newblsdata, control = ctree_control(mincriterion = 0.95))
visTree(potentialtree, interval = F, as.party.tree = F)

# Repeated - CTree
potentialtree<-ctree(kcal24h0~skcal+hunger+rrvfood+rest.eating+liking+wanting+age, data = blsdata, control = ctree_control(mincriterion = 0.95))
visTree(potentialtree, interval = F, as.party.tree = F)

# Categorical 
blsdataedit<-blsdata[,-7]
blsdataedit$bin<-0
blsdataedit$bin<-cut(blsdata$kcal24h0, unique(quantile(blsdata$kcal24h0)), include.lowest = TRUE, dig.lab = 4)
potentialtree<-ctree(bin~hunger+rrvfood+rest.eating+liking+wanting+disinhibition, data = blsdataedit, control = ctree_control(mincriterion = 0.95))
plot(potentialtree, gp = gpar(fontsize = 19, fontface = "bold"), type = "extended", ep_args = list(digits = 1))
visTree(potentialtree, interval = T, as.party.tree = FALSE, color.type = 4, alpha = 0.6)
# Pathway
list_node(potentialtree, F)

# rpart - Convert to a party object
library(rpart)
#library(rpart.plot)
potentialtree<-rpart(kcal24h0~., data = newblsdata, control = rpart.control(cp = 0.015))
#prp(potentialtree, extra = 1, cex = 1.75,split.cex = 1, branch.lwd = 4, type = 3)
newtree2<-as.party(potentialtree)
visTree(newtree2, NULL, FALSE, TRUE, 1, 0.3)
list_node(newtree2, T)
