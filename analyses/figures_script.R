###Plot selection gradients----
library(tidyverse)
library(ggplot2)
library(reshape2)
library(data.table)
library(ggpubr)

#First for relative pollination success----

#D. lapponica
load(file= "analyses/betas_poll_lap.RData")

mods= c("RRRCo-AP", "MR", "MRCo", "RRRCo-NP" )

#Variance-scaled Betas

summary_var = matrix(NA, nrow = 6, ncol = length(mods))
for(i in 1:length(mods)){
  summary_var[,i] = round(as.numeric(beta_pol_lap[[i]]$beta_var), 3)
}

colnames(summary_var) = mods
rownames(summary_var) = rownames(beta_pol_lap[[1]])
summary_var = t(summary_var)
summary_var

upper_var = matrix(NA, nrow = 6, ncol = length(mods))
for(i in 1:length(mods)){
  upper_var[,i] = round(as.numeric(beta_pol_lap[[i]]$U.1), 3)
}

colnames(upper_var) = mods
rownames(upper_var) = rownames(beta_pol_lap[[1]])
upper_var = t(upper_var)
upper_var

lower_var = matrix(NA, nrow = 6, ncol = length(mods))
for(i in 1:length(mods)){
  lower_var[,i] = round(as.numeric(beta_pol_lap[[i]]$L.1), 3)
}

colnames(lower_var) = mods
rownames(lower_var) = rownames(beta_pol_lap[[1]])
lower_var = t(lower_var)

#summary_var[4,6] = summary_var[4,6]*-1
#lower_var[4,6] = lower_var[4,6]*-1
#upper_var[4,6] = upper_var[4,6]*-1
#summary_var


bdfv = data.frame(summary_var, lower_var, upper_var)
names(bdfv)= c("B_PlantHeight", "B_FlwSize", "B_SpurLength", "B_SpurWidth", "B_nflow", "B_community", rep("Lower", times=6), rep("Upper", times=6))

bdfv <- cbind(model = rownames(bdfv), bdfv)

bdfmv= melt(setDT(bdfv), measure = patterns("^B", "^Lower", "^Upper"), value.name = c("Beta_var", "Lower", "Upper"), variable.name = "trait", variable.factor = "trait")
bdfmv$model= as.factor(bdfmv$model)

bdf2v= bdfmv %>% mutate(trait= recode(trait, "1"= "Plant height", "2"= "Flower size", "3"= "Spur length", "4"= "Spur width", "5"= "Flowers", "6"= "Community"), model= factor(model, levels=c("RRRCo-AP", "MR","MRCo", "RRRCo-NP")))

bdf2vn= bdf2v[complete.cases(bdf2v), ]

bdf2vn= bdf2vn %>% mutate(trait= factor(trait, levels=c("Plant height", "Flowers", "Flower size", "Spur length", "Spur width", "Community")))

str(bdf2vn)

VARplot_pol_lap= ggplot(bdf2vn, aes(x=model, y=Beta_var, colour= factor(model))) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), shape= 17, linetype = "solid", size=0.75)+ 
  #geom_errorbar(aes(ymin = Lower, ymax = Upper), linetype = "solid")+
  labs(y= expression(beta ~ "variance-scaled selection gradients"), x= expression(italic("Dactylorhiza majalis subsp. lapponica"),
  size=12))+
  scale_y_continuous(breaks = c(-0.60, -0.40, -0.20, 0, 0.20, 0.40, 0.60, 0.80), limits=c(-0.60, 1))+
  #facet_wrap(~ trait, nrow = 1L, strip.position = "top")+
  facet_grid(cols = vars(trait), scales = "free")+
  theme_bw(base_size = 15)+
  theme(text = element_text(size=15), axis.text.x= element_blank(),  axis.ticks.x = element_blank(), 
        legend.position="top", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        legend.title= element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        strip.text = element_text(size= 11))+ 
  geom_hline(yintercept=0, linetype="dashed", color = "black")


asterisks= tibble(
  trait= factor(c("Plant height", "Flowers", "Flowers", "Flowers", "Flowers", 
                  "Spur length","Spur length", "Spur length", "Spur length", "Spur width", "Spur width", "Spur width", "Spur width", "Community"),
                levels= c("Plant height", "Flowers", "Flower size", "Spur length","Spur width", "Community")),
  x= c(2, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1),
  xend= c(2, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1),
  y= c(0.33, 0.80, 0.75, 0.85, 0.77, 0.40, 0.45, 0.38, 0.44, -0.50, -0.57, -0.47, -0.55, 0.55),
  label= c("*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*"))


poll_lap= VARplot_pol_lap+ 
  geom_text(data= asterisks, aes(x= x, y= y, label= label), inherit.aes = FALSE, size= 6)

poll_lap
################################

# D. majalis

load(file= "analyses/betas_poll_maj.RData")

mods= c("RRRCo-AP", "MR", "MRCo")

#Variance-scaled Betas

summary_var = matrix(NA, nrow = 6, ncol = length(mods))
for(i in 1:length(mods)){
  summary_var[,i] = round(as.numeric(beta_pol_maj[[i]]$beta_var), 3)
}

colnames(summary_var) = mods
rownames(summary_var) = rownames(beta_pol_maj[[1]])
summary_var = t(summary_var)
summary_var

upper_var = matrix(NA, nrow = 6, ncol = length(mods))
for(i in 1:length(mods)){
  upper_var[,i] = round(as.numeric(beta_pol_maj[[i]]$U.1), 3)
}

colnames(upper_var) = mods
rownames(upper_var) = rownames(beta_pol_maj[[1]])
upper_var = t(upper_var)
upper_var

lower_var = matrix(NA, nrow = 6, ncol = length(mods))
for(i in 1:length(mods)){
  lower_var[,i] = round(as.numeric(beta_pol_maj[[i]]$L.1), 3)
}

colnames(lower_var) = mods
rownames(lower_var) = rownames(beta_pol_maj[[1]])
lower_var = t(lower_var)

summary_var[,6] = summary_var[,6]*-1
lower_var[,6] = lower_var[,6]*-1
upper_var[,6] = upper_var[,6]*-1
summary_var

bdfv = data.frame(summary_var, lower_var, upper_var)
names(bdfv)= c("B_PlantHeight", "B_FlwSize", "B_SpurLength", "B_opflow", "B_nflow", "B_community", rep("Lower", times=6), rep("Upper", times=6))

bdfv <- cbind(model = rownames(bdfv), bdfv)

bdfmv= melt(setDT(bdfv), measure = patterns("^B", "^Lower", "^Upper"), value.name = c("Beta_var", "Lower", "Upper"), variable.name = "trait", variable.factor = "trait")
bdfmv$model= as.factor(bdfmv$model)


bdf2v= bdfmv %>% mutate(trait= recode(trait, "1"= "Plant height", "2"= "Flower size", "3"= "Spur length", "4"= "Flowers", "5"= "n", "6"= "Community"), model= factor(model, levels=c("RRRCo-AP", "MR", "MRCo")))

bdf2v.n= bdf2v %>% filter(trait!= "n")%>%
  droplevels()

bdf2v.n2= bdf2v.n[complete.cases(bdf2v.n), ]

str(bdf2v.n2)
colores= c("#F8766D", "#7CAE00", "#00BFC4")

bdf2v.n2 = bdf2v.n2 %>% mutate(trait = factor(trait,levels= c("Plant height", "Flowers", "Flower size",  "Spur length", "Community")))

VARplot_pol_maj= ggplot(bdf2v.n2, aes(x=model, y=Beta_var, colour= model)) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), shape= 17, linetype = "solid", size=0.75)+ 
  #geom_errorbar(aes(ymin = Lower, ymax = Upper), linetype = "solid")+
  labs(y= expression(beta ~ "variance-scaled selection gradients"), x= expression(italic("Dactylorhiza majalis subsp. majalis"), 
   size=12)) +
  scale_y_continuous(breaks = c(-0.50, -0.25, 0, 0.25, 0.50, 0.75), limits=c(-0.50, 0.8))+
  #facet_wrap(~ trait, nrow = 1L, strip.position = "top")+ 
  facet_grid(cols=vars(trait), scales = "free")+
  theme_bw(base_size = 15)+
  theme(text = element_text(size=15), axis.text.x= element_blank(),  axis.ticks.x = element_blank(), legend.title= element_blank(), legend.position= "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        strip.text = element_text(size= 11))+ geom_hline(yintercept=0, linetype="dashed", color = "black")+
scale_color_manual(values = colores) +  scale_fill_manual(values= colores)

VARplot_pol_maj

asterisks= tibble(
  trait= factor(c("Flowers", "Flowers", "Flowers", "Community"),
                levels= c("Plant height", "Flowers", "Flower size",  "Spur length", "Community")),
  x= c( 1, 2, 3, 1),
  xend= c(1, 2, 3, 1),
  y= c(0.72, 0.70, 0.72, 0.38),
  label= c("*", "*", "*", "*"))

poll_maj= VARplot_pol_maj+
  geom_text(data= asterisks, aes(x= x, y= y, label= label), inherit.aes = FALSE, size= 6)

poll_maj

#Combine both plots
figure1= ggarrange(poll_lap, poll_maj,
                   labels = c("(a)", "(b)"),
                   ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
figure1

#Selection models for fruit set----

#First D. lapponica

load(file= "analyses/betas_fset_lap.RData")

mods= c("RRRCo-AP", "MR", "MRCo", "RRRCo-NP")

#Variance-scaled Betas

summary_var = matrix(NA, nrow = 6, ncol = length(mods))
for(i in 1:length(mods)){
  summary_var[,i] = round(as.numeric(beta_fset_lap[[i]]$beta_var), 3)
}

colnames(summary_var) = mods
rownames(summary_var) = rownames(beta_fset_lap[[1]])
summary_var = t(summary_var)
summary_var

summary_var[4,6]= NA

upper_var = matrix(NA, nrow = 6, ncol = length(mods))
for(i in 1:length(mods)){
  upper_var[,i] = round(as.numeric(beta_fset_lap[[i]]$U.1), 3)
}

colnames(upper_var) = mods
rownames(upper_var) = rownames(beta_fset_lap[[1]])
upper_var = t(upper_var)
upper_var

lower_var = matrix(NA, nrow = 6, ncol = length(mods))
for(i in 1:length(mods)){
  lower_var[,i] = round(as.numeric(beta_fset_lap[[i]]$L.1), 3)
}

colnames(lower_var) = mods
rownames(lower_var) = rownames(beta_fset_lap[[1]])
lower_var = t(lower_var)

summary_var[,6] = summary_var[,6]*-1 
lower_var[,6] = lower_var[,6]*-1
upper_var[,6] = upper_var[,6]*-1
summary_var

bdf = data.frame(summary_var, lower_var, upper_var)
names(bdf)= c("B_PlantHeight", "B_FlwSize", "B_SpurLength", "B_SpurWidth", "B_FlwOpen", "B_community", rep("Lower", times=6), rep("Upper", times=6))

bdf <- cbind(model = rownames(bdf), bdf)

bdfm= melt(setDT(bdf), measure = patterns("^B", "^Lower", "^Upper"), value.name = c("Beta_var", "Lower", "Upper"), variable.name = "trait", variable.factor = "trait")

bdf2= bdfm %>% mutate(trait= recode(trait, "1"= "Plant height", "2"= "Flower size", "3"= "Spur length", "4"= "Spur width", "5"= "Flowers", "6"= "Community"), 
                      model= factor(model, levels= c("RRRCo-AP", "MR", "MRCo", "RRRCo-NP")))

bdf2= bdf2 %>% mutate(trait = factor(trait,levels= c("Plant height", "Flowers", "Flower size",  "Spur length", "Spur width", "Community")))


VARplot_fset_lap= ggplot(bdf2, aes(x= model, y=Beta_var, colour=factor(model))) + 
  geom_pointrange(aes(ymin = Lower, ymax = Upper), shape= 17, linetype = "solid", size=0.75)+
  labs(y= expression(beta ~ "Variance-scaled selection gradients"), x= expression(italic("Dactylorhiza majalis subsp. lapponica"), size=12))+
  scale_y_continuous(breaks = c(-0.60, -0.40, -0.20, 0, 0.20, 0.40, 0.60, 0.80), limits=c(-0.60, 1))+
  facet_grid(cols = vars(trait), scales = "free")+
  theme_bw(base_size = 15)+
  theme(text = element_text(size=15), axis.text.x= element_blank(),  axis.ticks.x = element_blank(), legend.title= element_blank(), 
  legend.position="top",panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), strip.text = element_text(size= 12))+
  geom_hline(yintercept=0, linetype="dashed", color = "black")

VARplot_fset_lap

asterisks= tibble(
  trait= factor(c("Plant height", "Plant height", "Plant height", "Plant height", "Flowers", "Flowers", "Flowers", "Flowers", 
                  "Spur length","Spur length", "Spur length", "Spur length", "Spur width", "Spur width", "Spur width", "Spur width", "Community"),
                levels= c("Plant height", "Flowers", "Flower size", "Spur length","Spur width", "Community")),
  x= c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1),
  xend= c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1),
  y= c(0.4, 0.42, 0.44, 0.42, 0.44, 0.44, 0.46, 0.44, 0.32, 0.32, 0.35, 0.32, -0.48, -0.48, -0.50, -0.48, 0.32),
  label= c("*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*","*"))

fset_lap= VARplot_fset_lap+
  geom_text(data= asterisks, aes(x= x, y= y, label= label), inherit.aes = FALSE, size= 6)

fset_lap

#Second D. majalis:----

load(file= "analyses/betas_fset_maj.RData")

mods= c("RRRCo-AP", "MR", "MRCo")

#Variance-scaled Betas

summary_var = matrix(NA, nrow = 5, ncol = length(mods))
for(i in 1:length(mods)){
  summary_var[,i] = round(as.numeric(beta_fset_maj[[i]]$beta_var), 3)
}

colnames(summary_var) = mods
rownames(summary_var) = rownames(beta_fset_maj[[1]])
summary_var = t(summary_var)
summary_var

upper_var = matrix(NA, nrow = 5, ncol = length(mods))
for(i in 1:length(mods)){
  upper_var[,i] = round(as.numeric(beta_fset_maj[[i]]$U.1), 3)
}

colnames(upper_var) = mods
rownames(upper_var) = rownames(beta_fset_maj[[1]])
upper_var = t(upper_var)
upper_var

lower_var = matrix(NA, nrow = 5, ncol = length(mods))
for(i in 1:length(mods)){
  lower_var[,i] = round(as.numeric(beta_fset_maj[[i]]$L.1), 3)
}

colnames(lower_var) = mods
rownames(lower_var) = rownames(beta_fset_maj[[1]])
lower_var = t(lower_var)

summary_var[,5] = summary_var[,5]*-1 
lower_var[,5] = lower_var[,5]*-1
upper_var[,5] = upper_var[,5]*-1
summary_var

bdf = data.frame(summary_var, lower_var, upper_var)
names(bdf)= c("B_PlantHeight", "B_FlwSize", "B_SpurLength", "B_FlwOpen", "B_community", rep("Lower", times=5), rep("Upper", times=5))

bdf <- cbind(model = rownames(bdf), bdf)

bdfm= melt(setDT(bdf), measure = patterns("^B", "^Lower", "^Upper"), value.name = c("Beta_var", "Lower", "Upper"), variable.name = "trait", variable.factor = "trait")

bdf2= bdfm %>% mutate(trait= recode(trait, "1"= "Plant height", "2"= "Flower size", "3"= "Spur length", "4"= "Flowers", "5"= "Community"),
                      model= factor(model, levels= c("RRRCo-AP", "MR", "MRCo")))
bdf2= bdf2 %>% mutate(trait = factor(trait,levels= c("Plant height", "Flowers", "Flower size",  "Spur length", "Community")))

colores= c("#F8766D", "#7CAE00", "#00BFC4")

VARplot_fset_maj= ggplot(bdf2, aes(x=model, y=Beta_var, colour=factor(model))) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), shape= 17, linetype = "solid", size=0.75)+
  #geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.75) +
  labs(y= expression(beta ~ "Variance-scaled selection gradients"), x= expression(italic("Dactylorhiza majalis subsp. majalis"), size=12))+
  scale_y_continuous(breaks = c(-0.50, -0.25, 0, 0.25, 0.50, 0.75, 1), limits=c(-0.50, 1))+
  facet_grid(cols = vars(trait), scales = "free")+
  theme_bw(base_size = 15)+
  theme(text = element_text(size=15), axis.text.x= element_blank(),  axis.ticks.x = element_blank(), legend.title= element_blank(), legend.position= "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        strip.text = element_text(size= 11))+ geom_hline(yintercept=0, linetype="dashed", color = "black")+
  scale_color_manual(values = colores) +  scale_fill_manual(values= colores)

VARplot_fset_maj

asterisks= tibble(
  trait= factor(c("Plant height", "Plant height", "Plant height", "Flowers", "Flowers", "Flowers", 
                  "Community"),
  levels= c("Plant height", "Flowers", "Flower size", "Spur length","Spur width", "Community")),
  x= c(1, 2, 3, 1, 2, 3, 1),
  xend= c(1, 2, 3, 1, 2, 3, 1),
  y= c(0.94, 1, 0.82, 0.61, 0.61, 0.61, 0.78),
  label= c("*", "*", "*", "*", "*", "*", "*"))

fset_maj= VARplot_fset_maj+
  geom_text(data= asterisks, aes(x= x, y= y, label= label), inherit.aes = FALSE, size= 6)

fset_maj

#Combine both plots
figure2= ggarrange(fset_lap, fset_maj,
                   labels = c("(a)", "(b)"),
                   ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
figure2
