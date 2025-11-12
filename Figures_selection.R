
library(ggplot2)
library(dplyr)
library(tidyr)
#library(reshape2)
library(stringr)
library(patchwork)


#Figure 3: Selection gradients

#Beta coefficients from the four models for D. lapponica through poll. success
load(file = "analyses/betas_poll_lap.RData")
head(beta_pol_lap[[1]])

mods_lap= c("m", "m1", "m2", "m3")# the four models ("m" the one with the RRR animal-poll community axis)
rownames(beta_pol_lap[[1]])

rn_lap= c("Plant height", "Flower size", "Spur length", "Spur width", "No. of flowers", "Community")

ntraits_lap = length(rn_lap)
nmods_lap = length(mods_lap)

#Create the matrices
summary_var_lap = data.frame(matrix(NA, nrow = nmods_lap, ncol = ntraits_lap))
lower_var_lap = data.frame(matrix(NA, nrow = nmods_lap, ncol = ntraits_lap))
upper_var_lap = data.frame(matrix(NA, nrow = nmods_lap, ncol = ntraits_lap))

colnames(summary_var_lap) = rn_lap
colnames(lower_var_lap) = rn_lap
colnames(upper_var_lap) = rn_lap
rownames(summary_var_lap) = mods_lap
rownames(lower_var_lap) = mods_lap
rownames(upper_var_lap) = mods_lap

# Fill matrices
for(i in seq_along(mods_lap)) {
  summary_var_lap[i, ] = round(as.numeric(beta_pol_lap[[i]]$beta_var), 3)
  lower_var_lap[i, ] = round(as.numeric(beta_pol_lap[[i]]$L.1), 3)
  upper_var_lap[i, ] = round(as.numeric(beta_pol_lap[[i]]$U.1), 3)
}

# One df
bdf_lap = data.frame(
  model = mods_lap,
  summary_var_lap,
  lower_var_lap,
  upper_var_lap,
  check.names = FALSE
)

colnames(bdf_lap) = c(
  "model",
  paste0("B_", rn_lap),
  paste0("Lower_", rn_lap),
  paste0("Upper_", rn_lap)
)

head(bdf_lap)

#Reshape to long format
bdf_long= bdf_lap %>%
  mutate(model = rownames(.)) %>%
  pivot_longer(
    cols = -model,
    names_to = "variable",
    values_to = "value"
  )

#Extract the part of var. name (B_, Lower_, Upper_)
bdf_long= bdf_long %>%
  mutate(
    part = case_when(
      str_starts(variable, "B_") ~ "beta",
      str_starts(variable, "Lower_") ~ "lower",
      str_starts(variable, "Upper_") ~ "upper"
    ),
    trait = variable %>%
      str_remove("^B_|^Lower_|^Upper_") %>%
      str_replace_all("_", " ")
  ) %>%
  select(model, part, trait, value)

#Pivot wider again to combine beta/lower/upper in one row per trait
bdf_lap_final= bdf_long %>%
  pivot_wider(
    names_from = part,
    values_from = value
  ) %>%
  mutate(species = "D. lapponica") %>%
  relocate(model, trait, beta, lower, upper, species)

head(bdf_lap_final)

#Now the same for D. majalis:

#Beta coefficients from the three models for D. majalis through poll. success 

load(file = "analyses/betas_poll_maj.RData")  

head(beta_pol_maj[[1]])

mods_maj= c("m", "m1", "m2")# here three models ("m" the one with the RRR animal-poll community axis)
rownames(beta_pol_maj[[1]])

rn_maj= c("Plant height", "Flower size", "Spur length", "No. of flowers", "n", "Community")#here no data on spur width
#here we also have the variable "n" that just refers to number of flowers scored to assess pollinaria removed or deposition

ntraits_maj = length(rn_maj)
nmods_maj = length(mods_maj)

#Create the matrices
summary_var_maj = data.frame(matrix(NA, nrow = nmods_maj, ncol = ntraits_maj))
lower_var_maj = data.frame(matrix(NA, nrow = nmods_maj, ncol = ntraits_maj))
upper_var_maj = data.frame(matrix(NA, nrow = nmods_maj, ncol = ntraits_maj))

colnames(summary_var_maj) = rn_maj
colnames(lower_var_maj) = rn_maj
colnames(upper_var_maj) = rn_maj
rownames(summary_var_maj) = mods_maj
rownames(lower_var_maj) = mods_maj
rownames(upper_var_maj) = mods_maj

# Fill matrices
for(i in seq_along(mods_maj)) {
  summary_var_maj[i, ] = round(as.numeric(beta_pol_maj[[i]]$beta_var), 3)
  lower_var_maj[i, ] = round(as.numeric(beta_pol_maj[[i]]$L.1), 3)
  upper_var_maj[i, ] = round(as.numeric(beta_pol_maj[[i]]$U.1), 3)
}

# Invert Community axis sign (B regression coefficient for "Community axis" in model "m" was negative)
community_col_maj = which(colnames(summary_var_maj) == "Community")
if(length(community_col_maj) == 1) {
  summary_var_maj[, community_col_maj] = -summary_var_maj[, community_col_maj]
  lower_var_maj[, community_col_maj] = -lower_var_maj[, community_col_maj]
  upper_var_maj[, community_col_maj] = -upper_var_maj[, community_col_maj]
}

# One df
bdf_maj = data.frame(
  model = mods_maj,
  summary_var_maj,
  lower_var_maj,
  upper_var_maj,
  check.names = FALSE
)

colnames(bdf_maj) = c(
  "model",
  paste0("B_", rn_maj),
  paste0("Lower_", rn_maj),
  paste0("Upper_", rn_maj)
)

head(bdf_maj)

#Reshape to long format
bdf_long= bdf_maj %>%
  mutate(model = rownames(.)) %>%
  pivot_longer(
    cols = -model,
    names_to = "variable",
    values_to = "value"
  )

#Extract the part of var. name (B_, Lower_, Upper_)
bdf_long= bdf_long %>%
  mutate(
    part = case_when(
      str_starts(variable, "B_") ~ "beta",
      str_starts(variable, "Lower_") ~ "lower",
      str_starts(variable, "Upper_") ~ "upper"
    ),
    trait = variable %>%
      str_remove("^B_|^Lower_|^Upper_") %>%
      str_replace_all("_", " ")
  ) %>%
  select(model, part, trait, value)

#Pivot wider again to combine beta/lower/upper in one row per trait
bdf_maj_final= bdf_long %>%
  pivot_wider(
    names_from = part,
    values_from = value
  ) %>%
  mutate(species = "D. majalis") %>%
  relocate(model, trait, beta, lower, upper, species)

head(bdf_maj_final)

#Swap lower and upper credible intervals for the community axis to match the right order
bdf_maj_final= bdf_maj_final %>%
  mutate(
    temp = if_else(trait == "Community", lower, NA_real_),
    lower = if_else(trait == "Community", upper, lower),
    upper = if_else(trait == "Community", temp, upper),
    temp = NULL
  ) %>%
  relocate(model, trait, beta, lower, upper, species)

bdf_maj_final

#Combine dfs for plotting
betas.poll= bind_rows(bdf_lap_final, bdf_maj_final)

betas.poll$model= factor(betas.poll$model,   levels = unique(betas.poll$model))
betas.poll$species= factor(betas.poll$species, levels = unique(betas.poll$species))

#Filter by our main model "m"
betas.poll= betas.poll %>%
  filter(model== "m", !trait=="n")

# Make trait a factor to control order
trait_levels= c(
  "Plant height",
  "No. of flowers",
  "Flower size",
  "Spur length",
  "Spur width",
  "Community"
)

#Plotting Betas via pollination success (finally)----

betas.poll$trait= factor(betas.poll$trait, levels = trait_levels)

pos= position_dodge(width = 0.6, preserve= "single")


plot.poll= ggplot(betas.poll, aes(x = trait, y = beta, fill = species)) +
  geom_col(position = pos, width = 0.6, color = "black", na.rm = TRUE) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                position = pos,
                na.rm = TRUE) +
  scale_y_continuous(breaks = seq(-0.5, 1, 0.25)) +
  coord_cartesian(ylim = c(-0.5, 1)) +
  labs(
    title = "Pollination",
    y = "Variance-scaled regression coefficients (β ± CI)",
    x = NULL
  ) +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.ticks.x = element_line(size = 0.3),
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.85),
    legend.justification = "right",
    panel.grid = element_blank(),
    plot.margin = margin(3, 10, 3, 10)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colores)

print(plot.poll)

################

#Beta coefficients from the four models for D. lapponica through fruit set

load(file = "analyses/betas_fset_lap.RData")  

head(beta_fset_lap[[1]])

mods_lap= c("m", "m1", "m2", "m3")# the four models ("m" the one with the RRR animal-poll community axis)
rownames(beta_fset_lap[[1]])

rn_lap= c("Plant height", "Flower size", "Spur length", "Spur width", "No. of flowers", "Community")

ntraits_lap = length(rn_lap)
nmods_lap = length(mods_lap)

#Create the matrices
summary_var_lap = data.frame(matrix(NA, nrow = nmods_lap, ncol = ntraits_lap))
lower_var_lap = data.frame(matrix(NA, nrow = nmods_lap, ncol = ntraits_lap))
upper_var_lap = data.frame(matrix(NA, nrow = nmods_lap, ncol = ntraits_lap))

colnames(summary_var_lap) = rn_lap
colnames(lower_var_lap) = rn_lap
colnames(upper_var_lap) = rn_lap
rownames(summary_var_lap) = mods_lap
rownames(lower_var_lap) = mods_lap
rownames(upper_var_lap) = mods_lap

# Fill matrices
for(i in seq_along(mods_lap)) {
  summary_var_lap[i, ] = round(as.numeric(beta_fset_lap[[i]]$beta_var), 3)
  lower_var_lap[i, ] = round(as.numeric(beta_fset_lap[[i]]$L.1), 3)
  upper_var_lap[i, ] = round(as.numeric(beta_fset_lap[[i]]$U.1), 3)
}

# Invert Community axis sign (B regression coefficient for "Community axis" in model "m" was negative)
community_col_lap = which(colnames(summary_var_lap) == "Community")
if(length(community_col_lap) == 1) {
  summary_var_lap[, community_col_lap] = -summary_var_lap[, community_col_lap]
  lower_var_lap[, community_col_lap] = -lower_var_lap[, community_col_lap]
  upper_var_lap[, community_col_lap] = -upper_var_lap[, community_col_lap]
}

# One df
bdf_lap = data.frame(
  model = mods_lap,
  summary_var_lap,
  lower_var_lap,
  upper_var_lap,
  check.names = FALSE
)

colnames(bdf_lap) = c(
  "model",
  paste0("B_", rn_lap),
  paste0("Lower_", rn_lap),
  paste0("Upper_", rn_lap)
)

head(bdf_lap)

#Reshape to long format
bdf_long= bdf_lap %>%
  mutate(model = rownames(.)) %>%
  pivot_longer(
    cols = -model,
    names_to = "variable",
    values_to = "value"
  )

#Extract the part of var. name (B_, Lower_, Upper_)
bdf_long= bdf_long %>%
  mutate(
    part = case_when(
      str_starts(variable, "B_") ~ "beta",
      str_starts(variable, "Lower_") ~ "lower",
      str_starts(variable, "Upper_") ~ "upper"
    ),
    trait = variable %>%
      str_remove("^B_|^Lower_|^Upper_") %>%
      str_replace_all("_", " ")
  ) %>%
  select(model, part, trait, value)

#Pivot wider again to combine beta/lower/upper in one row per trait
bdf_lap_final= bdf_long %>%
  pivot_wider(
    names_from = part,
    values_from = value
  ) %>%
  mutate(species = "D. lapponica") %>%
  relocate(model, trait, beta, lower, upper, species)

head(bdf_lap_final)

#Swap lower and upper credible intervals for the community axis to match the right order
bdf_lap_final= bdf_lap_final %>%
  mutate(
    temp = if_else(trait == "Community", lower, NA_real_),
    lower = if_else(trait == "Community", upper, lower),
    upper = if_else(trait == "Community", temp, upper),
    temp = NULL
  ) %>%
  relocate(model, trait, beta, lower, upper, species)

bdf_lap_final

#Beta coefficients from the four models for D. majalis 

load(file = "analyses/betas_fset_maj.RData")  

head(beta_fset_maj[[1]])

mods_maj= c("m", "m1", "m2")# here three models ("m" the one with the RRR animal-poll community axis)
rownames(beta_fset_maj[[1]])

rn_maj= c("Plant height", "Flower size", "Spur length", "No. of flowers", "Community")#here no data on spur widht

ntraits_maj = length(rn_maj)
nmods_maj = length(mods_maj)

#Create the matrices
summary_var_maj = data.frame(matrix(NA, nrow = nmods_maj, ncol = ntraits_maj))
lower_var_maj = data.frame(matrix(NA, nrow = nmods_maj, ncol = ntraits_maj))
upper_var_maj = data.frame(matrix(NA, nrow = nmods_maj, ncol = ntraits_maj))

colnames(summary_var_maj) = rn_maj
colnames(lower_var_maj) = rn_maj
colnames(upper_var_maj) = rn_maj
rownames(summary_var_maj) = mods_maj
rownames(lower_var_maj) = mods_maj
rownames(upper_var_maj) = mods_maj

# Fill matrices
for(i in seq_along(mods_maj)) {
  summary_var_maj[i, ] = round(as.numeric(beta_fset_maj[[i]]$beta_var), 3)
  lower_var_maj[i, ] = round(as.numeric(beta_fset_maj[[i]]$L.1), 3)
  upper_var_maj[i, ] = round(as.numeric(beta_fset_maj[[i]]$U.1), 3)
}

# Invert Community axis sign (B regression coefficient for "Community axis" in model "m" was negative)
community_col_maj = which(colnames(summary_var_maj) == "Community")
if(length(community_col_maj) == 1) {
  summary_var_maj[, community_col_maj] = -summary_var_maj[, community_col_maj]
  lower_var_maj[, community_col_maj] = -lower_var_maj[, community_col_maj]
  upper_var_maj[, community_col_maj] = -upper_var_maj[, community_col_maj]
}

# One df
bdf_maj = data.frame(
  model = mods_maj,
  summary_var_maj,
  lower_var_maj,
  upper_var_maj,
  check.names = FALSE
)

colnames(bdf_maj) = c(
  "model",
  paste0("B_", rn_maj),
  paste0("Lower_", rn_maj),
  paste0("Upper_", rn_maj)
)

head(bdf_maj)

#Reshape to long format
bdf_long= bdf_maj %>%
  mutate(model = rownames(.)) %>%
  pivot_longer(
    cols = -model,
    names_to = "variable",
    values_to = "value"
  )

#Extract the part of var. name (B_, Lower_, Upper_)
bdf_long= bdf_long %>%
  mutate(
    part = case_when(
      str_starts(variable, "B_") ~ "beta",
      str_starts(variable, "Lower_") ~ "lower",
      str_starts(variable, "Upper_") ~ "upper"
    ),
    trait = variable %>%
      str_remove("^B_|^Lower_|^Upper_") %>%
      str_replace_all("_", " ")
  ) %>%
  select(model, part, trait, value)

#Pivot wider again to combine beta/lower/upper in one row per trait
bdf_maj_final= bdf_long %>%
  pivot_wider(
    names_from = part,
    values_from = value
  ) %>%
  mutate(species = "D. majalis") %>%
  relocate(model, trait, beta, lower, upper, species)

head(bdf_maj_final)

#Swap lower and upper credible intervals for the community axis to match the right order
bdf_maj_final= bdf_maj_final %>%
  mutate(
    temp = if_else(trait == "Community", lower, NA_real_),
    lower = if_else(trait == "Community", upper, lower),
    upper = if_else(trait == "Community", temp, upper),
    temp = NULL
  ) %>%
  relocate(model, trait, beta, lower, upper, species)

bdf_maj_final

bdf_lap_final

#Combine dfs for plotting
betas.fset= bind_rows(bdf_lap_final, bdf_maj_final)

betas.fset$model= factor(betas.fset$model,   levels = unique(betas.fset$model))
betas.fset$species= factor(betas.fset$species, levels = unique(betas.fset$species))

#Filter by our main model "m"
betas.fset= betas.fset %>%
  filter(model== "m")

# Make trait a factor to control order
trait_levels= c(
  "Plant height",
  "No. of flowers",
  "Flower size",
  "Spur length",
  "Spur width",
  "Community"
)

#Plotting Betas through fruit set----

betas.fset$trait= factor(betas.fset$trait, levels = trait_levels)

pos= position_dodge(width = 0.6, preserve= "single")


plot.fset= ggplot(betas.fset, aes(x = trait, y = beta, fill = species)) +
  geom_col(position = pos, width = 0.6, color = "black", na.rm = TRUE) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                position = pos,
                na.rm = TRUE) +
  scale_y_continuous(breaks = seq(-0.5, 1, 0.25)) +
  coord_cartesian(ylim = c(-0.5, 1)) +
  labs(
    title = "Reproductive fitness",
    y = "Variance-scaled regression coefficients (β ± CI)",
    x = NULL
  ) +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.ticks.x = element_line(size = 0.3),
    axis.title.y = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(3, 10, 3, 10)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = colores)

print(plot.fset)


fig.3= plot.poll + plot.fset + 
  plot_layout(ncol = 2, guides = "keep")+
  plot_annotation(tag_levels = 'a')   

print(fig.3)

ggsave(filename = "plots/fig3.svg", plot = fig.3,                
  width = 12,                    
  height = 6,                     
  dpi = 300)
