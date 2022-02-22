data = load("CentralProjections_data_centre.RData")
source("Functions.R")

# load all the necessary packages

packages <- c("gridExtra", "devtools", "readr", "dplyr", "gridExtra", "ggplot2",
              "tidyverse")
sapply(packages, require, character.only = TRUE, quietly = TRUE)
theme_set(theme_bw())

df =  CentralProjections_data_centre
df$Scenario = factor(df$Scenario, levels=c("Green", "Neutral", "Market"))

val_colors <- c(wesanderson::wes_palettes$Darjeeling1[1],
                wesanderson::wes_palettes$Darjeeling1[3],
                wesanderson::wes_palettes$Darjeeling1[5])
breaks_design <- c("design1", "design2", "design3")

# NPC against CO2-equivalent emissions --------------------------

p_scatter <- ggplot(df, aes(x=Emissions, y=NPC)) +
  geom_point(aes(col=Design, shape=Scenario)) +
  scale_color_manual(" ", breaks = breaks_design, values = val_colors,
                     labels = breaks_design) +
  labs(title ="", x = "Emissions (Mton)", y = "NPC (Millions Euros)")

p_scatter_scenario <- ggplot(df, aes(x=Emissions, y=NPC)) +
  geom_point(aes(col=Design, shape=Scenario)) +
  scale_color_manual(" ", breaks = breaks_design, values = val_colors,
                     labels = breaks_design) +
  labs(title ="", x = "Emissions (Mton)", y = "NPC (Millions Euros)") +
  facet_grid(cols = vars(Scenario))


# empirical cdfs for NPC ---------------------------------------

p_cdf_NPC_scenario <- ggplot(df, aes(NPC, linetype = Design)) + 
  stat_ecdf() + 
  scale_linetype_manual(" ", breaks = breaks_design,
                     values=c("solid", "twodash", "dotted"), 
                     labels = breaks_design) +
  labs(x = "NPC (Millions Euros)", y = " ") + 
  facet_grid(cols = vars(Scenario))

p_cdf_NPC_design <- ggplot(df, aes(NPC, linetype = Scenario)) +
  stat_ecdf() +
  scale_linetype_manual(values=c("solid", "twodash", "dotted")) +
  labs(x = "NPC (Millions Euros)", y = " ") + 
  facet_grid(cols = vars(Design), scales = "free")


# empirical cdfs for CO2 ----------------------------------------

p_cdf_CO2_scenario <-  ggplot(df, aes(Emissions, linetype = Design)) + 
  stat_ecdf() +
  scale_linetype_manual(" ", breaks = breaks_design, 
                        values = c("solid", "twodash", "dotted"),
                        labels = breaks_design) +
  labs(x = "Emissions (Mton)", y = " ") + 
  facet_grid(cols = vars(Scenario))

p_cdf_CO2_design <- ggplot(df, aes(Emissions, linetype = Scenario)) + 
  stat_ecdf() +
  scale_linetype_manual(values=c("solid", "twodash", "dotted")) +
  labs(x = "Emissions (Mton)", y = " ") + 
  facet_grid(cols = vars(Design), scales = "free")


# KS distance and p-values for NPC --------------------------------
df.NPC <- as_tibble(df) %>% select(Scenario, Design, NPC)
# comparison between design options
ks.NPC_market <- kolmogorov_smirnov_test(df = df.NPC, scenario = "Market",
                                         locate = 3)
ks.NPC_neutral <- kolmogorov_smirnov_test(df = df.NPC, scenario = "Neutral",
                                          locate = 3)
ks.NPC_green <- kolmogorov_smirnov_test(df = df.NPC, scenario = "Green",
                                        locate = 3)

# comparison between scenarios
ks.NPC_design1 <- kolmogorov_smirnov_test_sc(df = df.NPC, design = "design1",
                                             locate = 3)
ks.NPC_design2 <- kolmogorov_smirnov_test_sc(df = df.NPC, design = "design2",
                                             locate = 3)
ks.NPC_design3 <- kolmogorov_smirnov_test_sc(df = df.NPC, design = "design3",
                                             locate = 3)

# KS distance and p-values for CO2 ----------------------------------------
df.CO2 <- as_tibble(df) %>% select(Scenario, Design, Emissions)

# comparison between design options
ks.CO2_market <- kolmogorov_smirnov_test(df = df.CO2, scenario = "Market",
                                         locate = 3)
ks.CO2_neutral <- kolmogorov_smirnov_test(df = df.CO2, scenario = "Neutral",
                                          locate = 3)
ks.CO2_green <- kolmogorov_smirnov_test(df = df.CO2, scenario = "Green",
                                        locate = 3)

# comparison between scenarios
ks.CO2_design1 <- kolmogorov_smirnov_test_sc(df = df.CO2, design = "design1",
                                             locate = 3)
ks.CO2_design2 <- kolmogorov_smirnov_test_sc(df = df.CO2, design = "design2",
                                             locate = 3)
ks.CO2_design3 <- kolmogorov_smirnov_test_sc(df = df.CO2, design = "design3",
                                             locate = 3)

# Figure 5: Generalized simplex dispersion ordering -----------------------

simplex.df <- df %>%
  group_by(Scenario, Design) %>%
  group_modify(~ tibble(simplex = simplex_calc(x = .x))) %>% 
  ungroup()


p_cdf_Simp_scenario <- ggplot(simplex.df, aes(simplex, linetype = Design)) + 
  stat_ecdf() +
  scale_linetype_manual(" ", breaks = breaks_design, 
                        values = c("solid", "twodash", "dotted"),
                        labels = breaks_design) +
  labs(x = " ", y = " ") + 
  facet_grid(cols = vars(Scenario))

p_cdf_Simp_design <- ggplot(simplex.df, aes(simplex, linetype = Scenario)) + 
  stat_ecdf() +
  scale_linetype_manual(values=c("solid", "twodash", "dotted")) +
  labs(x = " ", y = " ") + 
  facet_grid(cols = vars(Design), scales = "free")


ggsave("HeatNetwork/PaperRevisions/Plot5Design.pdf", p_cdf_Simp_design,
       width = 10, height = 5)



# KS distance and p-value for G.S. disperion ordering ---------------------

ks.simplex_market <- kolmogorov_smirnov_test(df = simplex.df, 
                                             scenario = "Market", locate = 3)
ks.simplex_neutral <- kolmogorov_smirnov_test(df = simplex.df, 
                                              scenario = "Neutral", locate = 3)
ks.simplex_green <- kolmogorov_smirnov_test(df = simplex.df, 
                                            scenario = "Green", locate = 3)
ks.simplex_design1 <- kolmogorov_smirnov_test_sc(df = simplex.df, 
                                                 design = "design1", locate = 3)
ks.simplex_design2 <- kolmogorov_smirnov_test_sc(df = simplex.df, 
                                                 design = "design2", locate = 3)
ks.simplex_design3 <- kolmogorov_smirnov_test_sc(df = simplex.df, 
                                                 design = "design3", locate = 3)



