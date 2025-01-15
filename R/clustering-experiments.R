
here::i_am("R/clustering.R")
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(countrycode)
library(purrr)
library(cluster)
library(factoextra)
library(ggalluvial)
library(knitr)
source(here("R/country_classification.R"))
base_data <- as_tibble(fread(here("data/tidy/full_taxonomy_data.csv")))
first_year <- 2014
last_year <- 2018
countries <- unique(base_data$country)
years <- unique(base_data$year)

# ADD ADDITIONAL VARIABLES-----

if (FALSE){
  new_data <- WDI::WDI(
    country = countries, start = min(years), end = max(years), 
    indicator = c(
      "GDP_ppp"="NY.GDP.MKTP.PP.KD",
      "GDP_real"="NY.GDP.MKTP.KD")
    ) %>% 
    select(-c("country", "iso2c"))
  
  
  ma_data_fossil <- fread(here("data/raw/estat_nrg_ind_ffgae_en.csv")) %>%
    select(c("geo", "TIME_PERIOD", "OBS_VALUE")) %>%
    rename(ShareFossils_GrossAvEn=OBS_VALUE)
    
  ma_data_renew <- fread(here("data/raw/estat_nrg_ind_ren_en.csv")) %>%
    select(c("geo", "TIME_PERIOD", "OBS_VALUE")) %>%
    rename(ShareRenewables_GrossAvEn=OBS_VALUE)
  
  ma_data <- full_join(ma_data_fossil, ma_data_renew, by = c("geo", "TIME_PERIOD")) %>%
    dplyr::mutate(geo=countrycode(geo, "eurostat", "iso3c"))
  
  new_data <- left_join(new_data, ma_data, by = c("iso3c"="geo", "year"="TIME_PERIOD"))
  fwrite(new_data, here("data/tidy/new_data.csv"))
}
new_data <- fread(here("data/tidy/new_data.csv"))
reduced_data <- base_data %>% 
  filter(
    year<=last_year, year>=first_year
  ) %>% 
  left_join(., new_data, by = c("country"="iso3c", "year")) %>%
  mutate(population = population*1000) %>%
  mutate(# Here the normalization is done
    GreenPatents_normed = GreenPatents_n / (population/1000000), # Green patents per million capita -> YES
    EnergyConsumption_normed = FinalEnergyConsumption / population, # FinalEnergyConsumption per capita -> YES
    EnergyIntensity_va_normed = FinalEnergyConsumption / ValueAdded_pba, # energy intensity of the economy -> NO
    EnergyIntensity_gdp_normed = FinalEnergyConsumption / GDP_ppp, # energy intensity of the economy -> NO
    EnergyProduction_normed = PrimaryEnergyProduction/population, # PrimaryEnergyProduction per capita -> YES
    ShareRenewables_PrimEnProd_normed=ShareRenewables_PrimEnProd, # -> YES
    ShareFossils_PrimEnProd_normed=ShareFossils_PrimEnProd, # -> NO
    ShareFossils_GrossAvEn_normed = ShareFossils_GrossAvEn, # share of fossils in gross available consumption -> YES
    ShareRenewables_GrossAvEn_normed = ShareRenewables_GrossAvEn, # share of renewables in gross available consumption -> NO
    GDP_ppp_normed = GDP_ppp/population, # -> TEST
    GDP_real_normed = GDP_real/population,# -> TEST
    GWP_trade_normed = (GWP_Imports - GWP_Exports)/population, # GWP net imports per capita# -> TEST
    GWP_normed = GWP_pba/population, # GWP per capita# -> TEST
    # ValueAdded_normed = ValueAdded_pba/population, # ValueAdded per capita
    EnergyExports_pop_normed = EnergyNetTrade / population, # EnergyNetExports per capita -> NO
    EnergyExports_prod_normed = EnergyNetTrade / PrimaryEnergyProduction # Energy security -> NO
  ) %>%
  select(all_of(c("country")), contains("_normed")) %>%
  summarise(across(.cols = everything(), .fns = mean),.by = "country") %>% 
  mutate(country=countrycode(country, "iso3c", "country.name"))

# Explore data-------
get_vis <- function(var1, var2){
  reduced_data %>%
    summarise(across(.cols = everything(), 
                     .fns = ~ mean(.x, na.rm = TRUE)), .by = "country") %>%
    ggplot(
      data = ., 
      aes(
        x=.data[[var1]], 
        y=.data[[var2]], color=country)) +
    geom_point() +
    geom_label(aes(label=country)) +
    theme_light() +
    theme(legend.position = "none")
}
# Production and consumption -> keep both
get_vis("EnergyConsumption_normed", "EnergyProduction_normed")

# Kinds of renewables
get_vis("ShareRenewables_PrimEnProd_normed", "ShareRenewables_GrossAvEn_normed") 
get_vis("ShareFossils_PrimEnProd_normed", "ShareFossils_GrossAvEn_normed")


# The mismatch is surprising; gdp is more in line with official statistics
get_vis("EnergyIntensity_va_normed", "EnergyIntensity_gdp_normed")

# 
get_vis("EnergyIntensity_va_normed", "EnergyConsumption_normed")
get_vis("EnergyIntensity_gdp_normed", "EnergyConsumption_normed")
get_vis("EnergyIntensity_gdp_normed", "GDP_ppp_normed")
get_vis("EnergyConsumption_normed", "GDP_ppp_normed")


get_vis("FossilShare_normed", "RenewShare_normed")
# using energy exports?
get_vis("EnergyExports_prod_normed", "EnergyProduction_normed")
get_vis("EnergyExports_pop_normed", "EnergyProduction_normed")
get_vis("EnergyExports_prod_normed", "EnergyConsumption_normed")
get_vis("EnergyExports_pop_normed", "EnergyConsumption_normed") # -> correlation -> not used

reduced_data %>%
  summarise(across(.cols = everything(), 
                   .fns = ~ mean(.x, na.rm = TRUE)), .by = "country") %>%
  ggplot(
    data = ., 
    aes(
      x=reorder(country, -EnergyExports_prod_normed), 
      y=EnergyExports_prod_normed, 
      color=country, fill = country)) +
  geom_bar(stat="identity") +
  theme_light()
# Given the small variance and some outliers we decide not to use it



# CONDUCT CLUSTERING-------
# TODO: rename
reduced_data_used <- reduced_data %>%
  select(all_of(c("country", 
                  "GreenPatents_normed", 
                  "EnergyConsumption_normed", 
                  "EnergyProduction_normed", 
                  "ShareRenewables_PrimEnProd_normed", 
                  #"ShareFossils_GrossAvEn_normed",  
                  "GDP_ppp_normed", 
                  #"GDP_real_normed", 
                  "GWP_trade_normed", 
                  "GWP_normed"
                  )
))
# Coerce into data.frame structure:
reduced_data_df <- as.data.frame(reduced_data_used)
rownames(reduced_data_df) <- reduced_data_df$country
reduced_data_df$country <- NULL
# Scale data
reduced_data_scaled <- scale(reduced_data_df)
# Compute dissimilarity matrix
reduced_data_dist <- dist(reduced_data_scaled, method = "euclidean")
# Clustering coefficients

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
ac <- function(x) {
  agnes(reduced_data_dist, method = x)$ac
}
ac_overview <- map_dbl(m, ac)
tibble("Algorithm"=names(ac_overview), "Coefficient"=ac_overview)
hc_ward_agnes <- agnes(reduced_data_dist, method = "ward")

# DENDOGRAM-------
n_clusters <- 5

fviz_dend(hc_ward_agnes,
          main = "Titel",
          xlab = "Countries", ylab = "",
          k = n_clusters, # Cut in groups
          cex = 0.75, # label size
          rect = TRUE, # Add rectangle around groups
          rect_fill = TRUE,
          color_labels_by_k = TRUE, # color labels by groups
          # k_colors = RColorBrewer::brewer.pal(n_groups, "Dark2"),
          # rect_border = RColorBrewer::brewer.pal(n_groups, "Dark2"),
          horiz = TRUE
) + ggtitle("No ffossils")
ggsave(filename = here("plots/NoFossil.pdf"))

clusters_obtained <- cutree(as.hclust(hc_ward_agnes), k = n_clusters)
clusters_obtained_tab <- tibble(
  "country" = names(clusters_obtained),
  "Ecological model" = as.character(clusters_obtained)
) 
m1 <- filter(clusters_obtained_tab, `Ecological model`=="1") %>% select(country) %>% rename(`Model` = country)
m2 <- filter(clusters_obtained_tab, `Ecological model`=="2") %>% select(country) %>% rename(`Model` = country)
m3 <- filter(clusters_obtained_tab, `Ecological model`=="3") %>% select(country) %>% rename(`Model` = country)
m4 <- filter(clusters_obtained_tab, `Ecological model`=="4") %>% select(country) %>% rename(`Model` = country)
max_n <- max(nrow(m1), nrow(m2), nrow(m3), nrow(m4))
cluster_list <- list(
  "Ecological model 1" = m1 %>% add_row(`Model`=rep("", max_n-nrow(m1))) %>% pull(Model), 
  "Ecological model 2" = m2 %>% add_row(`Model`=rep("", max_n-nrow(m2))) %>% pull(Model),# %>% add_row(`Model 2`=rep(NA, max_n-nrow(m2))), 
  "Ecological model 3" = m3 %>% add_row(`Model`=rep("", max_n-nrow(m3))) %>% pull(Model),
  "Ecological model 4" = m4 %>% add_row(`Model`=rep("", max_n-nrow(m4))) %>% pull(Model)
)
as_tibble(cluster_list)


# COMPARE DEVELOPMENT-------
clusters_obtained_tb <- tibble(
  "country" = names(clusters_obtained),
  "Ecological model" = as.character(clusters_obtained),
  "Development model" = get_country_classification(
    countrycode(country, "country.name", "iso3c"), "jee")
) %>% 
  pivot_longer(
    cols = -country, names_to = "group", values_to = "code"
  )%>%
  mutate(time_=ifelse(group=="Ecological model", 2, 1))

ggplot(clusters_obtained_tb,
       aes(
         x = group, stratum = code, 
         alluvium = country,
         fill = code, label = code)
) +
  # scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(
    stat = "alluvium", lode.guidance = "frontback",
    color = "darkgray", curve_type = "linear"
  ) +
  geom_stratum() +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "bottom") +
  ggtitle("Development and ecological models") +
  theme_void() +
  theme(
    axis.text.x = element_text(), 
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5)
  )

# DESCRIPTIVES

# This is not weighted by population. Should we do this?
  
get_ecol_cluster <- function(country){
  as.character(unname(clusters_obtained[country]))
}
get_ecol_cluster <- Vectorize(get_ecol_cluster)

descriptive_data <- reduced_data_used %>%
  mutate(ecolmodel=get_ecol_cluster(country)) 

make_plot <- function(var_used){
  ggplot(
    descriptive_data, 
    aes(x=ecolmodel, y=.data[[var_used]], 
        color=ecolmodel, fill=ecolmodel, group=ecolmodel)
  ) +
    labs(title = var_used, x="Ecological model") +
    scale_y_continuous(labels = scales::label_number(scale = 0.001, suffix = "k")) +
    geom_jitter() + geom_violin(drop = FALSE, alpha=0.5) + 
    theme_linedraw() + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
}

vars_used <- setdiff(names(descriptive_data), c("country", "ecolmodel"))
p_list <- map(.x = vars_used, .f = make_plot)
library(ggpubr)
ggarrange(plotlist = p_list, ncol = 3, nrow = 4)








make_plot_bar <- function(var_used){
  ggplot(
    descriptive_data, 
    aes(x=reorder(country, -.data[[var_used]]), y=.data[[var_used]], 
        color=ecolmodel, fill=ecolmodel)
  ) +
    labs(title = var_used, x="Ecological model") +
    scale_y_continuous(labels = scales::label_number(scale = 0.001, suffix = "k")) +
    geom_bar(stat = "identity")+
    theme_linedraw() + theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
}

p_list_bar <- map(.x = vars_used, .f = make_plot_bar)
ggarrange(plotlist = p_list_bar, ncol = 3, nrow = 4)



