###
# Project: Decompose the recent fertility decline
# Purpose: Descriptive of the current fertility decline
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 2025/07/29
###

library(tidyverse)
library(HMDHFDplus)
library(patchwork)
library(latex2exp)
library(DemoDecomp)

# Load the functions
source("code/functions.R")

# Load the user details
source("U:/accounts/authentification.R")

# Create the directories
folders <- c("data", "raw", "text", "figures", "code")
lapply(folders, function(folder){if(!dir.exists(folder)) dir.create(folder)})

# Create a vector with country selection
country_selection <- c("France", "Denmark", "U.K.", "Finland", "Germany",  "Norway", "Sweden", "U.S.A")

### Query the data ------------------------


# Get the HFD countries
hfd_coun <- getHFDcountries()
hfd_coun <- hfd_coun %>% 
  mutate(cluster = case_when(
    Country %in% c("Austria", "Switzerland", "Germany") ~ "German-speaking",
    Country %in% c("Denmark", "Finland", "Norway", "Iceland", "Sweden") ~ "Nordic",
    Country %in% c("France", "Luxembourg", "Belgium", "Netherlands") ~ "Western European",
    Country %in% c("U.S.A", "U.K.", "Northern Ireland", "Canada", "Ireland", "England and Wales", "Scotland") ~ "Anglo-saxon",
    Country %in% c("Latvia", "Slovakia", "Ukraine", "Bulgaria", "Hungary", "Poland", "Estonia", "Czechia", "Belarus", "Croatia", "Russia", "Lithuania", "Slovenia") ~ "Eastern-European",
    Country %in% c("Japan", "Taiwan", "Republic of Korea") ~ "Southeast Asian",
    Country %in% c("Italy", "Spain", "Greece", "Portugal") ~ "Mediterranean",
    Country %in% c("Chile", "Israel", "West Germany", "East Germany") ~ "others"
  ))


file_tfr <- "data/tfr_annual.Rda"
file_asfr <- "data/asfr_annual.Rda"
file_stff <- "data/tfr_stff.Rda"

if (file.exists(file_tfr)) {
  
  load(file_tfr)
  
  } else {

  # Create the containers
  hfd_asfrs <- hfd_tfrs <- vector("list", length=nrow(countries))
  names(hfd_asfrs) <- names(hfd_tfrs) <- countries$Country
  
  # Select a country
  for (i in seq_along(countries$CNTRY)) {
  
    print(i)
    
    # Get the HFD years
    hfd_date <- getHFDdate(CNTR=countries$CNTRY[i])
    
    # Get the available itmes
    hfd_items <- getHFDitemavail(countries$CNTRY[i])
    
    ## Get the TFR data -----------------------
  
    # Get the TFR itme
    tfr_item <- hfd_items$item[hfd_items$measure=="Total fertility rate" & hfd_items$subtype=="all_birth_orders_combined"]
    
    # Load the tfr data
    hfd_tfr <- readHFDweb(countries$CNTRY[i], tfr_item, username=hfd_un, password=hfd_pw)
    
    # Assign the result
    hfd_tfrs[[countries$Country[i]]] <- hfd_tfr
  
  }
  
  # Create the national TFR data
  hfd_tfrs <- bind_rows(hfd_tfrs, .id="Country")
  
  # Save the data
  save(hfd_tfrs, file=file_tfr)

}

## Look at ASFRs ----------------------

if(file.exists(file_asfr)) {
  
  load(file_asfr)
  
  } else {
  
  
  # Select a country
  for (i in seq_along(countries$CNTRY)) {
    
    print(i)
    
    # Get the ASFR parity item
    asfr_item <- hfd_items$item[hfd_items$measure=="Age-specific fertility rates" & hfd_items$subtype=="all_birth_orders_combined" & hfd_items$lexis=="age-period"]
    
    # Load the tfr data
    hfd_asfr <- readHFDweb(cntry, asfr_item, username=hfd_un, password=hfd_pw)
    
    # Assign the result
    hfd_asfrs[[countries$Country[i]]] <- hfd_asfr
  
  }
  
  # Bind the results together
  hfd_asfrs <- bind_rows(hfd_asfrs, .id="Country")
  
  save(hfd_asfr, file=file_asfr)

}


# Create the mean age of childbearing
hfd_mac <- hfd_asfrs %>% 
  group_by(Country, Year) %>% 
  summarise(MAC = sum(Age * ASFR) / sum(ASFR), .groups="drop")


# Plot the long data
ggplot(data=subset(hfd_asfrs, Country%in%country_selection&Year>=2000), aes(x=Age, y=ASFR, colour=Year, group=Year)) +
  geom_line() +
  geom_segment(data=subset(hfd_mac, Country%in%country_selection&Year>=2000), aes(x=MAC, xend=MAC, y=-0.001, yend=0.01, colour=Year)) +
  scale_x_continuous("Age") +
  scale_y_continuous("Age-specific_fertility rate", expand=c(0, 0)) +
  facet_wrap(~ Country) +
  scale_colour_viridis_c(guide="legend", limits=c(2000, 2015), breaks=seq(2000, 2015, by=5)) +
  theme(
    legend.key.width=unit(2, "cm")
  )


# Create the tile in the postponement
hfd_asfrs %>% 
  arrange(Country, Age, Year) %>% 
  group_by(Country, Age) %>% 
  mutate(ASFR_change = lead(ASFR) - ASFR) %>%
  filter(Country %in% country_selection & !is.na(ASFR_change) & Year > 2000) %>% 
  ggplot(aes(x=Year, y=Age, fill=ASFR_change)) +
  geom_tile() +
  facet_wrap(~ Country) +
  scale_x_continuous("Year", expand=c(0, 0), n.breaks=10) +
  scale_y_continuous("Age", expand=c(0, 0), n.breaks=10) +
  scale_fill_gradient2(low="darkred", mid="grey", high="darkblue", midpoint=0) +
  theme(
    legend.key.width=unit(2, "cm")
  )
  

# Look at the cohort feritlity =======================

# Load the cohort fertility
lit_items <- getHFDitemavail(hfd_coun$CNTRY[hfd_coun=="Lithuania"])

# Load the cohort fertility rate
lit_fert_tab <- readHFDweb("LTU",  "tfrVHbo", hfd_un, hfd_pw)

# Decompose the parity table =========================

parity_decomposition <- function(cntry, t0, t1) {
  
  # Decompose fertility table
  par_tab <- readHFDweb(cntry, "asfrRRbo", hfd_un, hfd_pw)
  
  # Plot the trend in the total fertility rate
  par_tab |> 
    group_by(Year) |> 
    summarise(tfr=sum(ASFR)) |> 
    ggplot(aes(x=Year, y=tfr)) +
      geom_vline(xintercept=t0, linetype="dotted") +
      geom_vline(xintercept=t1, linetype="dotted") +
      scale_x_continuous(expand=c(0, 0), n.breaks=10) +
      scale_y_continuous("Total Fertility Rate", n.breaks=8) +
      geom_line(linewidth=2) +
      theme_test(base_size=16, base_family="serif")
  ggsave(filename = paste0("figures/tfr_trend_", cntry, "_", t0, "-", t1, ".pdf"))
  
  # Select the two tables
  asfr_t0 <- par_tab[par_tab$Year==t0, ]
  asfr_t1 <- par_tab[par_tab$Year==t1, ]
  
  # Vectorize the data
  asfr_t0 <- pivot_longer(asfr_t0, cols=matches("^ASFR\\d"), names_prefix="ASFR", names_to = "parity", values_to="asfr")
  asfr_t1 <- pivot_longer(asfr_t1, cols=matches("^ASFR\\d"), names_prefix="ASFR", names_to = "parity", values_to="asfr")
  
  # Decompose the data
  decomp_diff <- horiuchi(func = sum,
           pars1 = asfr_t0$asfr,
           pars2 = asfr_t1$asfr,
           N = 10)
  
  # Estimate the tfr change
  tfr_change <- sum(asfr_t1$asfr)-sum(asfr_t0$asfr)
  
  # Plot the result
  asfr_t0$contribution <- decomp_diff
  asfr_t0$rel_contribution <- asfr_t0$contribution/tfr_change
  
  # Plot the decomposition
  ggplot(asfr_t0, aes(x=Age, y=contribution, fill=parity)) +
    geom_hline(yintercept=0) +
    geom_col() +
    scale_x_continuous(expand=c(0, 0), limits = c(15, 50), breaks=seq(10, 60, by=5)) +
    scale_y_continuous("Parity-age specific ASFR change") +
    scale_fill_viridis_d("Parity") +
    theme_minimal(base_size = 16, base_family="serif") +
    theme(
      axis.text = element_text(colour="black")
    )
  ggsave(filename = paste0("figures/parity_decomp_abs_", cntry, "_", t0, "-", t1, ".pdf"))
  
  # Plot the relative contribution
  ggplot(asfr_t0, aes(x=Age, y=rel_contribution, fill=parity)) +
    geom_hline(yintercept=0) +
    geom_col() +
    scale_x_continuous(expand=c(0, 0), limits = c(15, 50), breaks=seq(10, 60, by=5)) +
    scale_y_continuous("Relative contribution", labels=scales::percent) +
    scale_fill_viridis_d("Parity") +
    theme_minimal(base_size = 16, base_family="serif")+
    theme(
      axis.text = element_text(colour="black")
    )
  ggsave(filename = paste0("figures/parity_decomp_rel_", cntry, "_", t0, "-", t1, ".pdf"))
  
  # Percent contribution by parity
  parity_contribution <- asfr_t0 |> 
    group_by(parity) |> 
    summarise(contribution=sum(contribution), 
              rel_contribution=contribution/tfr_change)
  # Plot
  ggplot(parity_contribution, aes(x=parity, y=rel_contribution, fill=parity)) +
    geom_hline(yintercept=0) +
    geom_col() +
    geom_text(aes(x=parity, label=paste(round(rel_contribution*100, 2), "%")), colour="white", family="serif", vjust=-1, size=8) +
    scale_x_discrete("Parity", expand=c(0, 0)) +
    scale_y_continuous("Relative contribution", labels=scales::percent) +
    scale_fill_viridis_d("Parity") +
    theme_test(base_size=16, base_family="serif") +
    guides(fill="none") +
    theme(
      rect=element_blank(), 
      axis.ticks.x=element_blank(),
      axis.text = element_text(colour="black")
    ) +
    annotate(geom="text", label=paste("TFR 1=", round(sum(asfr_t0$asfr), 2), paste0("(", t0, ")"), "\nTFR 2=",  round(sum(asfr_t1$asfr), 2),  paste0("(", t1, ")"), "\nTFR change=", round(-tfr_change, 2)), x="5p", y=-abs(parity_contribution$rel_contribution[5]*3), family="serif", size=6)
  ggsave(filename = paste0("figures/parity_contr_", cntry, "_", t0, "-", t1, ".pdf"))

}

# Decompose lithuania
parity_decomposition("LTU", 1972, 2003)
parity_decomposition("LTU", 2003, 2015)
parity_decomposition("LTU", 2015, 2020)

# Make the same for Norway
parity_decomposition("NOR", 1967, 1976)
parity_decomposition("NOR", 2003, 2010)
parity_decomposition("NOR", 2009, 2021)

## Get the monthly birth countrs =====================

# Load the short term fertility fluctuations
path_hfd_stff <- "https://www.humanfertility.org/File/GetDocumentFree/STFF/stff.csv"

# Load the data
hfd_stff <- read.csv(path_hfd_stff)

# Make all columns numberic
hfd_stff <- hfd_stff %>% 
  mutate(across(!CountryCode, as.numeric))

# Create the birth data
hfd_stff_births <- pivot_longer(hfd_stff[, 1:16], cols=c(month.name, "UNK"),  names_to="month", values_to="births")

# Create the TFR data
hfd_stff_tfrs <- pivot_longer(hfd_stff[, c(1:3, 17:ncol(hfd_stff))], cols=starts_with("TFR"),  names_to="month", values_to="tfr")
hfd_stff_tfrs$month <- str_remove(hfd_stff_tfrs$month, pattern="TFR")

# Create the data
hfd_stff_tfrs$date <- as.Date(paste(hfd_stff_tfrs$Year, match(hfd_stff_tfrs$month, month.name), "01", sep="-"))

# Merge with the country data
hfd_stff_tfrs <- merge(hfd_stff_tfrs, countries, by.x="CountryCode", by.y="CNTRY")

# Select the last observation for all coutnries
hfd_stff_labels <- hfd_tfrs %>% 
  arrange(Country, Year) %>% 
  group_by(Country) %>% 
  mutate(nr = row_number(), max_nr = max(nr)) %>% 
  filter(nr==max_nr) %>% 
  select(-nr, -max_nr)

# Plot the tfrs trend
plot_tfr_trend <- ggplot(subset(hfd_stff_tfrs, month!="TOT" & Country %in% country_selection), aes(x = date, y=tfr, colour=Country, group=Country)) +
  geom_rect(xmin = as.numeric(as.Date("2007-10-1")), xmax=as.numeric(as.Date("2009-07-1")), ymin=0, ymax=3, fill="grey90", colour="black") +
  geom_line(alpha=0.5) +
  geom_line(data=subset(hfd_tfrs, Year >= 2000 & Country %in% country_selection), aes(x=as.Date(paste(Year, "01", 01, sep="-")), y=TFR), linewidth=1.3) +
  geom_point(data=subset(hfd_tfrs, Year >= 2000 & Country %in% country_selection), aes(x=as.Date(paste(Year, "01", 01, sep="-")), y=TFR), size=3) +
  geom_label(data=subset(hfd_stff_labels, Country %in% country_selection), aes(x=as.Date(paste(Year, "01", 01, sep="-")), y=TFR, label=Country), nudge_x=50, hjust="left", size=4) +
  #facet_wrap(~ Country) +
  scale_x_date("Year", expand=c(0, 0.5), date_breaks="2.5 years", date_labels = "%Y", limits=c(as.Date("2000-01-01"), as.Date("2026-01-01"))) +
  scale_y_continuous("Total fertility rate", expand=c(0, 0), n.breaks = 10, limits=c(1.1, 2.27)) +
  annotate(geom="text", x=as.Date("2009-07-01"), y=2.25, label="Great Recession", family="serif", hjust="right", size=4) +
  guides(colour="none", shape="none") +
  scale_colour_brewer(palette="Set2") +
  theme(
    panel.grid.major.y=element_line(linetype="dotted", colour="black", linewidth=0.2)
  )

save_fig(filename="tfr_trend", figure=plot_tfr_trend, width=22)


# Estimate the trend
tfr_declines <- hfd_stff_tfrs %>% 
  group_by(Country, Year) %>% 
  summarise(TFR = mean(tfr)) %>% 
  filter(Year %in% c(2008, 2019)) %>% 
  mutate(abs_decline = diff(TFR), rel_decline = round(100*abs_decline / TFR, 2)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols=c("Country", "rel_decline", "abs_decline"), names_from="Year", values_from="TFR", names_prefix="tfr_") %>% 
  arrange(rel_decline)

# When did the fertility start declining
hfd_stff_change <- hfd_stff_tfrs %>% 
  group_by(Country, month) %>% 
  arrange(Country, date) %>% 
  mutate(change = 100 * (tfr - lag(tfr)) / tfr) %>% 
  filter(Year > 2005)

# Plot the change
ggplot(data=subset(hfd_stff_change, Country %in% country_selection), aes(x=date, y=change, group=Country, colour=Country)) +
    geom_hline(yintercept = 0) +
    geom_line() + 
    scale_x_date("Year", expand=c(0, 0), date_breaks="1 years", date_labels = "%Y", limits=c(as.Date("2006-01-01"), as.Date("2025-01-01"))) +
    scale_y_continuous(TeX("$\\Delta$TFR (in \\%)"), limits=c(-30, 20), n.breaks=10) +
  theme(
    panel.grid.major.y=element_line(linetype="dotted", colour="black", linewidth=0.2)
  )

# Plot the seasonally adjusted TFR
hfd_stff_change %>% 
  group_by(Country) %>% 
  mutate(decline = ifelse(change < -5 & lag(change) < -5 & lag(change, n=2) < -5 & lag(change, n=3) < -5, 1, 0)) %>% 
  filter(decline==1) %>% 
  arrange(Country, Year) %>% 
  mutate(nr = row_number()) %>% 
  filter(nr == 1) %>% 
  ggplot(aes(x=Year, y=change)) +
    geom_text(aes(label=Country), vjust="top") +
    geom_point() +
    scale_x_continuous("Onset of TFR decline") +
    scale_y_continuous("Change of TFR decline")


## Plot increasing countries -------------------

# Countries with increasing fertility
ggplot(data=subset(hfd_tfrs, Country %in% tfr_declines$Country[tfr_declines$abs_decline>0]), aes(x=Year, y=TFR, group=Country, colour=Country)) +
  geom_line() +
  geom_point() +
  coord_cartesian(xlim=c(2000, 2023), ylim=c(1.1, 3.0))
  




## Seasonal adjustment --------------------------

# Estimate the adjustment factor
tfrs_adjusted <- hfd_stff_tfrs %>% 
  filter(!is.na(tfr)& month != "TOT") %>% 
  group_by(Country, month) %>% 
  mutate(tfr_month=mean(tfr)) %>% 
  group_by(Country, Year) %>%  
  mutate(tfr_period=mean(tfr)) %>% 
  mutate(tfr_adjusted = tfr / (tfr_month/tfr_period))


# Plot the adjusted TFR
ggplot(data=tfrs_adjusted, aes(x=date, y=tfr_adjusted, group=Country, colour=Country)) +
  geom_line() +
  scale_x_date("Year", expand=c(0, 0.5), date_breaks="2.5 years", date_labels = "%Y", limits=c(as.Date("2000-01-01"), as.Date("2026-01-01"))) +
  scale_y_continuous("Total fertility rate", expand=c(0, 0), n.breaks = 10)

# Start of the fertility decline
tfr_decline_onset <- tfrs_adjusted %>% 
  group_by(Country, month) %>% 
  arrange(Country, date) %>% 
  mutate(change = 100 * (tfr_adjusted - lag(tfr_adjusted)) / tfr_adjusted) %>% 
  filter(Year > 2007 & Year <= 2019) %>% 
  group_by(Country) %>% 
  mutate(decline = ifelse(change < -5 & lag(change) < -5 , 1, 0)) %>% 
  filter(decline==1) %>% 
  arrange(Country, Year) %>% 
  mutate(nr = row_number()) %>% 
  filter(nr == 1)


# Plot the amount of decline
tfr_declines <- tfrs_adjusted %>% 
  group_by(Country, Year) %>% 
  summarise(tfr=mean(tfr), .groups="drop") %>% 
  filter(Year%in%c(2008, 2019) & Country != "England and Wales") %>% 
  group_by(Country) %>% 
  mutate(abs_decline = diff(tfr),
         rel_decline=100*diff(tfr)/tfr,
         positive = ifelse(rel_decline>=0, 0.8, -1)) %>% 
  filter(Year==2008)


# Plot the absolute declines
plot_tfr_decline_abs <- ggplot(tfr_declines, aes(x=fct_reorder(Country, abs_decline), y=abs_decline, fill=as.factor(positive))) +
  geom_col() +
  geom_hline(yintercept=0) +
  geom_text(aes(label=round(abs_decline, 2), y=abs_decline+(positive*0.02)), family="serif") +
  coord_flip() +
  scale_fill_manual(values=c("darkred", "darkblue")) +
  scale_y_continuous(TeX("$TFR_{2009}-TFR_{2018}$"), n.breaks=10) +
  guides(fill="none") +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )

save_fig(filename="abs_tfr_decline", figure=plot_tfr_decline_abs)

# Plot the relative decline
plot_tfr_decline_rel <- ggplot(tfr_declines, aes(x=fct_reorder(Country, rel_decline), y=rel_decline, fill=as.factor(positive))) +
  geom_col() +
  geom_hline(yintercept=0) +
  geom_text(aes(label=paste0(round(rel_decline, 2), "%"), y=rel_decline+(2*positive)), family="serif") +
  coord_flip() +
  scale_fill_manual(values=c("darkred", "darkblue")) +
  scale_y_continuous("Percentage TFR change (2009 and 2018)", n.breaks=10) +
  guides(fill="none") +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )

save_fig(filename="rel_tfr_decline", figure=plot_tfr_decline_rel)


# Create the histogram
plot_a <- ggplot(data=tfr_decline_onset, aes(x=date)) +
  geom_histogram(bins=20) +
  scale_x_date("Onset of the fertility decline", expand = c(0, 0), limits=c(as.Date("2008-01-01"), as.Date("2020-12-01"))) +
  theme_void()

# Plot the data
plot_tfr_decline_onset <- ggplot(data=subset(tfr_decline_onset, Country %in% unique(tfr_declines$Country[tfr_declines$abs_decline<0])), aes(x=date, y=fct_reorder(Country, desc(date)))) +
  geom_point(aes(colour=date)) +
  geom_linerange(aes(xmin=as.Date("2008-01-01"), xmax=date, colour=date)) +
  geom_text(aes(label = format(date, "%Y-%m")), hjust="left", nudge_x=50, family="serif") + 
  scale_x_date("Onset of the fertility decline", date_breaks="1 year", date_labels = "%Y", expand = c(0, 0), limits=c(as.Date("2008-01-01"), as.Date("2020-12-01"))) +
  scale_y_discrete("Country") +
  scale_colour_viridis_c(option="A") +
  guides(colour="none") +
  theme(
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.y=element_blank()
  )
  
save_fig(filename="onset_decline", figure=plot_tfr_decline_onset, height=20, width=20)
  

# Create the summary figure
plot_combined <- plot_tfr_trend / (plot_tfr_decline_abs + plot_tfr_decline_onset) + plot_annotation(tag_levels="A")
  
save_fig(filename="combined_figure", figure=plot_combined, height=30, width=20)

### END ###################################
