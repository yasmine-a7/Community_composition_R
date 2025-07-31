
# Doubtless bay percent cover
################################

# Setting up data

## Set Working directory
setwd("C:/Users/yassy/OneDrive - Victoria University of Wellington - STAFF/PhD/Video_Analysis_Communities/DoubtlessBay")


## Load in data frame

library(readr)
db_df <- read.csv("DB_PercentCover.csv")


# CHECK rows add to 100

## Identify metadata and species columns
metadata_cols <- c("Image.ID", "Image.name", "Location", "Drop", "Depth", "Latitude", "Longitude", "Site", "Sediment_mean", "Sediment_cat")
species_cols <- setdiff(names(db_df), metadata_cols)

## Calculate row sums for percent cover columns
db_df$row_sum <- rowSums(db_df[species_cols], na.rm = TRUE)

## Check summary statistics for row sums
summary(db_df$row_sum)
View(db_df)

# Delete row_sum
library(dplyr)
db_df <- db_df %>% select(-row_sum)



# Remove "Other"

db_df_no_other <- db_df %>%
  select(-Other)

## Identify metadata columns
metadata_cols <- c("Image.ID", "Image.name", "Location", "Drop", "Depth", "Latitude", "Longitude", "Site", "Sediment_mean", "Sediment_cat")
## Identify species columns (excluding "Other")
species_cols <- setdiff(names(db_df_no_other), metadata_cols)

## Calculate the sum of percent cover for each image (excluding "Other")
db_df_no_other <- db_df_no_other %>%
  rowwise() %>%
  mutate(total_cover = sum(c_across(all_of(species_cols)), na.rm = TRUE)) %>%
  ungroup()

## Rescale each species column so the sum is 100% per image
db_df_final <- db_df_no_other %>%
  mutate(across(all_of(species_cols), ~ .x / total_cover * 100))


# CHECK rows add to 100

## Calculate the row sums of the species columns
db_df_final <- db_df_final %>%
  rowwise() %>%
  mutate(row_sum = sum(c_across(all_of(species_cols)), na.rm = TRUE)) %>%
  ungroup()

# Check the summary of row_sum
summary(db_df_final$row_sum)

# REMOVE row_sum and total_cover
db_df_final <- db_df_final %>% select(-row_sum, -total_cover)


## Split into categories

meta <- db_df_final[, 1:8]                      # Drop, Site, etc
groups <- db_df_final[, 9:ncol(db_df_final)]          # Groups of species


##########################################

# Investigation

#####
## Plot sediment % per site
#####

library(ggplot2)

ggplot(db_df_final, aes(x = Drop, y = Sediment)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Sediment Percentage per Drop")



######
## PERMANOVA - sediment predictor of community composition
######

# Remove "Sediment"

db_df_no_sedi <- db_df_final %>%
  select(-Sediment)

## Identify metadata columns
metadata_cols <- c("Image.ID", "Image.name", "Location", "Drop", "Depth", "Latitude", "Longitude", "Site", "Sediment_mean", "Sediment_cat")
## Identify species columns (excluding "Other")
species_cols <- setdiff(names(db_df_no_sedi), metadata_cols)

## Calculate the sum of percent cover for each image (excluding "Other")
db_df_no_sedi <- db_df_no_sedi %>%
  rowwise() %>%
  mutate(total_cover = sum(c_across(all_of(species_cols)), na.rm = TRUE)) %>%
  ungroup()

## Rescale each species column so the sum is 100% per image
db_df_no_sedi_final <- db_df_no_sedi %>%
  mutate(across(all_of(species_cols), ~ .x / total_cover * 100))

# REMOVE and total_cover
db_df_no_sedi_final <- db_df_no_sedi_final %>% select(-total_cover)


no_sedi_nosed <- db_df_no_sedi_final %>%
  select(-c(1:8))  # Exclude sediment and metadata

sediment_vector <- db_df_final$Sediment

# No sediment - continuous sediment vector
set.seed(777)
adonis2(no_sedi_nosed ~ sediment_vector, method = "bray", permutations = 999)

# With sediment
adonis2(groups ~ sediment_vector, method = "bray", permutations = 999)

# Drop permanova
drop_vector <- db_df_final$Drop
adonis2(no_sedi_nosed ~ drop_vector, method = "bray", permutations = 999)

# Depth permanova
adonis2(no_sedi_nosed ~ db_df_final$Depth, method = "bray", permutations = 999)


# create sediment categories
db_df_final <- db_df_final %>%
  mutate(Sediment_cat_3 = cut(
    Sediment,
    breaks = c(-Inf, 33, 66, Inf),
    labels = c("Low", "Medium", "High"),
    right = FALSE
  ))



# Categorical sediment vector
adonis2(no_sedi_nosed ~ db_df_final$Sediment_cat_3, method = "bray", permutations = 999)

# Post-hoc SIMPER
simper_res <- simper(no_sedi_nosed, db_df_final$Sediment_cat_3)


#############################################


# Mean sediment cover per drop and add as new column
library(dplyr)



db_df_final <- db_df_final %>%
  group_by(Drop) %>%
  mutate(Sediment_mean = mean(Sediment, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Sediment_cat = cut(
    Sediment_mean,
    breaks = c(-Inf, 10, 25, 50, 75, Inf),
    labels = c("0–10%", "10–25%", "25–50%", "50–75%", "75–100%"),
    right = FALSE
  ))


## add sediment data to end of the df that excludes sediment
db_df_no_sedi_final <- db_df_no_sedi_final %>% left_join(db_df_final %>% select(Image.ID, Sediment_mean, Sediment_cat), by = "Image.ID")








# NMDS


## NMDS Ordination 
library(vegan)

set.seed(777)
nmds <- metaMDS(no_sedi_nosed, distance = "bray")


## Extract scores and add metadata
nmds_scores <- as.data.frame(scores(nmds)$sites)
nmds_scores$Drop <- db_df_no_sedi_final$Drop
nmds_scores$Image.ID <- db_df_no_sedi_final$Image.ID
nmds_scores$Sediment_mean <- db_df_no_sedi_final$Sediment_mean
nmds_scores$Sediment <- db_df_no_sedi_final$Sediment
nmds_scores$Sediment_cat <- db_df_no_sedi_final$Sediment_cat



# 17 drops, alternating solid and hollow shapes
solid_shapes  <- c(16, 17, 15, 18, 8, 7, 9, 11, 13)
hollow_shapes <- c(1, 2, 0, 5, 4, 3, 10, 12, 14)
shape_values  <- c(rbind(solid_shapes, hollow_shapes))[1:17]  # Interleave, then truncate to 17

# Assign each drop a shape
drops <- sort(unique(nmds_scores$Drop))
shape_map <- data.frame(
  Drop = drops,
  shape = shape_values
)

nmds_scores <- dplyr::left_join(nmds_scores, shape_map, by = "Drop")

# Sediment vector

library(vegan)

# Fit Sediment as a continuous environmental vector
fit <- envfit(nmds, db_df_final$Sediment, permutations = 999)

# Extract vector scores and correlation (r)
vector_scores <- as.data.frame(scores(fit, display = "vectors"))
vector_scores$variable <- rownames(vector_scores)
vector_scores$r <- fit$vectors$r


library(ggplot2)

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2,
                        shape = factor(Drop),
                        colour = Sediment_cat)) +
  geom_point(size = 4, stroke = 1.5, show.legend = TRUE) +
  scale_shape_manual(values = shape_map$shape) +
  scale_colour_manual(values = c(
    "0–10%" = "#1b9e77", 
    "10–25%" = "#d95f02",
    "25–50%" = "#7570b3",
    "50–75%" = "#e7298a",
    "75–100%" = "#66a61e"
  )) +
  # Overlay vector
  geom_segment(
    data = vector_scores,
    aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
    arrow = arrow(length = unit(0.3, "cm")),
    colour = "black",
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = vector_scores,
    aes(x = NMDS1, y = NMDS2, label = variable),
    colour = "black",
    vjust = -1,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  labs(shape = "ROV Drop", colour = "Sediment (%)") +
  theme_bw()



ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(shape = factor(Drop), colour = Sediment_mean), size = 4, stroke = 1.25) +
  scale_shape_manual(values = shape_map$shape) +
  scale_color_viridis(
    name = "Sediment (%)",
    option = "D",      # Try "D" or "plasma" for variation
    direction = -1     # Darker = higher sediment
  ) +
  theme_minimal() +
  labs(shape = "ROV Drop", colour = "Sediment (%)") + 
  # Overlay vector
  geom_segment(
    data = vector_scores,
    aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
    arrow = arrow(length = unit(0.3, "cm")),
    colour = "black",
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = vector_scores,
    aes(x = NMDS1, y = NMDS2, label = variable),
    colour = "black",
    vjust = -1,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9)
  )







###############################


# Import major groups

group_df <- read.csv("To_Major_Cats.csv")
group_df <- group_df %>%
  mutate(Species = make.names(Species))


library(dplyr)
library(tidyr)


# Pivot to long format
cover_long <- db_df_final %>%
  pivot_longer(
    cols = -(c(Image.ID, Image.name, Location, Drop, Depth, Latitude, Longitude, Site, Sediment_mean, Sediment_cat, Sediment_cat_3)), # adjust as needed
    names_to = "Species",
    values_to = "PercentCover"
  )

# Add Group column
cover_long_grouped <- cover_long %>%
  left_join(group_df, by = "Species")

# Summarise percent cover by group for each image

group_summary <- cover_long_grouped %>%
  group_by(Image.ID, Group) %>%
  summarise(TotalPercentCover = sum(PercentCover, na.rm = TRUE), .groups = "drop")

metadata <- db_df_final %>%
  select(Image.ID, Image.name, Location, Drop, Depth, Latitude, Longitude, Site, Sediment_mean, Sediment_cat) %>%  # add/remove columns as needed
  distinct()

# FINAL GROUPED FRAME LONG
group_summary_long <- group_summary %>%
  left_join(metadata, by = "Image.ID")

# FINAL GROUPED FRAME WIDE
group_summary_wide <- group_summary_long %>%
  pivot_wider(names_from = Group, values_from = TotalPercentCover, values_fill = 0)


######## 

# Bar chart average abundance by major groups

library(dplyr)

## Calculate mean percent cover per group
group_means <- group_summary_long %>%
  group_by(Group) %>%
  summarise(MeanPercentCover = mean(TotalPercentCover, na.rm = TRUE))

## SE calculation
group_means <- group_summary_long %>%
  group_by(Group) %>%
  summarise(
    MeanPercentCover = mean(TotalPercentCover, na.rm = TRUE),
    SE = sd(TotalPercentCover, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )


## Bar plot overall abundance major groups
library(ggplot2)

ggplot(group_means, aes(x = reorder(Group, -MeanPercentCover), y = MeanPercentCover, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanPercentCover - SE, ymax = MeanPercentCover + SE), width = 0.2) +
  labs(x = NULL, y = "Abundance (% cover)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#  Calculate mean percent cover per group per Drop
group_means_by_drop <- group_summary_long %>%
  group_by(Drop, Group) %>%
  summarise(MeanPercentCover = mean(TotalPercentCover, na.rm = TRUE), .groups = "drop")

library(tidytext)

group_means_by_drop <- group_means_by_drop %>%
  mutate(Drop_ordered = reorder_within(Drop, MeanPercentCover, Group))

group_means_by_drop <- group_summary_long %>%
  group_by(Drop, Group) %>%
  summarise(
    MeanPercentCover = mean(TotalPercentCover, na.rm = TRUE),
    SE = sd(TotalPercentCover, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )



ggplot(group_means_by_drop, aes(x = reorder(Group, -MeanPercentCover), y = MeanPercentCover, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = MeanPercentCover - SE, ymax = MeanPercentCover + SE), width = 0.2) +
  facet_wrap(~ Drop, nrow = 4, ncol= 5) +  # One wide row of panes, one per Drop
  labs(x = NULL, y = "Abundance (% cover)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "bottom", legend.direction = "horizontal")



# NMDS

groups_major <- group_summary_wide[, 11:ncol(group_summary_wide)]

## NMDS Ordination 
library(vegan)
nmds <- metaMDS(groups_major, distance = "bray")


## Extract scores and add metadata
nmds_scores <- as.data.frame(scores(nmds)$sites)
nmds_scores$Drop <- group_summary_wide$Drop
nmds_scores$Sediment_cat <- group_summary_wide$Sediment_cat


# Example: 17 drops, alternating solid and hollow shapes
solid_shapes  <- c(16, 17, 15, 18, 8, 7, 9, 11, 13)
hollow_shapes <- c(1, 2, 0, 5, 4, 3, 10, 12, 14)
shape_values  <- c(rbind(solid_shapes, hollow_shapes))[1:17]  # Interleave, then truncate to 17

# Assign each drop a shape
drops <- sort(unique(nmds_scores$Drop))
shape_map <- data.frame(
  Drop = drops,
  shape = shape_values
)

nmds_scores <- dplyr::left_join(nmds_scores, shape_map, by = "Drop")

# Sediment vector

library(vegan)

# Fit Sediment as a continuous environmental vector
fit <- envfit(nmds, group_summary_wide$Sediment, permutations = 999)

# Extract vector scores and correlation (r)
vector_scores <- as.data.frame(scores(fit, display = "vectors"))
vector_scores$variable <- rownames(vector_scores)
vector_scores$r <- fit$vectors$r




library(ggplot2)

ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2,
                        shape = factor(Drop),
                        colour = Sediment_cat)) +
  geom_point(size = 4, stroke = 1.5, show.legend = TRUE) +
  scale_shape_manual(values = shape_map$shape) +
  scale_colour_manual(values = c(
    "0–10%" = "#1b9e77", 
    "10–25%" = "#d95f02",
    "25–50%" = "#7570b3",
    "50–75%" = "#e7298a",
    "75–100%" = "#66a61e"
  )) +
  # Overlay vector if present
  geom_segment(
    data = vector_scores,
    aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
    arrow = arrow(length = unit(0.3, "cm")),
    colour = "black",
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = vector_scores,
    aes(x = NMDS1, y = NMDS2, label = variable),
    colour = "black",
    vjust = -1,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  labs(shape = "ROV Drop", colour = "Sediment (%)") +
  theme_bw()



