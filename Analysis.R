library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(readxl)
library(easystats)
library(lme4)
library(patchwork)
library(lmerTest)
library(stringr)
library(emmeans)

setwd("C:/Users/maxim/Desktop/Master thesis")

####################################################
#################### Recognition ###################
####################################################

RecognitionTest <- read_excel("RecognitionTest.xlsx")
General_IPT_Theme_VRSQ <- read_excel("General_IPT_Theme_VRSQ.xlsx")

# Define real and fake artwork/layout columns
real_artworks <- paste("Artwork", 1:20)
fake_artworks <- paste("Fake Artwork", 1:10)

real_layouts <- paste("Layout", 1:7)
fake_layouts <- paste("Fake Layout", 1:5)

# Compute hits and correct rejections
RecognitionTest$Hits_Artworks <- rowSums(RecognitionTest[real_artworks] == "Yes", na.rm = TRUE)
RecognitionTest$CRs_Artworks  <- rowSums(RecognitionTest[fake_artworks] == "No", na.rm = TRUE)

RecognitionTest$Hits_Layouts <- rowSums(RecognitionTest[real_layouts] == "Yes", na.rm = TRUE)
RecognitionTest$CRs_Layouts  <- rowSums(RecognitionTest[fake_layouts] == "No", na.rm = TRUE)

# Compute memory performance
RecognitionTest$RecognitionMP_Artworks <- (RecognitionTest$Hits_Artworks + RecognitionTest$CRs_Artworks) / (length(real_artworks) + length(fake_artworks))
RecognitionTest$RecognitionMP_Layouts  <- (RecognitionTest$Hits_Layouts + RecognitionTest$CRs_Layouts) / (length(real_layouts) + length(fake_layouts))

# Output relevant columns
memory_results <- RecognitionTest[, c("Participant", 
                 "Hits_Artworks", "CRs_Artworks", "RecognitionMP_Artworks",
                 "Hits_Layouts",  "CRs_Layouts",  "RecognitionMP_Layouts")]

gender_by_layout <- General_IPT_Theme_VRSQ %>%
  group_by(Layout, Gender) %>%
  summarise(Count = n(), .groups = "drop")

print(gender_by_layout)

# Merge with memory results by 'Participant' column
merged_data <- merge(General_IPT_Theme_VRSQ, memory_results, by = "Participant")

summary_by_layout <- merged_data %>%
  group_by(Layout) %>%
  summarise(
    N = n(),
    Mean_MP_Artworks = mean(RecognitionMP_Artworks, na.rm = TRUE),
    SD_MP_Artworks = sd(RecognitionMP_Artworks, na.rm = TRUE),
    Mean_MP_Layouts = mean(RecognitionMP_Layouts, na.rm = TRUE),
    SD_MP_Layouts = sd(RecognitionMP_Layouts, na.rm = TRUE),
    Median_Layouts = median(RecognitionMP_Layouts, na.rm = TRUE)
  )

print(summary_by_layout)

artwork_data <- summary_by_layout %>%
  transmute(Layout, Type = "Artworks", Mean = Mean_MP_Artworks, SD = SD_MP_Artworks)

layout_data <- summary_by_layout %>%
  transmute(Layout, Type = "Layouts", Mean = Mean_MP_Layouts, SD = SD_MP_Layouts)

plot_data <- bind_rows(artwork_data, layout_data)

names(General_IPT_Theme_VRSQ)[names(General_IPT_Theme_VRSQ) == "Score"] <- "IPT"
big_data <- merge(General_IPT_Theme_VRSQ, RecognitionTest, by="Participant")


####################################################
#################### Engagement ####################
####################################################

engagement <- read_excel("engagement_summary_detailed.xlsx")
names(engagement)[names(engagement) == "participant"] <- "Participant"

big_data <- merge(big_data, engagement, by="Participant")
big_data <- big_data %>% select(-c(`Email Address`, Fatigue, `General discomfort`,
                                   Eyestrain, `Difficulty focussing`, Headache,
                                   `Fullness of head`, `Blurred vision`, Dizzy,
                                   Vertigo, Theme, Time, Occupation, Age, `Artwork 1`,
                                   `Artwork 2`, `Artwork 3`, `Artwork 4`))

big_data %>%
  group_by(Layout) %>%
  summarise(
    mean = mean(RecognitionMP_Artworks, na.rm = TRUE),
    sd = sd(RecognitionMP_Artworks, na.rm = TRUE),
    min = min(RecognitionMP_Artworks, na.rm = TRUE),
    max = max(RecognitionMP_Artworks, na.rm = TRUE)
  )
big_data %>%
  group_by(Layout) %>%
  summarise(
    mean = mean(RecognitionMP_Layouts, na.rm = TRUE),
    sd = sd(RecognitionMP_Layouts, na.rm = TRUE),
    min = min(RecognitionMP_Layouts, na.rm = TRUE),
    max = max(RecognitionMP_Layouts, na.rm = TRUE)
  )

plot_data <- big_data %>%
  select(Layout, RecognitionMP_Artworks, RecognitionMP_Layouts) %>%
  pivot_longer(cols = c(RecognitionMP_Artworks, RecognitionMP_Layouts),
               names_to = "TestType",
               values_to = "Score")
plot_data$TestType <- factor(plot_data$TestType,
                             levels = c("RecognitionMP_Artworks", "RecognitionMP_Layouts"),
                             labels = c("Artwork", "Layout"))


####################################################
####################### Order ######################
####################################################

order <- read_excel("OrderTest.xlsx")
colnames(order) <- c("Participant", "layout", "Layout_1", "Layout_2", "Layout_3", "Layout_4", "Layout_5", "Layout_6", "Layout_7")

order <- order %>%
  mutate(
    Participant = as.integer(Participant),
    Layout_1 = as.character(Layout_1),
    Layout_2 = as.character(Layout_2),
    Layout_3 = as.character(Layout_3),
    Layout_4 = as.character(Layout_4),
    Layout_5 = as.character(Layout_5),
    Layout_6 = as.character(Layout_6),
  )

order_resting <- order %>% filter(layout == "Resting areas") %>%
  select(Participant, layout, Layout_1, Layout_2, Layout_3, Layout_4, Layout_5, Layout_6, Layout_7)

order_easiest <- order %>% filter(layout == "Easiest to Hardest") %>%
  select(Participant, layout, Layout_1, Layout_2, Layout_3, Layout_4, Layout_5, Layout_6, Layout_7)

# Funktion zum Zählen der vertauschten Paare (Inversionen)
count_inversions <- function(vec) {
  inv <- 0
  n <- length(vec)
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      if (vec[i] > vec[j]) inv <- inv + 1
    }
  }
  return(inv)
}

# Hauptfunktion zur Bewertung der Reihenfolge aus mehreren Spalten
memory_performance_df <- function(df, user_cols, korrekte_reihenfolge) {
  n <- nrow(df)
  
  max_positionsabweichung <- sum(abs(seq_along(korrekte_reihenfolge) - rev(seq_along(korrekte_reihenfolge))))
  max_vertauschte_paare <- choose(length(korrekte_reihenfolge), 2)
  
  scores <- numeric(n)
  
  for (i in seq_len(n)) {
    # Nutzer-Reihenfolge als Vektor aus den Spalten holen
    user <- as.numeric(df[i, user_cols])
    
    # Positionen der Nutzerantwort in der korrekten Reihenfolge finden
    zielpositionen <- match(user, korrekte_reihenfolge)
    
    # Positionsabweichung berechnen
    positionsabweichung <- abs(zielpositionen - seq_along(zielpositionen))
    
    # Vertauschte Paare zählen
    vertauschte_paare <- count_inversions(zielpositionen)
    
    # Score berechnen: 1 = perfekt, 0 = maximal falsch
    score <- 1 - ((sum(positionsabweichung) / max_positionsabweichung + vertauschte_paare / max_vertauschte_paare) / 2)
    scores[i] <- score
  }
  
  return(list(memory_performance = scores))
}

ord_eas <- order_easiest
ord_res <- order_resting

user_cols <- paste0("Layout_", 1:7)

# Korrekter Reihenfolge-Vektor
eas_korrekte_reihenfolge <- 1:7
res_korrekte_reihenfolge <- c(3,4,1,5,6,2,7)

eas_result <- memory_performance_df(ord_eas, user_cols, eas_korrekte_reihenfolge)
res_result <- memory_performance_df(ord_res, user_cols, res_korrekte_reihenfolge)

order_mp_easiest <- eas_result$memory_performance
order_mp_resting <- res_result$memory_performance

# Extract participants and layout info for easiest and resting separately
participants_eas <- ord_eas$Participant
layout_eas <- ord_eas$layout

participants_res <- ord_res$Participant
layout_res <- ord_res$layout

# Create data frames combining participant, layout, and score
df_eas <- data.frame(
  Participant = participants_eas,
  Layout = layout_eas,
  MemoryPerformance = order_mp_easiest
)

df_res <- data.frame(
  Participant = participants_res,
  Layout = layout_res,
  MemoryPerformance = order_mp_resting
)

# Combine both layouts into one data frame
order_df <- bind_rows(df_eas, df_res)

summary_stats <- order_df %>%
  group_by(Layout) %>%
  summarise(
    mean = mean(MemoryPerformance, na.rm = TRUE),
    sd = sd(MemoryPerformance, na.rm = TRUE),
    min = min(MemoryPerformance, na.rm = TRUE),
    max = max(MemoryPerformance, na.rm = TRUE),
    n = n()
  )

print(summary_stats)

order_df$Layout <- factor(order_df$Layout,
                               levels = c("Easiest to Hardest", "Resting areas"))  # switched order                             levels = c("Resting areas", "Easiest to Hardest"))

order_df <- order_df %>% select(-c(Layout))
big_data <- merge(big_data, order_df, by="Participant")
names(big_data)[names(big_data) == "MemoryPerformance"] <- "OrderMP"
big_data <- big_data %>% select(-c(`Artwork 5`, `Artwork 6`, `Artwork 7`, `Artwork 8`,
                                   `Artwork 9`, `Artwork 10`, `Artwork 11`, `Artwork 12`,
                                   `Artwork 13`, `Artwork 14`, `Artwork 15`, `Artwork 16`))
big_data <- big_data %>% select(-c(`Artwork 17`, `Artwork 18`, `Artwork 19`, `Artwork 20`,
                                   `Fake Artwork 1`, `Fake Artwork 2`, `Fake Artwork 3`,
                                   `Fake Artwork 4`, `Fake Artwork 5`, `Fake Artwork 6`,
                                   `Fake Artwork 7`, `Fake Artwork 8`, `Fake Artwork 9`,
                                   `Fake Artwork 10`, `Layout 1`, `Layout 2`, `Layout 3`,
                                   `Layout 4`, `Layout 5`, `Layout 6`, `Layout 7`,
                                   `Fake Layout 1`))
big_data <- big_data %>% select(-c(`Fake Layout 2`, `Fake Layout 3`, `Fake Layout 4`,
                                   `Fake Layout 5`))



recognition_plot_data <- big_data %>%
  select(Participant, Layout, RecognitionMP_Artworks, RecognitionMP_Layouts) %>%
  pivot_longer(cols = c(RecognitionMP_Artworks, RecognitionMP_Layouts),
               names_to = "TestType",
               values_to = "Score") %>%
  mutate(
    TestType = factor(TestType,
                      levels = c("RecognitionMP_Artworks", "RecognitionMP_Layouts"),
                      labels = c("Artwork recognition", "Layout recognition"))
  )

order_plot_data <- big_data %>%
  select(Participant, Layout, OrderMP) %>%
  mutate(
    TestType = "Order",
    Score = OrderMP
  ) %>%
  select(Participant, Layout, TestType, Score)

####################################################
####################### Recall #####################
####################################################

recall <- read_excel("RecallTest.xlsx")

recall_small <- recall %>% select(-c(`Room A`, `Room B`, `Room C`, RoomC,
                                     `Room D...8`, `Room D...9`, `Room D...10`,
                                     `Room D...11`, `Room E...12`, `Room E...13`,
                                     `Room E...14`, `Room E...15`, `Room E...16`,
                                     `Room F...17`, `Room F...18`, `Room F...19`,
                                     `Room F...20`, `Room G...21`, `Room G...22`,
                                     `Room G...23`, Location))

recall_small_cleaned <- drop_na(recall_small)

recall_small_cleaned %>%
  group_by(Layout) %>%
  summarise(
    mean_HR60 = mean(HR60, na.rm = TRUE),
    sd_HR60 = sd(HR60, na.rm = TRUE),
    min_HR60 = min(HR60, na.rm = TRUE),
    max_HR60 = max(HR60, na.rm = TRUE)
    
  )

big_data <- left_join(big_data, recall_small_cleaned, by = c("Participant", "Layout"))

recall_plot_data <- big_data %>%
  select(Participant, Layout, HR60) %>%
  mutate(
    TestType = "Recall",
    Score = HR60
  ) %>%
  select(Participant, Layout, TestType, Score)

combined_plot_data <- bind_rows(
  recognition_plot_data,
  order_plot_data,
  recall_plot_data
)

combined_plot_data$TestType <- factor(combined_plot_data$TestType,
                                      levels = c("Artwork recognition", "Layout recognition", "Order", "Recall"))

minimal_big_data <- big_data %>%
  select(Participant, Layout, IPT, Overall, overall_engagement, RecognitionMP_Artworks,
         RecognitionMP_Layouts, OrderMP, HR60)

names(minimal_big_data)[names(minimal_big_data) == "Overall"] <- "VRSQ"
names(minimal_big_data)[names(minimal_big_data) == "HR60"] <- "RecallMP"

ggplot(combined_plot_data, aes(x = TestType, y = Score, fill = Layout)) +
  geom_boxplot(position = position_dodge(width = .8), width = 0.5) +
  scale_fill_manual(values = c("Resting areas" = "#ff7f0e",
                               "Easiest to hardest" = "#1f77b4")) +
  scale_color_manual(values = c("Resting areas" = "darkgray",
                                "Easiest to hardest" = "darkgray")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Test Type",
    y = "Score",
    fill = "Layout Type",
    color = "Layout Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "right",
    axis.title.x = element_blank()
  ) + theme(panel.spacing = unit(2, "lines"))

density_rec_art <- ggplot(minimal_big_data, aes(x = RecognitionMP_Artworks, fill = Layout)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(
    "Resting areas" = "#ff7f0e",
    "Easiest to hardest" = "#1f77b4"
  )) +
  labs(x = "Artwork Recognition Task (Score)", y = "Density", fill = "Layout") +
  theme_minimal() + theme(legend.position = "none")

density_rec_lay <- ggplot(minimal_big_data, aes(x = RecognitionMP_Layouts, fill = Layout)) +
  geom_density(alpha = 0.7, adjust = 3) +
  scale_fill_manual(values = c(
    "Resting areas" = "#ff7f0e",
    "Easiest to hardest" = "#1f77b4"
  )) +
  labs(x = "Layout Recognition Task (Score)", y = "Density", fill = "Layout") +
  theme_minimal() + theme(legend.position = "none")

density_order <- ggplot(minimal_big_data, aes(x = OrderMP, fill = Layout)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(
    "Resting areas" = "#ff7f0e",
    "Easiest to hardest" = "#1f77b4"
  )) +
  labs(x = "Order Task (Score)", y = "Density", fill = "Layout") +
  theme_minimal() + theme(legend.position = "none")

density_recall <- ggplot(minimal_big_data, aes(x = RecallMP, fill = Layout)) +
  geom_density(alpha = 0.7) +
  scale_fill_manual(values = c(
    "Resting areas" = "#ff7f0e",
    "Easiest to hardest" = "#1f77b4"
  )) +
  labs(x = "Recall Task (Score)", y = "Density", fill = "Layout") +
  theme_minimal()

p1 <- density_rec_art 
p2 <- density_rec_lay 
p3 <- density_order 
p4 <- density_recall 

# Combine plots and add legend from one plot (e.g., p1)
(p1 | p2) / (p3 | p4) + plot_layout(guides = "collect") & theme(legend.position = "right")


####################################################
####################### Models #####################
####################################################

#######################
# Recognition Artwork #
#######################

ra_model <- lm(RecognitionMP_Artworks ~ Layout, data = minimal_big_data)
summary(ra_model)

#######################
# Recognition Layouts #
#######################

rl_model <- lm(RecognitionMP_Layouts ~ Layout, data = minimal_big_data)
summary(rl_model)

#######################
######## Order ########
#######################

o_model <- lm(OrderMP ~ Layout, data = minimal_big_data)
summary(o_model)

#######################
######## Recall #######
#######################

r_model <- lm(RecallMP ~ Layout, data = minimal_big_data)
summary(r_model)

e_r_model <- lm(RecallMP ~ overall_engagement, data = minimal_big_data)
summary(e_r_model)

###########################
######## engagement #######
###########################

e_model <- lm(overall_engagement ~ Layout, data = minimal_big_data)
summary(e_model)

summary(lm(RoomA_engagement ~ Layout, data = big_data))
summary(lm(RoomB_engagement ~ Layout, data = big_data))
summary(lm(RoomC_engagement ~ Layout, data = big_data))
summary(lm(RoomD_engagement ~ Layout, data = big_data))
summary(lm(RoomE_engagement ~ Layout, data = big_data))
summary(lm(RoomF_engagement ~ Layout, data = big_data))
summary(lm(RoomG_engagement ~ Layout, data = big_data))


############################
# Plot Engagement per Room #
############################

# Reshape only the Room*_engagement columns
long_data <- big_data %>%
  pivot_longer(
    cols = matches("^Room.*_engagement$"),
    names_to = "Room",
    values_to = "Engagement"
  )

long_data <- big_data %>%
  pivot_longer(
    cols = matches("^Room.*_engagement$"),
    names_to = "Room",
    values_to = "Engagement"
  ) %>%
  mutate(Room = paste("Room", str_extract(Room, "(?<=Room)[A-Z]")))

long_data <- long_data %>%
  mutate(Room = case_when(
    Room == "Room B" ~ "Room B*",
    Room == "Room C" ~ "Room C*",
    TRUE ~ Room
  ))

# Define your color mapping (adjust layout names to match your data exactly)
layout_colors <- c(
  "Easiest to hardest" = "#1f77b4",
  "Resting areas" = "#ff7f0e" 
)

ggplot(long_data, aes(x = Layout, y = Engagement, fill = Layout)) +
  geom_boxplot() +
  facet_wrap(~ Room, nrow = 1, scales = "fixed") +  # fixed y-scale
  labs(
    y = "Engagement"
  ) +
  scale_fill_manual(values = layout_colors) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    axis.text.y = element_text(),        # Show y-axis text by default
    axis.title.y = element_text(),       # Show y-axis title on leftmost plot
    axis.text.y.right = element_blank(), # Remove right y-axis text if present
    axis.ticks.y = element_line(),       # Show ticks
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )

#####################################
# Plot Mean Engagement per Position #
#####################################

# Define room order per layout
room_order_list <- list(
  "Easiest to hardest" = c("Room A", "Room B*", "Room C*", "Room D", "Room E", "Room F", "Room G"),
  "Resting areas"      = c("Room C*", "Room D", "Room A", "Room E", "Room F", "Room B*", "Room G")
)

# Build a data frame with position info
position_data <- bind_rows(lapply(names(room_order_list), function(layout) {
  data.frame(
    Layout = layout,
    Room = factor(room_order_list[[layout]], levels = room_order_list[[layout]]),
    Position = 1:7
  )
}))

# Compute medians and join with position info
medians <- long_data %>%
  group_by(Room, Layout) %>%
  summarise(median_engagement = median(Engagement, na.rm = TRUE), .groups = "drop") %>%
  inner_join(position_data, by = c("Room", "Layout"))

# Define layout colors
layout_colors <- c(
  "Easiest to hardest" = "#1f77b4",
  "Resting areas" = "#ff7f0e"
)

# Plot
ggplot(medians, aes(x = Position, y = median_engagement, group = Layout, color = Layout)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = layout_colors) +
  scale_x_continuous(breaks = 1:7, labels = paste("Position", 1:7)) +
  labs(
    x = NULL,
    y = "Median Engagement"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

#################################
# Plot Mean Engagement per Room #
#################################

mean_data <- long_data %>%
  group_by(Room, Layout) %>%
  summarise(mean_engagement = mean(Engagement, na.rm = TRUE), .groups = "drop")

rooms_sorted <- sort(unique(mean_data$Room))

# Erzeuge numerische Positionen von 0 bis 1, gleichmäßig verteilt
room_positions <- data.frame(
  Room = rooms_sorted,
  x = seq(0, 1, length.out = length(rooms_sorted))
)

# Join numerische Positionen zu mean_data
mean_data_num <- mean_data %>%
  left_join(room_positions, by = "Room")

# Plot mit numerischer x-Achse von 0 bis 1
ggplot(mean_data_num, aes(x = x, y = mean_engagement, group = Layout, color = Layout)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = layout_colors) +
  scale_x_continuous(limits = c(0, 1), breaks = room_positions$x, labels = room_positions$Room) +
  labs(
    x = "Room",
    y = "Mean Engagement",
    color = "Layout"
  ) +
  theme_minimal(base_size = 14)


#########################################################################
# Calculation for Easiest to hardest individual room recall performance #
#########################################################################

# Define chance probabilities (match to the room)
P_loc <- 1 / 20
P_room <- 4 / 20
P_neighbor <- 8 / 20

# Score weights
W_loc <- 3
W_room <- 2
W_neighbor <- 1

# Expected score by chance
E_chance <- W_loc * P_loc + W_room * P_room + W_neighbor * P_neighbor

# Observed scores for artworks (replace with data)
scores_E <- c(1, 1, 1, 1)

# Observed average
O_actual <- mean(scores_E)

# Corrected memory performance (normalized to max possible = 3)
Performance_corrected <- (O_actual - E_chance) / (3 - E_chance)

cat("Observed score:", O_actual, "\n")
cat("Chance score:", E_chance, "\n")
cat("Corrected memory performance:", Performance_corrected, "\n")


####################################################################
# Calculation for Resting areas individual room recall performance #
####################################################################

# Define chance probabilities (match to the room)
P_loc <- 1 / 20
P_room <- 4 / 20
P_neighbor <- 6 / 20

# Score weights
W_loc <- 3
W_room <- 2
W_neighbor <- 1

# Expected score by chance
E_chance <- W_loc * P_loc + W_room * P_room + W_neighbor * P_neighbor

# Observed scores for artworks (replace with data)
scores_E <- c(3, 0, 3, 3)

# Observed average
O_actual <- mean(scores_E)

# Corrected memory performance (normalized to max possible = 3)
Performance_corrected <- (O_actual - E_chance) / (3 - E_chance)

cat("Observed score:", O_actual, "\n")
cat("Chance score:", E_chance, "\n")
cat("Corrected memory performance:", Performance_corrected, "\n")



#######################################
# Individual Recall Performance Model #
#######################################

memory_long <- read_excel("memory_performance_long_format.xlsx")
memory_long$position[memory_long$position == 1] <- "1"
memory_long$position[memory_long$position == 2] <- "2"
memory_long$position[memory_long$position == 3] <- "3"
memory_long$position[memory_long$position == 4] <- "4"
memory_long$position[memory_long$position == 5] <- "5"
memory_long$position[memory_long$position == 6] <- "6"
memory_long$position[memory_long$position == 7] <- "7"

r_model_interac <- lmer(memory_score ~ position * room + (1 | participant_id), data = memory_long)
summary(r_model_interac)

interaction_means_interact <- emmeans(r_model_interac, ~ room * position)
contrast_within_rooms <- contrast(interaction_means_interact, interaction = "pairwise", by = "room")
contrast_within_rooms_clean <- na.omit(as.data.frame(contrast_within_rooms))
summary(contrast_within_rooms_clean, infer = TRUE)


r_model_pos <- lmer(memory_score ~ position + (1 | participant_id), data = memory_long)
summary(r_model_pos)

interaction_means_pos <- emmeans(r_model_pos, ~ position)
interaction_means_pos_df <- as.data.frame(interaction_means_pos)

ggplot(interaction_means_pos_df, aes(x = as.numeric(gsub("position", "", position)), y = emmean)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, color = "darkgreen") +
  scale_x_continuous(breaks = 1:7, labels = paste0("Position ", 1:7)) +
  ylab("Estimated Means") +
  xlab(NULL) +
  theme_minimal()




roomA_data <- subset(memory_long, room == "A")
summary(lm(memory_score ~ position, data = roomA_data))

roomB_data <- subset(memory_long, room == "B")
summary(lm(memory_score ~ position, data = roomB_data))

roomC_data <- subset(memory_long, room == "C")
summary(lm(memory_score ~ position, data = roomC_data))

roomD_data <- subset(memory_long, room == "D")
summary(lm(memory_score ~ position, data = roomD_data))

roomE_data <- subset(memory_long, room == "E")
summary(lm(memory_score ~ position, data = roomE_data))

roomF_data <- subset(memory_long, room == "F")
summary(lm(memory_score ~ position, data = roomF_data))


##########################
# Model correlation test #
##########################

int_model_art1 <- lm(RecognitionMP_Artworks ~ RecognitionMP_Layouts + OrderMP + RecallMP, data = minimal_big_data)
int_model_art2 <- lm(RecognitionMP_Artworks ~ RecognitionMP_Layouts * OrderMP * RecallMP, data = minimal_big_data)
int_model_art3 <- lm(RecognitionMP_Artworks ~ RecognitionMP_Layouts * OrderMP + RecallMP, data = minimal_big_data)
int_model_art4 <- lm(RecognitionMP_Artworks ~ RecognitionMP_Layouts + OrderMP * RecallMP, data = minimal_big_data)
compare_performance(int_model_art1, int_model_art2, int_model_art3, int_model_art4, rank=TRUE)
summary(int_model_art2)

int_model_lay1 <- lm(RecognitionMP_Layouts ~ RecognitionMP_Artworks + OrderMP + RecallMP, data = minimal_big_data)
int_model_lay2 <- lm(RecognitionMP_Layouts ~ RecognitionMP_Artworks * OrderMP * RecallMP, data = minimal_big_data)
int_model_lay3 <- lm(RecognitionMP_Layouts ~ RecognitionMP_Artworks * OrderMP + RecallMP, data = minimal_big_data)
int_model_lay4 <- lm(RecognitionMP_Layouts ~ RecognitionMP_Artworks + OrderMP * RecallMP, data = minimal_big_data)
compare_performance(int_model_lay1, int_model_lay2, int_model_lay3, int_model_lay4, rank=TRUE)
summary(int_model_lay1)

int_model_order1 <- lm(OrderMP ~ RecognitionMP_Artworks + RecognitionMP_Layouts + RecallMP, data = minimal_big_data)
int_model_order2 <- lm(OrderMP ~ RecognitionMP_Artworks * RecognitionMP_Layouts * RecallMP, data = minimal_big_data)
int_model_order3 <- lm(OrderMP ~ RecognitionMP_Artworks * RecognitionMP_Layouts + RecallMP, data = minimal_big_data)
int_model_order4 <- lm(OrderMP ~ RecognitionMP_Artworks + RecognitionMP_Layouts * RecallMP, data = minimal_big_data)
compare_performance(int_model_order1, int_model_order2, int_model_order3, int_model_order4, rank=TRUE)
summary(int_model_order2)

int_model_recall1 <- lm(RecallMP ~ RecognitionMP_Artworks + RecognitionMP_Layouts + OrderMP, data = minimal_big_data)
int_model_recall2 <- lm(RecallMP ~ RecognitionMP_Artworks * RecognitionMP_Layouts * OrderMP, data = minimal_big_data)
int_model_recall3 <- lm(RecallMP ~ RecognitionMP_Artworks * RecognitionMP_Layouts + OrderMP, data = minimal_big_data)
int_model_recall4 <- lm(RecallMP ~ RecognitionMP_Artworks + RecognitionMP_Layouts * OrderMP, data = minimal_big_data)
compare_performance(int_model_recall1, int_model_recall2, int_model_recall3, int_model_recall4, rank=TRUE)
summary(int_model_recall1)

