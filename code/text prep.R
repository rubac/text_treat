library(jsonlite)
library(tidyverse)
library(readr)
library(tidytext)
library(texteffect)

# ------------------------------
# Load text treatments
# ------------------------------
df <- fromJSON("data/text treatments/parenting_study_posts_streamed_batched_GPT41.json", flatten = TRUE)
df$post_id <- 1:nrow(df)
df$post_text <- gsub("\u2014", ", ", df$post_text)

# ------------------------------
# Survey round 2
# ------------------------------
df_survey2 <- read_csv(
  "data/survey 2/perspective+taking+2_September+15,+2025_01.53.csv", 
  col_names = TRUE, 
  col_types = cols(.default = "c")
)

df_survey2 <- df_survey2[-c(1, 2), ]
df_survey2$post_text <- df_survey2$`__js_post_text`
df_survey2$`__js_post_text` <- NULL

df_survey2 <- merge(df_survey2, df[, c("post_text", "condition", "tone", "post_id")], 
                    by = "post_text", all.x = TRUE)

# ------------------------------
# Survey round 1
# ------------------------------
df_survey1 <- read_csv(
  "data/survey 1/qualtrics/perspective+taking_August+13,+2025_06.13.csv", 
  col_names = TRUE, 
  col_types = cols(.default = "c")
)

df_survey1 <- df_survey1[-c(1, 2), ]
df_survey1$post_text <- df_survey1$`__js_post_text`
df_survey1$`__js_post_text` <- NULL

df_survey1 <- merge(df_survey1, df[, c("post_text", "condition", "tone", "post_id")], 
                    by = "post_text", all.x = TRUE)

# ------------------------------
# Sample from survey 1
# ------------------------------
set.seed(1)
df_survey1sample <- df_survey1 %>%
  group_by(post_id) %>%
  slice_sample(n = 5) %>% 
  filter(!is.na(post_id))

# ------------------------------
# Combine survey rounds
# ------------------------------
df_survey <- rbind(df_survey1sample, df_survey2)
rm(df_survey1, df_survey1sample, df_survey2)

# ------------------------------
# Construct indices
# ------------------------------
df_survey <- df_survey %>%
  mutate(Q297_n = case_when(
    Q297 == "Fair to both" ~ 0,
    Q297 == "Somewhat unfair to me" ~ -1,
    Q297 == "Very unfair to me" ~ -2,
    Q297 == "Somewhat unfair to my partner" ~ 1,
    Q297 == "Very unfair to my partner" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ),
  Q299_n = case_when(
    Q299 == "Fair to both" ~ 0,
    Q299 == "Somewhat unfair to me" ~ -1,
    Q299 == "Very unfair to me" ~ -2,
    Q299 == "Somewhat unfair to my partner" ~ 1,
    Q299 == "Very unfair to my partner" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ),
  Q300_n = case_when(
    Q300 == "Fair to both" ~ 0,
    Q300 == "Somewhat unfair to me" ~ -1,
    Q300 == "Very unfair to me" ~ -2,
    Q300 == "Somewhat unfair to my partner" ~ 1,
    Q300 == "Very unfair to my partner" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ),
  Q301_n = case_when(
    Q301 == "Fair to both" ~ 0,
    Q301 == "Somewhat unfair to me" ~ -1,
    Q301 == "Very unfair to me" ~ -2,
    Q301 == "Somewhat unfair to my partner" ~ 1,
    Q301 == "Very unfair to my partner" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ))

df_survey$perceived_fairness <- rowMeans(df_survey[, c("Q297_n","Q299_n","Q300_n","Q301_n")], na.rm = TRUE)


df_survey <- df_survey %>%
  mutate(pers_mot_HH = case_when(
    `pers mot HH` == "Neither agree nor disagree" ~ 0,
    `pers mot HH` == "Somewhat aree" ~ -1,
    `pers mot HH` == "Strongly agree" ~ -2,
    `pers mot HH` == "Somewhat disagree" ~ 1,
    `pers mot HH` == "Strongly disagree" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ),
  pers_mot_CC = case_when(
    `pers mot CC` == "Neither agree nor disagree" ~ 0,
    `pers mot CC` == "Somewhat agree" ~ -1,
    `pers mot CC` == "Strongly agree" ~ -2,
    `pers mot CC` == "Somewhat disagree" ~ 1,
    `pers mot CC` == "Strongly disagree" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ),
  pers_mot_HH_ment = case_when(
    `pers mot HH ment` == "Neither agree nor disagree" ~ 0,
    `pers mot HH ment` == "Somewhat agree" ~ -1,
    `pers mot HH ment` == "Strongly agree" ~ -2,
    `pers mot HH ment` == "Somewhat disagree" ~ 1,
    `pers mot HH ment` == "Strongly disagree" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ),
  pers_mot_CC_ment = case_when(
    `pers mot CC ment` == "Neither agree nor disagree" ~ 0,
    `pers mot CC ment` == "Somewhat agree" ~ -1,
    `pers mot CC ment` == "Strongly agree" ~ -2,
    `pers mot CC ment` == "Somewhat disagree" ~ 1,
    `pers mot CC ment` == "Strongly disagree" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ))

df_survey$pers_beliefs <- rowMeans(df_survey[, c("pers_mot_HH","pers_mot_CC","pers_mot_HH_ment","pers_mot_CC_ment")], na.rm = TRUE)

df_survey <- df_survey %>%
  mutate(expect_mothers_HH = case_when(
    `expect mothers HH` == "Neither agree nor disagree" ~ 0,
    `expect mothers HH` == "Somewhat aree" ~ -1,
    `expect mothers HH` == "Strongly agree" ~ -2,
    `expect mothers HH` == "Somewhat disagree" ~ 1,
    `expect mothers HH` == "Strongly disagree" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ),
  expect_mothers_CC = case_when(
    `Expect mothers CC` == "Neither agree nor disagree" ~ 0,
    `Expect mothers CC` == "Somewhat agree" ~ -1,
    `Expect mothers CC` == "Strongly agree" ~ -2,
    `Expect mothers CC` == "Somewhat disagree" ~ 1,
    `Expect mothers CC` == "Strongly disagree" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ),
  expect_mot_ment_HH = case_when(
    `Expect mot HH ment` == "Neither agree nor disagree" ~ 0,
    `Expect mot HH ment` == "Somewhat agree" ~ -1,
    `Expect mot HH ment` == "Strongly agree" ~ -2,
    `Expect mot HH ment` == "Somewhat disagree" ~ 1,
    `Expect mot HH ment` == "Strongly disagree" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ),
  expect_mot_ment_CC = case_when(
    `Expect mot CC ment` == "Neither agree nor disagree" ~ 0,
    `Expect mot CC ment` == "Somewhat agree" ~ -1,
    `Expect mot CC ment` == "Strongly agree" ~ -2,
    `Expect mot CC ment` == "Somewhat disagree" ~ 1,
    `Expect mot CC ment` == "Strongly disagree" ~ 2,
    TRUE ~ NA_real_  # In case of missing or unexpected values
  ))

df_survey$expect_moth <- rowMeans(df_survey[, c("expect_mothers_HH","expect_mothers_CC","expect_mot_ment_HH","expect_mot_ment_CC")], na.rm = TRUE)

for_pub_df <- df_survey %>%
  dplyr::select(-c(StartDate, EndDate, Status, Progress, `Duration (in seconds)`,
                   Finished, RecordedDate, ResponseId, DistributionChannel,
                   UserLanguage, Intro_text, Q32, PROLIFIC_PID))
write_csv(for_pub_df, "data/survey.csv")
# ------------------------------
# Simple regressions
# ------------------------------
summary(lm(pers_beliefs ~ tone, data = df_survey))
summary(lm(perceived_fairness ~ tone, data = df_survey))
summary(lm(expect_moth ~ tone, data = df_survey))

# ------------------------------
# Prep for sIBP
# ------------------------------
data("stop_words")

df_survey <- df_survey %>% filter(Finished == "True")

# Tokenize and build bag-of-words
bow_df <- df_survey %>%
  unnest_tokens(word, post_text, token = "words", drop = FALSE) %>% 
  anti_join(stop_words, by = "word") %>%  
  count(post_id, word) %>%            
  pivot_wider(
    names_from = word,
    values_from = n,
    values_fill = 0
  )

df_selected <- df_survey %>%
  dplyr::select(post_id, pers_beliefs, perceived_fairness, expect_moth)

# Merge
merged_df <- left_join(df_selected, bow_df, by = "post_id") %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  filter(rowSums(across(everything())) > 0)

# ------------------------------
# Sparsity filtering
# ------------------------------
exclude_cols <- c("post_id", "pers_beliefs", "perceived_fairness", "expect_moth")
threshold <- 7

nonzero_counts <- colSums(merged_df[ , !(names(merged_df) %in% exclude_cols)] != 0)
keep_predictors <- names(nonzero_counts[nonzero_counts >= threshold])

# Keep excluded cols + surviving predictors
merged_df <- merged_df[ , c(exclude_cols, keep_predictors)]

# ------------------------------
# Split into outcomes and predictors
# ------------------------------
Y1 <- merged_df$pers_beliefs
Y2 <- merged_df$perceived_fairness
Y3 <- merged_df$expect_moth
X  <- merged_df %>% dplyr::select(-all_of(exclude_cols))


set.seed(123)  # for reproducibility
train.ind <- sample(1:nrow(X), size = 0.5*nrow(X), replace = FALSE)

### Outcome Y1: pers_beliefs 
# Search sIBP for several parameter configurations; fit each to the training set
sibp.search <- sibp_param_search(X, Y1, K = 8, alphas = c(1,2,3,4,5), sigmasq.ns = c(0.8, 1, 1.2), 
                                 iters = 5, train.ind = train.ind)
sibp_rank_runs(sibp.search, X, 5)
sibp_top_words(sibp.search[["5"]][["1"]][[1]], colnames(X), 8, verbose = TRUE)

## once we have decided on a model, we evaluate it on the test set
sibp.fit <- sibp.search[["5"]][["1"]][[1]]
amce_Y1<-sibp_amce(sibp.fit, X, Y1)
sibp_amce_plot(amce_Y1)

###Latent treatment interpretations
# -----------------------------

# Topic 1
# In-text: Posts in this theme revolve around children’s milestones like bike riding 
# and parents’ emotional investment in cheering them on. The focus is on everyday 
# learning moments and the pride or frustration that comes with them.
# Table label: Milestones & learning

# Topic 2
# In-text: This theme highlights special occasions, planning details, and the stress 
# of organizing birthdays or celebrations, often with emotional undertones. The emphasis 
# is on logistics and the invisible work behind “making it nice.”
# Table label: Event planning & birthdays

# Topic 3
# In-text: Posts here capture the rhythm of daily life—weekday afternoons, routines, 
# and the contagious moods of children. Parents reflect on how energy levels shape 
# household dynamics.
# Table label: Daily routines & moods

# Topic 4
# In-text: This theme centers on managing meals, food preferences, allergies, and 
# the constant planning involved in feeding children. It highlights the unseen labor 
# of anticipating needs and balancing choices.
# Table label: Meal planning & food work

# Topic 5
# In-text: Posts in this cluster describe creative play, crafts, and imaginative 
# projects. The emotional tone ranges from joy in shared play to the stress of managing 
# mess and expectations.
# Table label: Play & creativity

# Topic 6
# In-text: This topic focuses on the logistics of keeping households running—checklists, 
# inventories, formula, fridge stock, and ongoing mental load. It reflects the cognitive 
# labor mothers take on invisibly.
# Table label: Household management & mental load

# Topic 7
# In-text: This cluster emphasizes identity and meaning in motherhood, including 
# clashing emotions, moments of joy, and reflections on the role itself. Posts highlight 
# both self-definition and relational dynamics.
# Table label: Motherhood identity & emotions

# Topic 8
# In-text: This theme is about childcare arrangements—nannies, backup care, waitlists, 
# and career implications when coverage falls through. Posts highlight the stress of 
# negotiating external support systems.
# Table label: Childcare logistics & career

# Define AMCE labels
amce_Y1_labels <- c(
  "intercept",
  "Milestones & learning",
  "Event planning & birthdays",
  "Daily routines & moods",
  "Meal planning & food work",
  "Play & creativity",
  "Household management & mental load",
  "Motherhood identity & emotions",
  "Childcare logistics & career"
)

amce_Y1$latent_ts <- amce_Y1_labels
amce_Y1_plot <- amce_Y1 %>% filter(latent_ts != "intercept")

# Plot Y1
amce_Y1_plottt <- ggplot(amce_Y1_plot, aes(x = reorder(latent_ts, effect), y = effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", size = 0.4) +
  geom_pointrange(aes(ymin = L, ymax = U), 
                  size = 0.6, fatten = 2, color = "black") +
  coord_flip() +
  labs(
    x = "Latent Text Treatments",
    y = "Average Marginal Component Effects",
    title = NULL
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(face = "bold", angle = 90),
    axis.title.x = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", size = 0.3)
  )



### Outcome Y2: perceived_fairness 
# Search sIBP for several parameter configurations; fit each to the training set
sibp.search <- sibp_param_search(X, Y2, K = 8, alphas = c(1,2,3,4,5), sigmasq.ns = c(0.8, 1, 1.2), 
                                 iters = 5, train.ind = train.ind)
sibp_rank_runs(sibp.search, X, 5)
sibp_top_words(sibp.search[["5"]][["1"]][[5]], colnames(X), 8, verbose = TRUE) ### strong!

## once we have decided on a model, we evaluate it on the test set
sibp.fit <- sibp.search[["5"]][["1"]][[5]]
amce_Y2<-sibp_amce(sibp.fit, X, Y2)
sibp_amce_plot(amce_Y2)


####
# Topic 1
# 
# In-text: This topic reflects parental reflections, expectations, and narratives about family life — from stories and watching children grow to feelings of chaos or gratitude.
# 
# Table label: Reflections & narratives
# 
# Topic 2
# 
# In-text: Focused on mistakes, expectations, and invisible struggles of parenting, this theme captures the pressure of “doing things right.”
# 
# Table label: Expectations & struggles
# 
# Topic 3
# 
# In-text: Centered on childcare reliability and backup planning, with stress about costs, coverage, and negotiating dependable support.
# 
# Table label: Childcare coverage & stress
# 
# Topic 4
# 
# In-text: This topic highlights budgeting and daycare logistics, including costs, communication, and negotiating financial constraints.
# 
# Table label: Daycare & finances
# 
# Topic 5
# 
# In-text: Focuses on infant needs — diapers, formula, feeding, and early baby care — often tied to economic or job stress.
# 
# Table label: Infant care & basics
# 
# Topic 6
# 
# In-text: Emphasizes partner dynamics, excuses, and extended family involvement in parenting labor, reflecting negotiation and division of responsibility.
# 
# Table label: Partners & support networks
# 
# Topic 7
# 
# In-text: A very clear milestone theme — learning to ride a bike, training wheels, cheering, and the pride in developmental steps.
# 
# Table label: Milestones & achievements
# 
# Topic 8
# 
# In-text: Captures preparation and planning — thoughtful, endless tasks, and maintaining routines that blur together in family life.
# 
# Table label: Planning & routines

amce_Y2_labels <- c(
  "intercept",
  "Reflections & narratives",
  "Expectations & struggles",
  "Childcare coverage & stress",
  "Daycare & finances",
  "Infant care & basics",
  "Partners & support networks",
  "Milestones & achievements",
  "Planning & routines"
)

# Add as new column to your dataframe
amce_Y2$latent_ts <- amce_Y2_labels
amce_Y2_plot <- amce_Y2 %>% filter(latent_ts != "intercept")


# Plot Y2
amce_Y2_plottt <- ggplot(amce_Y2_plot, aes(x = reorder(latent_ts, effect), y = effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", size = 0.4) +
  geom_pointrange(aes(ymin = L, ymax = U), 
                  size = 0.6, fatten = 2, color = "black") +
  coord_flip() +
  labs(
    x = "Latent Text Treatments",
    y = "Average Marginal Component Effects",
    title = NULL
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(face = "bold", angle = 90),
    axis.title.x = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", size = 0.3)
  )





### Outcome Y3: expect_moth 
# Search sIBP for several parameter configurations; fit each to the training set
sibp.search <- sibp_param_search(X, Y3, K = 8, alphas = c(1,2,3,4,5), sigmasq.ns = c(0.8, 1, 1.2), 
                                 iters = 5, train.ind = train.ind)
sibp_rank_runs(sibp.search, X, 5)
sibp_top_words(sibp.search[["3"]][["1"]][[2]], colnames(X), 8, verbose = TRUE) 


## once we have decided on a model, we evaluate it on the test set
sibp.fit <- sibp.search[["3"]][["1"]][[2]]
amce_Y3<-sibp_amce(sibp.fit, X, Y3)
sibp_amce_plot(amce_Y3)

###### TOPIC DESCRIPTIONS
# Topic 1
# 
# In-text: Reflects hidden or invisible labor — keeping things going behind the scenes, managing expectations, and handling routines like baths.
# 
# Table label: Invisible labor & routines
# 
# Topic 2
# 
# In-text: Centers on food and grocery work, from cooking to keeping kids healthy, with some emotional spillover.
# 
# Table label: Meals & food work
# 
# Topic 3
# 
# In-text: Captures family relationships and emotions — favorite moments, single parenting, feelings, and balancing partnerships.
# 
# Table label: Family roles & emotions
# 
# Topic 4
# 
# In-text: Focused on scheduling and managing time — pausing, managing, splitting tasks, and negotiating responsibilities with a partner.
# 
# Table label: Time management & division
# 
# Topic 5
# 
# In-text: Highlights partner involvement (or absence), upcoming events, and the strain of carrying responsibility when support is “rarely” present.
# 
# Table label: Partner support & events
# 
# Topic 6
# 
# In-text: About childcare logistics and work-life balance — waitlists, careers, nannies, meetings, and backup care when plans fall through.
# 
# Table label: Childcare & careers
# 
# Topic 7
# 
# In-text: Captures children’s routines and moods — preschool, morning rush, bedtime, play, excitement, and appreciation from kids.
# 
# Table label: Children’s routines & moods
# 
# Topic 8
# 
# In-text: Emphasizes storytelling, reading, and bedtime scenes — intimate moments like snuggling or voices that bring family life together.
# 
# Table label: Bedtime & bonding

# Define AMCE labels for this model
amce_Y3_labels <- c(
  "intercept",
  "Invisible labor & routines",
  "Meals & food work",
  "Family roles & emotions",
  "Time management & division",
  "Partner support & events",
  "Childcare & careers",
  "Children’s routines & moods",
  "Bedtime & bonding"
)

# Add as new column to your dataframe
amce_Y3$latent_ts <- amce_Y3_labels
amce_Y3_plot <- amce_Y3 %>% filter(latent_ts != "intercept")

amce_Y3_plot <- amce_Y3_plot %>%
  mutate(latent_ts = forcats::fct_reorder(latent_ts, effect))

# Plot Y3
amce_Y3_plottt <- ggplot(amce_Y3_plot, aes(x = latent_ts, y = effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", size = 0.4) +
  geom_pointrange(aes(ymin = L, ymax = U), 
                  size = 0.6, fatten = 2, color = "black") +
  coord_flip() +
  labs(
    x = "Latent Text Treatments",
    y = "Average Marginal Component Effects",
    title = NULL
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(face = "bold", angle = 90),
    axis.title.x = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", size = 0.3)
  )


plots <- list(
  Y1 = amce_Y1_plottt,
  Y2 = amce_Y2_plottt,
  Y3 = amce_Y3_plottt
)

for (nm in names(plots)) {
  ggsave(paste0("Figure_", nm, ".png"),
         plot = plots[[nm]],
         device = ragg::agg_png,
         dpi = 300, width = 10, height = 6, units = "in")
}

