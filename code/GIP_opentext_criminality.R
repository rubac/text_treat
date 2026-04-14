library(tidyverse)
library(text)
library(readxl)
library(haven)
library(tidytext)
library(glmnet)
library(texreg)
library(irr)
library(sandwich)
library(lmtest)
library(broom)
library(dotwhisker)

gipdf <- read_dta("Z:/2_Data/GIP_W76a_V3.dta")
gipdf_open <- read_excel("Z:/4_open_answers/W76a_GIP_Main_open_v1.xls", col_types = "text")


#code which image they have seen
gipdf_open$car_oldarab <- !is.na(gipdf_open$ES76009_txt)
gipdf_open$car_youngarab <- !is.na(gipdf_open$ES76010_txt)

gipdf_open$car_oldblack <- !is.na(gipdf_open$ES76011_txt)
gipdf_open$car_youngblack <- !is.na(gipdf_open$ES76012_txt)

gipdf_open$car_oldwhite <- !is.na(gipdf_open$ES76013_txt)
gipdf_open$car_youngwhite <- !is.na(gipdf_open$ES76014_txt)

gipdf_open$description_car <- coalesce(
  gipdf_open$ES76009_txt,  # old Arab
  gipdf_open$ES76010_txt,  # young Arab
  gipdf_open$ES76011_txt,  # old Black
  gipdf_open$ES76012_txt,  # young Black
  gipdf_open$ES76013_txt,  # old White
  gipdf_open$ES76014_txt   # young White
)

gipdf_open$house_oldarab <- !is.na(gipdf_open$ES76018_txt)
gipdf_open$house_youngarab <- !is.na(gipdf_open$ES76019_txt)

gipdf_open$house_oldblack <- !is.na(gipdf_open$ES76020_txt)
gipdf_open$house_youngblack <- !is.na(gipdf_open$ES76021_txt)

gipdf_open$house_oldwhite <- !is.na(gipdf_open$ES76022_txt)
gipdf_open$house_youngwhite <- !is.na(gipdf_open$ES76023_txt)

gipdf_open$description_house <- coalesce(
  gipdf_open$ES76018_txt,  # old Arab
  gipdf_open$ES76019_txt,  # young Arab
  gipdf_open$ES76020_txt,  # old Black
  gipdf_open$ES76021_txt,  # young Black
  gipdf_open$ES76022_txt,  # old White
  gipdf_open$ES76023_txt   # young White
)

gipdf_open$shopping_oldarab <- !is.na(gipdf_open$ES76027_txt)
gipdf_open$shopping_youngarab <- !is.na(gipdf_open$ES76028_txt)

gipdf_open$shopping_oldblack <- !is.na(gipdf_open$ES76029_txt)
gipdf_open$shopping_youngblack <- !is.na(gipdf_open$ES76030_txt)

gipdf_open$shopping_oldwhite <- !is.na(gipdf_open$ES76031_txt)
gipdf_open$shopping_youngwhite <- !is.na(gipdf_open$ES76032_txt)

gipdf_open$description_shopping <- coalesce(
  gipdf_open$ES76027_txt,  # old Arab
  gipdf_open$ES76028_txt,  # young Arab
  gipdf_open$ES76029_txt,  # old Black
  gipdf_open$ES76030_txt,  # young Black
  gipdf_open$ES76031_txt,  # old White
  gipdf_open$ES76032_txt   # young White
)



#### Tranform into long table

# reshape outcome variables to long format
long_data <- gipdf_open %>%
  select(Nr, starts_with("description_")) %>%  # keep only `Nr` and the 3 descriptions
  pivot_longer(
    cols = starts_with("description_"),
    names_to = "scene",
    names_prefix = "description_"
  )

# pivot indicator variables and extract age + ethnicity
age_ethnicity_long <- gipdf_open %>%
  select(Nr, matches("^(car|house|shopping)_(old|young)(arab|black|white)$")) %>%
  pivot_longer(
    cols = -Nr,
    names_to = c("scene", "age_ethnicity"),
    names_sep = "_",
    values_to = "indicator"
  ) %>%
  separate(age_ethnicity, into = c("age", "ethnicity"), sep = "(?<=old|young)", remove = FALSE) %>%
  filter(indicator == TRUE) %>%
  select(Nr, scene, age, ethnicity)

# merge and compute index
long_data <- long_data %>%
  left_join(age_ethnicity_long, by = c("Nr", "scene")) %>%
  mutate(
    age = factor(age, levels = c("young", "old")),
    ethnicity = factor(ethnicity, levels = c("white", "black", "arab")),
    scene = factor(scene, levels = c("car", "house", "shopping"))
  )


## drop fast thinkers as they provide no text
long_data <- long_data[!is.na(long_data$age),]
long_data$value <- ifelse(long_data$value=="-90", "", long_data$value)


###
# Descriptives
###
gipdf$thinkingfast <- gipdf$expES76009 == 1

table(gipdf$thinkingfast)

slowthinkersdf <- gipdf[gipdf$thinkingfast==FALSE,]

table(slowthinkersdf$gender_24)
prop.table(table(slowthinkersdf$gender_24))
slowthinkersdf$age <- 2025 - slowthinkersdf$year_of_birth_cat_24


table(is.na(long_data$value) | str_trim(long_data$value) == "")

# Word count per response
word_counts <- str_count(long_data$value, "\\S+")

# Summary of word counts
summary(word_counts)


###
# Analysis
###

# Create a training-test split

# Total number of rows to assign to train
n_total <- nrow(long_data)
n_train_total <- round(n_total * 0.10)

## Note: I created this, labelled, and then recreated this; hence seemingly weird workflow

#part I for defining codebook
#after having annotated
train_annotated <- read.csv("train_sample_labelled.csv")


#part II: labelled by all annotators
all_labelled <- read_excel("crime_scenes_to_labelled_final.xlsx", col_types = "text")
all_labelled <- all_labelled[,-1]
all_labelled <- all_labelled[!is.na(all_labelled$Nr),]

validation_set <- all_labelled[!is.na(all_labelled$label_0),]


# make data types consistent for joiining
long_data <- long_data %>%
  mutate(Nr = as.character(Nr),
         scene = as.character(scene))

train_annotated <- train_annotated %>%
  mutate(Nr = as.character(Nr),
         scene = as.character(scene))

all_labelled <- all_labelled %>%
  mutate(Nr = as.character(Nr),
         scene = as.character(scene))


# Add an identifier to long_data for joining
long_data <- long_data %>%
  mutate(is_labeled = if_else(paste(Nr, scene) %in% paste(train_annotated$Nr, train_annotated$scene), TRUE, FALSE),
         is_labeled_multiple = if_else(paste(Nr, scene) %in% paste(validation_set$Nr, validation_set$scene), TRUE, FALSE))


n_labeled <- sum(long_data$is_labeled) + sum(long_data$is_labeled_multiple)

# Assign splits
long_data <- long_data %>%
  mutate(split = case_when(
    is_labeled_multiple ~ "validate",
    is_labeled ~ "train",
    paste(Nr, scene) %in% paste(unlabeled_train_sample$Nr, unlabeled_train_sample$scene) ~ "train",
    TRUE ~ "test"
  )) %>%
  select(-is_labeled, -is_labeled_multiple)

table(long_data$split)



############
## Calculate key validation values
############

#merge labels into dataset 
long_data_labelled <- merge(long_data, all_labelled, by = c("Nr", "scene"), all.x = TRUE)
long_data_labelled <- merge(long_data_labelled, 
                            train_annotated[, !names(train_annotated) %in% c("age", "ethnicity", "split")], 
                            by = c("Nr", "scene"), 
                            all.x = TRUE)


long_data_labelled$normative_mentioned_1 <- long_data_labelled$label_1==1 | long_data_labelled$ambiguity_1==1
long_data_labelled$normative_mentioned_0 <- long_data_labelled$label_0==1 | long_data_labelled$ambiguity_0==1
long_data_labelled$normative_mentioned_2 <- long_data_labelled$label_2==1 | long_data_labelled$ambiguity_2==1

# Create split datasets
train_data <- filter(long_data_labelled, split == "train")
validation_data <- filter(long_data_labelled, split == "validate")
test_data <- filter(long_data_labelled, split == "test")


####
#intercoder reliability
###

# Prepare data matrices
labels_matrix <- t(as.matrix(sapply(validation_data[, c("label_0", "label_1", "label_2")], as.numeric)))

ambiguity_matrix <- t(as.matrix(sapply(validation_data[, c("ambiguity_0", "ambiguity_1", "ambiguity_2")], as.numeric)))

normativementioned_matrix <- t(as.matrix(sapply(validation_data[, c("normative_mentioned_0", "normative_mentioned_1", "normative_mentioned_2")], as.numeric)))

# Krippendorf alphas 0/1
kripp.alpha(labels_matrix, method = "nominal")
kripp.alpha(ambiguity_matrix, method = "nominal")
kripp.alpha(normativementioned_matrix, method = "nominal")




######
## Causal modelling
######

#create one final dataset with test
#set NA labels to 0 (where values was empty)

causal_data <- test_data
causal_data <- causal_data %>%
  select(-label, -label_0, -label_2,
         -ambiguity, -ambiguity_0, -ambiguity_2,
         -normative_mentioned_0,-normative_mentioned_2,) %>%
  rename(label = label_1,
         ambiguity = ambiguity_1,
         normative_mentioned = normative_mentioned_1) %>%
  mutate(
    label = if_else(is.na(label), 0, as.numeric(label)),
    ambiguity = if_else(is.na(ambiguity), 0, as.numeric(ambiguity)),
    normative_mentioned = if_else(is.na(normative_mentioned), FALSE, normative_mentioned)
  )

head(causal_data)

# Causal estimation for paper with standard robust errors

# Model 1: Main effects
m1 <- glm(
  normative_mentioned ~ ethnicity + scene + age,
  data = causal_data,
  family = binomial
)

# Model 2: Interactions
m2 <- glm(
  normative_mentioned ~ ethnicity * scene + age * scene,
  data = causal_data,
  family = binomial
)

vcov_m1 <- vcovCL(m1, cluster = causal_data$Nr)
vcov_m2 <- vcovCL(m2, cluster = causal_data$Nr)


#Plot

tidy_m1 <- tidy(m1, conf.int = TRUE) %>%
  mutate(model = "Main effects")

tidy_m2 <- tidy(m2, conf.int = TRUE) %>%
  mutate(model = "Interactions")

coefs <- bind_rows(tidy_m1, tidy_m2)

coefs_plot <- coefs %>%
  filter(term != "(Intercept)")


textasoutcome_plot <- dwplot(coefs_plot,
                                vline = geom_vline(
                                  xintercept = 0,
                                  colour = "grey40",
                                  linetype = "dashed", size = 0.4),
                                dot_args=list(size = 2.2, shape=16),
                                whisker_args=list(size=0.6),
                                dodge_size = 0.7,
                             model_order = c("Main effects",
                                             "Interactions")) %>%
  relabel_predictors(
    c("ethnicityblack" = "Black (ref: White)",
      "ethnicityarab" = "Arab (ref: White)",
      "scenehouse" = "Scene: House (ref: Car)",
      "sceneshopping" = "Scene: Shopping (ref: Car)",
      "ethnicityblack:scenehouse" = "Black x House",
      "ethnicityarab:scenehouse" = "Arab x House",
      "ethnicityblack:sceneshopping" = "Black x Shopping",
      "ethnicityarab:sceneshopping" = "Arab x Shopping",      
      "ageold" = "Age: Old (ref: Young)",
    "scenehouse:ageold" = "Old x House",
    "sceneshopping:ageold" = "Old x Shopping")
  ) +
  scale_color_discrete(name="",
                       labels=c("Interactions Model", "Main Effects Model"
                                
                       )) +
  labs(
    x = "Log-Odds Effects",
    y = "Vignette Dimensions",
    title = NULL
  ) +
  theme_minimal(base_size = 8) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title.y = element_text(face = "bold", angle = 90),
    axis.title.x = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", size = 0.3),
    legend.position = "bottom"
  )

textasoutcome_plot

ggsave("textasoutcome_plot.tiff",
       plot = textasoutcome_plot,
       #device = ragg::agg_png,
       dpi = 300, width = 12, height = 6, units = "cm", bg="white")
