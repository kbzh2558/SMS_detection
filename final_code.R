setwd('D:/PPT&WORD/McGill/MGSC401/R_workspace')

library(tidyverse)
library(here)
library(gridExtra)
library(cowplot)
library(MPV)
library(car)
library(corrplot)
library(ggfortify)
library(kableExtra)

# read
data <- read.csv("analysisdataset.csv")

# check
data %>% str
names(data)
table(data$Malicious)

## preprocessing

# save company brands
library(stringr)
library(SnowballC)
library(hunspell)
library(tm)
library(dplyr)
company_list <- unique(data$Brand)
company_list <- tolower(company_list)             # lower case
company_list <- trimws(company_list)              # rm extra leading/trailing whitespace
company_list <- company_list[company_list != ""]  # rm any blank entries
company_list <- unique(company_list)              # rm duplicates
company_list <- str_replace_all(company_list, "[[:punct:]]", " ")  # rm punctuation
company_list <- str_squish(company_list)          # squish multiple spaces to one


# keep only relevent high level cols
selected_cols <- c(
  "MainText", "SenderType", "Brand", 
  "URL.Subcategory", "Domain.Registrar", "Domain.Creation.Date", "Domain.Last.Update",
  "Message.Categories", "timeReceived", "Malicious"
)
data <- data[selected_cols]

# remove na rows in indicator
data <- data %>% filter(!is.na(Malicious))

# check nan
colSums(is.na(data))

# nlp cleaning
# spell correction
spell_corrected <- function(msg) {
  misspelled_index <- !hunspell_check(msg)
  misspelled_words <- msg[misspelled_index]
  suggestions <- hunspell_suggest(misspelled_words)
  
  for (i in seq_along(misspelled_words)) {
    if (length(suggestions[[i]]) > 0) {
      # replace misspelled word
      msg[msg == misspelled_words[i]] <- suggestions[[i]][1]
    }
  }
  return(msg)
}

# normalization func
normalize_text <- function(msg, company_list) {
  # lower case
  msg <- tolower(msg)
  
  # email with <email>
  msg <- str_replace_all(msg, "[\\w\\-.]+?@\\w+?\\.\\w{2,4}", " <email> ")
  
  # links with <url>
  msg <- str_replace_all(msg, "(http[s]?://\\S+)|(\\b\\w+\\.(com|net|org|info|us|page|link|biz|co|io|me|ly)\\S*)", " <url> ")
  
  # phone numbers with <phone_number>
  msg <- str_replace_all(msg, "\\b[0-9]{4}(-)?[0-9]{3}(-)?[0-9]{4}\\b", " <phone_number> ")
  
  # num with <number>
  msg <- str_replace_all(msg, "\\b\\d+(\\.\\d+)?\\b", " <number> ")
  
  # company names with <company>
  for (company in company_list) {
    # regex followed by (whitespace, punctuation, end of string)
    pattern <- paste0("\\b", company, "(?=[\\s[:punct:]]|$)")
    msg <- str_replace_all(msg, regex(pattern), " <company> ")
  }
  
  # "u" with "you"
  msg <- str_replace_all(msg, "\\bu\\b", " you ")
  
  # rm punc
  msg <- str_replace_all(msg, "(?<!<)[^\\w\\d\\s>](?!>)", " ")
  
  # rm whitespace
  msg <- str_squish(msg)
  
  return(msg)
}

# init
data$MainText <- iconv(data$MainText, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
stop_words <- stopwords("en")
n <- nrow(data)
corpus_not_stemmed <- vector("character", n)
corpus_stemmed <- vector("character", n)

# loop through data
for (i in seq_len(n)) {
  if (i %% 100 == 0) cat("Processing row:", i, "\n")
  
  tryCatch({
    # normalize text and tokenize
    msg <- normalize_text(data$MainText[i], company_list)
    tokens <- unlist(str_split(msg, "\\s+"))
    tokens <- tokens[tokens != ""]  # remove empty tokens
    
    # spell 
    tokens <- spell_corrected(tokens)
    corpus_not_stemmed[i] <- paste(tokens, collapse = " ")
    
    # rm stopwords and stemming
    tokens_stemmed <- wordStem(tokens[!tokens %in% stop_words], language = "en")
    corpus_stemmed[i] <- paste(tokens_stemmed, collapse = " ")
  }, error = function(e) {
    cat("Error in row", i, ":", e$message, "\n")
    corpus_not_stemmed[i] <- NA
    corpus_stemmed[i] <- NA
  })
}

data$not_stemmed <- corpus_not_stemmed
data$stemmed <- corpus_stemmed

# check
head(data[, c("MainText", "not_stemmed", "stemmed")], n=2)

# bag of words encoding
# corpus
corpus <- Corpus(VectorSource(data$stemmed))

# dtm
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(1, Inf)))
x <- as.matrix(dtm)
dim(x)

# extract time features from date cols

library(lubridate)
# func to parse time
safe_parse_mdy <- function(x, with_time = FALSE) {
  x[x == ""] <- NA # empty -> na
  if (with_time) {
    x <- gsub(",", "", x)  # rmv ,
    return(mdy_hms(x))
  } else {
    return(mdy(x))
  }
}

# timeReceived
data$time_parsed <- safe_parse_mdy(data$timeReceived, with_time = TRUE)
data <- data %>%
  mutate(
    received_year   = year(time_parsed),
    received_month  = month(time_parsed),
    received_day    = day(time_parsed),
    received_hour   = hour(time_parsed),
    received_minute = minute(time_parsed)
  )

# Domain.Creation.Date
data$domain_created_parsed <- safe_parse_mdy(data$Domain.Creation.Date)
data <- data %>%
  mutate(
    domain_created_year  = year(domain_created_parsed),
    domain_created_month = month(domain_created_parsed),
    domain_created_day   = day(domain_created_parsed)
  )

# manual fix
data$Domain.Last.Update[data$Domain.Last.Update == "10/22/20222"] <- "10/22/2022"
data$Domain.Last.Update[data$Domain.Last.Update == "27/2023"] <- NA  

data$domain_updated_parsed <- safe_parse_mdy(data$Domain.Last.Update)
data <- data %>%
  mutate(
    domain_updated_year  = year(domain_updated_parsed),
    domain_updated_month = month(domain_updated_parsed),
    domain_updated_day   = day(domain_updated_parsed)
  )

# drop cols
data %>% select(-c("Domain.Registrar", "Domain.Creation.Date", "Domain.Last.Update",
                   'time_parsed','domain_created_parsed','domain_updated_parsed'))

# check na in newly created cols
time_cols <- c(
  "received_year", "received_month", "received_day", "received_hour", "received_minute",
  "domain_created_year", "domain_created_month", "domain_created_day",
  "domain_updated_year", "domain_updated_month", "domain_updated_day"
)
colSums(is.na(data[, time_cols]))

# record url presence
url_present <- ifelse(x[, "url"] > 0, 1, 0)
data$url_present = url_present

# create indicator col if is missing
data$domain_created_missing <- ifelse(data$url_present == 1 & is.na(data$domain_created_year), 1, 0)
data$domain_updated_missing <- ifelse(data$url_present == 1 & is.na(data$domain_updated_year), 1, 0)

# fix na with -1
data[time_cols] <- lapply(data[time_cols], function(col) {
  col[is.na(col)] <- -1
  return(col)
})

# too many -> group
table(data$Brand)


data <- data %>%
  mutate(
    Brand_clean = tolower(trimws(Brand)),
    BrandCluster = case_when(
      Brand_clean == "" ~ "Unknown",

      Brand_clean %in% c("costco", "amazon", "amazon inc", "walgreens", 
                         "cvs cvs, publix, home depot, walmart, kroger, best buy",
                         "burger king", "sams club", "mcdonald's", "rebook", "ups", "walmart", "old row") ~ "Retail & Consumer Products",
      
      Brand_clean %in% c("wells fargo", "bank of america", "bank of the west", "bankofamerica",
                         "citizens bank", "citi", "citizen bank", "chase", "chase bank", "chase morgan",
                         "american express", "jp morgan", "union bank", "unionbank", "rbank", "rbc", "pnc",
                         "charles schwab") ~ "Banking & Finance",
      
      Brand_clean %in% c("cash app", "cash request online", "venmo", "paypal", "wepay") ~ "Payment / P2P Apps",
      
      Brand_clean %in% c("unicef", "irs", "u.s. postal service", "usps", "canada post", "anpost",
                         "first choice community healthcare") ~ "Non-Profit / Gov/Health",
      
      Brand_clean %in% c("whatsapp", "apple", "metamask", "netflix", "twitter", "snapchat",
                         "shark-tank", "shark tank", "loopback rewards") ~ "Tech, Media & Platforms",
      
      TRUE ~ "Misc / Others"
    )
  )

head(data[, c("Brand", "BrandCluster")])
table(data$BrandCluster)

# concat
merged_data <- cbind(data %>% select(-c('MainText','Brand','Domain.Registrar','time_parsed',
                                        'domain_created_parsed','domain_updated_parsed',
                                        'url_present','Brand_clean')), 
                     x)

# to binary
merged_data$Malicious <- ifelse(merged_data$Malicious > 0, 1, 0)
table(merged_data$Malicious)

# temp save
write.csv(data, "data_cleaned.csv", row.names = FALSE)
write.csv(merged_data, "train_data.csv", row.names = FALSE)

# eda
# top 6 appearing words
x_with_label <- data.frame(Malicious = merged_data$Malicious, x)
mal_bow <- x_with_label[x_with_label$Malicious == 1, -1]
nonmal_bow <- x_with_label[x_with_label$Malicious == 0, -1]
# count
mal_counts <- colSums(mal_bow)
nonmal_counts <- colSums(nonmal_bow)
top_mal <- sort(mal_counts, decreasing = TRUE)[1:6]
top_nonmal <- sort(nonmal_counts, decreasing = TRUE)[1:6]

# summariase
summary_table <- data.frame(
  Rank = 1:6,
  Top_Malicious_Word = names(top_mal),
  Malicious_Count = as.integer(top_mal),
  Top_NonMalicious_Word = names(top_nonmal),
  NonMalicious_Count = as.integer(top_nonmal)
)
summary_table

# name def
cleaned_names <- c(
  MainText = "Main Text",
  SenderType = "Sender Types",
  Brand = "Brand",
  URL.Subcategory = "URL Subcategory",
  Domain.Registrar = "Domain Registrar",
  Domain.Creation.Date = "Domain Creation Date",
  Domain.Last.Update = "Domain Last Update",
  Message.Categories = "Message Categories",
  timeReceived = "Time Received",
  Malicious = "Malicious (1/0)",
  not_stemmed = "Original Text",
  stemmed = "Stemmed Text",
  time_parsed = "Time Parsed",
  received_year = "Received Year",
  received_month = "Received Month",
  received_day = "Received Day",
  received_hour = "Received Hour",
  received_minute = "Received Minute",
  domain_created_parsed = "Domain Created Parsed",
  domain_created_year = "Domain Created Year",
  domain_created_month = "Domain Created Month",
  domain_created_day = "Domain Created Day",
  domain_updated_parsed = "Domain Updated Parsed",
  domain_updated_year = "Domain Updated Year",
  domain_updated_month = "Domain Updated Month",
  domain_updated_day = "Domain Updated Day",
  url_present = "URL Present (1/0)",
  domain_created_missing = "Domain Created Missing (1/0)",
  domain_updated_missing = "Domain Updated Missing (1/0)",
  Brand_clean = "Clean Brand",
  BrandCluster = "Brand Cluster"
)

# continuous vars
# box plot dist of continuous
data$Malicious <- ifelse(data$Malicious > 0, 'Malicious', 'Not Malicious')


continuous_data <- data %>% select(-c('MainText','timeReceived','time_parsed',
                                      'Brand_clean','domain_created_missing',
                                      'domain_created_missing','url_present','domain_updated_missing','domain_updated_parsed',
                                      'domain_created_parsed','not_stemmed','stemmed'))%>%
  select(where(is.numeric), Malicious) %>%
  pivot_longer(-Malicious, names_to = "Feature", values_to = "Value")

# change name
continuous_data <- continuous_data %>%
  mutate(Feature = as.character(Feature))

matched_keys <- intersect(unique(continuous_data$Feature), names(cleaned_names))
cleaned_names_filtered <- cleaned_names[matched_keys]

continuous_data <- continuous_data %>%
  mutate(Feature = dplyr::recode(Feature, !!!as.list(cleaned_names_filtered)))


ggplot(continuous_data, aes(x = Value, fill = as.factor(Malicious))) +
  geom_boxplot() +
  facet_wrap(~ Feature, scales = "free", ncol = 3) +
  labs(title = "Distribution of Continuous Features by Text Type", fill = "Malicious (1/0)") +
  theme_minimal()

final_plot <- ggplot(continuous_data, aes(x = Value, fill = as.factor(Malicious))) +
  geom_boxplot() +
  facet_wrap(~ Feature, scales = "free", ncol = 3) +
  labs(title = "Distribution of Continuous Features by Text Type", fill = "Malicious (1/0)") +
  theme_minimal()

ggsave(
  filename = "./img/matrix_continuous_dist.png",  
  plot = final_plot,
  width = 12, height = 10, units = "in",
  bg = "white",
  dpi = 300  
)

# categorical
categorical_data <- data %>% select(-c('MainText','timeReceived','time_parsed',
                                       'Brand_clean','domain_updated_parsed',
                                       'domain_created_parsed','not_stemmed','stemmed','Brand','Domain.Last.Update',"Domain.Registrar","Domain.Creation.Date")) %>%
  select(where(is.character), Malicious, domain_created_missing, domain_updated_missing,
         domain_created_missing,url_present) %>% 
  mutate(across(everything(), as.character)) %>%
  pivot_longer(-Malicious, names_to = "Feature", values_to = "Category")

# change name
matched_keys <- intersect(unique(categorical_data$Feature), names(cleaned_names))
cleaned_names_filtered <- cleaned_names[matched_keys]
categorical_data <- categorical_data %>%
  mutate(Feature = dplyr::recode(Feature, !!!as.list(cleaned_names_filtered)))

ggplot(categorical_data, aes(x = Category, fill = as.factor(Malicious))) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Feature, scales = "free", ncol = 3) +
  labs(title = "Count of Text Type by Categorical Features", fill='Malicious (1/0)',x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

final_plot <- ggplot(categorical_data, aes(x = Category, fill = as.factor(Malicious))) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Feature, scales = "free", ncol = 3) +
  labs(title = "Count of Text Type by Categorical Features", fill='Malicious (1/0)',x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "./img/matrix_categorical_dist.png",  
  plot = final_plot,
  width = 17, height = 10, units = "in",
  bg = "white",
  dpi = 300  
)

# token and charlen 

data <- data %>%
  mutate(
    tokens_stemmed = sapply(strsplit(stemmed, "\\s+"), length),
    characters_stemmed = nchar(stemmed),
    tokens_not_stemmed = sapply(strsplit(not_stemmed, "\\s+"), length),
    characters_not_stemmed = nchar(not_stemmed)
  )

y_graph <- data %>%
  group_by(Malicious) %>%
  summarise(
    tokens_stemmed = mean(tokens_stemmed, na.rm = TRUE),
    tokens_not_stemmed = mean(tokens_not_stemmed, na.rm = TRUE),
    characters_stemmed = mean(characters_stemmed, na.rm = TRUE),
    characters_not_stemmed = mean(characters_not_stemmed, na.rm = TRUE)
  )

library(patchwork)  

not_stemmed_colors <- c("Not Malicious" = "#2ca02c", "Malicious" = "#ff7f0e")   

p1 <- ggplot(data, aes(x = as.factor(Malicious), y = tokens_stemmed, fill = Malicious)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Token Count (Stemmed)", x = "Malicious", y = "Tokens") +
  theme_minimal()

p2 <- ggplot(data, aes(x = as.factor(Malicious), y = tokens_not_stemmed, fill = Malicious)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Token Count (Not Stemmed)", x = "Malicious", y = "Tokens") +
  theme_minimal() +
  scale_fill_manual(values = not_stemmed_colors) 

p3 <- ggplot(data, aes(x = as.factor(Malicious), y = characters_stemmed, fill = Malicious)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Character Count (Stemmed)", x = "Malicious", y = "Characters") +
  theme_minimal()

p4 <- ggplot(data, aes(x = as.factor(Malicious), y = characters_not_stemmed, fill = Malicious)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Character Count (Not Stemmed)", x = "Malicious", y = "Characters") +
  theme_minimal() +
  scale_fill_manual(values = not_stemmed_colors)

(p1 | p2) /
  (p3 | p4)

# save
final_plot <- (p1 | p2) /
  (p3 | p4)
ggsave(
  filename = "./img/matrix_token_char_count.png",  
  plot = final_plot,
  width = 12, height = 10, units = "in",  
  dpi = 300  
)



# matrix of categorical features
library(ggrepel)
library(forcats)
library(scales)

data$SenderType <- ifelse(trimws(data$SenderType) == "", "Unknown", data$SenderType)

p1_data <- data %>%
  count(SenderType) %>%
  mutate(perc = n / sum(n),
         percent_label = paste0(round(perc * 100), "%"),
         ypos = cumsum(perc) - 0.5 * perc)

p1_data <- p1_data %>%
  arrange(desc(SenderType)) %>% 
  mutate(prop = n / sum(n),
         ymax = cumsum(prop),
         ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2)

p1 <- ggplot(p1_data, aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = 0, fill = SenderType)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(-0.5, 1.5)) +
  theme_void() +
  labs(title = "Proportion of SMS Sender Types", fill = "Sender Type") +
  geom_text_repel(aes(x = 1.1, y = labelPosition, label = percent_label),
                  nudge_x = 0.2,
                  segment.color = "grey50",
                  show.legend = FALSE) + 
  scale_fill_manual(values = c("#8dd3c7", "#fdb462", "#bebada", "#fb8072"))


brand_data <- data %>%
  filter(Brand_clean != "") %>%
  count(Brand_clean) %>%
  top_n(5, n) %>%
  mutate(Brand_label = tools::toTitleCase(Brand_clean),
         perc = n / sum(n),
         percent_label = paste0(round(perc * 100), "%"),
         ypos = cumsum(perc) - 0.5 * perc)

brand_data <- brand_data %>%
  arrange(desc(Brand_label)) %>% 
  mutate(prop = n / sum(n),
         ymax = cumsum(prop),
         ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2)

p2 <- ggplot(brand_data, aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = 0, fill = Brand_label)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(-0.5, 1.5)) +
  theme_void() +
  labs(title = "Top 5 Most Mentioned Brands", fill = "Brand Label") +
  geom_text_repel(aes(x = 1.1, y = labelPosition, label = percent_label),
                  nudge_x = 0.2,
                  segment.color = "grey50",
                  show.legend = FALSE) + 
  scale_fill_manual(values = c("#8dd3c7", "#fdb462", "#bebada", "#fb8072",'lightblue'))


p3 <- data %>%
  filter(!is.na(Message.Categories), Message.Categories != "") %>%
  count(Message.Categories) %>%
  top_n(10, n) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = fct_reorder(Message.Categories, prop), y = prop, fill = Message.Categories)) +
  geom_bar(stat = "identity") +
  labs(title = "Top Message Categories", x = "Category", y = "Proportion") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none",
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 12))

p4 <- data %>%
  filter(!is.na(URL.Subcategory), URL.Subcategory != "") %>%
  count(URL.Subcategory) %>%
  top_n(5, n) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = fct_reorder(URL.Subcategory, prop), y = prop, fill = URL.Subcategory)) +
  geom_bar(stat = "identity") +
  labs(title = "Top URL Subcategories", x = "Subcategory", y = "Proportion") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none",
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 12))

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)

grid.arrange(g1, g2, g3, g4, ncol = 2)


final_plot <- grid.arrange(g1, g2, g3, g4, ncol = 2)
ggsave(
  filename = "./img/matrix_prop_category.png",  
  plot = final_plot,
  width = 12, height = 10, units = "in",  
  dpi = 300  
)


# num of malicious by time
daily_counts <- malicious_data %>%
  count(date)

ggplot(daily_counts, aes(x = date, y = n)) +
  geom_col(fill = "#FF4E42") +
  labs(title = "Malicious Messages by Date",
       x = "Date",
       y = "Number of Messages") +
  theme_minimal()

final_plot <- ggplot(daily_counts, aes(x = date, y = n)) +
  geom_col(fill = "#FF4E42") +
  labs(title = "Malicious Messages by Date",
       x = "Date",
       y = "Number of Messages") +
  theme_minimal()

ggsave(
  filename = "./img/malicious_msg_cnt_time.png",  
  plot = final_plot,
  width = 12, height = 10, units = "in",  
  bg = "white",
  dpi = 300  
)

# model
merged_data$SenderType <- ifelse(trimws(merged_data$SenderType) == "", "Unknown", merged_data$SenderType)
merged_data$URL.Subcategory <- ifelse(trimws(merged_data$URL.Subcategory) == "", "Unknown", merged_data$URL.Subcategory)

library(caret)
categorical_vars <- c("SenderType", "URL.Subcategory", "Message.Categories", "BrandCluster")

# to fac
merged_data[categorical_vars] <- lapply(merged_data[categorical_vars], as.factor)

model_df <- merged_data %>% select(-all_of(c("not_stemmed", "stemmed", "timeReceived", "Domain.Creation.Date","Domain.Last.Update")))
# to fac
model_df$Malicious <- as.factor(model_df$Malicious)

# train test split 8:2
train_idx <- createDataPartition(model_df$Malicious, p = 0.8, list = FALSE)
train_data <- model_df[train_idx, ]
test_data <- model_df[-train_idx, ]

library(MLmetrics)

# baseline logistic
# cross cv on training

# helped func
four_metrics <- function(data, lev = NULL, model = NULL) {
  precision <- Precision(as.numeric(as.character(data$pred)), as.numeric(as.character(data$obs)), positive = 1)
  recall <- Recall(as.numeric(as.character(data$pred)), as.numeric(as.character(data$obs)), positive = 1)
  f1 <- F1_Score(as.numeric(as.character(data$pred)), as.numeric(as.character(data$obs)), positive = 1)
  acc <- Accuracy(as.numeric(as.character(data$pred)), as.numeric(as.character(data$obs)))
  
  out <- c(Accuracy = acc, Precision = precision, Recall = recall, F1 = f1)
  return(out)
}

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  summaryFunction = four_metrics,
  savePredictions = "final"
)

logit_cv_model <- train(
  Malicious ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "Recall"
)

cat("\nAverage Metrics across 5 folds:\n")
logit_cv_model$resample %>%
  summarise(
    Accuracy = mean(Accuracy),
    Precision = mean(Precision),
    Recall = mean(Recall),
    F1 = mean(F1, na=T)
  )


logit_model <- glm(Malicious ~ ., data = train_data, family = "binomial")
# pred on test
logit_probs <- predict(logit_model, newdata = test_data, type = "response")
logit_preds <- ifelse(logit_probs > 0.5, 1, 0)
# ensure pos = 1
logit_preds <- factor(logit_preds, levels = c("0", "1"))
test_labels <- factor(test_data$Malicious, levels = c("0", "1"))

confusionMatrix(logit_preds, test_labels, positive = "1")

# near 0 variance vars
library(caret)

nzv <- nearZeroVar(train_data, saveMetrics = TRUE)
print(nzv[nzv$zeroVar == TRUE, ])


# helper func
compute_metrics_from_cm <- function(cm) {
  # save tbl
  tbl <- cm$table
  # extract from tbl
  TN <- tbl[1, 1]  # tn
  FP <- tbl[1, 2]  # fp
  FN <- tbl[2, 1]  # fn
  TP <- tbl[2, 2]  # tp
  
  # metrics
  accuracy <- (TP + TN) / sum(tbl)
  precision <- if ((TP + FP) == 0) NA else TP / (TP + FP)
  recall <- if ((TP + FN) == 0) NA else TP / (TP + FN)
  f1 <- if (is.na(precision) || is.na(recall) || (precision + recall == 0)) NA else 2 * precision * recall / (precision + recall)
  
  metrics <- list(
    Accuracy = round(accuracy, 4),
    Precision = round(precision, 4),
    Recall = round(recall, 4),
    F1_Score = round(f1, 4)
  )
  
  return(metrics)
}
compute_metrics_from_cm(confusionMatrix(logit_preds, test_labels, positive = "1"))

# xgboost
library(xgboost)

# default model
xgb_cv_model_default <- train(
  Malicious ~ .,
  data = train_data,
  method = "xgbTree",
  trControl = ctrl,
  metric = "Recall",
  tuneGrid = expand.grid(
    nrounds = 100,  # default
    max_depth = 6,  # default
    eta = 0.3,      # default
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)

cat("\nAverage CV Metrics (5 folds):\n")
xgb_cv_model_default$resample %>%
  summarise(
    Accuracy = mean(Accuracy),
    Precision = mean(Precision),
    Recall = mean(Recall),
    F1 = mean(F1)
  )


# on test
x_train <- train_data %>% select(-Malicious) %>% mutate_if(is.factor, as.numeric) %>% as.matrix()
y_train <- as.numeric(as.character(train_data$Malicious))

x_test <- test_data %>% select(-Malicious) %>% mutate_if(is.factor, as.numeric) %>% as.matrix()
y_test <- as.numeric(as.character(test_data$Malicious))

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

# fit
xgb_default_model <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  nrounds = 100,  # default
  verbose = 0
)

xgb_probs <- predict(xgb_default_model, newdata = dtest)
xgb_preds <- ifelse(xgb_probs > 0.5, 1, 0)
xgb_preds <- factor(xgb_preds, levels = c("0", "1"))

compute_metrics_from_cm(confusionMatrix(xgb_preds, test_labels, positive = "1"))

# fine tuning with cv
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6),
  eta = c(0.1, 0.3),
  gamma = c(0, 1, 5),
  colsample_bytree = c(0.3, 0.5, 0.7),
  min_child_weight = c(1, 5, 10),
  subsample = c(0.7, 1.0)
)
# parallel tuning
library(doParallel)


# helper func
four_metrics_parellel <- function(data, lev = NULL, model = NULL) {
  precision <- MLmetrics::Precision(as.numeric(as.character(data$pred)), 
                                    as.numeric(as.character(data$obs)), positive = 1)
  recall <- MLmetrics::Recall(as.numeric(as.character(data$pred)), 
                              as.numeric(as.character(data$obs)), positive = 1)
  f1 <- MLmetrics::F1_Score(as.numeric(as.character(data$pred)), 
                            as.numeric(as.character(data$obs)), positive = 1)
  acc <- MLmetrics::Accuracy(as.numeric(as.character(data$pred)), 
                             as.numeric(as.character(data$obs)))
  return(c(Accuracy = acc, Precision = precision, Recall = recall, F1 = f1))
}

# init
num_cores <- detectCores() - 1  
cl <- makeCluster(num_cores)
clusterEvalQ(cl, {
  library(MLmetrics)
})
registerDoParallel(cl)

ctrl_tune <- trainControl(
  method = "cv",
  number = 5,
  classProbs = FALSE,
  summaryFunction = four_metrics_parellel,
  savePredictions = "final",
  verboseIter = TRUE,       
  allowParallel = TRUE     
)

# train
set.seed(123)
xgb_cv_model <- train(
  Malicious ~ .,
  data = train_data,
  method = "xgbTree",
  metric = "Recall",    
  trControl = ctrl_tune,
  tuneGrid = xgb_grid
)

# stop
stopCluster(cl)
registerDoSEQ()  # reset 

# results
head(xgb_cv_model$results[order(-xgb_cv_model$results$Accuracy), ])

# best param
best_params <- xgb_cv_model$bestTune
best_results <- subset(xgb_cv_model$results,
                       nrounds == best_params$nrounds &
                         max_depth == best_params$max_depth &
                         eta == best_params$eta &
                         gamma == best_params$gamma &
                         colsample_bytree == best_params$colsample_bytree &
                         min_child_weight == best_params$min_child_weight &
                         subsample == best_params$subsample)
print(best_results)

# second grid
xgb_fine_grid <- expand.grid(
  nrounds = c(100, 150, 200),            # more rounds with same eta
  max_depth = c(2, 3, 4),                # refine around 3
  eta = c(0.05, 0.1, 0.15),              # narrow around 0.1
  gamma = c(3, 5, 7),                    # explore near 5
  colsample_bytree = c(0.4, 0.5, 0.6),   # tighten around 0.5
  min_child_weight = c(1, 2, 3),         # zoom in 1
  subsample = c(0.9, 1.0)                # slightly back off from 1.0
)

# init
num_cores <- detectCores() - 1  
cl <- makeCluster(num_cores)
clusterEvalQ(cl, {
  library(MLmetrics)
})
registerDoParallel(cl)

# train
set.seed(123)
xgb_cv_model1 <- train(
  Malicious ~ .,
  data = train_data,
  method = "xgbTree",
  metric = "Recall",    
  trControl = ctrl_tune,
  tuneGrid = xgb_fine_grid
)

# stop
stopCluster(cl)
registerDoSEQ()  # reset 

# best param
best_params1 <- xgb_cv_model1$bestTune
best_results1 <- subset(xgb_cv_model1$results,
                       nrounds == best_params1$nrounds &
                         max_depth == best_params1$max_depth &
                         eta == best_params1$eta &
                         gamma == best_params1$gamma &
                         colsample_bytree == best_params1$colsample_bytree &
                         min_child_weight == best_params1$min_child_weight &
                         subsample == best_params1$subsample)
# recall is higher at the price of precesion
print(best_results)
print(best_results1)

# v3
xgb_fine_grid_1 <- expand.grid(
  nrounds = c(70, 90 ,100, 150),            # more rounds with same eta
  max_depth = c(2, 3, 4),                # refine around 3
  eta = c(0.05, 0.1, 0.15),              # narrow around 0.1
  gamma = c(3, 5, 7),                    # explore near 5
  colsample_bytree = c(0.4, 0.5, 0.6),   # tighten around 0.5
  min_child_weight = c(1, 2, 3),         # zoom in 1
  subsample = c(0.9, 1.0)                # slightly back off from 1.0
)

# init
num_cores <- detectCores() - 1  
cl <- makeCluster(num_cores)
clusterEvalQ(cl, {
  library(MLmetrics)
})
registerDoParallel(cl)

# train
set.seed(123)
xgb_cv_model2 <- train(
  Malicious ~ .,
  data = train_data,
  method = "xgbTree",
  metric = "Recall",    
  trControl = ctrl_tune,
  tuneGrid = xgb_fine_grid_1
)

# stop
stopCluster(cl)
registerDoSEQ()  # reset 

# best param
best_params2 <- xgb_cv_model2$bestTune
best_results2 <- subset(xgb_cv_model2$results,
                        nrounds == best_params2$nrounds &
                          max_depth == best_params2$max_depth &
                          eta == best_params2$eta &
                          gamma == best_params2$gamma &
                          colsample_bytree == best_params2$colsample_bytree &
                          min_child_weight == best_params2$min_child_weight &
                          subsample == best_params2$subsample)
# recall is higher at the price of precesion
print(best_results)
print(best_results1)
print(best_results2)


# test performance
# v1
xgb_model1 <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  nrounds = 100,  
  max_depth = 3,
  eta = 0.1,
  gamma = 5,
  colsample_bytree = 0.5,
  min_child_weight = 1,
  subsample = 1,
  verbose = 0
)

xgb_probs1 <- predict(xgb_model1, newdata = dtest)
xgb_preds1 <- ifelse(xgb_probs1 > 0.5, 1, 0)
xgb_preds1 <- factor(xgb_preds1, levels = c("0", "1"))

compute_metrics_from_cm(confusionMatrix(xgb_preds1, test_labels, positive = "1"))

# v2
xgb_model2 <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  nrounds = 100,  
  max_depth = 2,
  eta = 0.1,
  gamma = 7,
  colsample_bytree = 0.5,
  min_child_weight = 2,
  subsample = 1,
  verbose = 0
)

xgb_probs2 <- predict(xgb_model2, newdata = dtest)
xgb_preds2 <- ifelse(xgb_probs2 > 0.5, 1, 0)
xgb_preds2 <- factor(xgb_preds2, levels = c("0", "1"))

compute_metrics_from_cm(confusionMatrix(xgb_preds2, test_labels, positive = "1"))

# v3
xgb_model3 <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  nrounds = 70,  
  max_depth = 2,
  eta = 0.1,
  gamma = 7,
  colsample_bytree = 0.5,
  min_child_weight = 2,
  subsample = 1,
  verbose = 0
)

xgb_probs3 <- predict(xgb_model3, newdata = dtest)
xgb_preds3 <- ifelse(xgb_probs3 > 0.5, 1, 0)
xgb_preds3 <- factor(xgb_preds3, levels = c("0", "1"))

compute_metrics_from_cm(confusionMatrix(xgb_preds3, test_labels, positive = "1"))

# vis of feature importance
feature_names <- colnames(x_train)
importance_matrix <- xgb.importance(feature_names = feature_names, model = xgb_model1)
head(importance_matrix, 10)  

# save top feature
top_20_v1 <- xgb.importance(feature_names = feature_names, model = xgb_model1)[1:20, ]
top_20_v2 <- xgb.importance(feature_names = feature_names, model = xgb_model2)[1:20, ]

top_20_v1$model <- "V1"
top_20_v2$model <- "V2"

top_all <- rbind(top_20_v1, top_20_v2)
top_all$Feature <- as.character(top_all$Feature)


top_all$CleanName <- gsub("\\.", " ", top_all$Feature)
top_all$CleanName <- gsub("([a-z])([A-Z])", "\\1 \\2", top_all$CleanName)
top_all$CleanName <- gsub("_", " ", top_all$CleanName)
top_all$CleanName <- tools::toTitleCase(top_all$CleanName)
is_bow <- grepl("^[a-z]+$", top_all$Feature)
top_all$CleanName[is_bow] <- paste("Word in Text:", 
                                   tools::toTitleCase(as.character(top_all$Feature[is_bow]))
)

shared_feats <- intersect(top_20_v1$Feature, top_20_v2$Feature)
top_all$Color <- ifelse(top_all$Feature %in% shared_feats, "Shared", "Unique")
color_map <- c("Shared" = "#4DAF4A", "Unique" = "#E41A1C")


all_levels <- unique(c(top_20_v1$Feature, top_20_v2$Feature))
clean_levels <- gsub("\\.", " ", all_levels)
clean_levels <- gsub("([a-z])([A-Z])", "\\1 \\2", clean_levels)
clean_levels <- gsub("_", " ", clean_levels)
clean_levels <- tools::toTitleCase(clean_levels)
is_bow_lvls <- grepl("^[a-z]+$", all_levels)
clean_levels[is_bow_lvls] <- paste("Word in Text:", 
                                   tools::toTitleCase(all_levels[is_bow_lvls])
)
top_all$CleanName <- factor(top_all$CleanName, levels = rev(clean_levels))


# v1
p1 <- ggplot(subset(top_all, model == "V1"), aes(x = CleanName, y = Gain, fill = Color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = color_map) +
  labs(
    title = "Top 20 Feature Importances (Gain) of V1 XgBoost",
    x = NULL, y = "Gain"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# v2
top_all_v2 <- subset(top_all, model == "V2")
top_all_v2 <- top_all_v2[order(top_all_v2$Gain, decreasing = FALSE), ]
top_all_v2$CleanName <- factor(top_all_v2$CleanName, levels = unique(top_all_v2$CleanName))

p2 <- ggplot(top_all_v2, aes(x = CleanName, y = Gain, fill = Color)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = color_map) +
  labs(
    title = "Top 20 Feature Importances (Gain) of V2 XgBoost",
    x = "Feature", y = "Gain"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# plot
p1 <- p1 + labs(x = 'Feature')
p2 <- p2 + labs(x = 'Feature')

(p1 / p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") 

final_plot <- (p1 / p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") 

ggsave(
  filename = "./img/feature_importance.png",  
  plot = final_plot,
  width = 12, height = 10, units = "in",  
  bg = "white",
  dpi = 300  
)

# roc
library(pROC)

roc_logit   <- roc(test_labels, logit_probs)
roc_xgb_def <- roc(test_labels, xgb_probs)
roc_xgb_v1  <- roc(test_labels, xgb_probs1)
roc_xgb_v2  <- roc(test_labels, xgb_probs2)

df_roc <- rbind(
  data.frame(Model = "Logistic",   fpr = 1 - roc_logit$specificities, tpr = roc_logit$sensitivities),
  data.frame(Model = "XGBoost (Default)", fpr = 1 - roc_xgb_def$specificities, tpr = roc_xgb_def$sensitivities),
  data.frame(Model = "XGBoost V1", fpr = 1 - roc_xgb_v1$specificities, tpr = roc_xgb_v1$sensitivities),
  data.frame(Model = "XGBoost V2", fpr = 1 - roc_xgb_v2$specificities, tpr = roc_xgb_v2$sensitivities)
)

auc_labels <- c(
  sprintf("Logistic (AUC = %.3f)", auc(roc_logit)),
  sprintf("XGBoost (Default) (AUC = %.3f)", auc(roc_xgb_def)),
  sprintf("XGBoost V1 (AUC = %.3f)", auc(roc_xgb_v1)),
  sprintf("XGBoost V2 (AUC = %.3f)", auc(roc_xgb_v2))
)

df_roc$Model <- factor(df_roc$Model, levels = c("Logistic", "XGBoost (Default)", "XGBoost V1", "XGBoost V2"), labels = auc_labels)

ggplot(df_roc, aes(x = fpr, y = tpr, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50") +
  labs(
    title = "ROC Curve Comparison",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

final_plot <- ggplot(df_roc, aes(x = fpr, y = tpr, color = Model)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50") +
  labs(
    title = "ROC Curve Comparison",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave(
  filename = "./img/roc_auc.png",  
  plot = final_plot,
  width = 12, height = 10, units = "in",  
  bg = "white",
  dpi = 300  
)

# tree path
tree0 <- xgb.plot.tree(
  model = xgb_cv_model1$finalModel,
  trees = 0,
  show_node_id = TRUE,
  plot_width = 700,
  plot_height = 400
)

tree1 <- xgb.plot.tree(
  model = xgb_cv_model1$finalModel,
  trees = 1,
  show_node_id = TRUE,
  plot_width = 700,
  plot_height = 400
)
library(DiagrammeRsvg)
rsvg::rsvg_png(charToRaw(DiagrammeRsvg::export_svg(xgb.plot.tree(model = xgb_cv_model1$finalModel, trees = 0, show_node_id = TRUE))), file = "tree0.png", width = 2000)
rsvg::rsvg_png(charToRaw(DiagrammeRsvg::export_svg(xgb.plot.tree(model = xgb_cv_model1$finalModel, trees = 1, show_node_id = TRUE))), file = "tree1.png", width = 2000)


# test result dataframe
test_results <- test_data
test_results$Predicted <- xgb_preds1
test_results$Probability <- xgb_probs1
test_results$Actual <- factor(test_data$Malicious, levels = c("0", "1"))

# add x
test_results$OriginalText <- merged_data[-train_idx, "not_stemmed"]
test_results$TimeReceived <- merged_data[-train_idx, "timeReceived"]
test_results$DomainCreationDate <- merged_data[-train_idx, "Domain.Creation.Date"]
test_results$DomainLastUpdate <- merged_data[-train_idx, "Domain.Last.Update"]

# example
example_correct_0 <- test_results[test_results$Predicted == "0" & test_results$Actual == "0", ][1, ]
example_correct_1 <- test_results[test_results$Predicted == "1" & test_results$Actual == "1", ][1, ]
example_fp <- test_results[test_results$Predicted == "1" & test_results$Actual == "0", ][1, ]
example_fn <- test_results[test_results$Predicted == "0" & test_results$Actual == "1", ][1, ]

library(dplyr)
examples <- bind_rows(
  example_correct_0,
  example_correct_1,
  example_fp,
  example_fn
) %>% 
  select(OriginalText, Actual, Predicted, Probability, 
         TimeReceived, DomainCreationDate, DomainLastUpdate)

print(examples)



