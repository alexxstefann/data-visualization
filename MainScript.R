df <- read.csv("Clean.csv")
head(df)


###### Q1
order <- c('Flop', 'Dormant', 'Average', 'Viral', 'Super-Viral')

df_q1 <- df %>%
  select(n_tokens_content, shares, shares_categorical) %>%
  mutate(shares_categorical = factor(shares_categorical, levels = order))

bar1 <- df_q1 %>%
  group_by(shares_categorical) %>%
  summarise(mean_n_tokens_content = mean(n_tokens_content))

fig_1 <- plot_ly(bar1, x = ~shares_categorical, y = ~mean_n_tokens_content, type = "bar") %>%
  layout(
    showlegend = FALSE,
    title = "Average Number of Tokens Per Class",
    xaxis = list(title = "Article Popularity"),
    yaxis = list(title = "Average Number of Tokens"),
    width = 600
  )

fig_1 <- fig_1 %>% add_trace(marker = list(color = "steelblue", width = 0.4))

fig_1 <- fig_1 %>% layout(xaxis = list(categoryorder = "array", categoryarray = order))

ggplotly(fig_1)

##### Q2

# 1.
#ggplot
fig_2 <- ggplot(df, aes(x = shares_categorical, y = n_tokens_title, fill = shares_categorical)) +
  geom_boxplot() +
  labs(x = "Article Popularity", y = "Title Length", title = "Title Length Distributions Per Class") +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(fig_2)

#plotly
fig_2 <- plot_ly(df, x = ~shares_categorical, y = ~n_tokens_title, color = ~shares_categorical, type = "box") %>%
  layout(
    showlegend = FALSE,
    title = "Title Length Distributions Per Class",
    xaxis = list(title = "Article Popularity"),
    yaxis = list(title = "Title Length")
  )

fig_2

# 2.
#ggplot
fig_2_2 <- ggplot(df, aes(x = shares_categorical, y = n_tokens_title, fill = shares_categorical)) +
  geom_boxplot() +
  labs(x = "Article Popularity", y = "Title Length", title = "Title Length Distributions Per Class") +
  scale_y_log10() +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(fig_2_2)

#plotly
fig_2_2 <- plot_ly(df, x = ~shares_categorical, y = ~n_tokens_title, color = ~shares_categorical, type = "box") %>%
  layout(
    showlegend = FALSE,
    title = "Title Length Distributions Per Class",
    xaxis = list(title = "Article Popularity"),
    yaxis = list(title = "Title Length", type = "log")
  )

fig_2_2


##### Q3

# 1.
# ggplot
fig_3 <- ggplot(df, aes(x = shares_categorical, y = n_non_stop_unique_tokens, fill = shares_categorical)) +
  geom_violin() +
  labs(x = "Article Popularity", y = "Non-stopwords [%]", title = "Distributions of Articles' Non-Stopwords Per Class") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(limits = order)

ggplotly(fig_3)

#plotly
fig_3 <- plot_ly(df, x = ~shares_categorical, y = ~n_non_stop_unique_tokens, color = ~shares_categorical, type = "violin") %>%
  layout(
    showlegend = FALSE,
    title = "Distributions of Articles' Non-Stopwords Per Class",
    xaxis = list(title = "Article Popularity"),
    yaxis = list(title = "Non-stopwords [%]"),
    violinmode = "group",
    violingap = 0
  )

ggplotly(fig_3)

# 2.
#ggplot
fig_3_1 <- ggplot(df, aes(x = shares_categorical, y = n_non_stop_unique_tokens, fill = shares_categorical)) +
  geom_violin() +
  labs(x = "Article Popularity", y = "Non-stopwords [%]", title = "Distributions of Articles' Non-Stopwords Per Class") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(limits = order) +
  scale_y_log10()

ggplotly(fig_3_1)

#plotly
fig_3_1 <- plot_ly(df, x = ~shares_categorical, y = ~n_non_stop_unique_tokens, color = ~shares_categorical, type = "violin") %>%
  layout(
    showlegend = FALSE,
    title = "Distributions of Articles' Non-Stopwords Per Class",
    xaxis = list(title = "Article Popularity"),
    yaxis = list(title = "Non-stopwords [%]", type = "log"),
    violinmode = "group",
    violingap = 0
  )

ggplotly(fig_3_1)

##### Q4
df_q4 <- df[, c('num_hrefs', 'num_self_hrefs', 'shares_categorical', 'shares')]
df_q4$num_ext_refs <- df_q4$num_hrefs - df_q4$num_self_hrefs
q4 <- df_q4 %>%
  group_by(shares_categorical) %>%
  summarize(num_self_hrefs = mean(num_self_hrefs), num_ext_refs = mean(num_ext_refs)) %>%
  ungroup()

#ggplot
fig_4 <- q4 %>%
  pivot_longer(cols = c("num_self_hrefs", "num_ext_refs"), names_to = "Links", values_to = "Average") %>%
  ggplot(aes(x = shares_categorical, y = Average, fill = Links)) +
  geom_bar(stat = "identity", position = "dodge", orientation = "v") +
  labs(title = "Average Number of Links Per Article", x = "Article Popularity", y = "Average Number of Links") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("num_self_hrefs", "num_ext_refs")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplotly(fig_4)

#plotly
fig_4 <- q4 %>%
  pivot_longer(cols = c("num_self_hrefs", "num_ext_refs"), names_to = "Links", values_to = "Average") %>%
  plot_ly(x = ~shares_categorical, y = ~Average, color = ~Links, type = "bar") %>%
  layout(title = "Average Number of Links Per Article", xaxis = list(title = "Article Popularity"), yaxis = list(title = "Average Number of Links")) %>%
  add_markers() %>%
  layout(showlegend = TRUE)

ggplotly(fig_4)

##### Q6

# 1.
fig_6 <- plot_ly(df, y = ~average_token_length, x = ~shares_categorical, color = ~shares_categorical, type = "box") %>%
  layout(showlegend = FALSE, title = "Average Token Length Per Article",
         xaxis = list(title = "Article Popularity"),
         yaxis = list(title = "Average Token Length"))

ggplotly(fig_6)

# 2.
fig_6_1 <- plot_ly(df, y = ~average_token_length, x = ~shares_categorical, color = ~shares_categorical, type = "box") %>%
  layout(showlegend = FALSE, title = "Average Token Length Per Article",
         xaxis = list(title = "Article Popularity"),
         yaxis = list(title = "Average Token Length", type = "log"))

ggplotly(fig_6_1)

##### Q7 - Not working (No. 2. has an error)

# 1.
# df_6 <- df[, c("Channel", "shares_categorical")] %>%
#   group_by(Channel, shares_categorical) %>%
#   summarise(Counts = n()) %>%
#   ungroup()
# 
# pivot_df <- spread(df_6, shares_categorical, Counts, fill = 0)
# 
# fig_7 <- plot_ly(df_6, x = ~Channel, y = ~Counts, color = ~shares_categorical, type = "bar", barmode = "group") %>%
#   layout(title = "Article Popularity Counts By Channel", width = 600)
# 
# ggplotly(fig_7)
# 
# # 2.
# df_61 <- df_6 %>%
#   group_by(Channel) %>%
#   summarise(Counts = sum(Counts))
# 
# newdf <- rep(df_61$Counts, each = 5)
# newdf <- data.frame(newdf)
# colnames(newdf) <- colnames(df_6)[1]
# 
# # Add missing rows to match the number of rows in df_6
# if (nrow(df_6) != nrow(newdf)) {
#   diff_rows <- nrow(df_6) - nrow(newdf)
#   newdf <- rbind(newdf, rep(NA, diff_rows))
# }
# 
# df_6 <- bind_cols(df_6, newdf)
# df_6$pct <- df_6$Counts / df_6[, 3] * 100
# 
# # 3.
# fig_7_1 <- df_6 %>%
#   plot_ly(x = ~Channel, y = ~pct, color = ~shares_categorical, type = "bar") %>%
#   layout(xaxis = list(title = "Channel Type"),
#          yaxis = list(title = "Percentage"),
#          title = "Article Popularity Counts By Channel",
#          barmode = "relative",
#          categoryorder = "array",
#          categoryarray = order)
# 
# ggplotly(fig_7_1)

##### Q8
df_8 <- df[c("global_subjectivity", "global_sentiment_polarity",
             "global_rate_positive_words", "global_rate_negative_words",
             "rate_positive_words", "rate_negative_words", "avg_positive_polarity",
             "min_positive_polarity", "max_positive_polarity",
             "avg_negative_polarity", "min_negative_polarity",
             "max_negative_polarity", "title_subjectivity",
             "title_sentiment_polarity", "abs_title_subjectivity",
             "abs_title_sentiment_polarity", "shares")]

fig_8 <- plot_ly(z = cor(df_8), type = "heatmap",
                 colorscale = "Viridis", reversescale = TRUE)

fig_8 <- fig_8 %>% layout(title = "Correlation Matrix",
                          xaxis = list(title = "Variables"),
                          yaxis = list(title = "Variables"),
                          showlegend = TRUE,
                          width = 500, height = 500)

ggplotly(fig_8)

##### Q9
# 1.
fig_9 <- plot_ly(df, x = ~kw_max_avg, y = ~shares, type = "scatter", mode = "markers")
fig_9 <- fig_9 %>% layout(title = 'High-Performing Keywords Against Popularity',
                          xaxis = list(title = 'High Performing Words/Article'),
                          yaxis = list(title = 'Number of Shares'),
                          showlegend = FALSE)

ggplotly(fig_9)

# 2.
fig_9_2 <- plot_ly(df, x = ~kw_min_avg, y = ~shares, type = "scatter", mode = "markers")
fig_9_2 <- fig_9_2 %>% layout(title = 'Low-Performing Keywords Against Popularity',
                              xaxis = list(title = 'Low Performing Words'),
                              yaxis = list(title = 'Number of Shares'),
                              showlegend = FALSE)

ggplotly(fig_9_2)
# 3.
fig_9_3 <- plot_ly(df, x = ~kw_avg_avg, y = ~shares, type = "scatter", mode = "markers")
fig_9_3 <- fig_9_3 %>% layout(title = 'Average-Performing Keywords Against Popularity',
                              xaxis = list(title = 'Average Performing Words'),
                              yaxis = list(title = 'Number of Shares'),
                              showlegend = FALSE)

ggplotly(fig_9_3)

##### Q10
# 1.
fig_10 <- plot_ly(df, x = ~rate_positive_words, y = ~shares, type = "scatter", mode = "markers")
fig_10 <- fig_10 %>% layout(title = 'Rates of Positive Words Against Popularity',
                            xaxis = list(title = 'Rate of Positive Words'),
                            yaxis = list(title = 'Number of Shares'),
                            showlegend = FALSE)

ggplotly(fig_10)

# 2.
fig_10_1 <- plot_ly(df, x = ~title_sentiment_polarity, y = ~shares, type = "scatter", mode = "markers")
fig_10_1 <- fig_10_1 %>% layout(title = 'Title Polarity against Article Popularity',
                                xaxis = list(title = 'Title Polarity'),
                                yaxis = list(title = 'Number of Shares'),
                                showlegend = FALSE)

ggplotly(fig_10_1)

# 3.
fig_10_2 <- plot_ly(df, x = ~rate_negative_words, y = ~shares, type = "scatter", mode = "markers")
fig_10_2 <- fig_10_2 %>% layout(title = 'Rate of Positive Words against Article Popularity',
                                xaxis = list(title = 'Rate of Positive Words'),
                                yaxis = list(title = 'Number of Shares'),
                                showlegend = FALSE)

ggplotly(fig_10_2)

##### Q11


# features <- subset(df, select = -c(shares_categorical, shares))
# 
# categorical_cols <- c("is_weekend", "Day", "Channel")
# for (col in categorical_cols) {
#   features[[col]] <- as.numeric(as.factor(features[[col]]))
# }
# 
# scaler <- caret::preProcess(features, method = c("range"))
# features <- predict(scaler, newdata = features)
# 
# df$shares_categorical <- as.factor(df$shares_categorical)
# 
# labels <- as.numeric(as.factor(df$shares_categorical))
# 
# set.seed(42)
# train_indices <- caret::createDataPartition(y = labels, p = 0.67, list = FALSE)
# X_train <- features[train_indices, ]
# X_test <- features[-train_indices, ]
# y_train <- labels[train_indices]
# y_test <- labels[-train_indices]




