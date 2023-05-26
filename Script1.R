df <- read.csv("Clean.csv")
head(df)
  


###### 1. Describe n_tokens_content
print(summary(df$n_tokens_content))



###### 2.
df_q1 <- df %>% select(n_tokens_content, shares, shares_categorical)
bar1 <- df_q1 %>%
  group_by(shares_categorical) %>%
  summarise(mean_n_tokens_content = mean(n_tokens_content))

# Plotting the bar chart
ggplot(bar1, aes(x = shares_categorical, y = mean_n_tokens_content)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "shares_categorical", y = "Mean n_tokens_content")

###### 2.
df_q1 <- df %>% select(n_tokens_content, shares, shares_categorical)
bar1 <- df_q1 %>%
  group_by(shares_categorical) %>%
  summarise(mean_n_tokens_content = mean(n_tokens_content))

# Creating an interactive bar chart using plotly
plot_ly(bar1, x = ~shares_categorical, y = ~mean_n_tokens_content, type = "bar") %>%
  layout(xaxis = list(title = "shares_categorical"), yaxis = list(title = "Mean n_tokens_content"))

##### 2.
df_q1 <- df %>% select(n_tokens_content, shares, shares_categorical)
bar1 <- df_q1 %>%
  group_by(shares_categorical) %>%
  summarise(mean_n_tokens_content = mean(n_tokens_content))

# Converting ggplot to plotly object
p <- ggplot(bar1, aes(x = shares_categorical, y = mean_n_tokens_content)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "shares_categorical", y = "Mean n_tokens_content")

ggplotly(p)



##### 3. 
# Generate a box plot
p <- plot_ly(df, y = ~n_tokens_title, x = ~shares_categorical, color = ~shares_categorical, type = "box", log_y = FALSE)
ggplotly(p)



##### 4.
df_q2 <- df %>% select(n_tokens_title, shares, shares_categorical)
bar2 <- df_q2 %>%
  group_by(shares_categorical) %>%
  summarise(mean_n_tokens_title = mean(n_tokens_title))

# Plotting the bar chart
p <- plot_ly(bar2, x = ~shares_categorical, y = ~mean_n_tokens_title, type = "bar") %>%
  layout(xaxis = list(title = "shares_categorical"), yaxis = list(title = "Mean n_tokens_title"))
ggplotly(p)



##### 5.
p <- plot_ly(df, y = ~n_non_stop_unique_tokens, x = ~shares, color = ~shares_categorical, type = "scatter")
p <- layout(p, yaxis = list(range = c(0, 1)))
ggplotly(p)



##### 6.
p <- plot_ly(df, y = ~n_non_stop_unique_tokens, x = ~shares_categorical, color = ~shares_categorical, type = "violin", box = list(visible = TRUE))
#p <- layout(p, yaxis = list(type = "log"))
ggplotly(p)



##### 7.
df_q4 <- df %>% select(num_hrefs, num_self_hrefs, shares_categorical, shares)
df_q4 <- df_q4 %>% mutate(num_ext_refs = num_hrefs - num_self_hrefs)



##### 8.
q4 <- df_q4 %>%
  group_by(shares_categorical) %>%
  summarise(num_self_hrefs = mean(num_self_hrefs), num_ext_refs = mean(num_ext_refs)) %>%
  ungroup()
print(q4)



##### 9.
p <- plot_ly(q4, x = ~shares_categorical, y = ~num_ext_refs, type = "bar", name = "num_ext_refs", opacity = 1) %>%
  add_trace(y = ~num_self_hrefs, name = "num_self_hrefs") %>%
  layout(xaxis = list(title = "shares_categorical"), yaxis = list(title = "Count"), barmode = "relative")
ggplotly(p)



##### 10.
p <- plot_ly(df, y = ~average_token_length, x = ~shares_categorical, color = ~shares_categorical, type = "box", boxpoints = "all")
ggplotly(p)



##### 11.
# Not working
#df_6 <- df %>% 
#  select(Channel, shares, shares_categorical) %>%
#  pivot_wider(names_from = shares_categorical, values_from = shares, values_fn = mean)
#
#Error in `values[spec$.name]`:
#  ! Can't subset columns with `spec$.name`.
#✖ Subscript `spec$.name` can't contain the empty string.
#✖ It has an empty string at location 6.
#

# Working
df_6 <- df %>%
  select(Channel, shares, shares_categorical) %>%
  mutate(shares_categorical = ifelse(shares_categorical == "", "N/A", shares_categorical)) %>%
  pivot_wider(names_from = shares_categorical, values_from = shares, values_fn = mean)



##### 12.
df_6 <- df %>%
  select(Channel, shares_categorical) %>%
  group_by(Channel, shares_categorical) %>%
  summarise(Counts = n()) %>%
  ungroup()



##### 13.
pivot_df <- df_6 %>%
  mutate(shares_categorical = ifelse(shares_categorical == "", "N/A", shares_categorical)) %>%
  pivot_wider(names_from = shares_categorical, values_from = Counts, values_fn = mean)



##### 14.
p <- plot_ly(df_6, x = ~Channel, y = ~Counts, color = ~shares_categorical, type = "bar", barmode = "group")
ggplotly(p)



##### 15.
df_61 <- df_6 %>%
  group_by(Channel) %>%
  summarise(Counts = sum(Counts)) %>%
  ungroup()

newdf <- df_61 %>%
  slice(rep(row_number(), each = 5)) %>%
  mutate(Counts = rep(Counts, length.out = n()))

df_6 <- bind_rows(df_6, newdf)



##### 16.
df_61 <- df_6 %>%
  group_by(Channel) %>%
  summarise(Counts = sum(Counts)) %>%
  ungroup()

newdf <- rep(unlist(df_61), each = 5)
names(newdf) <- colnames(df)

#df_6 <- data.frame(df_6, newdf)




##### 17.
#print(df_6)

##### 18.
#p <- plot_ly(df_6, x = ~Channel, y = ~pct, color = ~shares_categorical, type = "bar", barmode = "relative")
#ggplotly(p)


