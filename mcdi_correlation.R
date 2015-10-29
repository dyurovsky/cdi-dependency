library(dplyr)
library(wordbankr)
library(purrr)
library(tidyr)
library(langcog)
library(ggplot2)

#Load ENGLISH_WS Data
english_ws_admins <- get_administration_data("English", "WS")

english_ws_items <- get_item_data("English", "WS")

english_ws_data <- get_instrument_data(instrument_language = "English",
                                   instrument_form = "WS",
                                   administrations = english_ws_admins,
                                   iteminfo = TRUE) %>%
  filter(type == "word") 




# Make Correlation Matrix
table_data <- english_ws_data %>%
  mutate(value = ifelse(value == "produces", 1, 0))

words <- unique(table_data$item_id)

cor_matrix <- table_data %>%
  select(item_id, data_id, value) %>%
  group_by(data_id) %>%
  spread(item_id, value) %>%
  select(-data_id) %>%
  cor(use = "complete") %>%
  as.data.frame()

cor_matrix$item <- row.names(cor_matrix) 
  

# Tidy Matrix
tidy_matrix <- cor_matrix %>%
  gather(second_item, correlation, -item) 

row.names(tidy_matrix) <- NULL

ordered_matrix <- tidy_matrix %>%
  filter(!item == second_item) %>%
  arrange(item, second_item) %>%
  as.tbl %>%
  left_join(select(english_ws_items, item_id, lexical_class), 
            by = c("second_item" = "item_id")) %>%
  rename(second_lexical_class = lexical_class) %>%
  left_join(select(english_ws_items, item_id, lexical_class), 
            by = c("item" = "item_id"))



# Compute category correlation
category_correlations <- ordered_matrix %>%
  group_by(lexical_class, second_lexical_class) %>%
  multi_boot_standard("correlation", na.rm = T)

category_heatmap <- category_correlations %>%
  select(lexical_class, second_lexical_class, mean) %>%
  spread(second_lexical_class, mean) 
row.names(category_heatmap) <- category_heatmap$lexical_class
category_heatmap$lexical_class <- NULL
category_heatmap <- as.matrix(category_heatmap)
heatmap(category_heatmap)

ggplot(category_correlations,aes(x = lexical_class, y = mean, color = second_lexical_class)) +
  geom_pointrange(aes(ymin = ci_lower,
                      ymax = ci_upper),
                  size = 1,
                  position = position_dodge(.1)) +
  theme_bw(base_size=16) +
  theme(panel.grid = element_blank(),
        legend.position = "none") + 
  scale_color_brewer(palette = "Set1")



ggplot(category_correlations,aes(x = lexical_class, y = second_lexical_class)) +
  geom_tile(aes(fill = mean)) +
  scale_fill_gradient(low="white", high="red") +
  theme_bw(base_size=16) +
  theme(panel.grid = element_blank()) + 
  scale_color_brewer(palette = "Set1")
