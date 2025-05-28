
data <- read.csv("listings.csv")

df <- data[, c("neighbourhood_cleansed", "latitude", "longitude", "property_type", "room_type",
               "accommodates", "bathrooms", "bedrooms", "beds", "price", 
               "number_of_reviews", "review_scores_value")]

df$price <- as.numeric(gsub("\\$", "", df$price))

df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Batignolles-Monceau"] <- 1
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Bourse"] <- 2
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Buttes-Chaumont"] <- 3
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Buttes-Montmartre"] <- 4
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Élysée"] <- 5
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Entrepôt"] <- 6
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Gobelins"] <- 7
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Hôtel-de-Ville"] <- 8
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Louvre"] <- 9
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Luxembourg"] <- 10
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Ménilmontant"] <- 11
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Observatoire"] <- 12
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Opéra"] <- 13
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Palais-Bourbon"] <- 14
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Panthéon"] <- 15
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Passy"] <- 16
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Popincourt"] <- 17
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Reuilly"] <- 18
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Temple"] <- 19
df$neighbourhood_cleansed[df$neighbourhood_cleansed == "Vaugirard"] <- 20

df$neighbourhood_cleansed <- as.numeric(df$neighbourhood_cleansed)

df$room_type[df$room_type == "Entire home/apt"] <- 0
df$room_type[df$room_type == "Hotel room"] <- 1
df$room_type[df$room_type == "Private room"] <- 2
df$room_type[df$room_type == "Shared room"] <- 3
df$room_type <- as.numeric(df$room_type)

df <- unique(df)

colonnes_a_remplacer <- c("bathrooms", "bedrooms", "beds", "price", "review_scores_value")
for (colonne in colonnes_a_remplacer) {
  df[[colonne]][is.na(df[[colonne]])] <- mean(df[[colonne]], na.rm = TRUE)
}

handle_outliers <- function(df, columns) {
  for (colonne in columns) {
    Q1 <- quantile(df[[colonne]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[colonne]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    lower_limit <- Q1 - 2.5 * IQR
    upper_limit <- Q3 + 2.5 * IQR
    
    df[[colonne]] <- pmin(pmax(df[[colonne]], lower_limit), upper_limit)
  }
  return(df)
}

colonnes_a_verifier <- c("latitude", "longitude", "accommodates", "bathrooms", "bedrooms", "beds", "price", 
                         "number_of_reviews", "review_scores_value")

df <- handle_outliers(df, colonnes_a_verifier)

set.seed(100)
train_indices <- sample(1:nrow(df), size = 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

modele_multiple <- lm(price ~ neighbourhood_cleansed + room_type + accommodates + bathrooms + bedrooms + beds + number_of_reviews + review_scores_value, data = train_data)

summary(modele_multiple)

predictions_multiple <- predict(modele_multiple, newdata = test_data)

rmse_multiple <- sqrt(mean((predictions_multiple - test_data$price)^2))
print(paste("RMSE (multiple):", rmse_multiple))

rss_multiple <- sum((predictions_multiple - test_data$price)^2)
tss_multiple <- sum((test_data$price - mean(test_data$price))^2)
r_squared_multiple <- 1 - (rss_multiple / tss_multiple)
print(paste("R-squared (multiple):", r_squared_multiple))

# Save the model
saveRDS(modele_multiple, "modele_multiple.rds")