
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

# -------------------------------------------------------------------- graphics --------------------------------------
#Corrélation entre variables numériques et le prix

library(corrplot)

# Extraire uniquement les variables numériques
numeric_vars <- df[, sapply(df, is.numeric)]
cor_mat <- cor(numeric_vars, use = "complete.obs")

corrplot(cor_mat, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Corrélation entre variables numériques",
         addCoef.col = "black", number.cex = 0.7)

#Prix en fonction du nombre de personnes accueillies (accommodates)

library(ggplot2)

ggplot(df, aes(x = factor(accommodates), y = price)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Prix par capacité d'accueil",
       x = "Nombre de personnes",
       y = "Prix") +
  theme_minimal()

#Effet du nombre de salles de bain sur le prix

ggplot(df, aes(x = factor(round(bathrooms)), y = price)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Prix selon le nombre de salles de bain",
       x = "Nombre de salles de bain",
       y = "Prix") +
  theme_minimal()

#Prix moyen par type de logement (property_type)

# Afficher uniquement les 10 types de logement les plus fréquents
top_types <- names(sort(table(df$property_type), decreasing = TRUE))[1:10]

df_top_types <- df[df$property_type %in% top_types, ]

ggplot(df_top_types, aes(x = reorder(property_type, price, FUN = median), y = price)) +
  geom_boxplot(fill = "lightskyblue") +
  coord_flip() +
  labs(title = "Prix selon le type de logement",
       x = "Type de logement", y = "Prix") +
  theme_minimal()

#Lien entre score de review et prix

ggplot(df, aes(x = review_scores_value, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Score des avis vs Prix",
       x = "Score des avis (valeur)", y = "Prix") +
  theme_minimal()

#Histogramme groupé des prix par type de chambre

ggplot(df, aes(x = price, fill = factor(room_type))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
  labs(title = "Distribution des prix par type de chambre",
       x = "Prix", fill = "Type de chambre") +
  theme_minimal()

# ---------------------------------------------------------------- shiny app --------------------------------------------------------------------------
library(shiny)

ui <- fluidPage(
  titlePanel("Prédiction du Prix d'un Logement à Paris"),
  
  sidebarLayout(
    sidebarPanel(
      # numericInput("latitude", "Latitude:", value = 48.8566, min = -90, max = 90),
      # numericInput("longitude", "Longitude:", value = 2.3522, min = -180, max = 180),
      
      selectInput("neighbourhood", "Quartier:", choices = c("Batignolles-Monceau", "Bourse", "Buttes-Chaumont", "Buttes-Montmartre", "Élysée", 
                                                            "Entrepôt", "Gobelins", "Hôtel-de-Ville", "Louvre", "Luxembourg", 
                                                            "Ménilmontant", "Observatoire", "Opéra", "Palais-Bourbon", "Panthéon", 
                                                            "Passy", "Popincourt", "Reuilly", "Temple", "Vaugirard")),
      
      selectInput("room_type", "Type de Chambre:", choices = c("Entire home/apt", "Hotel room", "Private room", "Shared room")),
      
      numericInput("accommodates", "Nombre d'occupants:", value = 2, min = 1, max = 16),
      numericInput("bathrooms", "Nombre de salles de bains:", value = 1, min = 1, max = 10),
      numericInput("bedrooms", "Nombre de chambres:", value = 1, min = 1, max = 10),
      numericInput("beds", "Nombre de lits:", value = 1, min = 1, max = 10),
      numericInput("number_of_reviews", "Nombre d'avis:", value = 50, min = 0, max = 5000),
      numericInput("review_scores_value", "Note des avis:", value = 8, min = 0, max = 10),
      
      actionButton("predict", "Prédire le Prix")
    ),
    
    mainPanel(
      h3("Prix Prédit :"),
      verbatimTextOutput("predicted_price")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$predict, {
    neighbourhood_cleansed <- switch(input$neighbourhood,
                                     "Batignolles-Monceau" = 1,
                                     "Bourse" = 2,
                                     "Buttes-Chaumont" = 3,
                                     "Buttes-Montmartre" = 4,
                                     "Élysée" = 5,
                                     "Entrepôt" = 6,
                                     "Gobelins" = 7,
                                     "Hôtel-de-Ville" = 8,
                                     "Louvre" = 9,
                                     "Luxembourg" = 10,
                                     "Ménilmontant" = 11,
                                     "Observatoire" = 12,
                                     "Opéra" = 13,
                                     "Palais-Bourbon" = 14,
                                     "Panthéon" = 15,
                                     "Passy" = 16,
                                     "Popincourt" = 17,
                                     "Reuilly" = 18,
                                     "Temple" = 19,
                                     "Vaugirard" = 20)
    
    room_type <- switch(input$room_type,
                        "Entire home/apt" = 0,
                        "Hotel room" = 1,
                        "Private room" = 2,
                        "Shared room" = 3)
    
    new_data <- data.frame(
      # latitude = input$latitude,
      # longitude = input$longitude,
      neighbourhood_cleansed = neighbourhood_cleansed,
      room_type = room_type,
      accommodates = input$accommodates,
      bathrooms = input$bathrooms,
      bedrooms = input$bedrooms,
      beds = input$beds,
      number_of_reviews = input$number_of_reviews,
      review_scores_value = input$review_scores_value
    )
    
    predicted_price <- predict(modele_multiple, newdata = new_data)
    
    output$predicted_price <- renderText({
      paste("Le prix prédit est : ", round(predicted_price, 2), "€")
    })
  })
}

shinyApp(ui = ui, server = server)
