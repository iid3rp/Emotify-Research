library(ggplot2)
library(caret)
library(utils)
library(randomForest)

file_path1 <- 'musicDataList2.csv'
file_path2 <- 'musicDataList.csv'

if (file.exists(file_path1))
  data <- read.csv(file_path1)
else
  data <- read.csv(file_path2)

data[data == ""] <- NA

# this is the actual music data
musicData <- na.omit(data)

#create a column for the data:
musicData$Emotion <- character(nrow(musicData))

for (i in 1:nrow(musicData)) {
  if (musicData[i, "Valence"] <= 100 && musicData[i, "Valence"] > 80) 
  {
    if (musicData[i, "Energetic"] <= 100 && musicData[i, "Energetic"] > 80) 
      musicData$Emotion[i] <- "Exuberant"
    
    else if (musicData[i, "Energetic"] <= 80 && musicData[i, "Energetic"] > 60) 
      musicData$Emotion[i] <- "Hyper"
    
    else if (musicData[i, "Energetic"] <= 60 && musicData[i, "Energetic"] > 40) 
      musicData$Emotion[i] <- "Lively"
    
    else if (musicData[i, "Energetic"] <= 40 && musicData[i, "Energetic"] > 20) 
      musicData$Emotion[i] <- "Energetic"
    
    else
      musicData$Emotion[i] <- "Relaxed"
  } 
  else if (musicData[i, "Valence"] <= 80 && musicData[i, "Valence"] > 60) 
  {
    if (musicData[i, "Energetic"] <= 100 && musicData[i, "Energetic"] > 80) 
      musicData$Emotion[i] <- "Elated"

    else if (musicData[i, "Energetic"] <= 80 && musicData[i, "Energetic"] > 60) 
      musicData$Emotion[i] <- "Intense"

    else if (musicData[i, "Energetic"] <= 60 && musicData[i, "Energetic"] > 40) 
      musicData$Emotion[i] <- "Active"

    else if (musicData[i, "Energetic"] <= 40 && musicData[i, "Energetic"] > 20) 
      musicData$Emotion[i] <- "Cheerful"

    else 
      musicData$Emotion[i] <- "Mild"
  } 
  else if (musicData[i, "Valence"] <= 60 && musicData[i, "Valence"] > 40) 
  {
    if (musicData[i, "Energetic"] <= 100 && musicData[i, "Energetic"] > 80) 
      musicData$Emotion[i] <- "Vibrant"

    else if (musicData[i, "Energetic"] <= 80 && musicData[i, "Energetic"] > 60) 
      musicData$Emotion[i] <- "Dynamic"

    else if (musicData[i, "Energetic"] <= 60 && musicData[i, "Energetic"] > 40) 
      musicData$Emotion[i] <- "Stable"

    else if (musicData[i, "Energetic"] <= 40 && musicData[i, "Energetic"] > 20) 
      musicData$Emotion[i] <- "Balanced"

    else 
      musicData$Emotion[i] <- "Serene"
  } 
  else if (musicData[i, "Valence"] <= 40 && musicData[i, "Valence"] > 20) 
  {
    if (musicData[i, "Energetic"] <= 100 && musicData[i, "Energetic"] > 80) 
      musicData$Emotion[i] <- "Frenetic"

    else if (musicData[i, "Energetic"] <= 80 && musicData[i, "Energetic"] > 60) 
      musicData$Emotion[i] <- "Excited"
    
    else if (musicData[i, "Energetic"] <= 60 && musicData[i, "Energetic"] > 40) 
      musicData$Emotion[i] <- "Moderate"

    else if (musicData[i, "Energetic"] <= 40 && musicData[i, "Energetic"] > 20) 
      musicData$Emotion[i] <- "Mellow"

    else 
      musicData$Emotion[i] <- "Calm"
  } 
  else 
  {
    if (musicData[i, "Energetic"] <= 100 && musicData[i, "Energetic"] > 80) 
      musicData$Emotion[i] <- "Agitated"
    
    else if (musicData[i, "Energetic"] <= 80 && musicData[i, "Energetic"] > 60) 
      musicData$Emotion[i] <- "Restless"

    else if (musicData[i, "Energetic"] <= 60 && musicData[i, "Energetic"] > 40) 
      musicData$Emotion[i] <- "Dull"

    else if (musicData[i, "Energetic"] <= 40 && musicData[i, "Energetic"] > 20) 
      musicData$Emotion[i] <- "Tired"
    
    else 
      musicData$Emotion[i] <- "Sluggish"
  }
}

#refresh the data stated
musicData <- musicData

# oh btw the refreshed data should be added after the raw data is going to be added
write.csv(musicData, file = "musicDataList2.csv")

# region | Significance
shapiro.test(musicData$Valence)
shapiro.test(musicData$Energetic)

# Convert 'Emotion' to a factor
musicData$Emotion <- factor(musicData$Emotion)

# Create a boxplot to visualize the distributions
boxplot(Valence ~ Emotion, data = musicData, main = "Valence by Emotion")

wilcoxon_test_result <- wilcox.test(musicData$Valence, musicData$Energetic)
print(wilcoxon_test_result)

# Wilcoxon rank-sum test for Energetic
energetic_test <- pairwise.wilcox.test(musicData$Energetic, musicData$Emotion, p.adjust.method = "BH")
print(energetic_test)

# scatter plot with regression line
ggplot(musicData, aes(x = Energetic, y = Valence)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot",
       x = "Valence", y = "Energetic")

# Assuming 'Emotion' is the column you want to predict
targetColumn <- musicData$Emotion
predictors <- setdiff(names(musicData), "Emotion")

# Split the data into training and testing sets
set.seed(42)
train_indices <- createDataPartition(targetColumn, p = 0.8, list = FALSE)
train_data <- musicData[train_indices, ]
test_data <- musicData[-train_indices, ]

# Train the model (Random Forest as an example)
model <- randomForest(train_data[, predictors], train_data[, "Emotion"])

# Make predictions on the test set
predictions <- predict(model, newdata = test_data[, predictors])

# Evaluate the model
accuracy <- confusionMatrix(predictions, test_data[, "Emotion"])$overall["Accuracy"]
cat("Model Accuracy:", accuracy * 100, "%\n")

#we need the data to be at least 95% in testing tbh....
