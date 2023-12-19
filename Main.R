library(ggplot2)
library(caret)
library(utils)

data <- read.csv('musicDataList.csv')
data[data == ""] <- NA

# this is the actual music data
musicData <- na.omit(data)

#create a column for the data:
musicData$Emotion <- character(nrow(musicData))

for (i in 1:nrow(musicData)) {
  if (musicData[i, "Valence"] <= 100 && musicData[i, "Valence"] > 80) 
  {
    if (musicData[i, "Energetic"] <= 100 && musicData[i, "Energetic"] > 80) 
    {
      musicData$Emotion[i] <- "Exuberant"
    } 
    else if (musicData[i, "Energetic"] <= 80 && musicData[i, "Energetic"] > 60) 
    {
      musicData$Emotion[i] <- "Hyper"
    } 
    else if (musicData[i, "Energetic"] <= 60 && musicData[i, "Energetic"] > 40) 
    {
      musicData$Emotion[i] <- "Lively"
    } 
    else if (musicData[i, "Energetic"] <= 40 && musicData[i, "Energetic"] > 20) 
    {
      musicData$Emotion[i] <- "Energetic"
    } 
    else 
    {
      musicData$Emotion[i] <- "Relaxed"
    }
  } 
  else if (musicData[i, "Valence"] <= 80 && musicData[i, "Valence"] > 60) 
  {
    if (musicData[i, "Energetic"] <= 100 && musicData[i, "Energetic"] > 80) 
    {
      musicData$Emotion[i] <- "Elated"
    } 
    else if (musicData[i, "Energetic"] <= 80 && musicData[i, "Energetic"] > 60) 
    {
      musicData$Emotion[i] <- "Intense"
    } 
    else if (musicData[i, "Energetic"] <= 60 && musicData[i, "Energetic"] > 40) 
    {
      musicData$Emotion[i] <- "Active"
    } 
    else if (musicData[i, "Energetic"] <= 40 && musicData[i, "Energetic"] > 20) 
    {
      musicData$Emotion[i] <- "Cheerful"
    } 
    else 
    {
      musicData$Emotion[i] <- "Mild"
    }
  } 
  else if (musicData[i, "Valence"] <= 60 && musicData[i, "Valence"] > 40) 
  {
    if (musicData[i, "Energetic"] <= 100 && musicData[i, "Energetic"] > 80) 
    {
      musicData$Emotion[i] <- "Vibrant"
    } 
    else if (musicData[i, "Energetic"] <= 80 && musicData[i, "Energetic"] > 60) 
    {
      musicData$Emotion[i] <- "Dynamic"
    } 
    else if (musicData[i, "Energetic"] <= 60 && musicData[i, "Energetic"] > 40) 
    {
      musicData$Emotion[i] <- "Stable"
    } 
    else if (musicData[i, "Energetic"] <= 40 && musicData[i, "Energetic"] > 20) 
    {
      musicData$Emotion[i] <- "Balanced"
    } 
    else 
    {
      musicData$Emotion[i] <- "Serene"
    }
  } 
  else if (musicData[i, "Valence"] <= 40 && musicData[i, "Valence"] > 20) 
  {
    if (musicData[i, "Energetic"] <= 100 && musicData[i, "Energetic"] > 80) 
    {
      musicData$Emotion[i] <- "Frenetic"
    } 
    else if (musicData[i, "Energetic"] <= 80 && musicData[i, "Energetic"] > 60) 
    {
      musicData$Emotion[i] <- "Excited"
    } 
    else if (musicData[i, "Energetic"] <= 60 && musicData[i, "Energetic"] > 40) 
    {
      musicData$Emotion[i] <- "Moderate"
    } 
    else if (musicData[i, "Energetic"] <= 40 && musicData[i, "Energetic"] > 20) 
    {
      musicData$Emotion[i] <- "Mellow"
    } 
    else 
    {
      musicData$Emotion[i] <- "Calm"
    }
  } 
  else 
  {
    if (musicData[i, "Energetic"] <= 100 && musicData[i, "Energetic"] > 80) 
    {
      musicData$Emotion[i] <- "Agitated"
    } 
    else if (musicData[i, "Energetic"] <= 80 && musicData[i, "Energetic"] > 60) S
    {
      musicData$Emotion[i] <- "Restless"
    } 
    else if (musicData[i, "Energetic"] <= 60 && musicData[i, "Energetic"] > 40) 
    {
      musicData$Emotion[i] <- "Dull"
    } 
    else if (musicData[i, "Energetic"] <= 40 && musicData[i, "Energetic"] > 20) 
    {
      musicData$Emotion[i] <- "Tired"
    } 
    else 
    {
      musicData$Emotion[i] <- "Sluggish"
    }
  }
}

#refresh the data stated
musicData <- musicData

#region | Significance
shapiro.test(musicData$Valence)
shapiro.test(musicData$Energetic)


