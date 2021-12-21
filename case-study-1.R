
rm(list=ls()) # Clean up R's memory

source("gelicopt/sign-mappers.R")
source("gelicopt/guessability.R")
source("gelicopt/inference.R")
source("gelicopt/prediction.R")

library(ggplot2)
library(gridExtra) # To plot the two graphs sie by side

# Here, as example we use a real dataset by Baily et al. (2013)
# This is a special study. Notice that keys and gestures can be repeated multiple times (in separation).
# However, a certain combination of a key and a gesture should only appear once
data <- read.csv("datasets/bailly2013.csv", stringsAsFactors=F)

# For each participant, there are five columns, where the first captures the key and the second captures the gesture  
keys <- data[, seq(2, ncol(data), by=5)] # These are participants' proposals of keys
gestures <- data[, seq(3, ncol(data), by=5)] # These are participants' proposals of key gestures

# Also build a table with keys + gestures combined.
keys_gestures = data.frame(keys)
for (r in 1:nrow(keys_gestures)) {
    keys_gestures[r,] = paste0(keys_gestures[r,], "-", gestures[r,])
}

# Replace the column names by participant IDs
pIDs = paste0("P", 1:ncol(keys))
names(keys_gestures) <- pIDs

# Replace the row names by referent IDs
row.names(keys_gestures) <- data$cmd

# Create the bootstrapping distribution. By default, it contains R = 200 samples, but you could set R to a different number.
boot_samples <- bootstrap(keys_gestures, hungarian_sign_mapper)
signs <- getSigns(boot_samples, conf.min = .1) # Show only signs with at least 10% confidence

sign.names <- row.names(keys_gestures)
for(i in 1: length(signs)){
  cat(sign.names[i], ": ", flatten(signs[i]),"\n")
}  

mappings <- hungarian_sign_mapper(keys_gestures)

guess <- guessability(mappings, keys_gestures)
cat("\nGuessability =", guess, "\n")

guess <- guessability.loocv(keys_gestures, hungarian_sign_mapper)
cat("LOOCV Guessability =", guess, "\n")

# Learning curves and prediction
gevolution <- guessability.evolution(keys_gestures, hungarian_sign_mapper)

plot <- ggplot(gevolution, aes(x = N))+
  geom_line(aes(y = validation*100), color = "orange") +
  geom_line(aes(y = training*100), color = "steelblue") +
  ylab("Guessability (%)") + 
  xlab("Participants") +
  theme( panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  annotate("text", x = 9, y = 33, label = "training curve", size = 3, hjust = 0) +
  annotate("text", x = 9, y = 17, label = "cross-validation curve", size = 3, hjust = 0) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 22, 2), limits = c(0, 22)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40))

trainingDatasets <- c("training/synthetic-hungarian-1.csv", "training/synthetic-hungarian-2.csv")
lmodel <- buildModel(trainingDatasets)
predicted <- predictError(lmodel, gevolution)
gevolution$fit <- predicted[,"fit"] 
gevolution$lwr <- predicted[,"lwr"] 
gevolution$upr <- predicted[,"upr"] 

plot_error <- ggplot(gevolution, aes(x = N)) +
  geom_line(aes(y = (training - validation)*100), color = "chartreuse4") +
  geom_line(aes(y = fit*100), color = "blue") +
  geom_line(linetype = 2, aes(y = lwr*100), color = "blue") +
  geom_line(linetype = 2, aes(y = upr*100), color = "blue") +
  ylab("Guessability Error (%)") + xlab("Participants") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  annotate("text", x = 9, y = 13, label = "training minus cross-validation curve", size = 3, hjust = 0) +
  annotate("text", x = 4, y = 9.5, label = "predicted error", size = 3, hjust = 0) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 22, 2), limits = c(0, 22)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25))

grid.arrange(plot, plot_error, ncol=2)
