# Set my working directory
setwd("/home/mario/Dropbox/Teaching/Essex/2020-2021/MA334_Data_Analysis_and_Statistics_in_R/Labs/Lab_10_Linear_Regression")

#Import CSV
rawdf <- read.csv("election-context-2018.csv")

str(rawdf)

demographics <- names(rawdf)[24:38]

rawdf$dem16_pct <- rawdf$clinton16 / (rawdf$trump16 + rawdf$clinton16 + rawdf$otherpres16) * 100
rawdf$rep16_pct <- rawdf$trump16 / (rawdf$trump16 + rawdf$clinton16 + rawdf$otherpres16) * 100

library(dplyr)

# Define the columns wanted in a vector
cols_to_keep <- c(c("state","county","fips","dem16_pct","rep16_pct"),demographics)

# Select only the columns wanted
df <- select(rawdf, cols_to_keep)

# Summary Statistics
summary(df)

# Remove rows with NA's
df <- na.omit(df)

### CORRELATION ###

library(ggplot2)
library(cowplot) # This library allows us to use the polot_grid() function

options(repr.plot.width=6, repr.plot.height=3) # These are settings for the size of the output plot

p1 <- ggplot(df, aes(x=white_pct, y=dem16_pct)) + geom_point(col=4, size=0.5) + ylim(0,100) + labs(x="% White Pop.",y="% Democratic Vote")
p2 <- ggplot(df, aes(x=white_pct, y=rep16_pct)) + geom_point(col=2, size=0.5) + ylim(0,100) + labs(x="% White Pop.",y="% Republican Vote") 

plot_grid(p1, p2, ncol=2) # This places the two independent plots in grid (2 x 1 in this case)


corr_dem_white <- cor.test(df$dem16_pct,df$white_pct, method="pearson") # Correlation % democrats - % white
print(corr_dem_white)

corr_rep_white <- cor.test(df$rep16_pct,df$white_pct, method="pearson") # Correlation % republican - % white
print(corr_rep_white)

### LINEAR REGRESSION ###

linearModelRepWhite <- lm(rep16_pct ~ white_pct, data=df)
summary(linearModelRepWhite)

A <- as.numeric(linearModelRepWhite$coefficients[2])
paste0("A = ", round(A,2))

B <- as.numeric(linearModelRepWhite$coefficients[1])
paste0("B = ", round(B,2))

R2 <- summary(linearModelRepWhite)$adj.r.squared
paste0("R2 = ", round(R2,2))

# Generate the text for the equation
eq_text = paste0("Y = ",round(A,2),"X + ",round(B,2))

options(repr.plot.width=4, repr.plot.height=4) # These are settings for the size of the output plot

ggplot(df, aes(x=white_pct, y=rep16_pct)) + 
  geom_point(col=2, size=0.5) + ylim(0,100) + labs(x="% White Pop.",y="% Republican Vote") + 
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_text(x = 20, y = 95, label = eq_text, size=3)


### RESIDUALS ###

#Computing the MAE for % Republican Vote - % White Pop.
MAE <- mean(abs(linearModelRepWhite$residuals))
paste0("MAE Republican-White = ", round(MAE,2), "%")

# Get the values from X and residuals from the linear model output
residuals <- linearModelRepWhite$residuals
X <- linearModelRepWhite$fitted.values
# Create a data frame with the values
dfres <- data.frame(X,residuals)

options(repr.plot.width=4, repr.plot.height=4) # These are settings for the size of the output plot
ggplot(dfres, aes(x=X, y=residuals)) + geom_point(col=4, size=0.5) + geom_hline(yintercept=0, lwd=1) + ylim(-50,50) + labs(title="Residuals Plot")

# Shapiro-Wilk normality test for the residuals of linearModelRepWhite
shapiro.test(linearModelRepWhite$residuals)
