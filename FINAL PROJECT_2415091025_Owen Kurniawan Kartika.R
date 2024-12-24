# Library
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

# Step 1: Load the data into R
crop.data <- read.csv("C:/Users/Owen Kartika/Documents/R/Sample/crop.data.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric"))
summary(crop.data)

# Step 2: Perform the ANOVA Test
# ONE-WAY ANOVA
one.way <- aov(yield ~ fertilizer, data = crop.data)
summary(one.way)

# TWo-WAY ANOVA
two.way <- aov(yield ~ fertilizer + density, data = crop.data)
summary(two.way)

# Adding interaction between variables
interaction <- aov(yield ~ fertilizer*density, data = crop.data)
summary(interaction)

# Adding a blocking variables
blocking <- aov(yield ~ fertilizer + density + block, data = crop.data)
summary(blocking)

# Step 3: Find the best fit models
library(AICcmodavg)
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names)

# Step 4: Check for homoscedasticity
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

# Step 5: Do a post-hoc test
tukey.two.way<-TukeyHSD(two.way)
tukey.two.way

# Step 6: Plot the results in a graph
# Find the groupwise differences
tukey.plot.aov<-aov(yield ~ fertilizer:density, data=crop.data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

# Make a data frame with the group labels
mean.yield.data <- crop.data %>%
  group_by(fertilizer, density) %>%
  summarise(
    yield = mean(yield)
  )
mean.yield.data$group <- c("a","b","b","b","b","c")

mean.yield.data

# Plot the raw data
two.way.plot <- ggplot(crop.data, aes(x = density, y = yield, group=fertilizer)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot

# Add the means and standard errors to the graph
two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.yield.data, aes(x=density, y=yield))

two.way.plot

# Split up the data
two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) +
  facet_wrap(~ fertilizer)

two.way.plot

# Make the graph ready for publication
two.way.plot <- two.way.plot +
  theme_classic2() +
  labs(title = "Crop yield in response to fertilizer mix and planting density",
       x = "Planting density (1=low density, 2=high density)",
       y = "Yield (bushels per acre)")

two.way.plot

# Interpretation
# We found a statistically-significant difference in average crop yield by both fertilizer type (F(2)=9.018, p < 0.001) and by planting density (F(1)=15.316, p < 0.001).
# A Tukey post-hoc test revealed that fertilizer mix 3 resulted in a higher yield on average than fertilizer mix 1 (0.59 bushels/acre), and a higher yield on average than fertilizer mix 2 (0.42 bushels/acre). Planting density was also significant, with planting density 2 resulting in an higher yield on average of 0.46 bushels/acre over planting density 1.
# A subsequent groupwise comparison showed the strongest yield gains at planting density 2, fertilizer mix 3, suggesting that this mix of treatments was most advantageous for crop growth under our experimental conditions.
