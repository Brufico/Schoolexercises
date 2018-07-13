#' ---
#' title: Galton's heights data
#' subtitle: downloadable at: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/T0HSJ1
#' author: sir Francis Galton
#' date: 1885
#' ---


#' gownloadable 
#' https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/T0HSJ1/WO9T4N
#' 

#' Description (Help galton {UsingR})
#' 
#' * Data set from tabulated data set used by Galton in 1885 to study the relationship between a 
#' parent's height and their childrens.
#' 
#' * Usage
#' 
#' data(galton)
#' 
#' * Format
#' 
#' child
#' The child's height
#' 
#' parent
#' The “midparent” height
#' 
#' * Details
#' The midparent's height is an average of the fathers height and 1.08 times the mother's. 
#' In the data there are 205 different parents and 928 children. The data here is truncated at the ends for 
#' both parents and children so that it can be treated as numeric data. 
#' The data were tabulated and consequently made discrete. 
#' The father.son data set is similar data used by Galton and is continuous.
#' 
#' 

library(UsingR)
library(reshape2)
library(dplyr)

# help("GaltonFamilies") == see this data

data(galton)

head(galton)
tail(galton)
summary(galton)

long_galton <- melt(galton)

head(long_galton)
tail(long_galton)


# general theme settings
theme_set(new = theme_grey() + 
            theme(panel.background =element_rect(fill = "lightgrey"),
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5))
          )

#  density plots
gdens <- ggplot(long_galton, aes(value, group = variable)) + 
  geom_density( aes(fill = variable), alpha = 0.4, adjust = 1.4) +
  labs(x = "height", y = "densité", title = "Taille des enfants et des parents")
gdens


floor(min(galton$child))
max(galton$child)

#  scatterplot

gscat <- ggplot(data = galton, mapping = aes(x = parent, y = child)) + 
  geom_jitter(width = 0.5, height = 0.5, alpha = 0.5) + 
  scale_x_continuous("Parent's mid_height (inches)", 
                     minor_breaks = seq(floor(min(galton$child)), 
                                        max(galton$child), by = 0.5 )
                       ) +
  scale_y_continuous("Child's height (inches)", 
                     minor_breaks = seq(floor(min(galton$child)), 
                                        max(galton$child) + 1, by = 0.5 )
  ) +
  labs(title = "Taille des enfants et des parents")
gscat


gscat_loess <- gscat + geom_smooth() +
  labs(title = "Taille des enfants et des parents, avec fonction de moyenne")
gscat_loess

gscat_lm <- gscat + geom_smooth(method = "lm")+
  labs(title = "Taille des enfants et des parents, avec fonction de moyenne")
gscat_lm

# model
g_mod <- lm(formula = child ~ parent, data = galton)

gscat_lm_diag <- gscat_lm + 
  geom_abline(slope = 1, intercept = 0, #mean(galton$child) - mean(galton$parent),
              color = "red") + geom_text( x= 72.5, y = 72.5, label = "child = parent", color = "red")
gscat_lm_diag
