rm(list=ls(all.names=T))

library(tidyverse)
library(ggplot2)
library(MASS)
library(RColorBrewer)

library(carData)
library(car)
library(alr4)

library(zipcode)
data("zipcode")

dt_location <- '/Users/nantang/Google Drive/STAT 423/Project-Proposal'
setwd(dt_location)

# prepare dataset
house_dt <- read.csv('kc_house_data.csv')

# remove variable 'id', 'date', 'sqft_living', 'sqft_lot'
house_dt <- house_dt[, -c(1, 2, 6, 7)]

# check missing values
NA_index <- numeric(ncol(house_dt))
for (i in 1:ncol(house_dt)) {
  NA_index[i] <- length(which(is.na(house_dt[,i])))
}
NA_index ## no missing value


### function remove outliers 
outliers_remove <- function(dt, variable_dt) {
  outliers_value <- boxplot.stats(variable_dt)$out
  new_dt <- dt[-which(variable_dt %in% outliers_value),]
  return(new_dt)
}
###

# remove bedroom outliers 
house_dt <- outliers_remove(house_dt, house_dt$bedrooms)

# remove sqft_living15 outliers
house_dt <- outliers_remove(house_dt, house_dt$sqft_living15)

# remove sqft_lot15 outliers
house_dt <- outliers_remove(house_dt, house_dt$sqft_lot15)

# remove sqft_above outliers
house_dt <- outliers_remove(house_dt, house_dt$sqft_above)

# remove sqft_basement outliers
house_dt <- outliers_remove(house_dt, house_dt$sqft_basement)


# correlation matrix before transform
house_corr <- cor(house_dt)
house_corr_df <- melt(house_corr)
house_corr_df <- house_corr_df %>% 
  mutate(value = replace(value, value==1, NA))

col_fill <- c('red', 'blue')

p1 <- ggplot(data=house_corr_df, mapping=aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white', size=0.75) + 
  scale_fill_gradient2(midpoint=0, low='#edf8b1', mid="#41b6c4",
                       high="#253494", na.value='gray95') +
  ggtitle('Matrix of Correlation Coefficients') +
  theme (
    axis.text.x = element_text(angle=45, vjust=0.6),
    axis.text.y = element_text(angle=0, hjust=0.6), 
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text( hjust=0.5, vjust=0),
    legend.text = element_text(face='bold'),
    legend.key.width = grid::unit(0.3,'cm'),
    panel.background = element_blank(),
    panel.border=element_blank(),
    panel.grid.major = element_blank()
  )

width = 7
ggsave("cm_pre.pdf",
       width=width,
       height=width/1.2)


# zipcode clustering
zip_city <- zipcode %>%
  select(zipcode=zip, city) 

house_dt$zipcode <- as.character(house_dt$zipcode)
  
house_dt <- left_join(house_dt, zip_city, by='zipcode')

house_dt$city <- as.factor(house_dt$city)


# data transform and check co-linearity
house_dt_new <- house_dt %>% 
  mutate(room_avg_sqft = sqft_above / (bedrooms + bathrooms + 1) ) %>%
  mutate(house_age = (2015 - yr_built)) %>%
  mutate(renovated = ifelse(yr_renovated==0, 0, 1)) %>%
  select(-grade, -bedrooms, -bathrooms, -sqft_above, -zipcode, -sqft_living15, -yr_built, -yr_renovated)


#  visualizing correlation matrix
house_corr <- cor(house_dt_new)
house_corr_df <- melt(house_corr)
house_corr_df <- house_corr_df %>% 
  mutate(value = replace(value, value==1, NA))

col_fill <- c('red', 'blue')

p2 <- ggplot(data=house_corr_df, mapping=aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color='white', size=0.75) + 
  scale_fill_gradient2(midpoint=0, low='#c7e9b4', mid="#41b6c4",
                       high="#225ea8", na.value='gray95') +
  ggtitle('Matrix of Correlation Coefficients') +
  theme (
    axis.text.x = element_text(angle=45, vjust=0.6),
    axis.text.y = element_text(angle=0, hjust=0.6), 
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text( hjust=0.5, vjust=0),
    legend.text = element_text(face='bold'),
    legend.key.width = grid::unit(0.3,'cm'),
    panel.background = element_blank(),
    panel.border=element_blank(),
    panel.grid.major = element_blank()
  )

ggsave("cm_post.pdf",
       width=width,
       height=width/1.2)

# set factor 
house_dt_new$waterfront <- as.factor(house_dt_new$waterfront)
house_dt_new$renovated <- as.factor(house_dt_new$renovated)
  

# box-cox for linearity 
par(mfrow=c(1,1))
lm_fit1 <- lm(data=house_dt_new, formula = price ~.)
bc <- boxcox(lm_fit1, plotit = F)
lambda <- bc$x[which.max(bc$y)]
lambda <- -0.1

# backward step, BIC
lm_fit1 <- lm(data=house_dt_new, formula = price ~.)
step(object=lm_fit1, direction='backward', k=log(nrow(house_dt_new)))
## all should be included 

# forward step, BIC
lm_empty <- lm(formula=price ~ 1, data=house_dt_new)
step(object=lm_empty, direction='forward', k=log(nrow(house_dt_new)), scope=list(upper=lm_fit1, lowwer=lm_empty))
## same as backward selection

write.csv(house_dt_new, 'house_dt_new.csv')



