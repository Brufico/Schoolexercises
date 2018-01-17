#' ---
#' title: Exploration of the insteval dataset
#' author "Bruno Fischer Colonimos"
#' output: 
#'     html_document:
#'         number_section: yes
#'         toc: yes
#'         toc_depth: 4
#' ---



#' ***********************************
#' 
#' ```{r setup, include=FALSE}
#' knitr::opts_chunk$set(echo = TRUE)
#' ```
#' 

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))



# load data
# ===========
df <- read.csv(file = file.path("data", "insteval.csv"),
               sep = ";", row.names = 1)

head(df)
tail(df)


# Data exploration
# ===============

# remove row with missing values
# ------------------------------

#  detect
sapply(df, function(col) {sum(is.na(col))} )
# remove last row
df <- df[!is.na(df$studage), ]
#  verify
sapply(df, function(col) {sum(is.na(col))} )



# studage
ggplot(data = df, aes(x = studage)) + geom_bar( aes(y = ..prop..), width = 0.5)

# lectage
ggplot(data = df, aes(x = lectage)) + geom_bar( aes(y = ..prop..), width = 0.5)


# service
ggplot(data = df, aes(x = service)) + geom_bar( aes(y = ..prop..), width = 0.5)


# dept
ggplot(data = df, aes(x = dept)) + geom_bar( aes(y = ..prop..), width = 0.5)


# y
ggplot(data = df, aes(y)) + geom_bar( aes(y = ..prop..), width = 0.5) + geom_vline(xintercept = mean(df$y))


convar <- as.name("studage")
mean_y_df <- df %>%
  group_by_(convar) %>%
  summarise(mean_y = mean(y))

# =================================================================================
rename(mean_y_df, varvar = studage) 
rename_(.data = mean_y_df, )


ggplot(data = df, aes(y)) + geom_bar( aes(y = ..prop..), width = 0.5) +
  geom_vline(data = mean_y_df, aes(xintercept = mean_y)) +
  facet_grid_(studage ~ .)

ggplot(data = df, aes(y)) + geom_bar( aes(y = ..prop..), width = 0.5) +
  facet_grid(lectage ~ .)

ggplot(data = df, aes(y)) + geom_bar( aes(y = ..prop..), width = 0.5) +
  facet_grid(service ~ .)

ggplot(data = df, aes(y)) + geom_bar( aes(y = ..prop..), width = 0.5) +
  facet_grid(dept ~ .)

