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



# one-variable bar charts
# function
g_bar <- function(varstr, dfr = df) {
        vname <- as.name(varstr)
        ggplot(data = dfr, aes_(vname)) +
                geom_bar( aes(y = ..prop..), width = 0.5)
}
# actual
g_bar("studage")
g_bar("service")
g_bar("dept")

g_bar("y") +
        geom_vline(xintercept = mean(df$y)) +
        geom_text(data = data.frame(mean_y = mean(df$y)),
                  aes(x = mean_y, y = .2, label = format(mean_y, digits = 3)),
                  hjust = -.2)


# y with boxplot

ggplot(data = df, aes(x = 1, y = y)) + geom_boxplot( width = 0.5) +
  geom_point(data = data.frame(mean_y = mean(df$y)), aes(y = mean_y), size = 4, color = "red", alpha = .5 ) +
  geom_text(data = data.frame(mean_y = mean(df$y)), aes(y = mean_y, label = format(mean_y, digits = 3)),
            hjust = -.2)


#  ========================================================================================================
# comparing barplots



myfacet <- function(row = ".", col = "."){
  fmla <- as.formula(paste0(row, " ~ ", col))
  facet_grid(fmla)
}


ybar_by2 <- function(vrow = "." , vcol = "." , labheight = .2) {
  # print(missing(vcol))
  # compute the means by groups

  mvrow <- missing(vrow)
  mvcol <- missing(vcol)

  # function for grouping dataframes
  groupif <- function(dframe, vrow, vcol, mvrow, mvcol) {
       rowvar <- as.name(vrow)
      colvar <- as.name(vcol)
    if (!mvrow & ! mvcol) {
      group_by_(dframe, rowvar, colvar)
    } else if (!mvrow){
      group_by_(dframe, rowvar)
    }else if (!mvcol) {
      group_by_(dframe, colvar)
    } else {
      dframe
    }
  }

  # compute dataframe of conditional means
  mean_y_df <- df %>%
    groupif(vrow, vcol, mvrow, mvcol ) %>%
    summarise(mean_y = mean(y))

  # make the ggplot
  ggplot(data = df, aes(y)) + geom_bar( aes(y = ..prop..), width = 0.5) +
    geom_vline(data = mean_y_df, aes(xintercept = mean_y)) +
    geom_text(data = mean_y_df, aes(x = mean_y, y = labheight, label = format(mean_y, digits = 3)),
              hjust = -.1) +
    myfacet(vrow, vcol)
}

# -----------------------------------------------------------------------------------------




ybar_by2("studage")
ybar_by2("lectage")
ybar_by2("service")
ybar_by2("dept")


ybar_by2("lectage", "studage")

ybar_by2("studage", "service")
ybar_by2("studage")
ybar_by2(, "service")
ybar_by2("service")
ybar_by2("dept", "service")


#  ========================================================================================================
# comparing boxplots






