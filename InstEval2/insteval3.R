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

setwd("InstEval2")

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))



# load data
# ===========
df <- read.csv(file = file.path("data", "InstEval.csv"),
               sep = ";", row.names = 1)

head(df)
tail(df)


# Data exploration
# ===============

# Cleaning - remove row with missing values
# ------------------------------

#  detect
sapply(df, function(col) {sum(is.na(col))} )
# remove last row
df <- df[!is.na(df$studage), ]
#  verify
sapply(df, function(col) {sum(is.na(col))} )
tail(df)


#  essais par etudiants

unique(df$d)


df1 <- df[df$d == 1002,]
df1 <- arrange(df1, service, dept)

# by student
dfs <- df %>%
        group_by(s, lectage, dept, service) %>%
        summarise(m_y = mean(y), m_age = mean(studage))

ggplot(dfs, aes(dept, m_y)) +
        geom_boxplot(aes(group = dept)) +
        geom_jitter( height = 0, alpha = .1) +
        geom_smooth( method = "lm")






# one-variable bar charts
# function
g_bar <- function(varstr, dfr = df) {
        vname <- as.name(varstr)
        ggplot(data = dfr, aes_(vname)) +
                geom_bar( aes(y = ..prop..), width = 0.5)
}
# actual
g_bar("studage")
g_bar("lectage")
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


# ==============================================================================================
# comparing barplots


# facets with string aguments
myfacet <- function(row = ".", col = "."){
  fmla <- as.formula(paste0(row, " ~ ", col))
  facet_grid(fmla)
}


ybar_by <- function(vrow = "." , vcol = ".",
                    colfill="skyblue",
                    meansize = 4, meandigits = 3, meancolor = "red",
                    labheight = .2, labjust = -.2) {
        # missing arguments
        mvrow <- missing(vrow)
        mvcol <- missing(vcol)
        condpresent <- 2 - (as.integer(mvrow) + as.integer(mvcol))
        # compute the means by groups ----------

        # function for grouping dataframes
        groupif <- function(dframe, vrow, vcol, mvrow, mvcol) {
                rowvar <- as.name(vrow)
                colvar <- as.name(vcol)
                if (!mvrow & ! mvcol) {
                        group_by_(dframe, rowvar, colvar)
                } else if (!mvrow) {
                        group_by_(dframe, rowvar)
                } else if (!mvcol) {
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
        plot <- ggplot(data = df, aes(y)) +
                geom_bar( aes(y = ..prop..), width = 0.5, fill = colfill) +
                geom_vline(data = mean_y_df, aes(xintercept = mean_y), color = meancolor) +
                geom_text(data = mean_y_df, aes(x = mean_y, y = labheight, label = format(mean_y, digits = meandigits)),
                          color = meancolor, hjust = labjust)


        # + myfacet(vrow, vcol)

        if (!mvrow | !mvcol) {
                plot <- plot + myfacet(row = vrow, col = vcol)
        }

        # add title to plot
        if (condpresent <= 0) {
                plot_title <- "Y distribution and mean"
        } else if (condpresent == 1) {
                plot_title <- paste0("Y distribution and mean by ", ifelse (!mvrow , vrow, vcol)  )
        } else if (condpresent == 2) {
                plot_title <- paste0("Y distribution  and mean by ",  vrow, " and ", vcol)
        } else {
                stop( paste0("Error in ybar-by2: bad value for condpresent: ", condpresent) , call. = TRUE )
        }

        # return plot
        plot + labs(title = plot_title)

}

# -----------------------------------------------------------------------------------------
#  apply
ybar_by()
ybar_by("studage")
ybar_by("lectage")
ybar_by("service")
ybar_by("dept")


ybar_by("lectage", "studage")

ybar_by("studage", "service")
ybar_by("studage")
ybar_by(, "service")
ybar_by("service")
ybar_by("dept", "service") # evidence of interaction


# ========================================================================================================
# comparing boxplots



ybox_by <- function(vx, vrow = "." , vcol = "." ,
                    meansize = 4, meandigits = 3, meancolor = "red",
                    labheight = .2, labjust = -.2) { # all args = strings
        # missing arguments
        mvrow <- missing(vrow)
        mvcol <- missing(vcol)
        condpresent <- 2 - (as.integer(mvrow) + as.integer(mvcol))

        # preparation for circomventing nse
        xn <- as.name(vx)
        yn <- as.name("y")
        # compute the means by groups ----------

        # function for grouping dataframes
        groupif3 <- function(dframe, vx, vrow, vcol, mvrow, mvcol) {
                xvar <- as.name(vx)
                rowvar <- as.name(vrow)
                colvar <- as.name(vcol)
                if (!mvrow & ! mvcol) {
                        group_by_(dframe, xvar, rowvar, colvar)
                } else if (!mvrow) {
                        group_by_(dframe, xvar, rowvar)
                } else if (!mvcol) {
                        group_by_(dframe, xvar, colvar)
                } else {
                        group_by_(dframe, xvar)
                }
        }

        # compute dataframe of conditional means
        mean_y_df <- df %>%
                groupif3(vx, vrow, vcol, mvrow, mvcol ) %>%
                summarise(mean_y = mean(y))

        # make the ggplot

        plot <- ggplot(data = df,
                       aes_(xn, yn, group = xn) ) +
                geom_boxplot(width = 0.5, varwidth = TRUE) +
                geom_point(data = mean_y_df, aes(y = mean_y), size = meansize, color = meancolor, alpha = .5 ) +
                geom_line(data = mean_y_df, aes_(xn, quote(mean_y), group = 1), color = meancolor) +
                geom_text(data = mean_y_df, aes(y = mean_y, label = format(mean_y, digits = meandigits)),
                          hjust = labjust) # + geom_smooth(aes_(xn, yn), method = "lm")

        if (!mvrow | !mvcol) {
                plot <- plot + myfacet(row = vrow, col = vcol)
        }



        # add title to plot
        if (condpresent <= 0) {
                plot_title <- paste0("Y distribution and mean by ", vx)
        } else if (condpresent == 1) {
                plot_title <- paste0("Y distribution and mean by ", vx, " and ", ifelse (!mvrow , vrow, vcol)  )
        } else if (condpresent == 2) {
                plot_title <- paste0("Y distribution  and mean by ", vx, " and ",  vrow, " and ", vcol)
        } else {
                stop( paste0("Error in ybar-by2: bad value for condpresent: ", condpresent) , call. = TRUE )
        }

        # return plot
        plot  + labs(title = plot_title)

}


# tests

ybox_by("studage")
ybox_by("lectage")
ybox_by("service")
ybox_by("dept")


ybox_by("service", "lectage", meancolor = "blue", meansize = 3)
ybox_by("lectage", "service", meancolor = "blue", meansize = 3)
ybox_by("lectage", "dept", meancolor = "blue", meansize = 3)
ybox_by("lectage", "dept", "service", meancolor = "blue", meansize = 3)



# approaches by the mean:

mdf <- df %>%
        group_by(studage, lectage, service, dept) %>%
        summarise(mean_y = mean(y), cnt = n())

summary(mdf$mean_y)



# ========================================================================
# graphing regressions ========================================================================
# ========================================================================
# not yet ok

yreg_by <- function(vx, vrow = "." , vcln = ".", vcol = "",
                    linreg = TRUE,
                    meansize = 4, meandigits = 3, meancolor = "red",
                    labheight = .2, labjust = -.2) { # all args = strings
        # missing arguments
        mvrow <- missing(vrow)
        mvcln <- missing(vcln)
        mvcol <- missing(vcol)
        condpresent <- 2 - (as.integer(mvrow) + as.integer(mvcln))

        # preparation for circomventing nse
        xn <- as.name(vx)
        yn <- as.name("y")
        if (!mvcol) {vcol_n <- as.name(vcol)}
        # compute the means by groups ----------

        # function for grouping dataframes
        groupif3 <- function(dframe, vx, vrow, vcln, mvrow, mvcln) {
                xvar <- as.name(vx)
                rowvar <- as.name(vrow)
                clnvar <- as.name(vcln)
                if (!mvrow & ! mvcln) {
                        group_by_(dframe, xvar, rowvar, clnvar)
                } else if (!mvrow) {
                        group_by_(dframe, xvar, rowvar)
                } else if (!mvcln) {
                        group_by_(dframe, xvar, clnvar)
                } else {
                        group_by_(dframe, xvar)
                }
        }

        # compute dataframe of conditional means
        mean_y_df <- df %>%
                groupif3(vx, vrow, vcln, mvrow, mvcln ) %>%
                summarise(mean_y = mean(y))


        # make the ggplot
        # base
        if (mvcol) {
              plot <- ggplot(data = df, aes_(xn, yn) )
        } else {
                plot <- ggplot(data = df, aes_(xn, yn, color = vcol_n) )
        }
        # more components
        plot <- plot +
                geom_boxplot(aes_(group = xn), width = 0.5, varwidth = TRUE) +
                geom_point(data = mean_y_df, aes(y = mean_y), size = meansize, color = meancolor, alpha = .5 ) +
                geom_line(data = mean_y_df, aes_(xn, quote(mean_y), group = 1), color = meancolor) +
                geom_text(data = mean_y_df, aes(y = mean_y, label = format(mean_y, digits = meandigits)),
                          hjust = labjust)
        # dbug
        # print(plot)
        if (linreg) {
                plot <- plot + geom_smooth(# aes_(xn, yn),
                                           method = "lm")
        }

        if (!mvrow | !mvcln) {
                plot <- plot + myfacet(row = vrow, col = vcln)
        }



        # add title to plot
        if (condpresent <= 0) {
                plot_title <- paste0("Y distribution and mean by ", vx)
        } else if (condpresent == 1) {
                plot_title <- paste0("Y distribution and mean by ", vx, " and ", ifelse (!mvrow , vrow, vcln)  )
        } else if (condpresent == 2) {
                plot_title <- paste0("Y distribution  and mean by ", vx, " and ",  vrow, " and ", vcln)
        } else {
                stop( paste0("Error in ybar-by2: bad value for condpresent: ", condpresent) , call. = TRUE )
        }

        # return plot
        plot  + labs(title = plot_title)

}


# application

yreg_by("studage")
yreg_by("lectage")
yreg_by("service")
yreg_by("dept")


ybox_by("service", "lectage", meancolor = "blue", meansize = 3)

yreg_by("lectage", "service") #===========================================
yreg_by("lectage", vcol = "service") ######################

yreg_by("lectage", "dept")
yreg_by("lectage", vcol = "dept") #################
yreg_by("lectage", "dept", "service", meansize = 3)


df


