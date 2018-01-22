


dyreg_by <- function(vx, vrow = "." , vcln = ".", vcol = "",
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

        # aes_(xn, yn, fill = vcol_n)

        # make the ggplot
        # base
        if (mvcol) {
                plot <- ggplot(data = df, aes_(xn, yn) )
        } else {
                plot <- ggplot(data = df, aes_(xn, yn, fill = vcol_n) )
        }
        # more components
        plot <- plot + geom_smooth(method="lm")
        # plot <- plot +
        #         geom_boxplot(aes_(group = xn), width = 0.5, varwidth = TRUE) +
        #         geom_point(data = mean_y_df, aes(y = mean_y), size = meansize, color = meancolor, alpha = .5 ) +
        #         geom_line(data = mean_y_df, aes_(xn, quote(mean_y), group = 1), color = meancolor) +
        #         geom_text(data = mean_y_df, aes(y = mean_y, label = format(mean_y, digits = meandigits)),
        #                   hjust = labjust)
        #
        #
        # if (linreg) {
        #         plot <- plot + geom_smooth(aes_(xn, yn), method = "lm")
        # }
        #
        # if (!mvrow | !mvcln) {
        #         plot <- plot + myfacet(row = vrow, col = vcln)
        # }
        #
        #
        #
        # # add title to plot
        # if (condpresent <= 0) {
        #         plot_title <- paste0("Y distribution and mean by ", vx)
        # } else if (condpresent == 1) {
        #         plot_title <- paste0("Y distribution and mean by ", vx, " and ", ifelse (!mvrow , vrow, vcln)  )
        # } else if (condpresent == 2) {
        #         plot_title <- paste0("Y distribution  and mean by ", vx, " and ",  vrow, " and ", vcln)
        # } else {
        #         stop( paste0("Error in ybar-by2: bad value for condpresent: ", condpresent) , call. = TRUE )
        # }
        #
        # return plot
        # plot <- plot  + labs(title = plot_title)
        # return plot
        plot
}


# application

yreg_by("studage")
yreg_by("lectage")
yreg_by("service")
yreg_by("dept")


ybox_by("service", "lectage", meancolor = "blue", meansize = 3)

yreg_by("lectage", "service") #===========================================

dyreg_by("lectage", "dept")
dyreg_by("lectage", vcol = "dept")
