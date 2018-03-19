#' ---
#' title: Caisses
#' subtitle: Exercice "caisses: nombre d'erreurs à la caisse d'un supermarché pendant 30 demi-journées"
#' author: "Bruno Fischer Colonimos"
#' date: Jeudi 15 mars 2018
#' ---


#  Libs
library(ggplot2)


# Génération des données
# runif(1,1,100)

# Paramètres généraux
morning <- "Matin"
afternoon <- "Après-midi"



# Summary function definitions
 #----------------------------
#  population sd
popsd <- function(x) {(length(x) - 1) / length(x) * sd(x)}

my_summary <- function(x) {
        lsumm <- lapply( c(length,
                           mean,
                           popsd,
                           function(x) {quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1),  type = 2)}
        ), 
        function(f) f(x))
        summ <- unlist(lsumm)
        names(summ) <- c("n", "Moyenne", "Ecart-type", "Minimum", "Q1", "Médiane", "Q3", "Maximum")
        summ
}
        


# Make and show data
#--------------------
makedata <-  function(seed = 46, # the random generator seed
                      lambda_morning = 5, # the poisson parameter (morning)
                      shiftmorning = 0, # shift = number to add (+) or substract to the random errors (morning)
                      lambda_afternoon = 10,
                      shiftafternoon = - 4,
                      size = 15, # size of sample to generate
                      morethan_morning = NULL, # minimum error number
                      lessthan_morning = NULL, # maximum error number                     
                      morethan_after = NULL,
                      lessthan_after = NULL
                      ) {
        # generation
        set.seed(seed)
        xam <- rpois(size, lambda = lambda_morning) + shiftmorning  # matinées
        xpm <- rpois(size, lambda = lambda_afternoon) + shiftafternoon # après-midis
        # use max and min limits
        if (!is.null(lessthan_morning)) xam[xam > lessthan_morning] <- lessthan_morning
        if (!is.null(morethan_morning)) xam[xam < morethan_morning] <- morethan_morning
        if (!is.null(lessthan_after)) xpm[xpm > lessthan_after] <- lessthan_after
        if (!is.null(morethan_after)) xpm[xpm < morethan_after] <- morethan_after
        # labels
        lam <- rep(morning, times = size)
        lpm <- rep("Après-midi", times = size)        
        
        
        # vérification / summaries

        
        dfsum <- data.frame( Matin = my_summary(xam), Après_midi = my_summary(xpm))
        # print(dfsum)
        
        # graphes
        #  generated sample data: all error number for all halfdays
        dfgr <- data.frame(halfday = factor(c(lam, lpm), levels = c(morning, afternoon)),
                           errors = c(xam, xpm))
        
        # boxplot
        boxp <- ggplot(data = dfgr, aes(halfday, errors)) + geom_boxplot()
        # barplot
        barp <- ggplot(data = dfgr, aes(errors)) + geom_bar(width = .5)
        
        # return dataframe and summary
        list(data = dfgr, 
             summaries = dfsum,
             barp = barp,
             boxp = boxp 
             )
}


data1 <- makedata(46) # ok
makedata(50)
data2 <- makedata(51, lambda_morning = 6, shiftmorning = 0, lambda_afternoon = 12, shiftafternoon = -4) # ok

data3 <- makedata(55, lambda_morning = 8, shiftmorning = -2, 
         lambda_afternoon = 10,shiftafternoon = -4, morethan_after = 3) # ok

# see results
data1

data2
data3

# new:
dataB1 <- makedata(46, size = 60, morethan_after = 0, lessthan_morning = 10)
dataB1
