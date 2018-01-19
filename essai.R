
df_freq2 <- df_freq
df_freq2[1] <- as.character(df_freq[1])
df_freq2[1] <- sapply(df_freq[1], as.character)


vlookup <- function(value, searchtable, searchcol = 1, returncol= 2){
        col <- searchtable[ , searchcol]
        print(col)
        i <- match(value, col)
        print(paste("i= ", i))
        searchtable[i, returncol]
}

vlookup("Lockup", df_freq)


