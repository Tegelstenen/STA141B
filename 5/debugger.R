# -----------------------------------
# ----------DEBUG FUNCTIONS----------
# -----------------------------------

# Get libraries ----------
setwd("/Users/filipsjostrand/Documents/UC Davis/Courses/STA 141B/assignments/5")
source("funs5.R")

# Load the data ----------
utf_df <- readRDS("utf_df.rds")

# Functions ----------
mismatcher <- function(utf_df) {
    resmat <- matrix(nrow = nrow(utf_df), ncol = 5)
    equality <- logical(nrow(utf_df))
    for (i in 1:nrow(utf_df)) {
        to_result <- utf_df$Character[i]
        to_test <- utf_df$From.UTF.8[i]
        original <- URLdecode(to_test)
        preallocated <- URLdecode_preallocated(to_test)
        vectorized <- URLdecode_vectorized(to_test, utf_df)
        equality[i] <- length(unique(c(to_result, original, preallocated, vectorized))) == 1
        resmat[i,] <- c(to_test, to_result, original, preallocated, vectorized)
    }
    colnames(resmat) <- c("UTF", "table", "original", "preallocated", "vectorized")
    resmat[!equality, ]
}

remapper <- function(utf_df) {
    #
    # Write a function called remapper that takes in `utf_df` and iterates
    # through the whole data frame. It should take each element of `From.UTF.8`, 
    # give it to `URLdecode()`, compate to the output given in `Character`,
    # if different, replace it with `URLdecode()` output.
    #
    for (i in 1:nrow(utf_df)) {
        expected_char <- utf_df$Character[i]
        utf_char <- URLdecode(utf_df$From.UTF.8[i])
        
        if (utf_char != expected_char) {
            utf_df$Character[i] <- utf_char
            utf_df
        }
    }
    
    utf_df
}
