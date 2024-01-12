# -----------------------------------
# ----------Functions Script---------
# -----------------------------------

# Get libraries ----------
setwd("/Users/filipsjostrand/Documents/UC Davis/Courses/STA 141B/assignments/5")
library(stringi)
library(stringr)



generate_strings <- function(
        utf_df,
        min_size = 1000,
        max_size = 10000,
        step = 1000
) {
    #
    # Generate a random string combining normal characters and UTF encodings.
    # The strings sizes are roughly starting at min_size and ending at max_size,
    # with step as incrament.
    #
    input_sizes <- seq(min_size, max_size, by = step)
    iters <- length(input_sizes)
    sample_strings <- character(iters)
    output_sizes <- numeric(iters)
    
    for (i in seq_along(input_sizes)) {
        iter_string <- ""
        while(nchar(iter_string) < input_sizes[i]) {
            rand_num <- sample(seq(1, input_sizes[i]), 1)
            chars <- stri_rand_strings(1, rand_num)
            utf_code <- sample(utf_df$From.UTF.8, 1)
            iter_string <- paste0(iter_string, utf_code, chars)
        }
        
        sample_strings[[i]] <- iter_string
        output_sizes[[i]] <- nchar(iter_string)
    }
    
    data.frame(string = sample_strings, num_char = output_sizes)
}




run_time <- function(sample_strings, FUN = URLdecode, ...) {
    #
    # Generates run times as a function of string lengths.
    # For each sample string, measure time for the given decoding function.
    #
    run_times <- sapply(
        sample_strings,
        function(string) {
            start_time <- Sys.time()
            FUN(string, ...)
            end_time <- Sys.time()
            as.numeric(difftime(end_time, start_time, units = "secs"))
        }
    )
    
    df <- data.frame(run_time = run_times)
    rownames(df) <- NULL
    df
}






URLdecode_preallocated <- function(URL) {
    vapply(URL, function(URL) {
        x <- charToRaw(URL)
        pc <- charToRaw("%")
        # preallocate out to maximum possible size
        out <- raw(length(x))
        i <- 1L
        j <- 1L
        while (i <= length(x)) {
            if (x[i] != pc) {
                out[j] <- x[i]
                i <- i + 1L
                j <- j + 1L
            }
            else {
                y <- as.integer(x[i + 1L:2L])
                y[y > 96L] <- y[y > 96L] - 32L
                y[y > 57L] <- y[y > 57L] - 7L
                y <- sum((y - 48L) * c(16L, 1L))
                out[j] <- as.raw(as.character(y))
                i <- i + 3L
                j <- j + 1L
            }
        }
        # trim out to correct size
        out <- out[1:(j - 1)]
        rawToChar(out)
    }, character(1), USE.NAMES = FALSE)
}










# VECTORIZED -------------------------------------------
find_percent <- function(input_string, utf_df) {
    #
    # Extracts all UTF encoding in a string
    #
    pattern <- paste0(utf_df$From.UTF.8, collapse = "|")
    
    matches <- str_extract_all(input_string, pattern)[[1]]
    matrix(matches, ncol = 1, dimnames = list(NULL, c("Match")))
}

index_percent <- function(URL, utf_df) {
    #
    # Finds where the matches starts and ends.
    #
    pattern <- paste0(utf_df$From.UTF.8, collapse = "|")
    
    indices <- str_locate_all(URL, pattern)[[1]]
    colnames(indices) <- c("Start", "End")
    indices
}

convert_percent <- function(URL, utf_df) {
    #
    # Convert UTF encodings into its equivalent character.
    # Use match() to find the corresponding elements in the refrence table.
    #
    matches <- find_percent(URL, utf_df)[,1]
    convert <- utf_df$Character[match(matches, utf_df$From.UTF.8)]
    matrix(convert, ncol = 1, dimnames = list(NULL, c("Converted")))
}


merge_outputs <- function(URL, utf_df) {
    #
    # Creates a matrix with all valuable infromation regarding string
    # manipulation.
    #
    matches <- find_percent(URL, utf_df)
    indices <- index_percent(URL, utf_df)
    converted <- convert_percent(URL, utf_df)
    
    cbind(indices, matches, converted)
}


replace_percent <- function(URL, utf_df) {
    #
    # Replace all UTF encodings in the string according to to_do_matrix.
    # Uses a copy of the original URL as reference for finding where to insert
    # replacements.
    # 
    to_do_matrix <- merge_outputs(URL, utf_df)
    original_URL <- URL  
    
    starts <- as.integer(to_do_matrix[, "Start"])
    ends <- as.integer(to_do_matrix[, "End"])
    replacements <- to_do_matrix[, "Converted"]
    matches <- substring(original_URL, starts, ends)  
    
    stri_replace_all_fixed(URL, matches, replacements, vectorize_all = FALSE)  # replace the matches in the modified string
}

URLdecode_vectorized <- function(URL, utf_df) {
    #
    # Vectorized application to all URLS in a list.
    #
    sapply(URL, replace_percent, utf_df)
}





