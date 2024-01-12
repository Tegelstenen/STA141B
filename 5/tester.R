# -----------------------------------
# ----------Test Script--------------
# -----------------------------------

# Get libraries ----------
setwd("/Users/filipsjostrand/Documents/UC Davis/Courses/STA 141B/assignments/5")
library(stringr)
source("funs5.R")

# Load the data ----------
utf_df <- readRDS("utf_df.rds")

# To test with ----------
input_all <- paste0(utf_df$From.UTF.8, collapse = "")
output_all <- paste0(utf_df$Character, collapse = "")
known_inputs <- c("%24abc%5D%2B", input_all)
known_outputs <- c("$abc]+", output_all)
large_input <- generate_strings(
    utf_df,
    min_size = 150000,
    max_size = 150000,
    step = 0
)$string
random_inputs <- generate_strings(utf_df)$string

# Function to test known outputs -----------
test_known_outputs <- function(func, inputs, expected_outputs, utf_df = NULL) {
    for (i in seq_along(inputs)) {
        # Conditionally call the function with or without utf_df
        result <- if (is.null(utf_df)) {
            func(inputs[i])
        } else {
            func(inputs[i], utf_df)
        }
        
        if (result != expected_outputs[i]) {
            print(paste("Test failed for input:", inputs[i]))
            print(paste("Expected output:", expected_outputs[i]))
            print(paste("Actual output:", result))
        }
        
        stopifnot(result == expected_outputs[i])
    }
}


# Function to test random comparison to original ----------
test_random_comparison <- function(func, inputs, utf_df = NULL) {
    for (input in inputs) {
        # Check if utf_df is NULL and call func accordingly
        result <- if (is.null(utf_df)) {
            func(input)
        } else {
            func(input, utf_df)
        }
        
        original <- URLdecode(input)
        
        if (result != original) {
            print("First mismatch:")
            differing_positions <- str_locate_all(result, ".")[[1]]
            first_mismatch_pos <- differing_positions[1, "start"]
            mismatch_char_result <- str_sub(result, first_mismatch_pos, first_mismatch_pos)
            mismatch_char_original <- str_sub(original, first_mismatch_pos, first_mismatch_pos)
            
            context_start <- max(first_mismatch_pos - 5, 1)  # start 5 characters before the mismatch
            context_end <- min(first_mismatch_pos + 5, nchar(result))  # end 5 characters after the mismatch
            
            context_result <- str_sub(result, context_start, context_end)
            context_original <- str_sub(original, context_start, context_end)
            
            print(paste("Mismatch Position:", first_mismatch_pos))
            print(paste("Mismatch Character (Result):", mismatch_char_result))
            print(paste("Mismatch Character (Original):", mismatch_char_original))
            print(paste("Context (Result):", context_result))
            print(paste("Context (Original):", context_original))
            
            break  # stop the loop after the first mismatch
        }
        
        stopifnot(result == original)
    }
}



# Function to test very large outputs and comparison -----------
test_large_inputs <- function(decode_function, large_input, utf_df = NULL) {
    result <- if (is.null(utf_df)) decode_function(large_input) else decode_function(large_input, utf_df)
    original <- URLdecode(large_input)
    stopifnot(result == original)
}

# TESTS FOR PREALLOCATED -------------------------------------------------------
test_random_comparison(URLdecode_preallocated, random_inputs)
test_known_outputs(URLdecode_preallocated, known_inputs, known_outputs)
test_large_inputs(URLdecode_preallocated, large_input)

# TESTS FOR VECTORIZED ---------------------------------------------------------
test_random_comparison(URLdecode_vectorized, random_inputs, utf_df)
test_known_outputs(URLdecode_vectorized, known_inputs, known_outputs, utf_df)
test_large_inputs(URLdecode_vectorized, large_input, utf_df)
