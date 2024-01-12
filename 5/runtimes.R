# -----------------------------------
# ----------Run times----------------
# -----------------------------------

setwd("/Users/filipsjostrand/Documents/UC Davis/Courses/STA 141B/assignments/5")

# Source the functions ----------
source("funs5.R")

# Load and generate the data ----------
utf_df <- readRDS("utf_df.rds")
real_URL <- readLines("PercentEncodedString.txt")
sample_strings <- generate_strings(utf_df, max_size = 150000, step = 5000)

# Run times
r_time <- run_time(sample_strings$string)
r_time_prealoc <- run_time(sample_strings$string, FUN = URLdecode_preallocated)
r_time_vector <- run_time(
    sample_strings$string,
    FUN = URLdecode_vectorized, 
    utf_df
)

r_time_real <- run_time(real_URL)
r_time_prealoc_real <- run_time(real_URL, FUN = URLdecode_preallocated)
r_time_vector_real <- run_time(real_URL, FUN = URLdecode_vectorized, utf_df)

# Save the simulations to a file ----------
saveRDS(sample_strings, "sample_strings.rds")

saveRDS(r_time, "r_time.rds")
saveRDS(r_time_prealoc, "r_time_prealoc.rds")
saveRDS(r_time_vector, "r_time_vector.rds")

saveRDS(r_time_real, "r_time_real.rds")
saveRDS(r_time_prealoc_real, "r_time_prealoc_real.rds")
saveRDS(r_time_vector_real, "r_time_vector_real.rds")

