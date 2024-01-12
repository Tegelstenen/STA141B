library(stringr)

split_tables <-
    #
    # Splitting table based on indexes.
    # start at an index, read lines until next is reached, iterate starting
    # from last reached.
    #
    function(lines, start_index) {
        tables <- c()
        start_stop_indexes <- append(start_index, (length(lines) + 2))
        
        tables <- list()
        for (i in 1:(length(start_stop_indexes) - 1)) {
            start <- start_stop_indexes[i] + 1
            stop <- start_stop_indexes[i + 1] - 2
            tables[[i]] <- lines[start:stop]
        }
        tables
    }



rx_tester <- 
    #
    # Test whether a regular expression does not match a row, for each table.
    # Counts the occurrences of TRUE and FALSE. 
    # If list contains FALSE, return the counts and which table it belongs to.
    #
    function(table_list, rx) {
        
        res <- list()
        for (i in 1:length(table_list)) {
            w <- grepl(rx, table_list[[i]])   
            
            if (FALSE %in% names(table(w))) {
                which_table <- paste("table", i)
                res[[which_table]] <- table(w)
                return(res)
            }
        }
    }

parser <- 
    #
    # Parse each file in a list at the specified regex.
    # If rx_tester() returns nothing, parser() will take same regex and extract 
    # the string, and remove it from the raw files.
    #
    function(raw_file, rx) {
        if(is.null(rx_tester(raw_file, rx))) {
            extractions <- lapply(raw_file, str_extract, rx)
            new_raw_file <- lapply(raw_file, gsub, pattern = rx, repl = "")
        } else {
            print("False Returns")
        }
        list(extractions, new_raw_file)
    }

matches <- 
    #
    # Returns a column vector of matching results.
    #
    function(column, rx) {
        logic_result <- grepl(rx, column)
        result <- column[logic_result]
        result
    }


