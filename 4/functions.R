library(httr)
library(rvest)
library(purrr)
library(stringr)





# Functions for parsing pages -------------------------------------------------
parse_page <- function(url) {
    #
    # Download and parse a page given its URL.
    # Makes a GET request to the URL and check if the request was successful.
    # If successful, parse the HTML content of the response.
    # If the request failed, print an error message and return NULL
    # 
    response <- GET(url)
    
    if (response$status_code == 200) {
        parsed_page <- read_html(response)
        
        return(parsed_page)
    } else {
        print(paste("Failed to download page:", url))
        print(response$status_code)
        
        return(NULL)
    }
}



get_links <- function(page, xp) {
    #
    # Extracts the URLs of the questions on a page.
    # First, use XPath to find the links and parse its href attribute.
    # Then, prepend the base URL to the relative URLs.
    # 
    links <- page %>%
        html_elements(xpath = xp) %>% 
        html_attr("href")
    
    full_links <- paste0("https://stackoverflow.com", links)
    
    if (identical(links, character(0))) {
        return(NA)
    } else {
        return(full_links)    
    }
}





# Functions for scraping post data ---------------------------------------------
get_views <- function(page) {
    #
    # Extracts the number of views from a question page.
    # Uses XPath to locate the div containing the views count.
    # If this fails (e.g., migrated post), it tries to find the views count
    # from an alternative location.
    # Returns the views count as a numeric value.
    #
    views_raw <- page %>%
        html_elements(xpath = "//div[@class = 'd-flex fw-wrap pb8 mb16 bb bc-black-075']//div[@class = 'flex--item ws-nowrap mb8 mr16' or @class = 'flex--item ws-nowrap mb8']") %>% 
        html_text()
    
    views_extract <- gsub("[^0-9k]", "", views_raw)
    
    views <- gsub("k", "000", views_extract)
    
    return(as.numeric(views))
}



get_votes <- function(page) {
    #
    # Extracts the votes count from a question page.
    # Uses XPath to locate the div with the vote count.
    # Returns the vote count as an integer.
    #
    votes <- page %>%
        html_element(xpath = "//div[@class[starts-with(., 'js-vote-count')]]") %>% 
        html_text(trim = TRUE) %>%
        as.integer()
    
    return(votes)
}



get_text <- function(page) {
    #
    # Extracts the text of the question from a question page.
    # Uses XPath to locate the div containing the question text.
    # Returns the text as a string.
    # Makes sure it is the latest body of text in the question.
    #
    text <- page %>%
        html_elements(xpath = "//div[@class='s-prose js-post-body']") %>% 
        html_text(trim = TRUE)
    
    if (length(text) > 0) {
        text <- text[1] # Select first element
    }
    
    return(text)
}



get_tags <- function(page) {
    #
    # Extracts the tags of the question from a question page.
    # Uses XPath to locate the a tags containing the tags.
    # Returns the tags as a list.
    #
    tags <- page %>%
        html_elements(xpath = "//ul[@class='ml0 list-ls-none js-post-tag-list-wrapper d-inline']/li/a") %>% 
        html_text(trim = TRUE)
    
    tags <- unique(tags)
    tags <- paste(tags, sep="", collapse=", ")
    
    return(tags)
}



get_post_time <- function(page) {
    #
    # Extracts the time the question was posted from a question page.
    # Uses XPath to locate the time tag with attribute itemprop set to dateCreated.
    # Extracts the datetime attribute and returns it as a datetime object.
    # Make sure it is the time of the first post.
    #
    datetime_string <- page %>%
        html_elements(xpath = "//time[@itemprop='dateCreated']") %>% 
        html_attr("datetime") 
    
    if (length(datetime_string) > 0) {
        datetime_string <- datetime_string[1] # Select first element
    }
    
    datetime <- as.POSIXct(
        datetime_string,
        format = "%Y-%m-%dT%H:%M:%S",
        tz = "UTC")
    
    return(datetime)
}



get_user_name <- function(page) {
    #
    # Extracts the user name of the person posting the question.
    # Uses XPath to locate the div tag with user details.
    # Returns the user name as a string.
    # Since, e.g., migrated posts differ, it first tries to extract the user 
    # name from an "a" element. If there is none, it tries to extract the user
    # name directly from the div.
    # Adapted to fecth name from commnuity wiki as well.
    #
    user <- page %>%
        html_element(xpath = "(//div[contains(@class, 'post-signature') and contains(@class, 'owner')]//div[@class='user-details']/a | //div[contains(@class, 'post-signature') and contains(@class, 'flex--item')]//div[@class='user-details']/span[@class='community-wiki'])[1]") %>%
        html_text(trim = TRUE)
    
    return(user)
}

post_row <- function(page) {
    #
    # Combines the each of the scraped data for the post table.
    #
    observation <- list(
        views = get_views(page),
        votes = get_votes(page),
        body = get_text(page),
        tags = get_tags(page),
        post_time = get_post_time(page),
        user = get_user_name(page)
    )
    
    return(data.frame(observation))
}





# Functions for scraping user data ---------------------------------------------
get_badge <- function(page, badge_class) {
    #
    # Gets user badges.
    # If a badge is empty, replace it with zero.
    #
    badge <- page %>%
        html_elements(xpath = "//div[@class = 'post-signature owner flex--item']//div[@class = 'user-details']") %>%
        html_elements(
            xpath = paste0(
                ".//span[@class = '",
                badge_class,
                "']/following-sibling::span[@class = 'badgecount']"
            )) %>%
        html_text()
    
    if (length(badge) > 0) {
        return(as.integer(badge))
    } else {
        return(0)
    }
}



get_user_details <- function(page) {
    #
    # Extracts the user details from a question page.
    # Locates the user-details container and extracts the reputation score, 
    # and the number of gold, silver, and bronze badges. 
    # Makes sure to handle varying amount user badges.
    # If reputatation score is missing, replace with NA.
    #
    user_details <- page %>%
        html_elements(xpath = "//div[@class = 'post-signature owner flex--item']//div[@class = 'user-details']")
    
    reputation_score_elements <- user_details %>%
        html_elements(xpath = ".//span[@class='reputation-score']")
    
    reputation_score <- if(length(reputation_score_elements) == 0) {
        NA_real_
    } else {
        reputation_score_elements %>%
            html_text(trim = TRUE) %>%
            gsub(",", "", .) %>% # remove commas
            gsub("k", "e3", .) %>% # 'k' to 'e3' to make as.numeric() work
            as.numeric()
    }
    
    user_details <- list(
        user = get_user_name(page),
        reputation = reputation_score,
        gold = get_badge(page, "badge1"),
        silver = get_badge(page, "badge2"),
        bronze = get_badge(page, "badge3")
    )
    
    user_details <- data.frame(user_details)
    
    return(user_details)
}





# Functions for scraping edit data ---------------------------------------------
editors_times <- function(revision_page) {
    #
    # Extracts both user names and the time of editing of editors.
    # Since the name comes as a href we have to use regex to extract everhythin
    # after the last backslash.
    # Some edit types are wrapped by 'a' some by 'time'.
    # We extract the time as datetime type.
    #
    editor_names <- revision_page %>%
        html_elements(xpath = "//div[contains(@class, 'flex--item fl1 wmn1')]//div[contains(@class, 's-user-card--info')]/*") %>%
        html_text(trim = TRUE)
    
    edit_times <- revision_page %>%
        html_elements(xpath = "//div[contains(@class, 'flex--item fl1 wmn1')]//div[contains(@class, 's-user-card--time')]/*[name()='a' or name()='time']/span") %>%
        html_attr("title") %>%
        strptime(format = "%Y-%m-%d %H:%M:%SZ")
    
    df <- data.frame(list(editors = editor_names, times = edit_times))
    
    return(df)
}





# Functions for scraping answer data ---------------------------------------------
isolate_answers <- function(page) {
    #
    # Gets all the answer elements of a question page.
    # First isolate the complete answer element, then isolate each answer
    # individually.
    #
    page %>% 
        html_elements(xpath = "//div[@id = 'answers']") %>% 
        html_elements(xpath = ".//*[@class[starts-with(., 'answer js')]]")
}

get_answer_text <- function(answer) {
    #
    # Strips text from the body in the answers.
    #
    
    text <- answer %>% 
        html_elements(xpath = ".//div[@class = 's-prose js-post-body']/p") %>% 
        html_text()
    
    return(paste(text, sep = " ", collapse =""))
}

get_answer_user_info <- function(answer) {
    #
    # Gets user name, bagdge info, and reputaiton.
    # Utilizes previous functions (see under user functions).
    # Make sure only 'answered' to not scrape data from editors.
    #
    
    # Fetching user name
    user <- answer %>% 
        html_element(xpath = ".//div[contains(., 'answered')]/following-sibling::div[@class='user-details']/a") %>% 
        html_text()
    
    # Fetching reputation
    reputation <- answer %>% 
        html_elements(xpath = ".//div[contains(., 'answered')]/following-sibling::div[@class='user-details']//span[@class = 'reputation-score']") %>% 
        html_text()
    
    reputation <- if(length(reputation) == 0) {
        NA_real_
    } else {
        reputation %>%
            gsub(",", "", .) %>% 
            gsub("k", "e3", .) %>% 
            as.numeric()
    }
    
    # Fetching badge info
    badge_class <- c("badge1", "badge2", "badge3")
    badge <- list()
    for (i in 1:3) {
        badge[[i]] <- answer %>%
            html_elements(
                xpath = paste0(
                    ".//div[contains(., 'answered')]/following-sibling::div[@class='user-details']//span[@class = '",
                    badge_class[[i]],
                    "']/following-sibling::span[@class = 'badgecount']"
                )) %>%
            html_text()
        
        if (length(badge[[i]]) > 0) {
            badge[[i]] <- as.integer(badge[[i]])
        } else {
            badge[[i]] <- 0
        }
    }
    
    names(badge) <- c("Gold", "Silver", "bronze")
    names(user) <- "user"
    names(reputation) <- "reputation"
    
    return(list(user, reputation, badge))
}

answer_combiner <- function(answers) {
    #
    # One observation of answer data
    #
    result <- list()
    for (i in 1:length(answers)) {
        result[[i]] <- c(
            unlist(get_answer_user_info(answers[[i]])),
            body = get_answer_text(answers[[i]])
        )
    }
    
    combined_dfs <- list()
    for (i in 1:length(result)) {
        combined_dfs[[i]] <- data.frame(t(result[[i]]))
    }
    
    finished_dataframe <- do.call(rbind, combined_dfs)
    
    return(finished_dataframe)
}

answer_per_question <- function(pages) {
    #
    # Automation of the combination of several answers of several questions.
    #
    answer <- isolate_answers(pages)
    if (length(answer) == 0){
        return(NA)
    } else {
        df <- answer_combiner(answer)
        return(df)    
    }
}






