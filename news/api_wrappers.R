##### Define wrapper for "Top headlines" endpoint for News API
get_top_headlines <- function(apiKey, country = NULL, category = NULL, 
                              sources = NULL, keyword = NULL, pageSize = NULL, 
                              page = NULL) {
  # Set base URL for top headlines
  url <- "https://newsapi.org/v2/top-headlines?language=en&"
  
  # Add country search if input is not NULL
  if (!is.null(country)) {
    url <- paste0(url, "country=", country, "&")
  }
  
  # Add category search if input is not NULL
  if (!is.null(category)) {
    url <- paste0(url, "category=", category, "&")
  }
  
  # Add sources input if input is not NULL and there are no conflict
  if (!is.null(sources)) {
    if (!is.null(country) | !is.null(category)) {
      warning("Sources parameter cannot be mixed with country or category")
      return("") # Early return if illegal combination of parameters specified
    } else {
      url <- paste0(url, "sources=", sources, "&")
    }
  }
  
  # Add keyword search if input is not NULL
  if (!is.null(keyword)) {
    url <- paste0(url, "q=", keyword, "&")
  }
  
  # Add pageSize parameter if input is not NULL
  if (!is.null(pageSize)) {
    if (pageSize > 100 | pageSize <= 0) {
      warning("Page size must be between 1 and 100")
    } else {
      url <- paste0(url, "pageSize=", pageSize, "&")
    }
  }
  
  # Add page parameter if input is not NULL
  if (!is.null(page)) {
    if (page <= 0) {
      warning("Please enter a page number greater than zero")
    } else {
      url <- paste0(url, "page=", page, "&")
    }
  }
  
  # Add api key
  url <- paste0(url, "apiKey=", apiKey)
  
  # Read the data in json format
  json_data <- fromJSON(url)
  
  # Extract relevant data as tibble
  as_tibble(json_data$articles)
}

##### Define wrapper to the "Sources" endpoint for News API

get_sources <- function(apiKey, category = NULL, 
                        country = NULL) {
  
  # Set base URL for sources
  url <- "https://newsapi.org/v2/sources?language=en&"
  
  # Add category search if input is not NULL
  if (!is.null(category)) {
    url <- paste0(url, "category=", category, "&")
    }
  
  # Add country search if input is not NULL
  if (!is.null(country)) {
    url <- paste0(url, "country=", country, "&")
    }
  
  # Add api key
  url <- paste0(url, "apiKey=", apiKey)
  
  # Read the data in json format
  json_data <- fromJSON(url)
  
  # Extract relevant data as tibble
  as.tibble(json_data$sources)
}

##### Define wrapper to make POST requests to SENTIM-API
get_sentim <- function(x) {
  
  # Weed out invalid function arguments
  if(typeof(x) != "character" | length(x) != 1 | x == "") {
    warning("Input must be an atomic character vector of length 1")
  } else {
    
    # Make POST request and save response (code taken directly from Shawn)
    r <- POST(
      url    = "https://sentim-api.herokuapp.com/api/v1/",
      config = add_headers("Accept"       = "application/json",
                           "Content-Type" = "application/json"),
      body   = list(text = x),
      encode = "json")
    
    # extract content from POST response
    content <- content(r, "text", encoding = "UTF-8")
    
    # Return overall polarity rating
    text_subset <- str_extract(string = content, pattern = ".*\"\\},")
    str_extract(string = text_subset, "-?[0-9]*\\.[0-9]*")
  }
}
