#' Extract all results of a given resource or form
#'
#' This function allows you to express your love of cats.
#' @param base_url Base formio URL
#' @param resource_path Name of the path of the form ie: 'user'.
#' @param token The formio token to allow access
#' @keywords formio
#' @export
#' @examples
#' FormioInterface()
FormioInterface <-  R6::R6Class("FormioInterface",   
    public = list(
    base_url = NULL,
    resource_path = NULL,
    token = NULL,
    filter_string = NULL,
    select_string = NULL,
    limit_string = NULL,
    chunk_size_string = NULL,
    first_chunk = NULL,
    initialize = function(base_url = NA, resource_path = NA, token = NA) {
      self$base_url <- base_url
      self$resource_path <- resource_path
      self$token <- token
    },
    
    #' Sets the proper Filter URL for the GET CALL
    #'
    #' Parses the list of filters to a single string
    #' @param filter The list of filters
    #' @keywords filter
    #' @examples
    #' filterToString()
    filterToString = function(filters) {
      filterString <- ''
      for(filter in filters){
        element <- filter[[1]]
        query <- filter[[2]]
        value <- filter[[3]]
        
        if(query == "="){
          filterString <- paste(filterString, element, '=', value, '&', sep ='')
        }
        else if(query == "!="){
          filterString <- paste(filterString, element, '__ne=', value, '&', sep ='')
        }
        else if(query == ">"){
          filterString <- paste(filterString, element, '__gt=', value, '&', sep ='')
        }
        else if(query == ">="){
          filterString <- paste(filterString, element, '__gte=', value, '&', sep ='')
        }
        else if(query == "<"){
          filterString <- paste(filterString, element, '__lt=', value, '&', sep ='')
        }
        else if(query == "<="){
          filterString <- paste(filterString, element, '__lte=', value, '&', sep ='')
        }
        # Implementation pending
        else if(query == "in"){
          filterString <- paste(filterString, element, '__ne=', value, '&', sep ='')
        }
        # Implementation pending
        else if(query == "nin"){
          filterString <- paste(filterString, element, '__ne=', value, '&', sep ='')
        }
        else if(query == "exists"){
          filterString <- paste(filterString, element, '__exists=', TRUE, '&', sep ='')
        }
        else if(query == "!exists"){
          filterString <- paste(filterString, element, '__ne=', FALSE, '&', sep ='')
        }
        else if(query == "regex"){
          filterString <- paste(filterString, element, '__regex=', value, '&', sep ='')
        }
      }
      filterString <- substr(filterString, 1, nchar(filterString)-1) 
      return(filterString)
    },
    
    #' Sets the proper Select URL for the GET CALL
    #'
    #' Parses the list of select elements to a single string
    #' @param filter The list of items to include in the select
    #' @keywords filter
    #' @examples
    #' selectToString()
    selectToString = function(select){
      selectString <- '_id,owner,modified,'
      for(attribute in select){
        selectString = paste(selectString,attribute,',',sep='')
      }
      selectString <- substr(selectString, 1, nchar(selectString)-1) 
      return(selectString)
    },
    getQueryString = function(){
      
      url <- paste(self$base_url, self$resource_path, "/submission?", sep='')
      first_chunk <- NULL
      
      # Set the final queryString
      if(is.character(self$filter_string)) {
        url <- paste(url, self$filter_string, '&', sep="")
      } 
      if(is.character(self$select_string)) {
        url <- paste(url,'select=', self$select_string, '&', sep="")
      } 
      return(url)
      
    },
    getChunkSize = function(){
      first_chunk <- NULL
      url <- ''
      if(is.character(self$limit_string) && is.character(self$chunk_size_string))
      {
        if(as.numeric(self$chunk_size_string) >= as.numeric(self$limit_string)) {
          first_chunk <- self$limit_string
        } else {
          first_chunk <- self$chunk_size_string
        }
      }
      if(is.character(self$limit_string) && !is.character(self$chunk_size_string)) {
        first_chunk <- self$limit_string
      }
      if(!is.character(self$limit_string) && is.character(self$chunk_size_string)) {
        first_chunk <- self$chunk_size_string
      }
      if(is.character(first_chunk)) {
        url <- paste(url,'limit=', first_chunk, '&', sep="")
      }
      self$first_chunk <- first_chunk
      return(url)
    },
    #' Extract all results of a given resource or form
    #'
    #' This function allows you to express your love of cats.
    #' @param chunk_size How many elements to extract, usefull for large Data sets
    #' @keywords formio
    #' @export
    #' @examples
    #' get()
    get = function() {
      url <- paste(self$getQueryString(), self$getChunkSize(), sep="")
      url <- substr(url, 1, nchar(url)-1) 
      last_iteration_limit <- NULL
      # GET and Parse the content
      firstResult <- httr::GET(url, httr::add_headers("x-token" = self$token), httr::accept_json())
      
      error <- httr::http_error(firstResult)
      if(error){
        print('The query has an error or no results')
        el <- list()
        return(el)
      }
      text_content <- httr::content(firstResult, as = "text", encoding = "UTF-8")
      json_content <- jsonlite::fromJSON(text_content, flatten = TRUE)
      
      # Calculate iterations
      total <- strsplit(firstResult$headers$`content-range`, "/")[[1]][[2]]
      iterations_needed <- 0
      iterations_left <- 0
      if(!is.character(self$limit_string) && is.character(self$chunk_size_string)){
        iterations_needed <- ceiling(as.numeric(total) / as.numeric(self$chunk_size_string)) 
        iterations_left <- as.numeric(iterations_needed) -1
      }
      # Most complicated case
      else if(is.character(self$limit_string) && is.character(self$chunk_size_string)){
        if(as.numeric(total) >= as.numeric(self$limit_string)){
          possible_results = 0
          if(as.numeric(total) >= as.numeric(self$limit_string)) {
            possible_results <- as.numeric(self$limit_string)
          } else {
            possible_results <- as.numeric(total)
          }
          if(possible_results == 0) {
            iterations_needed <- 0
            iterations_left <- 0
          }
          else{
            iterations_needed <- ceiling( possible_results / as.numeric(self$first_chunk))
            last_iteration_limit <- possible_results / as.numeric(self$first_chunk)
            last_iteration_limit <- (as.numeric(last_iteration_limit) - floor(last_iteration_limit)) * as.numeric(self$first_chunk)
            if(as.numeric(last_iteration_limit) == 0) {
              last_iteration_limit <- as.numeric(self$first_chunk)
            }
            iterations_left <- as.numeric(iterations_needed) -1
          }
          
        }
      }
      
      
      chunks <- list()
      chunks[[1]] = json_content
      i <- 1
      while( as.numeric(iterations_left) > 0){
        # Calculate the next URL
        skip_amount <- as.character(as.numeric(self$chunk_size_string) * (as.numeric(iterations_needed) - as.numeric(iterations_left) ))
        limit <- self$chunk_size_string
        if(as.numeric(iterations_left) == 1){
          if(is.numeric(last_iteration_limit)) {
            limit <- last_iteration_limit
          }
        }
        it_url <- paste(self$getQueryString(), "&limit=", limit, "&skip=", skip_amount, sep=""); 
        # GET and Parse the content
        more_result <- httr::GET(it_url, httr::add_headers("x-token" = self$token), httr::accept_json())
        more_content <- httr::content(more_result, as = "text", encoding = "UTF-8")
        more_json <- jsonlite::fromJSON(more_content, flatten = TRUE)
        
        # Merge with previous results
        #total_results = rbind_pages(total_results, more_json)
        chunks[[i+1]] <- more_json
        
        # Next iteration
        iterations_left <- (as.numeric(iterations_left) - 1 )
        i <- i + 1
      }
      
      final_result <- jsonlite::rbind_pages(chunks)
      return(final_result)
    },
    #' Applies the filters
    #'
    #' @param filters the list of filters
    #' @export
    #' @examples
    #' filter()
    filter = function(filters){
      self$filter_string = self$filterToString(filters)
      return(self)
    },
    #' Applies the select
    #'
    #' @param select the list of select
    #' @export
    #' @examples
    #' select()
    select = function(select){
      self$select_string = self$selectToString(select)
      return(self)
    },
    #' Applies the limit to the Query
    #'
    #' @param limit the limit for the query
    #' @export
    #' @examples
    #' limit()
    limit = function(limit){
      self$limit_string = as.character(limit)
      return(self)
    },
    #' Defines how many elements to call each time
    #'
    #' @param size Number of elements on each call
    #' @export
    #' @examples
    #' limit()
    chunkSize = function(size){
      self$chunk_size_string = as.character(size)
      return(self)
    }
   )
  )

