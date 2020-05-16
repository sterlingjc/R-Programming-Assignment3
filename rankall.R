
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  
  if( is.character(num) ){
    num <- trimws(toupper(num))
  }
  
  
  if(num == 'BEST'){
    
    num <-1
    
  }
  
  
  num_args <- c('BEST', 'WORST')
  
  outcome <- trimws(toupper(outcome))
  
  outcomes <- c('HEART ATTACK', 'HEART FAILURE', 'PNEUMONIA')
  
  outcomes <- toupper(outcomes)
  
  outcomes <- trimws(outcomes)
  
  cols_of_interest <- c(2,7,11,17,23)
  
  outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings='Not Available', stringsAsFactors=FALSE)[,cols_of_interest]
  
  names(outcome_df) <- c('name','state','HEART ATTACK', 'HEART FAILURE', 'PNEUMONIA')
  
  
  for (i in 3:5) {
    
    outcome_df[, i] <- as.numeric(outcome_df[, i])
    
  }
  
  states <- unique(outcome_df$state)
  
  
  ## Check that num and outcome are valid
  
  if(!(outcome %in% outcomes)) {
    
    stop('invalid outcome')
    
  }
  
  
  
  if( !(is.numeric(num))   & !(outcome %in% outcomes) ) {
    
    stop('invalid num')
    
  }
  
  
  
  ## For each state, find the hospital of the given rank
  
  
  outcome_df <- outcome_df[!is.na(outcome_df[, outcome]) , c(1,2, 2+which(outcomes==outcome)   ) ]
  
  if (num=='WORST') {
    outcome_df <- outcome_df[ order(outcome_df$state , -outcome_df[ , outcome], outcome_df$name ) , ]
    num <- 1
    
  } else {
    outcome_df <- outcome_df[ order(outcome_df$state , outcome_df[ , outcome], outcome_df$name ) , ]
    
  }
  
  list_of_data_frames <- split(outcome_df, outcome_df$state)
  
  state_list <- sapply(  list_of_data_frames , function(x){ x$name[num]   })
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  result <- as.data.frame(cbind( state_list , names(state_list)))
  
  names(result) <- c('state', 'name')

  
  rownames(result) <- names(state_list)
  
  result
  
  }