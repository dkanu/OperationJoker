#Helpers

#' Filter by Dates
#'
#' @param x tibble; Display & Video 360 Report
#' @param method char; type of date filter to enact
#' custom - filters custom start and end dates given in %m-%d-%Y
#' lag - filters date range so that the start date is to max date - lag days
#' and the end date is the max date in the data frame.
#' all_time - filters from min date to max date in the data frame
#' @param start char; for use with custom method, format %m-%d-%Y
#' @param end char; for use with custom method, format %m-%d-%Y
#' @param n_lag integer - lagged days from max date in the dataframe for start date
#'
#' @return tibble
#' @export
#'
#' @examples
filterDate <- function(x, method = c('custom','lag','all_time'), start = NULL, end = NULL, n_lag = NULL){
  
  if (method == 'custom'){
    end_date = as.Date(end, "%m-%d-%Y")
    start_date = as.Date(start, "%m-%d-%Y")
  }
  else if (method == 'lag'){
    end_date = as.Date(max(x$Date, na.rm = TRUE), "%m-%d-%Y")
    start_date = as.Date(end_date-n_lag, "%m-%d-%Y")
  }
  else if (method == 'all_time'){
    end_date = as.Date(max(x$Date, na.rm = TRUE), "%m-%d-%Y")
    start_date = as.Date(min(x$Date, na.rm = TRUE), "%m-%d-%Y")
  }
  
  # print(sprintf('Filtered dates %s to %s', toString(start_date), toString(end_date)))
  
  x %>% 
    filter(`Date` >= start_date & `Date` <= end_date)
}

#' Group DV360 Advertisments
#'
#' @param x tibble; Display & Video 360 Report
#' @param entity char; type of grouping to perform
#' insertion_order - group by `Insertion Order ID`
#' line_item - group by group by `Line Item ID` and `Insertion Order`
#' placement - group by `App/Url`
#'
#'
#' @return tibble grouping type, grouping ids, spend, impressions, clicks, converstions, cpa, cmp,
#' average daily spend, average daily impressions, average daily clicks, average daily conversions
#' @export
#'
#' @examples
groupAds <- function(x,entity = c('insertion_order','line_item','placement_naive','placement_sophisticated')){
  end_date <- as.Date(max(x$Date, na.rm = TRUE), "%m-%d-%Y")
  start_date <- as.Date(min(x$Date, na.rm = TRUE), "%m-%d-%Y")
  
  delta = as.numeric(end_date - start_date, unit = "days") + 1
  
  #(print(delta))
  
  
  if (entity == 'insertion_order') {
    x <- x %>%
      group_by(`Insertion Order ID`) %>%
      summarize(
        `Insertion Order` = unique(`Insertion Order`),
        Spend = sum(`Revenue (Adv Currency)`),
        Impressions = sum(`Impressions`),
        Clicks = sum(`Clicks`),
        Conversions = sum(`Total Conversions`),
        
        CPA = ifelse(
          is.nan(Spend / Conversions) |
            !is.finite(Spend / Conversions),
          0,
          Spend / Conversions
        ),
        
        CPM = ifelse(
          is.nan((Spend / Impressions) * 1000) |
            !is.finite((Spend / Impressions) * 1000),
          0,
          (Spend / Impressions) * 1000
          )
    )
  }
  
  else if (entity == 'line_item') {
    x <- x %>%
      group_by(`Line Item ID`, `Insertion Order`, `Insertion Order ID`) %>%
      summarize(`Line Item` = unique(`Line Item`) ,
                Spend = sum(`Revenue (Adv Currency)`),
                Impressions = sum(`Impressions`),
                Clicks = sum(`Clicks`),
                Conversions = sum(`Total Conversions`),
                CPA = ifelse(
                  is.nan(Spend / Conversions) |
                    !is.finite(Spend / Conversions),
                  0,
                  Spend / Conversions
                  ),
                CPM = ifelse(
                  is.nan((Spend / Impressions) * 1000) |
                    !is.finite((Spend / Impressions) * 1000),
                  0,
                  (Spend / Impressions) * 1000
                  )
      )
    }
  
  else if (entity == 'placement_naive') {
    x <- x %>%
      group_by(`App/URL ID`) %>%
      summarize(`App/URL` = unique(`App/URL`),
                Spend = sum(`Revenue (Adv Currency)`),
                Impressions = sum(`Impressions`),
                Clicks = sum(`Clicks`),
                Conversions = sum(`Total Conversions`),
                
                CPA = ifelse(
                  is.nan(Spend / Conversions) |
                    !is.finite(Spend / Conversions),
                  0,
                  Spend / Conversions
                  ),
                
                CPM = ifelse(
                  is.nan((Spend / Impressions) * 1000) |
                    !is.finite((Spend / Impressions) * 1000),
                  0,
                  (Spend / Impressions) * 1000
                  )
      )
  }
    
    else if (entity == 'placement_sophisticated'){
      x <- x %>%
        group_by(`Insertion Order ID`, `Insertion Order`, `Line Item ID`, `Line Item`, `App/URL ID`) %>%
        summarize(`App/URL` = unique(`App/URL`),
                  Spend = sum(`Revenue (Adv Currency)`),
                  Impressions = sum(`Impressions`),
                  Clicks = sum(`Clicks`),
                  Conversions = sum(`Total Conversions`),
                  
                  CPA = ifelse(
                    is.nan(Spend / Conversions) |
                      !is.finite(Spend / Conversions),
                    0,
                    Spend / Conversions
                  ),
                  
                  CPM = ifelse(
                    is.nan((Spend / Impressions) * 1000) |
                      !is.finite((Spend / Impressions) * 1000),
                    0,
                    (Spend / Impressions) * 1000
                  )
        )
    }
  
  x <- x %>%
    {
      if (delta == 1){
        mutate(.,
          Avg_Daily_Spend = Spend,
          Avg_Daily_Impressions = Impressions,
          Avg_Daily_Clicks = Clicks,
          Avg_Daily_Conversions = Conversions
        )
      }
      else{
        mutate(.,
          Avg_Daily_Spend = Spend/delta,
          Avg_Daily_Impressions = Impressions/delta,
          Avg_Daily_Clicks = Clicks/delta,
          Avg_Daily_Conversions = Conversions/delta
        )
      }
    }
  x
}

#################################
## Interesting Comparisons
## Compare t0 to t1 arbitrarily
## Error + Flags
#################################
#' Compare Periods
#'
#' @param x tibble
#' @param tol double between 0 and 1 to adjust sensitivity of comparison. Larger values increase senstivity.
#'
#' @return tibble with comparsion columns: low_spend, spend_below, impressions_below, clicsk_below, conversions_below
#' @export
#'
#' @examples
ComparePeriods <- function(x, tol = .5){
  x %>%
    filter(Spend1 < 1 | 
             Spend1 < (Avg_Daily_Spend*tol) |
             Impressions1 < (Avg_Daily_Impressions*tol) | 
             Clicks1 < (Avg_Daily_Clicks*tol) | 
             Conversions1 < (Avg_Daily_Conversions*tol)
         ) %>%
    
  mutate(
    low_spend = case_when(Spend1 < 1 ~ 1,
                          Spend1 >= 1 ~ 0),
    
    spend_below = case_when(Spend1 < (Avg_Daily_Spend*tol) ~ 1,
                            Spend1 >= (Avg_Daily_Spend*tol) ~ 0),
    
    impressions_below = case_when(Impressions1 < (Avg_Daily_Impressions*tol) ~ 1,
                                  Impressions1 >= (Avg_Daily_Impressions*tol) ~ 0),
    
    clicks_below = case_when(Clicks1 < (Avg_Daily_Clicks*tol) ~ 1,
                             Clicks1 >= (Avg_Daily_Clicks*tol) ~ 0),
    
    conversions_below = case_when(Conversions1 < (Avg_Daily_Conversions*tol) ~ 1,
                                  Conversions1 >= (Avg_Daily_Conversions*tol) ~ 0),
    
    error_sum = low_spend + spend_below + impressions_below + clicks_below + conversions_below
  ) %>%
    arrange(-error_sum, -Spend1)
  
}
#################################
## Filter convinience functions
#################################
filterLineItemIDs <- function(x, targets){
  filter(x, `Line Item ID` %in% targets)
}
filterLineItems <- function(x, targets){
  filter(x, grepl(targets, `Line Item ID`))
}
filterIoIDs <- function(x, targets){
  filter(x, `Insertion Order ID` %in% targets)
}
filterIo <- function(x, targets){
  filter(x, grepl(targets, `Insertion Order`))
}
filterAppUrl <- function(x, targets){
  filter(x, grepl(targets, `App/URL`))
}
filterAppUrlIDs <- function(x, targets){
  filter(x, `App/URL ID` %in% targets)
}
###############################################
## Diagnostics Functions
###############################################
ErrorDiagnostics <- function(x){
  ed0 <- sprintf("\t\trows: %i \n", nrow(x))
  ed1 <- sprintf("\t\tcolumns: %i \n", ncol(x))
  ed2 <- sprintf("\t\tlow spend: %i \n", sum(x$low_spend))
  ed3 <- sprintf("\t\tspend_below: %i \n", sum(x$spend_below))
  ed4 <- sprintf("\t\timpressions_below: %i \n", sum(x$impressions_below))
  ed5 <- sprintf("\t\tclicks_below: %i \n", sum(x$clicks_below))
  ed6 <- sprintf("\t\tconversions_below: %d \n\n", sum(x$conversions_below))
  cat(paste0(ed0,ed1,ed2,ed3,ed4,ed5,ed6))
}

UniqueIDDiagnostics <- function(x, entity=c("insertion_order", "line_item", "placement")){
  
  if(entity == "insertion_order"){
    title <- "Insertion Orders: \n"
    io <- sprintf("\tUnique IO IDs: %i \n", length(unique(x$`Insertion Order ID`)))
    cat(paste0(title, io))
  }
  
  else if(entity == "line_item"){
    title <- "Line Items: \n"
    io <- sprintf("\tUnique IO IDs: %i \n", length(unique(x$`Insertion Order ID`)))
    li <- sprintf("\tUnique Line Item IDs: %i \n", length(unique(x$`Line Item ID`)))
    cat(paste0(title, io, li))
  }
  
  else if(entity == "placement"){
    title <- "Placements: \n"
    io <- sprintf("\tUnique IO IDs: %i \n", length(unique(x$`Insertion Order ID`)))
    li <- sprintf("\tUnique Line Item IDs: %i \n", length(unique(x$`Line Item ID`)))
    p <- sprintf("\tUnique App URL IDs: %i \n", length(unique(x$`App/URL ID`)))
    cat(paste0(title, io, li, p))
  }
}

###############################################
## JSON Storage Easy Access
###############################################
CreateJsonDict <- function(x, entity = c('insertion_order','line_item')){
  if (entity == "insertion_order"){
    x <- x %>%
      select(`Insertion Order`, `Insertion Order ID`)
  }
  
  else if (entity == "line_item"){
    x <- x %>%
      select(`Insertion Order ID`, `Insertion Order`, `Line Item ID`, `Line Item`)
  }
  
  write(toJSON(x, pretty = TRUE), 
        file = (sprintf('%s_out.txt', entity))
        )
}
################################################
## Testing Structure
################################################
#' Testing Structure
#'
#' @param t1 tibble; t1 is a period longer than t0
#' @param t2 tibble; t0 is a period shorter than t1
#' @param diagnostics boolean; print diagnostic information?
#'
#' @return list of tibbles with error reporting at different entity levels
#' @export
#'
#' @examples
doTest <- function(t1,t2, diagnostics = FALSE){
  
  periods <- list(t1,t2)
  
  # Grouping
  io <- lapply(periods, groupAds, entity = 'insertion_order')
  li <- lapply(periods, groupAds, entity = 'line_item')
  pn <- lapply(periods, groupAds, entity = 'placement_naive')
  ps <- lapply(periods, groupAds, entity = 'placement_sophisticated')
  
  # Merge
  io_m <- merge(io[[1]],io[[2]], by = 'Insertion Order ID', suffixes = c('','1'))
  li_m <- merge(li[[1]],li[[2]], by = 'Line Item ID', suffixes = c('','1'))
  pn_m <- merge(pn[[1]],pn[[2]], by = 'App/URL ID', suffixes = c('','1'))
  ps_m <- merge(ps[[1]],ps[[2]], by = 'App/URL ID', suffixes = c('','1'))
  
  #Compare
  io_c <- ComparePeriods(io_m, tol = 1)
  li_c <- ComparePeriods(li_m, tol = 1)
  pn_c <- ComparePeriods(pn_m, tol = 1)
  ps_c <- ComparePeriods(ps_m, tol = 1)
  
  if(diagnostics == TRUE){
    #Unique ID Diagonsitcs + Error Diagnostics
    UniqueIDDiagnostics(io_c, entity = "insertion_order")
    ErrorDiagnostics(io_c)
    
    UniqueIDDiagnostics(li_c, entity = "line_item")
    ErrorDiagnostics(li_c)
    
    UniqueIDDiagnostics(pn_c, entity = "placement")
    ErrorDiagnostics(pn_c)
    
    UniqueIDDiagnostics(ps_c, entity = "placement")
    ErrorDiagnostics(ps_c)
  }
  
  out <- list(io_c, li_c, pn_c, ps_c)
  names(out) <- c('io','li','pn','ps')
  return(out)
}
