
#' A function to read ATracker csv data file
#'
#' @param path_to_csv where to find the csv file
#' @keywords read data
#' @export
#' @import stringr
#' @import dplyr
#' @importFrom lubridate dmy_hms
#' @examples
#' read_ATracker_data("./data.csv")

read_ATracker_data <- function(path_to_csv) {
  data <- readr::read_csv2(path_to_csv)
  names(data) <- str_trim(names(data))

  data2 <- data %>%
    mutate(start_time = `Start time` %>%
             str_replace_all('[.] ', '') %>%
             str_replace_all(', ', '') %>%
             str_replace_all('[.]', ''),
           start_month = str_extract(start_time, '[a-z]{3}'),
           end_time = `End time` %>%
             str_replace_all('[.] ', '') %>%
             str_replace_all(', ', '') %>%
             str_replace_all('[.]', ''),
           end_month = str_extract(end_time, '[a-z]{3}'))

  lookup_table <- data_frame(month = tolower(month.abb),
                             month_num = c(paste0('0', 1:9), 10:12))

  data3 <- left_join(data2, lookup_table, c('start_month' = 'month')) %>%
    left_join(., lookup_table, c('end_month' = 'month'))

  data4 <- data3 %>%
    mutate(start_time = str_replace(start_time, '[a-z]{3}', month_num.x),
           end_time = str_replace(end_time, '[a-z]{3}', month_num.y),
           start_time = ifelse(nchar(start_time) == 13, paste0('0', start_time), start_time),
           end_time = ifelse(nchar(end_time) == 13, paste0('0', end_time), end_time),
           start_time = as.character(dmy_hms(start_time)),
           end_time = as.character(dmy_hms(end_time)),
           duration_hours = round(as.numeric(difftime(end_time, start_time, units = 'hours')), 2),
           duration_mins = round(as.numeric(difftime(end_time, start_time, units = 'mins')), 2),
           start_date = as.character(as.Date(start_time)),
           end_date = as.character(as.Date(end_time))) %>%
    rename(name = `Task name`) %>%
    select(name, start_time, end_time, duration_hours, duration_mins, start_date, end_date)

  return(data4)
}
