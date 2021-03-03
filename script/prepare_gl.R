# select and filter for only the necessary columns
# adds a column stating whether visitor or home team won
# saves time writing this as a function rather than writing the select and filter multiple times
# selects for columns in positions 1, 5, 8, and 11 through 13
# filters for extra inning games or regular inning games
# requires an existing data object
prepare_gl <- function(.data, filter_var){
  assign(.data, dplyr::select(get(.data), 1, 5, 8, 11:13), envir = .GlobalEnv)
  if(filter_var == "extra"){
    assign(.data, dplyr::filter(get(.data), length_of_game_outs > 54), envir = .GlobalEnv)
  }
  else if(filter_var == "regular"){
    assign(.data, dplyr::filter(get(.data), length_of_game_outs <= 54), envir = .GlobalEnv)
  }
  assign(.data, dplyr::mutate(get(.data), winning_team = case_when((.data$visiting_score > .data$home_score) ~ "visitor",
                                                                   (.data$visiting_score < .data$home_score) ~ "home")),
         envir = .GlobalEnv)
}
