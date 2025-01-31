# install tidyverse package
# version 1.3.0
#install.packages("tidyverse")

# loads tidyverse library for working with data
# version 1.3.0
library(tidyverse)


# read_gl function
# reads in a game log dataset from a given directory and creates a data object using a given name ->
# uses the here() function from the here package to read data directory
# using the rename function() from dplyr package ->
# renames columns to something more representative and understandable
# using mutate() function from dplyr ->
# adds a column with the year of the season
read_gl <- function(.data, file_dir){
  assign(.data, read.table(here::here(file_dir), header = FALSE, sep = ","), envir = .GlobalEnv)
  assign(.data, dplyr::rename(get(.data), date = V1, game_type = V2, weekday = V3, visiting_team = V4, visiting_league = V5,
                              visiting_game_no = V6, home_team = V7, home_league = V8, home_game_no = V9, visiting_score = V10,
                              home_score = V11, length_of_game_outs = V12, day_night = V13, completion = V14, forfeit = V15,
                              protest = V16, parkid = V17, attendance = V18, time_of_game_mins = V19, visiting_scoreline = V20,
                              home_scoreline = V21, visiting_ab = V22, visiting_hits = V23, visiting_dbls = V24, visiting_trpls = V25,
                              visiting_hrs = V26, visiting_rbi = V27, visiting_sachit = V28, visiting_sacfly = V29, visiting_hbp = V30,
                              visiting_bb = V31, visiting_ibb = V32, visiting_k = V33, visiting_sb = V34, visiting_cs = V35,
                              visiting_odp = V36, visiting_ci = V37, visiting_lob = V38, visiting_pitcher_no = V39, visiting_ier = V40,
                              visiting_ter = V41, visiting_wp = V42, visiting_balks = V43, visiting_putouts = V44, visiting_assists = V45,
                              visiting_errors = V46, visiting_passedballs = V47, visiting_ddp = V48, visiting_dtp = V49, home_ab = V50,
                              home_hits = V51, home_dbls = V52, home_trpls = V53, home_hrs = V54, home_rbi = V55, home_sachit = V56,
                              home_sacfly = V57, home_hbp = V58, home_bb = V59, home_ibb = V60, home_k = V61, home_sb = V62, home_cs = V63,
                              home_odp = V64, home_ci = V65, home_lob = V66, home_pitcher_no = V67, home_ier = V68, home_ter = V69,
                              home_wp = V70, home_balks = V71, home_putouts = V72, home_assists = V73, home_errors = V74,
                              home_passedballs = V75, home_ddp = V76, home_dtp = V77, umphome_id = V78, umphome_name = V79, ump1b_id = V80,
                              ump1b_name = V81, ump2b_id = V82, ump2b_name = V83, ump3b_id = V84, ump3b_name = V85, umplf_id = V86,
                              umplf_name = V87, umprf_id = V88, umprf_name = V89, visiting_mngrid = V90, visiting_mngrname = V91,
                              home_mngrid = V92, home_mngrname = V93, win_pitchid = V94, win_pitchname = V95, lose_pitchid = V96,
                              lose_pitchname = V97, sv_pitchid = V98, sv_pitchname = V99, game_winning_rbi_id = V100,
                              game_winning_rbi_name = V101, vis_strtp_id = V102, vis_strtp_name = V103, home_strtp_id = V104,
                              home_strtp_name = V105, vis_strt1_id = V106, vis_strt1_name = V107, vis_strt1_def = V108, vis_strt2_id = V109,
                              vis_strt2_name = V110, vis_strt2_def = V111, vis_strt3_id = V112, vis_strt3_name = V113, vis_strt3_def = V114,
                              vis_strt4_id = V115, vis_strt4_name = V116, vis_strt4_def = V117, vis_strt5_id = V118, vis_strt5_name = V119,
                              vis_strt5_def = V120, vis_strt6_id = V124, vis_strt6_name = V122, vis_strt6_def = V123, vis_strt7_id = V124,
                              vis_strt7_name = V125, vis_strt7_def = V126, vis_strt8_id = V127, vis_strt8_name = V128, vis_strt8_def = V129,
                              vis_strt9_id = V130, vis_strt9_name = V131, vis_strt9_def = V132, home_strt1_id = V133, home_strt1_name = V134,
                              home_strt1_def = V135, home_strt2_id = V136, home_strt2_name = V137, home_strt2_def = V138, home_strt3_id = V139,
                              home_strt3_name = V140, home_strt3_def = V141, home_strt4_id = V142, home_strt4_name = V143, home_strt4_def = V144,
                              home_strt5_id = V145, home_strt5_name = V146, home_strt5_def = V147, home_strt6_id = V148, home_strt6_name = V149,
                              home_strt6_def = V150, home_strt7_id = V151, home_strt7_name = V152, home_strt7_def = V153, home_strt8_id = V154,
                              home_strt8_name = V155, home_strt8_def = V156, home_strt9_id = V157, home_strt9_name = V158, home_strt9_def = V159,
                              additional_info = V160, acquisition_info = V161), envir = .GlobalEnv)
  assign(.data, dplyr::mutate(get(.data), year = substr(.data$date, start = 1, stop = 4), .before = 1), envir = .GlobalEnv)
}
