# EDA source code Version 1.4

library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(lubridate)

usage_name <- c("인문사회계", "이공계", "예술계", "행정지원시설", "연구시설", 
                "강의지원시설", "편의시설", "학술지원시설", "숙박시설")
inst_name <- c("인문대학", "사범대학", "법과대학", "사회과학대학", "자연과학대학",
               "약학대학", "공과대학", "미술대학", "음악대학", "행정대학원", "경영대학", 
               "수의과대학", "환경대학원", "국제대학원", "농업생명과학대학", "보건대학원",
               "생활과학대학")
to_inst_name <- function(code){
  ifelse(as.numeric(code) <= 17, inst_name[code], paste0("기관", code))
}

############### Raw data Processing ###################
raw_data_process_all <- function(directory){
  # Expects subdirectories only have raw data, consists of year/month
  year_mon <- get_all_year_mon(directory)
  return(raw_data_process(directory, year_mon[[1]], year_mon[[2]]))
}

raw_data_year <- function(directory, year){
  return(raw_data_process(directory, rep(year, 12), 1:12))
}

get_all_year_mon <- function(directory){
  years <- c()
  months <- c()
  
  for(y in list.dirs(directory, recursive = FALSE)){
    mm <- parse_number(list.dirs(y, recursive = FALSE, full.names = FALSE))
    for(m in mm){
      years <- c(years, parse_number(y))
      months <- c(months, m)
    }
  }
  return(list(years, months))
}

raw_data_process <- function(directory, years, months){
  n1 <- length(years)
  n2 <- length(months)
  if(n1 != n2){
    print("Warnings: length of years and months not matching. Removing Extra elements.")
  }
  n <- min(n1, n2)
  
  tidy_data <- list()
  for(i in 1:n){
    if(months[i] < 12){
      nyear = years[i]
      nmonth = months[i] + 1
    }
    else{
      nyear = years[i] + 1
      nmonth = 1
    }
    tot_obs = ((ymd(paste(nyear, nmonth, "01", sep = "-")) - ymd(paste(years[i], months[i], "01", sep = "-"))) %>% as.numeric("days")) * 24
    
    data_dir <- paste0(directory, "/", years[i], "년/", months[i], "월")
    print(paste0("Processing ", data_dir))
    bldg <- list.files(path = data_dir, pattern = ".csv") %>% str_sub(end = -5)
    print(paste0(length(bldg), " buildings detected."))
    for(b in bldg){
      bldg_data <- NULL
      tryCatch(suppressMessages(bldg_data <- read_csv(paste0(data_dir, "/", b, ".csv"))),
               error = function(e) {print(paste0("Error occured while processing: ", data_dir, "/", b, ".csv"))},
               warning = function(w) {print(paste0("Problem occured while processing: ", data_dir, "/", b, ".csv, check the file."))}
               )
      if(is_null(bldg_data)) next
      tryCatch({bym_data <- bldg_data %>% gather(ends_with("시"), key = 시각, value = 사용량) %>% 
          mutate(건물 = b, 날짜 = ymd(날짜), 시각 = parse_number(시각)) %>% 
          mutate(일시 = ymd_hms(paste(날짜, hms::hms(hours = 시각)))) %>%
          select(건물, 일시, 사용량) %>% filter(일시 >= ymd(paste(years[i], months[i], "01", sep = "-")))
        if(!is.numeric(bym_data$사용량[1])) stop("not a number.")
      },
      error = function(e) {print(paste0("Error occured while processing: ", data_dir, "/", b, ".csv"))},
      warning = function(w) {print(paste0("Problem occured while processing: ", data_dir, "/", b, ".csv, check the file."))},
      finally = {
        check_zero = (nrow(bym_data) - tot_obs)
        if(check_zero > 0){print(paste0(check_zero, " values are empty in ", data_dir, "/", b, ".csv"))}
        tidy_data[[paste(i, b, sep="-")]] <- bym_data
      }
      )
    }
  }
  result = bind_rows(tidy_data) %>% arrange(건물, 일시) 
  ext = result %>% group_by(건물, year(일시), month(일시)) %>% summarise(사용량 = min(사용량)) %>% filter(사용량 <= 0)
  if (nrow(ext) > 0){
    print("Zero values are detected in:")
    for(i in 1:nrow(ext)){
      aa = unlist(ext[i,])
      print(paste0(aa[1], " - ", aa[2], "년 ", aa[3],"월"))
    }
  }
  
  return(result)
}

############### Basic graphs ###################

#to_monthly <- function(elec){
#  elec %>% mutate(연도 = year(날짜), 월 = month(날짜)) %>% group_by(건물, 연도, 월) %>% 
#    summarise(사용량 = sum(사용량)) %>% ungroup() %>%
#    mutate(날짜 = ymd(str_c(연도, 월, "01", sep = "-"))) %>% select(건물, 날짜, 사용량)
#}

graph_bar_year <- function(elec, bldg_list, bldg_data, bstart, bend, mode = "indiv", code_list = NULL){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  if(is.null(code_list)) code_name <- bldg_d$code %>% unique()
  
  get_code <- function(b){
    bldg_d$code[match(b, bldg_d$건물)]
  }
  
  elec_dd <- elec %>% filter(건물 %in% bldg_list, 날짜 >= bstart, 날짜 <= bend) %>% 
    mutate(code = get_code(건물)) %>% filter(code %in% code_name) %>%  group_by(code) %>% group_split()
  
  gp_list <- list()
  for(i in 1:length(elec_dd)){
    code <- as.character(elec_dd[[i]]$code[1])
    # print(paste0("Processing: ", code))
    gp_list[[code]] <- elec_dd[[i]] %>% mutate(연도 = as.factor(year(날짜))) %>% 
      group_by(연도) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
      ggplot(aes(x=연도, y =사용량, fill = 연도)) + geom_bar(stat = 'identity', position = 'dodge') + 
      scale_fill_brewer(palette="Paired") + theme_light() + labs(title = paste0("연간 전력사용량 그래프 (", code, ")"), y = "사용량 (kWH)") +
      theme(legend.position = "none")
  }
  gp_list
}

graph_bar_month <- function(elec, bldg_list, bldg_data, bstart, bend, mode = "indiv", code_list = NULL){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  if(is.null(code_list)) code_name <- bldg_d$code %>% unique()
  
  get_code <- function(b){
    bldg_d$code[match(b, bldg_d$건물)]
  }
  
  elec_dd <- elec %>% filter(건물 %in% bldg_list, 날짜 >= bstart, 날짜 <= bend) %>% 
    mutate(code = get_code(건물)) %>% filter(code %in% code_name) %>%  group_by(code) %>% group_split()
  
  gp_list <- list()
  for(i in 1:length(elec_dd)){
    code <- as.character(elec_dd[[i]]$code[1])
    # print(paste0("Processing: ", code))
    gp_list[[code]] <- elec_dd[[i]] %>% mutate(연도 = as.factor(year(날짜)), 월 = as.factor(month(날짜))) %>% 
      group_by(연도, 월) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
      ggplot(aes(x=월, y =사용량, fill = 연도)) + geom_bar(stat = 'identity', position = 'dodge') + 
      scale_fill_brewer(palette="Paired") + theme_light() + labs(title = paste0("월간 전력사용량 그래프 (", code, ")"), y = "사용량 (kWH)")
  }
  gp_list
}

graph_bar_weekday <- function(elec, bldg_list, bldg_data, bstart, bend, mode = "indiv", code_list = NULL){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  if(is.null(code_list)) code_name <- bldg_d$code %>% unique()
  
  get_code <- function(b){
    bldg_d$code[match(b, bldg_d$건물)]
  }
  
  elec_dd <- elec %>% filter(건물 %in% bldg_list, 날짜 >= bstart, 날짜 <= bend) %>% 
    mutate(code = get_code(건물)) %>% filter(code %in% code_name) %>%  group_by(code) %>% group_split()
  
  gp_list <- list()
  for(i in 1:length(elec_dd)){
    code <- as.character(elec_dd[[i]]$code[1])
    # print(paste0("Processing: ", code))
    gp_list[[code]] <- elec_dd[[i]] %>% mutate(연도 = as.factor(year(날짜)), 요일 = factor(weekdays(날짜), levels = c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일"))) %>% 
      group_by(연도, 요일) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
      ggplot(aes(x=요일, y =사용량, fill = 연도)) + geom_bar(stat = 'identity', position = 'dodge') + 
      scale_fill_brewer(palette="Paired") + theme_light() + labs(title = paste0("요일간 전력사용량 그래프 (", code, ")"), y = "사용량 (kWH)")
  }
  gp_list
}

graph_bar_day_month <- function(elec, bldg_list, bldg_data, bstart, bend, m, mode = "indiv", code_list = NULL){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  if(is.null(code_list)) code_name <- bldg_d$code %>% unique()
  
  get_code <- function(b){
    bldg_d$code[match(b, bldg_d$건물)]
  }
  
  wkd <- c("월", "화", "수", "목", "금", "토", "일")
  wk <- c(1,2,3,4)
  wkd <- rep(wkd, 4)
  wk <- rep(wk, rep(7, 4))
  wk_fct <- str_c(wk, wkd, sep = "-")
  
  elec_dd <- elec %>% filter(건물 %in% bldg_list, 날짜 >= bstart, 날짜 <= bend, month(날짜) == m) %>% 
    mutate(code = get_code(건물)) %>% filter(code %in% code_name) %>% group_by(code) %>% group_split()
  
  gp_list <- list()
  for(i in 1:length(elec_dd)){
    code <- as.character(elec_dd[[i]]$code[1])
    # print(paste0("Processing: ", code))
    gp_list[[code]] <- elec_dd[[i]] %>% mutate(연도 = year(날짜)) %>%
      mutate(주차 = as.numeric(날짜 - ymd(str_c(연도, m, "01", sep = "-")))%/%7 + 1) %>%
      filter(주차 <= 4) %>%
      mutate(요일 = str_sub(weekdays(날짜), end = 1)) %>% mutate(일자 = factor(str_c(주차, 요일, sep = "-"), levels = wk_fct)) %>%
      group_by(연도, 일자) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>% mutate(연도 = as.factor(연도)) %>%
      ggplot(aes(x=일자, y =사용량, fill = 연도)) + geom_bar(stat = 'identity', position = 'dodge') + 
      scale_fill_brewer(palette="Paired") + theme_light() + labs(title = paste0("일간 전력사용량 그래프 (", code, ")"), y = "사용량 (kWH)")
  }
  gp_list
}

graph_bar_hour <- function(elec, bldg_list, bldg_data, bstart, bend, mode = "indiv", code_list = NULL){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  if(is.null(code_list)) code_name <- bldg_d$code %>% unique()
  
  get_code <- function(b){
    bldg_d$code[match(b, bldg_d$건물)]
  }
  
  elec_dd <- elec %>% filter(건물 %in% bldg_list, 일시 >= bstart, 일시 <= bend) %>% 
    mutate(code = get_code(건물)) %>% filter(code %in% code_name) %>% group_by(code) %>% group_split()
  
  gp_list <- list()
  for(i in 1:length(elec_dd)){
    code <- as.character(elec_dd[[i]]$code[1])
    # print(paste0("Processing: ", code))
    gp_list[[code]] <- elec_dd[[i]] %>% mutate(시각 = as.factor(hour(일시)), 연도 = as.factor(year(일시))) %>% 
      group_by(시각, 연도) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
      ggplot(aes(x=시각, y =사용량, fill = 연도)) + geom_bar(stat = 'identity', position = 'dodge') + 
      scale_fill_brewer(palette="Paired") + theme_light() + labs(title = paste0("시간대별 전력사용량 그래프 (", code, ")"), y = "사용량 (kWH)")
  }
  gp_list
}

graph_bar_day <- function(elec, bldg_list, bldg_data, tstart, tend, y, mode = "indiv", code_list = NULL){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  if(is.null(code_list)) code_name <- bldg_d$code %>% unique()
  
  get_code <- function(b){
    bldg_d$code[match(b, bldg_d$건물)]
  }
  
  tys <- c()
  tye <- c()
  
  day_start <- paste0(month(tstart),"/",day(tstart))
  day_end <- paste0(month(tend),"/",day(tend))
  
  dd <- as.numeric(tend-tstart)
  
  for(i in 1:y){
    tys[i] <- as_date(tstart - 364*(i-1))
    tye[i] <- as_date(tend - 364*(i-1))
  }
  
  elec_d <- elec %>% filter(건물 %in% bldg_list) %>% mutate(code = get_code(건물)) %>% filter(code %in% code_name)
  
  elec_dd <- NULL
  for(i in 1:y){
    elec_dd <- elec_dd %>% bind_rows(elec_d %>% filter(날짜 >= tys[i], 날짜 <= tye[i]) %>% mutate(일차 = as.numeric(날짜 - tys[i]) + 1))
  }
  elec_dd <- elec_dd %>% group_by(code) %>% group_split()
  
  gp_list <- list()
  for(i in 1:length(elec_dd)){
    code <- as.character(elec_dd[[i]]$code[1])
    # print(paste0("Processing: ", code))
    gp_list[[code]] <- elec_dd[[i]] %>% mutate(연도 = as.factor(year(날짜))) %>% 
      group_by(일차, 연도) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
      ggplot(aes(x=일차, y =사용량, fill = 연도)) + geom_bar(stat = 'identity', position = 'dodge') + 
      scale_fill_brewer(palette="Paired") + theme_light() + labs(title = paste0("(", day_start ," - ", day_end ,") 일간 전력사용량 그래프 비교 (", code, ")"), y = "사용량 (kWH)")
  }
  gp_list
}

graph_time <- function(elec, bldg_list, bldg_data, bstart, bend, mode = "indiv", monthly = FALSE, code_list = NULL, log_scale = FALSE){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  if(is.null(code_list)) code_name <- bldg_d$code %>% unique()
  
  get_code <- function(b){
    bldg_d$code[match(b, bldg_d$건물)]
  }
  
  elec_dd <- elec %>% filter(건물 %in% bldg_list, 날짜 >= bstart, 날짜 <= bend)
  if(monthly){
    elec_dd <- elec_dd %>% mutate(연도 = year(날짜), 월 = month(날짜)) %>% group_by(건물, 연도, 월) %>% 
      summarise(사용량 = sum(사용량)) %>% ungroup() %>%
      mutate(날짜 = ymd(str_c(연도, 월, "01", sep = "-"))) %>% select(건물, 날짜, 사용량)
  }
  elec_dd <- elec_dd %>% mutate(code = get_code(건물)) %>% filter(code %in% code_name) %>% group_by(code) %>% group_split()
  
  gp_list <- list()
  for(i in 1:length(elec_dd)){
    code <- as.character(elec_dd[[i]]$code[1])
    gp_list[[code]] <- elec_dd[[i]] %>% group_by(날짜) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
      ggplot(aes(x = 날짜, y = 사용량))+ scale_x_date(date_labels = "%Y-%b") + theme_bw() + labs(title = paste0("전력사용량 그래프 (", code, ")"), y = "사용량 (kWH)")
    if(monthly) {gp_list[[code]] <- gp_list[[code]] + geom_bar(stat='identity')
    } else {
      gp_list[[code]] <- gp_list[[code]] + geom_line() 
    }
    if(log_scale) gp_list[[code]] <- gp_list[[code]] + scale_y_log10()
  }
  gp_list
}

season <- function(d){
  s <- c(rep("겨울",3), rep("봄", 2), rep("여름", 3), rep("가을", 2), rep("겨울",2))
  s[month(d)]
}

graph_temp <- function(elec, temp, bldg_list, bldg_data, bstart, bend, mode = "indiv", code_list = NULL, log_scale = FALSE){
  gp_list <- list()
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  
  if(is.null(code_list)) code_name <- bldg_d$code %>% unique()
  
  get_code <- function(b){
    bldg_d$code[match(b, bldg_d$건물)]
  }
  
  elec_dd <- elec %>% filter(건물 %in% bldg_list, 날짜 >= bstart, 날짜 <= bend) %>% 
    mutate(code = get_code(건물)) %>% filter(code %in% code_name) %>% group_by(code) %>% group_split()
  
  for(i in 1:length(elec_dd)){
    code <- as.character(elec_dd[[i]]$code[1])
    gp_list[[code]] <- elec_dd[[i]] %>% group_by(날짜) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
      inner_join(temp, by = "날짜") %>% mutate(휴일 = as.factor(1*(요일 > 5)), 계절 = season(날짜)) %>%
      ggplot(aes(x = 기온, y = 사용량, shape = 휴일, colour = 계절)) + geom_point(size = 1.5) +
      scale_shape_manual(values=c(16,1)) +
      theme_bw() + labs(title = paste0("기온-전력사용량 그래프 (", code, ")"), y = "사용량 (kWH)")
    if(log_scale){
      gp_list[[code]] <- gp_list[[code]] + scale_y_log10()
    }
  }
  gp_list
}

gen_stats <- function(elec_d, temp_d){
  bldg <- elec_d$건물[1]
  b_data <- elec_d %>% inner_join(temp_d, by = "날짜") %>% mutate(log사용량 = log(사용량 + 0.01), 휴일 = (요일 > 5)*1)
  
  # 온도민감도 계산
  bldg_model <- lm(log사용량 ~ 휴일 + 기온, data = b_data)
  tibble(건물 = bldg, 온도민감도 = bldg_model$coefficients[3])
}

all_stats <- function(elec_h, elec_d, temp_d, bldg_data, bldg_list, bstart, bend, mode = "indiv", code_list = NULL){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  
  elec_d <- elec_d %>% filter(날짜 >= bstart, 날짜 <= bend, !is.na(사용량))
  
  #에너지 집약도
  eui <- inner_join(elec_d %>% group_by(건물) %>% summarise(사용량 = sum(사용량)),
                    bldg_d %>% select(건물, 연면적, code) %>% filter(!is.na(연면적)), by = "건물") %>% 
    group_by(code) %>% summarise(사용량 = sum(사용량), 연면적 = sum(연면적)) %>%
    mutate(EUI = 사용량/연면적) %>% select(code, 사용량, EUI) %>% mutate(건물 = code) %>% select(건물, 사용량, EUI)
  
  elec_h <- elec_h %>% filter(일시 >= bstart, 일시 <= bend, 건물 %in% bldg_list) %>%
    left_join(bldg_d %>% select(건물, code), by = "건물") %>% 
    filter(!is.na(code)) %>% group_by(일시, code) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
    mutate(건물 = code)
  
  #기저부하율
  h_sum <- elec_h %>% mutate(시각 = as.factor(hour(일시))) %>% group_by(건물, 시각) %>% 
    summarise(사용량 = mean(사용량)) %>% summarise(최소사용량 = min(사용량), 평균사용량 = mean(사용량)) %>%
    mutate(기저부하율 = 최소사용량/평균사용량) %>% select(건물, 기저부하율)
  
  if(is.null(code_list)) code_name <- bldg_d %>% filter(건물 %in% bldg_list) %>% .$code %>% unique()
  code_name <- as.character(code_name)
  
  elec_dd <- elec_d %>% left_join(bldg_d %>% select(건물, code), by = "건물") %>% 
    filter(건물 %in% bldg_list, code %in% code_name) %>% group_by(날짜, code) %>% summarise(사용량 = sum(사용량)) %>% 
    ungroup() %>% mutate(건물 = code) %>% group_by(건물) %>% group_split()
  
  result <- map_dfr(elec_dd, ~{gen_stats(., temp_d)}) %>% 
    inner_join(eui, by = "건물") %>% inner_join(h_sum, by = "건물")
  
  if(mode == "indiv") return(result[order(as.numeric(str_extract(result$건물, "^[0-9]+"))),])
  result %>% arrange(건물)
}

gen_stats_y <- function(elec_d, temp_d){
  bldg <- elec_d$건물[1]
  b_data <- elec_d %>% inner_join(temp_d, by = "날짜") %>% mutate(log사용량 = log(사용량 + 0.01), 휴일 = (요일 > 5)*1)
  
  # 온도민감도 계산
  change_point <- min_point(b_data$기온, b_data$사용량)
  b_data <- b_data %>% mutate(냉방 = hinge(기온, change_point), 난방 = hinge(-기온, -change_point))
  bldg_model <- lm(log사용량 ~ 휴일 + 냉방 + 난방, data = b_data)
  
  tibble(건물 = bldg, 냉방민감도 = bldg_model$coefficients[3], 난방민감도 = bldg_model$coefficients[4])
}

all_stats_y <- function(elec_h, elec_d, temp_d, bldg_data, bldg_list, bstart, bend, mode = "indiv", code_list = NULL){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  
  elec_d <- elec_d %>% filter(날짜 >= bstart, 날짜 <= bend, !is.na(사용량))
  
  #에너지 집약도
  eui <- inner_join(elec_d %>% group_by(건물) %>% summarise(사용량 = sum(사용량)),
                    bldg_d %>% select(건물, 연면적, code) %>% filter(!is.na(연면적)), by = "건물") %>% 
    group_by(code) %>% summarise(사용량 = sum(사용량), 연면적 = sum(연면적)) %>%
    mutate(EUI = 사용량/연면적) %>% select(code, 사용량, EUI) %>% mutate(건물 = code) %>% select(건물, 사용량, EUI)
  
  elec_h <- elec_h %>% filter(일시 >= bstart, 일시 <= bend, 건물 %in% bldg_list) %>%
    left_join(bldg_d %>% select(건물, code), by = "건물") %>% 
    filter(!is.na(code)) %>% group_by(일시, code) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
    mutate(건물 = code)
  
  #기저부하율
  h_sum <- elec_h %>% mutate(시각 = as.factor(hour(일시))) %>% group_by(건물, 시각) %>% 
    summarise(사용량 = mean(사용량)) %>% summarise(최소사용량 = min(사용량), 평균사용량 = mean(사용량)) %>%
    mutate(기저부하율 = 최소사용량/평균사용량) %>% select(건물, 기저부하율)
  
  if(is.null(code_list)) code_name <- bldg_d %>% filter(건물 %in% bldg_list) %>% .$code %>% unique()
  code_name <- as.character(code_name)
  
  elec_dd <- elec_d %>% left_join(bldg_d %>% select(건물, code), by = "건물") %>% 
    filter(건물 %in% bldg_list, code %in% code_name) %>% group_by(날짜, code) %>% summarise(사용량 = sum(사용량)) %>% 
    ungroup() %>% mutate(건물 = code) %>% group_by(건물) %>% group_split()
  
  result <- map_dfr(elec_dd, ~{gen_stats_y(., temp_d)}) %>% 
    inner_join(eui, by = "건물") %>% inner_join(h_sum, by = "건물")
  
  if(mode == "indiv") return(result[order(as.numeric(str_extract(result$건물, "^[0-9]+"))),])
  result %>% arrange(건물)
}


############### generating log linear models ###################

# Helper functions for model
hinge <- function(x, h){
  (x - h)*(x - h>0)
}
min_point <- function(x, y){
  n <- length(x)
  order_x <- order(x)
  x_o <- x[order_x] # ordered by temp
  y_o <- y[order_x] # ordered by temp
  minx <- ceiling(x_o[1])
  maxx <- floor(x_o[n])
  possible_seq <- seq(minx, maxx, 1)
  cur_ind <- 1
  start_point <- 1
  ind <- 1:length(possible_seq)
  means <- c()
  samples <- c()
  for(j in 1:(n-1)){
    if(x_o[j] > possible_seq[cur_ind] + 0.5){
      means[cur_ind] <- mean(y_o[start_point:(j-1)])
      samples[cur_ind] <- j-start_point
      cur_ind <- cur_ind + 1
      start_point <- j
    }
  }
  means[cur_ind] <- mean(y_o[start_point:n])
  return(possible_seq[which.min(means)])
}

# 전체 모형 적합


gen_model_loglinear <- function(bldg_data, temp, bstart, bend, rstart, rend, mode = "indiv"){
  bldg <- bldg_data$건물[1]
  
  b_data <- bldg_data %>% 
    filter(날짜 >= bstart, 날짜 <= bend) %>%
    inner_join(temp, by = "날짜") %>% mutate(log사용량 = log(사용량 + 0.01), 휴일 = (요일 > 5)*1)
  
  bldg_model <- lm(log사용량 ~ 휴일 + 기온, data = b_data)
  r_data <- bldg_data %>%
    filter(날짜 >= rstart, 날짜 <= rend) %>%
    inner_join(temp, by = "날짜") %>% mutate(log사용량 = log(사용량 + 0.01), 휴일 = (요일 > 5)*1)
  
  r_data <- r_data %>% mutate(베이스라인조정량 = exp(predict(bldg_model, newdata = .))) %>% mutate(절감량 = 베이스라인조정량 - 사용량)
  
  monthly_data <- r_data %>% mutate(연도 = year(날짜), 월 = month(날짜)) %>% group_by(연도, 월) %>%
    summarise(사용량 = sum(사용량), 베이스라인조정량 = sum(베이스라인조정량), 절감량 = sum(절감량)) %>% ungroup()
  
  all_sum <- monthly_data %>% summarise_all(sum) %>% mutate(연도 = 0, 월 = 0)
  
  graph_data <- b_data %>% mutate(비고 = "베이스라인사용량") %>% select(날짜, 사용량, 비고)
  graph_data <- graph_data %>% bind_rows(r_data %>% mutate(비고 = "보고기간사용량") %>% select(날짜, 사용량, 비고),
                                         r_data %>% mutate(비고 = "베이스라인조정량") %>% select(날짜, 베이스라인조정량, 비고) %>% rename(사용량 = 베이스라인조정량))
  
  b_plot <- graph_data %>% filter(비고 == "베이스라인사용량") %>% ggplot(aes(x = 날짜, y = 사용량, colour = 비고, linetype = 비고)) + 
    geom_line(colour = "grey40") +
    scale_x_date(date_labels = "%Y-%b") +
    theme_bw() + labs(title = paste0("베이스라인 사용량 (", bldg, ")"), y = "사용량 (kWH)")
  
  r_plot <- graph_data %>% filter(비고 != "베이스라인사용량") %>% ggplot(aes(x = 날짜, y = 사용량, colour = 비고, linetype = 비고)) + geom_line() +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_color_manual(values = c("royalblue", "tomato")) +
    scale_x_date(date_labels = "%Y-%b") +
    theme_bw() + labs(title = paste0("보고기간 사용량 및 베이스라인 조정량 (", bldg, ")"), y = "사용량 (kWH)")
  
  #if(mode == "indiv"){
  #  b_plot <- b_plot + labs(title = paste0("베이스라인 사용량 (", bldg, ")"))
  #  r_plot <- r_plot + labs(title = paste0("보고기간 사용량 및 베이스라인 조정량 (", bldg, ")"))
  #} 
  #if(mode == "usage"){
  #  b_plot <- b_plot + labs(title = paste0("베이스라인 사용량 (용도코드", bldg, ")"))
  #  r_plot <- r_plot + labs(title = paste0("보고기간 사용량 및 베이스라인 조정량 (용도코드", bldg, ")"))
  #} 
  #if(mode == "inst"){
  #  b_plot <- b_plot + labs(title = paste0("베이스라인 사용량 (기관코드", bldg, ")"))
  #  r_plot <- r_plot + labs(title = paste0("보고기간 사용량 및 베이스라인 조정량 (기관코드", bldg, ")"))
  #} 
  
  coef <- bldg_model$coefficients
  bldg_summary <- summary(bldg_model)
  
  tibble(건물 = bldg, 보고기간사용량 = all_sum$사용량[1], 베이스라인조정량 = all_sum$베이스라인조정량[1], 절감량 = all_sum$절감량[1], 
           r2 = bldg_summary$r.squared, Intercept = coef[1], 휴일 = coef[2], 기온 = coef[3]) %>%
    bind_cols(r_data %>% select(날짜, 사용량, 베이스라인조정량, 절감량) %>% nest(일별 = everything()), monthly_data %>% bind_rows(all_sum) %>% nest(월별=everything())) %>%
    mutate(b_plot = list(b_plot), r_plot = list(r_plot))
}

models_loglinear <- function(all_daily_energy, all_daily_temp, bldg_data, bldg_list, bstart, bend, rstart, rend, mode = "indiv", code_list = NULL){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  if(is.null(code_list)) code_name <- bldg_d %>% filter(건물 %in% bldg_list) %>% .$code %>% unique()
  code_name <- as.character(code_name)
  
  elec_dd <- elec_d %>% filter(건물 %in% bldg_list, 날짜 >= bstart, 날짜 <= rend) %>% 
    left_join(bldg_d %>% select(건물, code), by = "건물") %>% 
    filter(code %in% code_name) %>% group_by(날짜, code) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
    mutate(건물 = code) %>% group_by(건물) %>% group_split()
  
  map_dfr(elec_dd, ~{gen_model_loglinear(., all_daily_temp, bstart, bend, rstart, rend, mode)})
}

gen_model_loglinear_y <- function(elec_d, temp, bstart, bend, rstart, rend, mode = "indiv"){
  bldg <- elec_d$건물[1]
  b_data <- elec_d %>% 
    filter(날짜 >= bstart, 날짜 <= bend) %>%
    inner_join(temp, by = "날짜") %>% mutate(log사용량 = log(사용량 + 0.01), 휴일 = (요일 > 5)*1)
  
  change_point <- min_point(b_data$기온, b_data$사용량)
  b_data <- b_data %>% mutate(냉방 = hinge(기온, change_point), 난방 = hinge(-기온, -change_point))
  
  bldg_model <- lm(log사용량 ~ 휴일 + 냉방 + 난방, data = b_data)
  
  r_data <- elec_d %>%
    filter(날짜 >= rstart, 날짜 <= rend) %>%
    inner_join(temp, by = "날짜") %>% mutate(log사용량 = log(사용량 + 0.01), 휴일 = (요일 > 5)*1) %>%
    mutate(냉방 = hinge(기온, change_point), 난방 = hinge(-기온, -change_point))
  
  r_data <- r_data %>% mutate(베이스라인조정량 = exp(predict(bldg_model, newdata = .))) %>% mutate(절감량 = 베이스라인조정량 - 사용량)
  monthly_data <- r_data %>% mutate(연도 = year(날짜), 월 = month(날짜)) %>% group_by(연도, 월) %>%
    summarise(사용량 = sum(사용량), 베이스라인조정량 = sum(베이스라인조정량), 절감량 = sum(절감량)) %>% ungroup()
  
  all_sum <- monthly_data %>% summarise_all(sum) %>% mutate(연도 = 0, 월 = 0)
  
  graph_data <- b_data %>% mutate(비고 = "베이스라인사용량") %>% select(날짜, 사용량, 비고)
  graph_data <- graph_data %>% bind_rows(r_data %>% mutate(비고 = "보고기간사용량") %>% select(날짜, 사용량, 비고),
                                         r_data %>% mutate(비고 = "베이스라인조정량") %>% select(날짜, 베이스라인조정량, 비고) %>% rename(사용량 = 베이스라인조정량))
  
  b_plot <- graph_data %>% filter(비고 == "베이스라인사용량") %>% ggplot(aes(x = 날짜, y = 사용량, colour = 비고, linetype = 비고)) + 
    geom_line(colour = "grey40") +
    scale_x_date(date_labels = "%Y-%b") +
    theme_bw() + labs(title = paste0("베이스라인 사용량 (", bldg, ")"), y = "사용량 (kWH)")
  
  r_plot <- graph_data %>% filter(비고 != "베이스라인사용량") %>% ggplot(aes(x = 날짜, y = 사용량, colour = 비고, linetype = 비고)) + geom_line() +
    scale_linetype_manual(values = c("dashed", "solid")) +
    scale_color_manual(values = c("royalblue", "tomato")) +
    scale_x_date(date_labels = "%Y-%b") +
    theme_bw() + labs(title = paste0("보고기간 사용량 및 베이스라인 조정량 (", bldg, ")"), y = "사용량 (kWH)")
  
  coef <- bldg_model$coefficients
  bldg_summary <- summary(bldg_model)
  
  
  tibble(건물 = bldg, 보고기간사용량 = all_sum$사용량[1], 베이스라인조정량 = all_sum$베이스라인조정량[1], 절감량 = all_sum$절감량[1], 
           r2 = bldg_summary$r.squared, Intercept = coef[1], 휴일 = coef[2], 냉방 = coef[3], 난방 = coef[4]) %>%
    bind_cols(r_data %>% select(날짜, 사용량, 베이스라인조정량, 절감량) %>% nest(일별 = everything()), monthly_data %>% bind_rows(all_sum) %>% nest(월별=everything())) %>%
    mutate(b_plot = list(b_plot), r_plot = list(r_plot))
}

models_loglinear_y <- function(all_daily_energy, all_daily_temp, bldg_data, bldg_list, bstart, bend, rstart, rend, mode = "indiv", code_list = NULL){
  if(mode == "indiv"){
    bldg_d <- bldg_data %>% mutate(code = 건물)
  } else if (mode == "usage"){
    bldg_d <- bldg_data %>% mutate(code = usage_name[용도코드])
    code_name <- usage_name[code_list]
  } else if (mode == "inst"){
    bldg_d <- bldg_data %>% mutate(code = to_inst_name(기관코드))
    code_name <- to_inst_name(code_list)
  } else {
    print("mode not recognized.")
    return()
  }
  if(is.null(code_list)) code_name <- bldg_d %>% filter(건물 %in% bldg_list) %>% .$code %>% unique()
  code_name <- as.character(code_name)
  
  elec_dd <- elec_d %>% filter(건물 %in% bldg_list, 날짜 >= bstart, 날짜 <= rend) %>% 
    left_join(bldg_d %>% select(건물, code), by = "건물") %>% 
    filter(code %in% code_name) %>% group_by(날짜, code) %>% summarise(사용량 = sum(사용량)) %>% ungroup() %>%
    mutate(건물 = code) %>% group_by(건물) %>% group_split()
  
  map_dfr(elec_dd, ~{gen_model_loglinear_y(., all_daily_temp, bstart, bend, rstart, rend, mode)})
}

