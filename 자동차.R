library(rvest)
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(httr)

# 크롤링할 웹 페이지 주소
url <-"https://finance.naver.com/sise/sise_group_detail.naver?type=upjong&no=273"
# HTML 페이지 파싱
page <- read_html(url, encoding="euc-kr")

# 테이블 데이터 추출
table <- page %>% html_nodes("table")%>% html_table()

# 필요한 테이블 선택
target_table <- table[[3]]

# 데이터 프레임에서 결측치가 있는 열 제거
clean_data <- target_table[, colSums(is.na(target_table)) == 0]
data<-clean_data[c(-1,-11,-12),]

# 종목코드 추출하기 
links <- page %>% html_nodes("a")  # 모든 <a> 태그 선택
codes <- links %>% html_attr("href") %>% 
  str_extract("(?<=code=)[0-9]+")  # 종목 코드 추출

# 중복 제거 및 결측치 제거
codes <- codes[!duplicated(codes)]  # 중복 제거
codes <- codes[!is.na(codes)]  # 결측치 제거
# 결과 출력
print(codes)

# 데이터 프레임 합치기
combined_data <- cbind(data, codes)
colnames(combined_data)[ncol(combined_data)] <- "종목코드"
combined_data

combined_data
# 거래량 열의 데이터에서 숫자만추출
volume <- as.numeric(gsub(",", "", combined_data$거래량))

# 변환된 데이터를 데이터프레임에 다시 할당
combined_data$거래량 <- volume

#거래대금 숫자만 추출 
daegum <- as.numeric(gsub(",", "", combined_data$거래대금))
combined_data$거래대금<- daegum

#거래량이 높은 순서대로 데이터 시각화 
library(ggplot2)

# 거래량이 높은 순으로 데이터 정렬
sorted_data <- combined_data[order(combined_data$거래량, decreasing = TRUE), ]

# 거래대금이 높은 순으로 데이터 정렬
daegum_data <- combined_data[order(combined_data$거래대금, decreasing = TRUE), ]

install.packages("gridExtra")
library(gridExtra)
library(dplyr)

# 거래량 그래프
plot_volume <- ggplot(sorted_data, aes(x = 거래량, y = reorder(종목명, 거래량))) +
  geom_bar(stat = "identity", fill = "gray", width = 0.3) +
  labs(x = "거래량", y = "종목명", title = "종목별 거래량") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(family = "AppleGothic"))

# 거래대금 그래프
plot_amount <- ggplot(daegum_data, aes(x = 거래대금, y = reorder(종목명, 거래대금))) +
  geom_col(fill = "gray", width = 0.3) +
  labs(x = "거래대금", y = "종목명", title = "종목별 거래대금") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(family = "AppleGothic"))

# 거래량이 증가하면 거래대금도 증가하나 ?
# 그래프를 함께 표시
grid.arrange(plot_volume, plot_amount, ncol = 2)


#현대차 관련주 거래량과 전체 거래량의 평균 

# 필요한 종목만 추출
subset_data <- combined_data[combined_data$종목명 %in% c("현대차3우B", "현대차우", "현대차", "현대차2우B"), ]

data_h<-subset_data %>% select(종목명,거래량,거래대금)
data_h

mean(subset_data$거래량)
# 전체 거래량 평균 계산
total_volume_mean <- mean(subset_data$거래량)

# 그래프 그리기
ggplot(data_h, aes(x = 종목명, y = 거래량, fill = 종목명)) +
  geom_bar(stat = "identity", width = 0.3) +
  geom_hline(yintercept = total_volume_mean, color = "red", 
             linetype = "dashed", size = 0.5) +
  labs(x = "종목명", y = "거래량", title = "현대차 관련주 거래량과 전체 거래량 평균") +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)+
  theme(text = element_text(family = "AppleGothic"))


#기아, 현대차 데이터 ( 2013~2023 거래량)
get_stock_data <- function(codes, start_date, end_date) {
  dfs <- list()  # 종목 데이터를 저장할 리스트 생성
  
  for (code in codes) {
    # API 주소 생성
    url <- sprintf("https://api.finance.naver.com/siseJson.naver?symbol=%s&requestType=1&startTime=%s&endTime=%s&timeframe=day",
                   code, start_date, end_date)
    # API 요청 보내기 
    response <- GET(url)
    content <- content(response, as = "text")
    
    # JSON 데이터 추출 및 가공
    json_data <- gsub("'{1,2}", "\"", content)
    json_data <- gsub("^.*?\\[\\[", "[[", json_data)
    json_data <- gsub(",\\]", "]", json_data)
    
    # 데이터프레임으로 변환
    df <- fromJSON(json_data, simplifyDataFrame = TRUE)
    
    # 첫 번째 행을 열 이름으로 설정
    colnames(df) <- unlist(df[1, ])
    df <- df[-1, ]
    
    # 종목 데이터를 리스트에 저장
    dfs[[code]] <- df
  }
  
  return(dfs)
}


# 기아, 현대차
codes <- c("000270", "005380") 

# 시작일과 종료일 설정
start_date <- "20130608"
end_date <- "20230608"

# 종목 데이터 가져오기
dfs <- get_stock_data(codes, start_date, end_date)

# 각 종목 데이터에 접근하여 확인
for (code in codes) {
  df <- dfs[[code]]
  print(paste("Stock Code:", code))
  print(head(df))
  cat("\n")
}

# 결과를 저장할 빈 데이터 프레임 생성
volume_sum_df <- data.frame(Stock_Code = character(), Year = numeric(), Volume_Sum = numeric(),stringsAsFactors = FALSE)

# 각 종목 코드에 대해 반복
for (code in codes) {
  # 해당 종목 코드에 해당하는 데이터프레임 가져오기
  df <- dfs[[code]]
  
  # 연도별 거래량 합을 저장할 빈 데이터 프레임 생성
  year_volume_df <- data.frame(Year = numeric(), Volume_Sum = numeric(), stringsAsFactors = FALSE)
  
  # 2017부터 2023까지 각 연도에 대해 반복
  for (year in 2013:2023) {
    # 해당 연도에 해당하는 데이터 필터링
    df_year <- df[grepl(as.character(year), df[,"날짜"]), ]
    
    # 해당 연도의 거래량 합 계산
    volume_sum <- sum(as.numeric(df_year[,"거래량"]), na.rm = TRUE)
    
    # 연도와 거래량 합을 데이터 프레임으로 생성
    result <- data.frame(Year = year, Volume_Sum = volume_sum)
    
    # 결과를 year_volume_df 데이터 프레임에 추가
    year_volume_df <- rbind(year_volume_df, result)
  }
  
  # 종목 코드를 year_volume_df 데이터 프레임에 추가
  year_volume_df$Stock_Code <- code
  
  # year_volume_df 데이터 프레임을 volume_sum_df 데이터 프레임에 추가
  volume_sum_df <- rbind(volume_sum_df, year_volume_df)
}

# 종목 코드와 기업 종목명을 매핑하는 데이터프레임 생성
code_mapping <- data.frame(Stock_Code = c("000270", "005380"), 
                           Company_Name = c("기아", "현대차"))

# wide_df와 code_mapping을 조인하여 기업 종목명으로 변환
volume_df <- volume_sum_df %>% 
  spread(key = Year, value = Volume_Sum) %>% 
  left_join(code_mapping, by = "Stock_Code") %>% 
  select(Company_Name, everything())
# 종목 코드 열 삭제
volume_df$Stock_Code <- NULL

# 10년간 년도별로 기아와 현대차의 거래량 분포 
print(volume_df)

#10년간 년도별로  기아와 현대차의 종가 가격분포 
# 결과를 저장할 빈 데이터 프레임 생성
price_sum_df <- data.frame(Stock_Code = character(), Year = numeric(), Price_Sum = numeric(), stringsAsFactors = FALSE)

# 각 종목 코드에 대해 반복
for (code in codes) {
  # 해당 종목 코드에 해당하는 데이터프레임 가져오기
  df <- dfs[[code]]
  
  # 연도별 종가 합을 저장할 빈 데이터 프레임 생성
  year_price_df <- data.frame(Year = numeric(), Price_Sum = numeric(), stringsAsFactors = FALSE)
  
  # 2013부터 2023까지 각 연도에 대해 반복
  for (year in 2013:2023) {
    # 해당 연도에 해당하는 데이터 필터링
    df_year <- df[grepl(as.character(year), df[,"날짜"]), ]
    
    # 해당 연도의 종가 합 계산 
    price_sum <- sum(as.numeric(df_year[,"종가"]), na.rm = TRUE)
    
    # 연도와 종가 합을 데이터 프레임으로 생성
    result <- data.frame(Year = year, Price_Sum = price_sum)
    
    # 결과를 year_price_df 데이터 프레임에 추가
    year_price_df <- rbind(year_price_df, result)
  }
  
  # 종목 코드를 year_price_df 데이터 프레임에 추가
  year_price_df$Stock_Code <- code
  
  # year_price_df 데이터 프레임을 price_sum_df 데이터 프레임에 추가
  price_sum_df <- rbind(price_sum_df, year_price_df)
}

# 종목 코드와 기업 종목명을 매핑하는 데이터프레임 생성
code_mapping <- data.frame(Stock_Code = c("000270", "005380"), 
                           Company_Name = c("기아", "현대차"))

# wide_df와 code_mapping을 조인하여 기업 종목명으로 변환
price_df <- price_sum_df %>%
  spread(key = Year, value = Price_Sum) %>%
  left_join(code_mapping, by = "Stock_Code") %>%
  select(Company_Name, everything())
# 종목 코드 열 삭제
price_df$Stock_Code <- NULL

# 2013년부터 2023년까지의 종가 합 데이터프레임 출력
print(price_df)

# wide 형식에서 long 형식으로 변환
long_df_v <- tidyr::gather(volume_df, Year, Volume, -Company_Name)
long_df_p <-tidyr::gather(price_df, Year, price, -Company_Name)
# Year 열을 숫자로 변환
long_df_v$Year <- as.numeric(long_df_v$Year)
long_df_p$Year <- as.numeric(long_df_p$Year)

# 두 데이터프레임 합치기
merged_df <- merge(long_df_v, long_df_p, by = c("Company_Name", "Year"))

# 합친 데이터프레임 출력
print(merged_df)

# 그래프 생성

ggplot(merged_df, aes(x = Year, group = Company_Name)) +
  geom_line(aes(y = Volume, color = Company_Name, linetype = "Volume"), size = 0.5) +
  geom_line(aes(y = price, color = Company_Name, linetype = "Price"), size = 0.5) +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_minimal() +
  geom_point(aes(y = Volume, color = Company_Name), size = 1) +
  geom_point(aes(y = price, color = Company_Name), size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./1e6, name = "거래량(백만)")) +
  theme(text = element_text(family = "AppleGothic")) +
  scale_x_continuous(breaks = seq(2012, 2023, 1), labels = seq(2012, 2023, 1))+
  guides(color = guide_legend(title = "종목"), linetype = guide_legend(title = "데이터"))
