library(RSelenium)
library(rvest)
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(httr)
library(gridExtra)

#터미널에 입력 
system("docker run -d -p 4455:4444 selenium/standalone-chrome")
system("docker ps")

remDr<-remoteDriver(
  remoteServerAddr = 'localhost', 
  port = 4455, 
  browserName = 'chrome'
) 

remDr$getStatus()
remDr$close()
remDr$open()

remDr$navigate("https://finance.naver.com/sise/sise_group_detail.naver?type=upjong&no=273")

webElem <- remDr$findElements(using = "css", value = "input[checked]")

for (i in 1:length(webElem)) webElem[[i]]$clickElement()

option <- paste("#option", 1:27, sep="")

#원하는 항목 선택
for (i in option[c(1,3,4,13,15,19)]) {
  webElem <- remDr$findElement(using = "css", i)
  
  webElem$clickElement()
}

# "적용하기" 버튼 클릭
applyButton <- remDr$findElement(using = "css", ".item_btn a:first-child")
applyButton$clickElement()

remDr$screenshot(display = TRUE)

# 페이지 소스를 가져오기
html <- remDr$getPageSource()[[1]]
# 페이지 소스를 HTML로 파싱 
page <- read_html(html)
# 원하는 테이블을  추출
table <- page %>%
  html_nodes("table") %>% 
  html_table()
table
# 필요한 테이블 선택
target_table<-table[[3]]

#NA없이 
data<-target_table[c(-1,-11,-12),]
data <- data[, colSums(is.na(data)) == 0]

#종목코드
# 종목코드 추출하기 
links <- page %>% html_nodes("a")  # 모든 <a> 태그 선택
codes <- links %>% html_attr("href") %>% 
  str_extract("(?<=code=)[0-9]+") 
# 중복 제거 및 결측치 제거
codes <- codes[!duplicated(codes)]  # 중복 제거
codes <- codes[!is.na(codes)]  # 결측치 제거

# 데이터 프레임 합치기
combined_data <- cbind(data, codes)
colnames(combined_data)[ncol(combined_data)] <- "종목코드"
combined_data

# 거래량 열의 데이터에서 숫자만추출
volume <- as.numeric(gsub(",", "", combined_data$거래량))

# 변환된 데이터를 데이터프레임에 다시 할당
combined_data$거래량 <- volume

# 거래량 그래프
ggplot(combined_data, aes(x = 거래량, y = reorder(종목명, 거래량),fill = 종목명)) +
  geom_bar(stat = "identity",  width = 0.3) +
  labs(x = "거래량", y = "종목명", title = "종목별 거래량")+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()

# 당일 거래량을 통해서 
# 자동차업종에서는 엘브이엠씨홀딩스에 투자 관심이 가장 많다는 점을 알았다.


# 시가총액 열의 데이터에서 숫자만추출
market<- as.numeric(gsub(",", "", combined_data$시가총액))
# 변환된 데이터를 데이터프레임에 다시 할당
combined_data$시가총액 <- market


# 시가총액 막대 그래프 (시가총액 높은 순서대로)
ggplot(combined_data, aes(x = reorder(종목명, -시가총액), y = 시가총액,fill = 종목명)) +
  geom_bar(stat = "identity", width = 0.3) +
  labs(x = "종목명", y = "시가총액", title = "종목별 시가총액") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


#시가총액 top2 기아, 현대차 10년간 거래량, 거래대금(종가) 상관관계분석 

#거래량데이터 ( 2013~2023)
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


# 종목들 10년간 데이터 
codes <- codes

# 시작일과 종료일 설정
start_date <- "20130608"
end_date <- "20230608"

# 종목 데이터 가져오기
dfs <- get_stock_data(codes, start_date, end_date)

# 종가, 거래량 그래프 
#1년마다 거래량들을 합해서 간단하게 년도별로 거래량 보기 
# 결과를 저장할 빈 데이터 프레임 생성
volume_sum_df <- data.frame(Stock_Code = character(), Year = numeric(), Volume_Sum = numeric(),stringsAsFactors = FALSE)

# 각 종목 코드에 대해 반복
for (code in codes) {
  # 해당 종목 코드에 해당하는 데이터프레임 가져오기
  df <- dfs[[code]]
  
  # 연도별 거래량 합을 저장할 빈 데이터 프레임 생성
  year_volume_df <- data.frame(Year = numeric(), Volume_Sum = numeric(), stringsAsFactors = FALSE)
  
  # 2013부터 2023까지 각 연도에 대해 반복
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
code_mapping <- data.frame(Stock_Code = c(codes), 
                           Company_Name = c(combined_data$종목명))

# wide_df와 code_mapping을 조인하여 기업 종목명으로 변환
volume_df <- volume_sum_df %>% 
  spread(key = Year, value = Volume_Sum) %>% 
  left_join(code_mapping, by = "Stock_Code") %>% 
  select(Company_Name, everything())

# 종목 코드 열 삭제
volume_df$Stock_Code <- NULL

# 년도별로 종목의 거래량의 합 
volume_df

#10년간 년도별로  기아와 현대차의 종가 가격분포 (가격대금)
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
code_mapping <- data.frame(Stock_Code = c(codes), 
                           Company_Name = c(combined_data$종목명))

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

sum_by_company <- merged_df %>%
  group_by(Company_Name) %>%
  summarise(
    Total_Volume = sum(Volume),
    Total_Price = sum(price))

print(sum_by_company)

# 거래량을 기준으로 내림차순 정렬
sum_by_company_volume <- sum_by_company[order(sum_by_company$Total_Volume, decreasing = TRUE), ]

# 거래대금을 기준으로 내림차순 정렬
sum_by_company_price <- sum_by_company[order(sum_by_company$Total_Price, decreasing = TRUE), ]

# 종목별로 색상 할당
company_colors <- rainbow(nrow(sum_by_company))

# 거래량을 기준으로 내림차순으로 정렬된 그래프 
ggplot(sum_by_company_volume, aes(y= reorder(Company_Name, -Total_Volume), x = Total_Volume, 
                                           fill = Company_Name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = company_colors) +
  ggtitle("종목별 10년간 거래량") +
  xlab("종목명") +
  ylab("총 거래량") +
  theme(plot.title = element_text(hjust = 0.3)) +
  coord_flip()+
  scale_x_continuous(labels = scales::comma)

# 거래대금을 기준으로 내림차순 정렬된 그래프
ggplot(sum_by_company_price, aes(y = reorder(Company_Name, -Total_Price), 
                                          x = Total_Price,fill = Company_Name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = company_colors) +
  ggtitle("종목별 10년간 거래대금") +
  xlab("종목명") +
  ylab("총 거래대금") +
  coord_flip()+
  scale_x_continuous(labels = scales::comma)



#시가총액 top2 기아와 현대차 데이터만 비교하기 
#기아와 현대차 데이터만 뽑기 
subset_data <- merged_df[merged_df$Company_Name %in% c("기아", "현대차"), ]
print(subset_data)

# 거래량 그래프
ggplot(subset_data, aes(x = Year, y = Volume, color = Company_Name)) +
  geom_line() +
  labs(x = "Year", y = "거래량", title = "종목별 거래량(10년)") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)

#거래량 막대그래프
ggplot(subset_data, aes(x = Year, y = Volume, fill = Company_Name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "거래량", title = "종목별 거래량(10년)") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# 거래대금 그래프
ggplot(subset_data, aes(x = Year, y = price, color = Company_Name)) +
  geom_line() +
  labs(x = "Year", y = "거래대금(종가)", title = "종목별 거래대금(종가)") +
  scale_color_manual(values = c("red", "blue")) +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)

#거래대금 막대그래프 
ggplot(subset_data, aes(x = Year, y = price, fill = Company_Name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "거래대금(종가)", title = "종목별 거래대금(종가)") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)


# Filter the data for 기아 and calculate the correlation
kia_data <- merged_df[merged_df$Company_Name == "기아", ]
kia_data<- kia_data %>% select(Volume,price)
cor(kia_data)
summary(kia_data)
# Scatter plot for 기아
sp1<-ggplot(kia_data, aes(x = Volume, y = price)) +
  geom_point(size=3,color="red") +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") + #추세선
  labs(title = "Correlation: 기아") +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)
  

# Filter the data for 현대차 and calculate the correlation
hyundai_data <- merged_df[merged_df$Company_Name == "현대차", ]
hyundai_data<-hyundai_data %>% select(Volume,price)
cor(hyundai_data)
summary(hyundai_data)

# Scatter plot for 현대차
sp2<-ggplot(hyundai_data, aes(x = Volume, y = price)) +
  geom_point(size=3,color="blue") +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") + #추세선
  labs(title = "Correlation: 현대차") +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

grid.arrange(sp1, sp2, ncol = 2)


#거래량 변화율 
calculate_daily_return <- function(df) {
  # Convert "날짜" column to Date format
  df$날짜 <- as.Date(df$날짜, format = "%Y%m%d")
  
  # 2023년 5월 1일부터 2023년 6월 8일까지의 데이터 추출
  df <- df[df$날짜 >= "2023-05-01" & df$날짜 <= "2023-06-08", ]
  
  # Extract month and day from the "날짜" column
  df$날짜 <- format(df$날짜, "%m-%d")
  
  # Convert "종가" column to numeric
  df$거래량 <- as.numeric(df$거래량)
  
  # 종가의 등락률 계산
  df$변화율 <- (df$거래량 - lag(df$거래량)) / lag(df$거래량) * 100
  
  # 첫 번째 날짜의 등락률을 NA로 설정
  df$변화율[1] <- NA
  
  return(df)
}

#기아
kia_df <- as.data.frame(dfs[["000270"]])
kia_change<-kia_df %>% select(날짜,종가, 거래량,외국인소진율)

# 기아의 변화율
kia_change_m<-calculate_daily_return(kia_change)


# Create the individual plots
plot1 <- ggplot(kia_change_m, aes(x = 날짜, y = 변화율, group = 1)) +
  geom_line(color = "red") +
  labs(x = "날짜", y = "등락률", title = "기아 거래량 변화율") +
  theme_minimal()

plot2 <- ggplot(kia_change_m, aes(x = 날짜, y = 외국인소진율, group = 1)) +
  geom_line(color = "red") +
  labs(x = "날짜", y = "외국인 소진율", title = "기아 외국인 소진율 변화") +
  theme_minimal()

# Arrange the plots side by side
grid.arrange(plot1, plot2, nrow = 2)

#현대
hyundai_df <- as.data.frame(dfs[["005380"]])
h_change<-hyundai_df %>% select(날짜,종가, 거래량,외국인소진율)

h_change_m<-calculate_daily_return(h_change)
h_change_m

# 변화율 그래프 그리기
plot3<- ggplot(h_change_m, aes(x = 날짜, y =변화율,group=1)) +
  geom_line(color="blue") +
  labs(x = "날짜", y = "거래량 변화율", title = "현대 거래량변화율") +
  theme_minimal()

# 외국인 소진율을 꺾은선 그래프로 표현
plot4<-ggplot(h_change_m, aes(x = 날짜, y = 외국인소진율,group=1)) +
  geom_line(color="blue") +
  labs(x = "날짜", y = "외국인 소진율", title = "현대 외국인 소진율 변화") +
  theme_minimal()

grid.arrange(plot3, plot4, nrow = 2)
