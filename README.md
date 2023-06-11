---
title: "증권데이터분석"
output: html_document
date: "2023-06-11"
---

### 증권데이터 분석하기

도로에서 흔히 볼수있는 자동차 회사는 기아와 현대가있다.\
기아와 현대의 10년간 거래량 데이터를 불러와서 거래량이 높은 연도에 각 기업에 무슨 이슈가 있는지 알아보고싶어졌다.

기아와 현대차 분석 전에 자동차 관련주에는 어떤 종목이 있는지 살펴보았다.

```
library(rvest)
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(dplyr)

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
data
```


매수호가,매도호가,거래량,거래대금,전일거래량이 모두 0의 값을 가지는 기업은 "스마트솔루션즈" 이다.\
모두 0인것의 의미는 무엇일까 ?\
모두 0인 주식은 주식시장에서 주목받지 않는 종목으로 간주할 수 있다. 일반적으로 현재 거래가 없거나 거래정지 상태인 주식을 의미한다.\
스마트 솔루션즈는 IT 기업인데 자동차 관련주에 있는 이유는 자동차 산업이 디지털화 되어 가면서 자동차 제조업체들에게 스마트 기술의 도입등을 지원하고 , 
자율주행 기술 , 차량 간의 통신과 인터넷 연결성을 강화해 안전성과 편의성 등 당야한 서비스를 제공하게 하기 때문에 자동차 업종과 함께 일을 하고 있다.\
이런 기업의 주식 거래량이 0인 이유는 현재 시점에서 거래가 없는것이라고 볼 수 있다.\
거래량과 거래대금을 살펴보자

# 단순히 거래량이 많다 = 거래대금이 많다 일까 ?

```# 종목코드 추출하기 
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

# 거래량이 높은 순으로 데이터 정렬
sorted_data <- combined_data[order(combined_data$거래량, decreasing = TRUE), ]

# 거래대금이 높은 순으로 데이터 정렬
daegum_data <- combined_data[order(combined_data$거래대금, decreasing = TRUE), ]

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

#두 그래프를 같이보고싶다.
library(gridExtra)
# 그래프를 함께 표시
grid.arrange(plot_volume, plot_amount, ncol = 2)
```

거래량이 증가하면 거래대금도 증가하나 ?

거래량이 가장 높은 종목은 엘브이엠씨 홀딩스 , 하지만 거래대금이 가장 높은 종목은 기아 이다.
이처럼 단순하게 거래대금이 많다고 해서 거래량이 많은것도 아니고 그 반대로 생각해도 같지는 않다.
거래량과 거래 대금이 적절하게 증가했다는 것은 주식 시장에서 관심이 집중되었다는 뜻이다.
\
\
종목명에서 처음보는 이름들이 있다.\
현대차, 현대차3우B, 현대차2우B, 현대차우 가 해당한다.\
이들은 다같은 현대라는 이름을 가지고있는데 무엇이 다른걸까 ?\
\
현대차는 보통주로 가장 일반적인 형태의 주식이다. 일반 투자자들에게 제공된다.\
\
현대차3우B는 3우선주로 일반주주와 2우선주보다 더 우선적인 권리를 가진다.\
배당 및 처분 우선권에 있어서 다양한 우선권 혜택을 받을 수 있다.\
\
현대차2우B는 2 우선주로 3우선주 다음으로 우선권이 있다.\
즉 이 두개의 종목은 주식의 우선권 차이를 가지고 있다고 볼수있다.
\
현대차우는 우선주이다.\
현대차보다 상대적으로 더 저렴한 가격으로 거래되는 주식으로, 주식의 가격 차이를 이용해 수익을 얻기위해서 투자되는 종목이다.\
\
따라서 투자자는 현대차보다 저렴한 가격에 우선주를 구매해 주가 상승시 이익을 얻을 수 있는 가능성이 있다.

```
#현대차 관련주 거래량과 전체 거래량의 평균 

# 필요한 종목만 추출
subset_data <- combined_data[combined_data$종목명 %in% c("현대차3우B", "현대차우", "현대차", "현대차2우B"), ]

data_h<-subset_data %>% select(종목명,거래량,거래대금)
data_h

mean(subset_data$거래량)
# 전체 거래량 평균 계산
total_volume_mean <- mean(subset_data$거래량)
```

현대차 관련주 종목들의 총 거래량의 평균은 127453.2에 해당한다.

# 현대차 관련주의 종목들을 이용해서 총 거래량의 평균과 거래량 시각화

```{r setu, include=TRUE}
knitr::opts_chunk$set(echo = F,message=F,warning = F)
# 그래프 그리기
ggplot(data_h, aes(x = 종목명, y = 거래량, fill = 종목명)) +
  geom_bar(stat = "identity", width = 0.3) +
  geom_hline(yintercept = total_volume_mean, color = "red", 
             linetype = "dashed", size = 0.5) +
  labs(x = "종목명", y = "거래량", title = "현대차 관련주 거래량과 전체 거래량 평균") +
  theme_minimal()+
  scale_y_continuous(labels = scales::comma)+
  theme(text = element_text(family = "AppleGothic"))
```


그래프를 보면 총 거래량의 평균보다 높은 종목은 현대차가 해당하는데 이는 시장에서 주목받는 종목이나 투자자들의 관심이 집중되어 있다는 것을 의미한다.
현대차 관련주 들 중에서는 현대차 투자가 높다는 것이다.
\
\
거래량과 거래대금이 적절하게 높은 기아 
현대차 관련주들 중에서 투자 관심이 높은 현대차
이 두가지의 거래량,종가 분석을 해보겠다.

# 기아와 현대차 10년간의 거래량, 종가를 분석

```
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
library(httr)
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
  labs(title = "주식 가격 및 거래량 변화", x = "연도", y = "가격 및 거래량") +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_minimal() +
  geom_point(aes(y = Volume, color = Company_Name), size = 1) +
  geom_point(aes(y = price, color = Company_Name), size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./1e6, name = "거래량(백만)")) +
  theme(text = element_text(family = "AppleGothic")) +
  scale_x_continuous(breaks = seq(2012, 2023, 1), labels = seq(2012, 2023, 1))+
  guides(color = guide_legend(title = "종목"), linetype = guide_legend(title = "데이터"))
```

거래량은 매수 + 매도를 더한 양이기 때문에 매수하는 사람만 많다고 해서 급증하지 않는다.
거래량의 변화를 나타내는 그래프이다.


종가의 가격이 오른다고 거래량도 오를까 ?
그래프를 보면 모든 상황에서 그렇다고 확정지을수는 없다.
2018년에서 2019년 현대차의 종가는 하락했지만 거래량은 상승한다.

현대차는 기아와 비슷한 시기 2019년부터 2020년까지는 상승했지만 그밑으로의 거래량의 하락세를 나타낸다.

기아는 2019년 부터 2021년까지 꾸준히 상승했다.

기사를 찾아보니 기아의 전기차인 ev6의 출시가 2021년 이었던 점 ,
애플이 기아에 4조원 규모의 투자를 했고 , 기아가 애플 프로젝트를 맡는다는 뉴스가 이시기에 있었다는 점이 기아 거래량의 상승 요인 중에 하나가 아닐까 생각한다.
