# 증권데이터 분석하기

### 1. 자동차 종목에서 인기종목

자동차 관련 주식에는 무엇이 있는지 살펴보았습니다.

<img width="691" alt="스크린샷 2023-06-23 오전 3 45 14" src="https://github.com/hyeimii/final/assets/128936045/f1a13985-d82f-4cf7-9d8d-e9cce83c2162">

엘브이엠씨홀딩스, 디피코, 현대차우, KG모빌리티, 현대차3우B,기아,현대차2우B, 현대차,KR모터스가있습니다.

현재 전일비, 거래량, 거래대금, 고가, 등락률, 저가, 외국인비율이 0에 해당하는 디피코는 거래중지 상태인 종목입니다.

모두 0인 주식은 주식시장에서 주목받지 않는 종목으로 간주되며 
주식 거래량이 0인 이유는 현재 시점에서 거래가 없는것이라고 볼 수 있습니다.

현재가는 주식시장에서 해당 주식을 살 떄 현재 지불해야하는 가격을 의미합니다.
전일비란 현재가와 전날 마감시의 주가와의 차이를 나타내는데 \
주식 가격의 변동을 나타내고 양수인 경우 주가상승, 음수인경우 주가하락을 의미합니다. \
등락률은 주식의 상승과 하락 비율입니다. \
거래량은 매수 + 매도이기 때문에 매수하는 사람만 많다고 해서 거래량이 급증하지는 않습니다. \
하지만 거래량이 많다는건 당일 그 종목을 매수하거나 매도하려는 양이 많았다는 의미로 투자자들의 관심이 높은 종목이라는 것을 알 수 있습니다. 

#### 종목별 거래량 그래프 
![종목별거래량 그래프 (당일)](https://github.com/hyeimii/final/assets/128936045/cd53c364-13b0-492c-8282-18870b530ebb)

당일 거래량이 가장 높은 종목은 엘브이엠씨홀딩스 입니다. \
엘브이엠씨홀딩스는 주로 자동차의 제조 및 판매에 종사하는 한국 기반 회사입니다. \
이 회사의 당일 거래량이 가장 높았다는것으로 투자자들의 관심 종목이었다는 것을 파악할수있었습니다.\
거래량이 높을수록 유동성이 높아집니다.\
즉 주식을 쉽게 사고 팔 수 있는 환경을 제공한다는것입니다. 

시가총액이란 한 회사의 주식 시장 가치를 나타내는 지표입니다. \
주식의 현재 시장가격과 회사의 총 발행 주식수를 곱하여 계산됩니다. 

### 2. 시가총액이 가장 높은 종목 
#### 종목별 시가총액 

![종목별 시가총액](https://github.com/hyeimii/final/assets/128936045/ef1bc980-fd3f-4709-b11f-62e1eeb7f9f5)

그래프를 통해서 알수있듯이 자동차 관련 종목들에서 시가총액이 가장 높은 기업은 현대차 기업입니다. \
그다음은 기아, 그리고 현대차2우B가 3번째를 차지하고있습니다. 
현대차2우B, 현대차우, 현대차3우B는 현대차 우선주들에 해당합니다.

현대차는 보통주로 가장 일반적인 형태의 주식입니다. \
일반 투자자들에게 제공됩니다.

현대차3우B는 3우선주로 일반주주와 2우선주보다 더 우선적인 권리를 가지며 \
배당 및 처분 우선권에 있어서 다양한 우선권 혜택을 받을 수 있습니다.
현대차2우B는 2 우선주로 3우선주 다음으로 우선권이 있는 종목입니다. \
즉 이 두개의 종목은 주식의 우선권 차이를 가지고 있다고 볼수있다.
현대차우는 우선주로 현대차보다 상대적으로 더 저렴한 가격으로 거래되는 주식으로, \
주식의 가격 차이를 이용해 수익을 얻기위해서 투자됩니다.
따라서 투자자는 현대차보다 저렴한 가격에 우선주를 구매해 주가 상승시 이익을 얻을 수 있는 가능성이 있습니다.


### 3. 10년간 거래대금과 거래량이 높은 종목은 같을까? 
10년의 거래대금과 거래량의 데이터를 처리해서 가장 높은 종목이 무엇인지 알아보았습니다. 

#### 10년간의 거래량 종목 순위 
![종목별 10년간 거래량 최종](https://github.com/hyeimii/final/assets/128936045/d86136fa-6a69-491e-926a-0081d6efdad5)

#### 10년간의 거래대금 종목 순위 
![종목별 10년간ㄱ ㅓ래대금 최종](https://github.com/hyeimii/final/assets/128936045/5f843019-87bc-40d4-b683-b183b4428fcd)

10년간 거래량이 가장 높은 종목은 기아입니다.\
하지만 10년간 거래 대금이 가장 높은 종목은 현대차로 나타났습니다. \
단순하게 거래대금이 많다고 해서 거래량이 많은것도 아니고 그 반대로 거래대금이 적다고해서 거래량이 적은것도 아닙니다.


### 4. 시가총액 top2 기업 기아와 현대차 분석 
시가총액이 가장 높았던 현대차와 기아의 10년간의 거래량과 거래대금(종가)을 분석해보겠습니다. 

#### 기아와 현대차의 10년간 거래량 그래프 

![종목별거래량(10년)](https://github.com/hyeimii/final/assets/128936045/b75e8128-9867-4972-9692-39062b1dd483)

기아와 현대차의 거래량을 비교해보면 비슷한듯 하지만 양의 차이를 보이고 있습니다. \
그래프만 본다면 거래량이 작은 현대차가 기아보다 투자자들의 관심이 부족했다라고 생각할 수 있습니다. \
관심이 부족했던 시기도 있었겠지만 모든 상황에서 그렇다고 확정 지을 수는 없습니다. 

#### 기아와 현대차의 10년간 거래대금(종가)그래프 

![종목별거래대금(종가)10년](https://github.com/hyeimii/final/assets/128936045/7a8ebd3f-7c02-4b05-ba20-79aff642c61e)

거래대금은 기아보다 현대차가 확연히 높음을 알 수 있습니다. \
거래대금 그래프까지 함께 보게된다면 2020년에서 2021년 현대차의 종가는 상승했지만 거래량은 하락했습니다.\
현대차는 기아와 비슷한 시기 2019년부터 2020년까지는 상승했지만 그밑으로의 거래량의 하락세가 나타납니다.\
기아는 2019년 부터 2021년까지 꾸준히 상승하다가 21년 이후 현재까지 하락세를 보이고 있는데 \
기아차 출시 시기를 보았을 때 전기차인 ev6의 출시가 2021년 이었던 점 , \
애플이 기아에 4조원 규모의 투자를 했고 , 
기아가 애플 프로젝트를 맡는다는 뉴스가 이시기에 있었다는 점이 2020년 기아 거래량의 상승에 영향이 있었다고 생각합니다. \
현대차 또한 전기차 아이오닉이 출시된 2019년 부터 주가 거래량이 상승함을 보이고, \
전기차전용공장에도 세액공제 해택을 주고 전기차를 소유하면 세금을 감면해주는 등 전기차 관련 정책이 증가하고 있고 \
전기차 여러 이슈가 자동차 주가 변동에 영향이 있지 않을까 생각합니다. 

### 통계분석 
#### 기아의 10년간 거래량과 거래대금의 통계분석
<img width="308" alt="스크린샷 2023-06-23 오전 3 56 45" src="https://github.com/hyeimii/final/assets/128936045/c0bdac7d-a6de-4dc2-b20f-cefc07003c1c">

#### 현대차의 10년간 거래량과 거래대금의 통계분석 
<img width="305" alt="스크린샷 2023-06-23 오전 3 56 57" src="https://github.com/hyeimii/final/assets/128936045/ecf82d40-f15c-4e59-81a1-c906df23d4a3">

### 기아와 현대차의 거래량과 거래대금간의 상관관계 분석 
#### 산점도 그래프 
![기아 현대 상관관계](https://github.com/hyeimii/final/assets/128936045/bc53c0f1-abbb-4829-acd6-0afe3920771b)

그래프에서 나타나는 것 처럼 기아와 현대차 모두 거래량과 거래대금은 양의 상관관계를 나타내고 있습니다. \
두 변수 간에 양의 방향으로 관련성이 있다는 것을 나타냅니다. \
즉 한변수의 값이 증가하면 다른 변수의 값도 증가하는 경향을 보인다는 것입니다. \
일반적으로 주식시장에서 거래량과 거래대금은 긍정적인 관련성을 가지고 있고 더 많은 거래가 이루어지면 거래 대금도 증가하는데 이는 주식 시장의 활발한 거래활동을 나타내 주식 시장의 활발성과 관련이 있습니다. \
하지만 인과관계를 나타내지는 않습니다. \
거래량과 거래대금 사이 상관관계가 높다고 해서 원인으로 인식 할 수는 없다는 것입니다.

### 기아와 현대차의 한달간 거래량 변화율과 외국인소진율 분석 
시장에서 어느 종목이 인기가 있는지 분석해 보기 위해서 외국인 소진율과 거래량을 이용해 접근해 보았습니다. \
외국인 투자자들의 참여와 시장의 거래활동 간의 연관성을 파악 할 수 있었습니다.

외국인 소진율은 외국인 투자자들이 보유한 주식의 비율로 , 해당시장에 대한 외국인 투자 선호도와 인기정도를 나타냅니다. \
외국인 소진율이 증가한다는 것은 외국인 투자자들이 해당 시장에 관심을 가지고 투자를 증가시키고 있다는 의미일 수 있습니다. \
외국인 소진율과 거래량을 함께 분석해 시장의 인기정도와 거래 활발성을 파악합니다. \
예를 들어 , \
외국인 소진율은 상승, 거래량이 증가  = 외국인 투자자들의 참여가 시장에 큰 영향을 미치고있으며, 시장의 인기도 높아지는것을 의미합니다. \
외국인 소진율 증가, 거래량이 감소 = 외국인 투자자들의 참여가 많아져도 실제 거래 활동은 상대적으로 덜 활발할수 있습니다. \
이는 외국인 투자자들이 장기투자를 선호하거나 투자 전략이 변화했다는 의미입니다. 

#### 기아 한달간 거래량 변화율과 외국인 소진율 그래프 
![기아 거래량,외국인소진율](https://github.com/hyeimii/final/assets/128936045/8cb2cd0f-e732-40b3-a2f6-4048f7673fbc)

그래프를 보면 5/30 ~ 6/5 일 거래량 변화율은 낮아지고 외국인 소진율은 상승하고 있음을 볼 수 있습니다. \
그렇다는것은 외국인 투자자들의 투자전략이 변화했을 수 있다는 의미입니다. 

#### 현대차 한달간 거래량 변화율과 외국인 소진율 그래프 
![현대](https://github.com/hyeimii/final/assets/128936045/b2be784e-459c-481c-863d-fc761bab5dbc)

현대차는 외국인 소진율은 큰 변동이 없지만 거래량 변화율에 변동이 많은것을 볼 수 있습니다. \
하루나 이틀의 간격으로 소진율과 변화율 모두 증가했다가 거래량 변화율은 다시 낮아지는것을 볼 수 있습니다. \
한달간의 데이터이지만 거래량 변동이 많다는 것은 시장의 활발성과 불안정성을 나타내는데 불안정한 시장은 투자에 위험이 높을 수 있습니다. \
투자자들이 투자예측을 하기에는 어려울 수 있어 현대차 기업에 투자를 할때는 다른 요소들을 고려해 투자를 해야합니다. 

### 5. 결론 
거래량과 거래대금 , 외국인 소진율 뿐 아니라 주식 투자에는 여러가지 요인들이 있고 투자를 예측하기 위해서는 다양한 상황들도 고려해야 합니다. \
자동차 종목에서는 단순하게 전기차의 출시 뿐 아니라 미국이나 외국에서의 차 판매량 또한 외국인 소진율 , 거래량등에 영향을 준다는것을 여러 보고서를 통해서 알게되었습니다. \
시가총액이 크다고 해서 거래량이 높은 종목도 아니며 거래대금이 높은 종목도 아니었습니다. \
거래량과 거래대금이 관계성은 있지만 서로 원인이 되는 관계가 아니라는것에 주의해서 투자 예측을 세워야할 것 같습니다. 
