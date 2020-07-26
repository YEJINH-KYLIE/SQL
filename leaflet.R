library( tidyverse )
library( readxl )
library( leaflet )

frcode <- read_excel( "./data/week03/FR_CODE_2019.xlsx" )
names(frcode)
frcode

stationInfo <- read.csv("./data/week03/stationInfo.csv")
stationInfo



### Leaflet 객체 생성
stationInfo %>%
  leaflet()

### 지도 표시
stationInfo %>%
  leaflet() %>%
  addTiles()

### 중심 위치 지정과 줌 레벨 지정
stationInfo %>%
  leaflet() %>%
  setView(lng=126.9813, lat=37.5228, zoom=10) %>%
  addTiles()

### Marker를 이용하여 표시하기
stationInfo %>%
  leaflet() %>%
  setView(lng=126.9813, lat=37.5228, zoom=10) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~long, lat = ~lat, radius = 2, 
                   fillOpacity = 0.8, stroke = FALSE,
                   label = ~station)

#### 클러스터 옵션
stationInfo %>%
  leaflet() %>%
  setView(lng=126.9813, lat=37.5228, zoom=10) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~long, lat = ~lat, radius = 2, 
                   fillOpacity = 0.8, stroke = FALSE,
                   label = ~station,
                   clusterOptions = markerClusterOptions())


#### 지도 변경하기
stationInfo %>%
  leaflet() %>%
  setView(lng=126.9813, lat=37.5228, zoom=10) %>%
  addProviderTiles("Stamen.Watercolor") %>%
  addCircleMarkers(lng = ~long, lat = ~lat, radius = 2, 
                   fillOpacity = 0.8, stroke = FALSE,
                   label = ~station,
                   clusterOptions = markerClusterOptions())


### 지하철역 표시하기

#### 데이터 준비

se_station <- read_excel("./data/week03/se_station.xlsx")
datatable( se_station )



#### 데이터 결합
sm_no_lines <- stationInfo %>% 
  left_join( se_station, by=c("fr_code" = "외부코드") ) %>%
  select(-전철역명)

datatable(sm_no_lines)


#### 색상표 만들기
table( sm_no_lines$호선 )

pal <- colorFactor(
  c("#0D3692", "#33A23D", "#FE5B10",
    "#32A1C8", "#8B50A4", "#C55C1D",
    "#54640D", "#F51361", "#AA9872",
    "#32C6A6", "#32C6A6", "#3681B7",
    "#FFB300", "#FFB300", "#DB0029",
    "#4EA346", "#FDA600", "#FFCC00"), 
  domain = names( 
    table( sm_no_lines$호선 )  
  )
)

pal("02호선")



#### 최종 코드
sm_no_lines %>%
  leaflet() %>%
  setView(lng=126.9813, lat=37.5228, zoom = 10) %>%
  addTiles() %>%
  addCircleMarkers(lng=~long, lat=~lat, radius=5, 
                   fillOpacity = 0.8, stroke=FALSE,
                   color = ~pal(호선), 
                   label = ~station)


#saveRDS(sm_no_lines, "./data/RDS/snl.rds")
