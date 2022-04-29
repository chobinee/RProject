#1. 아동기 경제적 생활 상태에 따른 최종학력 비교
#2. 지역별 자가 주택 가격 비교
#3. 성별, 연령대별 흡연률


library(foreign)

raw_welfare <- read.spss(file = "Koweps_hpda15_2020_beta1.1.sav", use.value.labels = F, to.data.frame = T)



welfare <- raw_welfare

library(dplyr)


#데이터 분석을 위한 필요한 데이터 이름 renaming
welfare <- rename(welfare,
                  sex = h15_g3,
                  birth = h15_g4,
                  code_region = h15_reg7,
                 ch_eco_con = np1506_2,
                 fin_edu = p1507_3aq1,
                 smoking = p1505_3aq5,
                 occupancy_of_house = h1506_3,
                 price_of_house = h1506_6)

#ch_eco_con ->  childhood economic condition 아동기 경제적 생활 상태
#fin_edu -> 최종학력
#smoking -> 흡연여부
#occupancy_of_house -> 집의 점유 형태
#price_of_house -> 집의 가격

View(welfare)

#1. 아동기 경제적 생활 상태에 따른 최종학력 비교
#필요한 데이터 : ch_eco_con(아동기 경제적 생활 상태), fin_edu(최종학력)

class(welfare$ch_eco_con)
table(welfare$ch_eco_con)

class(welfare$fin_edu)
table(welfare$fin_edu)

list_ch_eco_con <- data.frame(ch_eco_con = c(1:5), eco_con = c("매우가난", "가난", "보통","부유","매우부유"))

list_fin_edu <- data.frame(fin_edu = c(1:5), edu_state = c("중학교 졸업 이하", "고등학교", "전문대학","대학교(4년제)","대학원 이상"))

list_ch_eco_con
list_fin_edu

#list와 원본 데이터 join
welfare <- left_join(welfare, list_ch_eco_con, id = "ch_eco_con")

welfare <- left_join(welfare, list_fin_edu, id = "fin_edu")

#아동기 경제적 생활상태와 최종학력 이어붙이기
ch_eco_fin_edu<- welfare %>%
  filter(!is.na(ch_eco_con)) %>%
  select(ch_eco_con, eco_con, fin_edu, edu_state)

ch_eco_fin_edu

library(ggplot2)


ggplot(data = ch_eco_fin_edu, aes(x = eco_con , y = edu_state, colour = "navy", alpha = 0.7)) + geom_point() +ggtitle('아동기 경제적 생활상태에 따른 최종학력')+ scale_x_discrete(limits = c("매우가난", "가난", "보통", "부유", "매우부유"))+ scale_y_discrete(limits = c("중학교 졸업 이하", "고등학교", "전문대학", "대학교(4년제)", "대학원 이상"))

#1 후기 : 산점도로 아동기 경제적 생활상태에 따른 최종학력의 분포를 알아보았다. 아동기에 매우 가난했을 경우 최고학력은 전문대학이었고, 보통 고등학교 졸업이나, 중학교 졸업 이하가 최고 학력이었다. 점점 부유해질 수록 중학교 졸업 이하는 보이지 않고 최소 고등학교 졸업은 했다는 것을 볼 수 있었으며 대학교나 대학원 이상에 분포하는 수가 많아졌다. 매우 부유할 경우 대학원 이상은 가지 않고 4년제 대학교까지 나온 것을 알 수 있었다.

#2. 지역별 자가 주택 가격 비교
# 필요한 데이터 : code_region(지역 코드), 지역명, occupancy_of_house(집의 점유 형태), price_of_house(집의 가격)


#지역코드와 지역명 이어붙이기
list_region <- data.frame(code_region = c(1:7), region = c("서울", "수도권(인천/경기)", "부산/경남/울산","대구/경북","대전/충남","강원/충북","광주/전남/전북/제주도"))

welfare <- left_join(welfare, list_region, id = "code_region")

#지역별 자가 집 가격
own_house_price_by_region <- welfare %>%
  filter(occupancy_of_house==1) %>%
  select(code_region, region, price_of_house) %>%
  arrange(code_region)

table(own_house_price_by_region$price_of_house)

order<- list_region %>%
  arrange(desc(code_region))

#지역별 자가 집 가격 boxplot으로 나타내기
ggplot(data = own_house_price_by_region, aes(x = region , y = price_of_house, colour = "navy", alpha = 0.7)) + geom_boxplot() +  scale_x_discrete(limits = order$region) + coord_flip(ylim = c(0,200000))



#지역별 자가 집 가격 평균 구하기
mean_own_house_price_by_region <- own_house_price_by_region %>%
  group_by(region) %>%
  summarise(mean_price = mean(price_of_house)) %>%
  arrange(mean_price)


mean_own_house_price_by_region

#지역별 자가 집 가격 평균 막대 그래프 그리기
ggplot(data = mean_own_house_price_by_region, aes(x = region , y = mean_price)) + geom_col() + coord_flip() + scale_x_discrete(limits = mean_own_house_price_by_region$region) + ggtitle('지역별 자가 집 가격 평균 막대 그래프')

#2 후기 : 지역별 자가 집 가격을 boxplot, 가격 평균을 막대 그래프로 그려보았는데 확실히 서울과 수도권의 집 가격이 높음을 알 수 있었다. 허나 boxplot으로 그림을 그려봤을 때는 서울과 다른 지역과 유의미한 차이가 보다 적게 보였었는데, 평균으로 보니 바로 다음인 경기보다도 2배 정도 차이나는 것을 볼 수 있었다.

#3. 성별, 연령대별 흡연률
# 필요한 데이터 : sex(성별) , birth(탄생년도) -> age(나이), smoking(흡연 여부)

#성별을 numeric에서 character로 변경
welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
class(welfare$sex)

#흡연여부를 numeric에서 character로 변경
table(is.na(welfare$smoking))

welfare$smoking <- ifelse(welfare$smoking == 1, "흡연", ifelse(welfare$smoking == 2, "비흡연", NA))

table(welfare$smoking)


#birth를 age로 변환
welfare$age <- 2020 - welfare$birth + 1
summary(welfare$age)

#성인 이상의 흡연률을 구하므로 adult 변수 만들기
welfare$adult <- ifelse(welfare$age>20, "yes", NA)

table(is.na(welfare$adult))

#연령대별 흡연율 구하기
age_smoking <- welfare %>%
  filter(!is.na(smoking) & !is.na(adult)) %>%
  group_by(age, smoking) %>%
  summarise(n = n()) %>%
  mutate(total_smoking =sum(n)) %>%
  mutate(pct = round(n/total_smoking*100,1))

  
age_smoking
  

age_smoking_pct <- age_smoking %>%
  filter(smoking == "흡연") %>%
  select(age, pct)

age_smoking_pct

ggplot(data = age_smoking_pct, aes(x=age, y=pct)) + geom_col()

#성별별 흡연률

sex_smoking <- welfare %>%
  filter(!is.na(smoking) & !is.na(adult)) %>%
  group_by(sex, smoking) %>%
  summarise(n = n()) %>%
  mutate(total_smoking =sum(n)) %>%
  mutate(pct = round(n/total_smoking*100,1))

sex_smoking

sex_smoking_pct <- sex_smoking %>%
  filter(smoking == "흡연") %>%
  select(sex, pct)

sex_smoking_pct

ggplot(data = sex_smoking_pct, aes(x=sex, y=pct)) + geom_col()

#나이 범주형 변수로 변경

welfare$age<-ifelse(welfare$age>60, "old", ifelse(welfare$age>40, "middle", ifelse(welfare$age>20, "young", NA)))

table(welfare$age)

#성별, 연령대별 흡연률

age_sex_smoking <- welfare %>%
  filter(!is.na(smoking) & !is.na(age)) %>%
  group_by(age, sex, smoking) %>%
  summarise(n = n()) %>%
  mutate(total_group = sum(n)) %>%
  mutate(pct = round(n/total_group*100,1))

age_sex_smoking

df_smoking <- age_sex_smoking %>%
  filter(smoking == "흡연") %>%
  select(age, sex, pct)


df_smoking

ggplot(data = df_smoking, aes(x = age, y = pct, fill = sex)) + geom_col(position = "dodge") + scale_x_discrete(limits = c("young", "middle","old")) + ggtitle('성별, 연령대별 흡연률') + coord_cartesian(ylim = c(0,50))

#3 후기 : 주위 여자인 친구들이 흡연을 많이해서 성별, 연령대별 흡연률을 막대 그래프로 확인해봤는데 남성에 비해 여성은 턱없는 흡연률을 가지고 있는 듯하다. 또한 놀라웠던 점은 중년 남성(40~60세)의 흡연률이 거의 절반 가까이 된다는 것이었다. 이 수치를 가지고 이후 흡연에 따른 질병 여부 같은 것도 확인해보면 좋을것 같았다.