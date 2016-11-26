# 熵值法建模(故障停电敏感模型)训练集(以户号匹配)
rm(list = ls()) #清理系统内存

#### 加载相关R包 ####

library(RODBC)
library(dplyr)

#### 从oracle中读取宽表数据 ####

oracle <- odbcConnect('oracle12c',uid = 'system',pwd = 'Chenzhifeng520') 
temp_gt <- sqlFetch(oracle,sqtable = "TEMP_GT_TRAIN_V4") #读入训练集数据 
odbcClose(oracle) #关闭数据库连接

#### 自定义函数 ####

pMiss <- function(x) {sum(is.na(x))/length(x)*100} # 自定义函数：查看字段缺失值占比
negative_pmiss <- function(x){sum( x < 0)} # 查看各字段 负数个数 情况
zero_Miss <- function(x) {sum(x == 0)/length(x)} # 查看各字段 为0 情况
z_scale <- function(x){(x-min(x))/(max(x)-min(x))+0.000001} #归一化 正向因子(指标越大越好)
f_scale <- function(x){(max(x)-x)/(max(x)-min(x))+0.000001} #归一化 负向因子(指标越小越好)
p_value <- function(x){ x/sum(x) } # 计算字段比重

#### 数据初探 ####

names(temp_gt) #查看字段名
table(temp_gt$IS_REAL) # 敏感 1:70064 ; 不敏感 0:513215
temp_gt_1 <- temp_gt[,c(1,2,54)] # 取出 CONS_NO;TG_ID;IS_REAL 字段

temp_gt_2 <- temp_gt[,-c(1,2,54)] # 建模需要的字段
apply(temp_gt_2,2,pMiss) # 查看字段缺失值占比情况 (无缺失值)
apply(temp_gt_2,2,negative_pmiss) # 查看字段负数情况 (CALL_AVG_12M 字段有2个负值)
temp_gt_3 <- temp_gt_2[-which(temp_gt_2$CALL_AVG_12M < 0),] # 剔除 负值记录
temp_gt_1 <- temp_gt_1[-which(temp_gt_2$CALL_AVG_12M < 0),] # 剔除 负值记录
apply(temp_gt_3,2,zero_Miss) # 查看字段零值情况 (有7个字段的取值全部为零值)
# SJ_AVG_MIN_1M,SJ_MAX_INTERVAL_1M,SJ_MIN_INTERVAL_1M,
# SJ_AVG_MIN_3M,SJ_MAX_INTERVAL_3M,SJ_MIN_INTERVAL_3M,SJ_MIN_INTERVAL_12M
temp_gt_4 <- temp_gt_3[,-c(15,16,17,32,33,34,51)] # 剔除 全部为零值的字段
### 相关性分析 ###
temp_gt_cor <- cor(temp_gt_4) # 剔除15个相关性较强的字段
write.csv(temp_gt_cor,file = "C://Users//Administrator//Desktop//停电敏感项目//故障停电(户号)相关性.csv")  #输出相关性数据
temp_gt_5 <- temp_gt_4[,-c(2,3,4,5,9,11,16,17,18,25,30,31,32,38,39)]

#### 熵值法建模 ####

### 1、归一化处理 ###
temp_gt_5[,c(1,2,3,4,5,9,10,11,12,13,14,15,19,20,21,22,23,24)] <- lapply(temp_gt_5[,c(1,2,3,4,5,9,10,11,12,13,14,15,19,20,21,22,23,24)],z_scale)#正因子 标准化
temp_gt_5[,c(6,7,8,16,17,18,25,26,27,28,29)] <- lapply(temp_gt_5[,c(6,7,8,16,17,18,25,26,27,28,29)],f_scale)#负因子(含有 MIN和INTERVAL的字段) 标准化

### 2、计算第i个记录第j个指标值得比重 ###
temp_gt_6 <- as.data.frame(lapply(temp_gt_5,p_value))
write.csv(temp_gt_6,file = "C://Users//Administrator//Desktop//停电敏感项目//故障停电(户号)比重.csv")  #输出比重数据

### 3、计算第j项指标的信息熵 ###
k <- nrow(temp_gt_6)
entropy <- function(x){- (1/log(k))*sum(x*log(x))} # 计算信息熵
temp_gt_7 <- as.data.frame(lapply(temp_gt_6,entropy))

### 4、计算信息熵与1之间的差值 ###
temp_gt_8 <- 1 - temp_gt_7

### 5、计算第j项指标的权重 ###
temp_gt_9 <- temp_gt_8/sum(temp_gt_8)
write.csv(temp_gt_9,file = "C://Users//Administrator//Desktop//停电敏感项目//故障停电(户号)权重.csv") # 输出各字段权重

### 6、指标加权计算得分 ###
matrix1 <- as.matrix(temp_gt_6) # 比重矩阵
matrix2 <- t(as.matrix(temp_gt_9)) # 权重矩阵
matrix3 <- matrix1 %*% matrix2
temp_gt_score <- as.data.frame((100*matrix3))
temp_gt_score1 <- temp_gt_score/max(temp_gt_score)*100 #得分标准化

temp_gt_10 <- cbind(temp_gt_1,temp_gt_score1)
names(temp_gt_10) <- c('CONS_NO','TG_ID','IS_REAL','SCORE')
write.csv(temp_gt_10,file = "C://Users//Administrator//Desktop//停电敏感项目//temp_gt_10.csv")

#### 模型检验 ####
# 敏感 1:70064 ; 不敏感 0:513215
temp_gt_11 <- arrange(temp_gt_10,desc(SCORE))
sum(temp_gt_11[1:29163,]$IS_REAL == 1) #19815  (5%)
sum(temp_gt_11[29164:58327,]$IS_REAL == 1) #1515  (10%)
sum(temp_gt_11[58328:87491,]$IS_REAL == 1) #3185  (15%)
sum(temp_gt_11[87492:116655,]$IS_REAL == 1) #26628  (20%)
sum(temp_gt_11[116656:145819,]$IS_REAL == 1) #18593  (25%)
# 前25% 的实际敏感客户 69736
sum(temp_gt_11[145820:174983,]$IS_REAL == 1) #10  (30%)
sum(temp_gt_11[174984:204146,]$IS_REAL == 1) #8  (35%)
sum(temp_gt_11[204147:233310,]$IS_REAL == 1) #56  (40%)
sum(temp_gt_11[233311:262474,]$IS_REAL == 1) #100  (45%)
sum(temp_gt_11[262475:291638,]$IS_REAL == 1) #8  (50%)
sum(temp_gt_11[291639:320802,]$IS_REAL == 1) #0  (55%)
sum(temp_gt_11[320803:349966,]$IS_REAL == 1) #0  (60%)
sum(temp_gt_11[349967:379130,]$IS_REAL == 1) #0  (65%)
sum(temp_gt_11[379131:408293,]$IS_REAL == 1) #0  (70%)
sum(temp_gt_11[408294:437457,]$IS_REAL == 1) #9  (75%)
sum(temp_gt_11[437458:466621,]$IS_REAL == 1) #5  (80%)
sum(temp_gt_11[466622:495785,]$IS_REAL == 1) #0  (85%)
sum(temp_gt_11[495786:524949,]$IS_REAL == 1) #1  (90%)
sum(temp_gt_11[524950:554113,]$IS_REAL == 1) #0  (95%)
sum(temp_gt_11[554114:583277,]$IS_REAL == 1) #131  (100%)



