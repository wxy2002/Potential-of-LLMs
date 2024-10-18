# 数据处理
library(tidyverse)
library(FSA)
library(psych)
library(DescTools)
library(ggpubr)
library(stringr)
set.seed(28)
data <- readxl::read_excel("data.xlsx")
data$`1、评价专家编号` <- as.numeric(data$`1、评价专家编号`)
data <- data %>%
  filter(`1、评价专家编号` <= 4)

test2p <- function(test){
  test <- test$res %>%
    filter(P.adj < 0.05)
  if (nrow(test) < 1){
    return(NULL)
  }
  p <- as.data.frame(str_split(test$Comparison, ' - ', simplify = T))
  p$pvalue <- test$P.adj
  colnames(p) <- c("group1", 'group2', 'p')
  p$signif <- ifelse(p$p > 0.05, 'ns', ifelse(
    p$p > 0.01, "*", ifelse(
      p$p > 0.001, '**', "***"
    )
  ))
  return (p)
}

# 诊断问题
data1 <- data %>% 
  select(c("1、评价专家编号", "2、评价病例编号", "3、生成回答模型编号", "4、评价任务编号", 
           "5、主诊断是否相同？", '10、主要诊断ICD10/ICD11编码是否正确？','25、回答和实际病例情况的重合度',
           "26、对回答进行总体评分")) %>%
  filter(`4、评价任务编号` == "诊断任务") %>%
  filter(`2、评价病例编号` != "003010165") %>%
  filter(`2、评价病例编号` != "000498774") %>%
  filter(`3、生成回答模型编号` != 'Google Gemini')
colnames(data1) <- c('专家', "病例", "模型", "任务", "主诊断", "ICD10", '重合度', '评分')
data1 <- data1 %>%
  filter(!duplicated(cbind(专家, 病例, 模型)))
xtabs(~专家 + 病例, data1)

# 主诊断正确率
data1$主诊断 <- factor(data1$主诊断)
data1$模型 <- factor(data1$模型)
p <- xtabs(~模型 + 主诊断, data1)
fisher.test(p)
p2 <- data.frame(p) %>%
  tidyr::spread(key = '主诊断', value = 'Freq', fill = 0)
p <- prop.table(p, margin = 1) %>%
  data.frame() %>%
  filter(主诊断 == "是")
ggplot(p, aes(x = 模型, y = Freq)) + 
  geom_bar(stat="identity", fill = 'lightblue', color = 'black') + 
  ylab('Accuracy') + xlab('Model') + theme_classic()
# ICD编码正确率
data1$ICD10 <- factor(data1$ICD10)
data1$模型 <- factor(data1$模型)
p <- xtabs(~ICD10 + 模型, data1)
fisher.test(p)
p2 <- data.frame(p) %>%
  tidyr::spread(key = 'ICD10', value = 'Freq', fill = 0)
p <- prop.table(p, margin = 1) %>%
  data.frame() %>%
  filter(ICD10 == "正确")
ggplot(p, aes(x = 模型, y = Freq)) + 
  geom_bar(stat="identity", fill = 'lightblue', color = 'black') + theme_classic() + 
  ylab('Accuracy') + xlab('Model')
# 得分
data1 <- data %>% 
  select(c("1、评价专家编号", "2、评价病例编号", "3、生成回答模型编号", "4、评价任务编号", 
           "5、主诊断是否相同？", '10、主要诊断ICD10/ICD11编码是否正确？','25、回答和实际病例情况的重合度',
           "26、对回答进行总体评分")) %>%
  filter(`4、评价任务编号` == "诊断任务") %>%
  filter(`2、评价病例编号` != "003010165") %>%
  filter(`2、评价病例编号` != "000498774") 
colnames(data1) <- c('专家', "病例", "模型", "任务", "主诊断", "ICD10", '重合度', '评分')
data1 <- data1 %>%
  filter(!duplicated(cbind(专家, 病例, 模型)))
data1 <- data1 %>%
  filter(模型 != 'Google Gemini')
data1$评分 <- as.numeric(data1$评分)
data1$模型 <- factor(data1$模型)
data1$重合度 <- as.numeric(data1$重合度)
xtabs(~专家 + 病例, data1)
data2 <- data1 %>%
  group_by(专家, 病例, 模型) %>%
  summarize(score = mean(评分), .groups = 'rowwise')
xtabs(~专家 + 病例, data2)
data2.1 <- data2 %>%
  filter(专家 == '1')
data2.2 <- data2 %>%
  filter(专家 == '2')
data2.3 <- data2 %>%
  filter(专家 == '3')
data2.4 <- data2 %>%
  filter(专家 == '4')
data2 <- merge(data2.1, data2.2, by = c('病例', '模型'))
data3 <- merge(data2.3, data2.4, by = c('病例', '模型'))
data2 <- merge(data2, data3, by = c('病例', '模型'))
KendallW(data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")],
         TRUE, test = TRUE)
cohen.kappa(x = data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")])
kruskal.test(评分 ~ 模型,
             data = data1)
test <- dunnTest(评分~模型,
                 data = data1, method = 'bh')
p <- test2p(test)
p$y.position <- c(6.5, 6, 7, 5.5, 7.5)
ggplot(data1, aes(x = 模型, y = 评分)) +
  geom_violin(color = 'black', fill = "lightblue") +
  geom_boxplot(width = 0.2, fill = 'white') + theme_classic() + 
  ylab('Score') + xlab('Model') + 
  stat_pvalue_manual(p, label = 'signif')
p2 <- data1 %>%
  count(模型, 评分) %>%
  spread(key = '评分', value = 'n', fill = 0)
p2[, c(2:6)] <- p2[, c(2:6)] / rowSums(p2[, c(2:6)])

# 检验检查问题
data1 <- data %>% 
  select(c("1、评价专家编号", "2、评价病例编号", "3、生成回答模型编号", "4、评价任务编号", 
           "13、基础检验检查项目评分", '14、和主要、次要诊断相关检验检查项目评分',
           '15、和鉴别诊断相关检验检查项目评分', "16、患者禁忌评价")) %>%
  filter(`4、评价任务编号` == "检验检查任务") %>%
  filter(`2、评价病例编号` != "003010165") %>%
  filter(`2、评价病例编号` != "000498774") %>%
  filter(`3、生成回答模型编号` != 'Google Gemini')
colnames(data1) <- c('专家', "病例", "模型", "任务", 
                     "基础", "诊断", '鉴别', '禁忌')
data1 <- data1 %>%
  filter(!duplicated(cbind(专家, 病例, 模型, 任务)))
data1$基础 <- as.numeric(data1$基础)
data1$诊断 <- as.numeric(data1$诊断)
data1$鉴别 <- as.numeric(data1$鉴别)
data1$禁忌 <- as.numeric(data1$禁忌)
data1$病例 <- as.numeric(data1$病例)
data1$模型 <- as.factor(data1$模型)
# 基础得分
data2 <- data1 %>%
  group_by(专家, 病例, 模型) %>%
  summarize(score = mean(基础), .groups = 'rowwise')
xtabs(~专家 + 病例, data2)
data2.1 <- data2 %>%
  filter(专家 == '1')
data2.2 <- data2 %>%
  filter(专家 == '2')
data2.3 <- data2 %>%
  filter(专家 == '3')
data2.4 <- data2 %>%
  filter(专家 == '4')
data2 <- merge(data2.1, data2.2, by = c('病例', '模型'))
data3 <- merge(data2.3, data2.4, by = c('病例', '模型'))
data2 <- merge(data2, data3, by = c('病例', '模型'))
cohen.kappa(x = data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")])
KendallW(data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")],
         TRUE, test = TRUE)
kruskal.test(基础 ~ 模型,
             data = data1)
test <- dunnTest(基础 ~ 模型,
                 data = data1, method = 'bh')
p <- test2p(test)
p$y.position <- c(6, 5.5, 6.5)
ggplot(data1, aes(x = 模型, y = 基础)) +
  geom_violin(color = 'black', fill = 'lightblue') +
  geom_boxplot(width = 0.2, fill = 'white') + theme_classic() + 
  ylab('Score') + xlab('Model') + stat_pvalue_manual(p, label = 'signif')
p2 <- data1 %>%
  count(模型, 基础) %>%
  spread(key = '基础', value = 'n', fill = 0)
p2[, c(2:5)] <- p2[, c(2:5)] / rowSums(p2[, c(2:5)])
# 诊断得分
data2 <- data1 %>%
  group_by(专家, 病例, 模型) %>%
  summarize(score = mean(诊断), .groups = 'rowwise')
xtabs(~专家 + 病例, data2)
data2.1 <- data2 %>%
  filter(专家 == '1')
data2.2 <- data2 %>%
  filter(专家 == '2')
data2.3 <- data2 %>%
  filter(专家 == '3')
data2.4 <- data2 %>%
  filter(专家 == '4')
data2 <- merge(data2.1, data2.2, by = c('病例', '模型'))
data3 <- merge(data2.3, data2.4, by = c('病例', '模型'))
data2 <- merge(data2, data3, by = c('病例', '模型'))
cohen.kappa(x = data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")])
KendallW(data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")],
         TRUE, test = TRUE)
kruskal.test(诊断 ~ 模型,
             data = data1)
test <- dunnTest(诊断 ~ 模型,
                 data = data1, method = 'bh')
p <- test2p(test)
p$y.position <- c(6, 5.5)
ggplot(data1, aes(x = 模型, y = 诊断)) +
  geom_violin(color = 'black', fill = 'lightblue') +
  geom_boxplot(width = 0.2, fill = 'white') + theme_classic() + 
  ylab('Score') + xlab('Model') +
  stat_pvalue_manual(p, label = 'signif')
p2 <- data1 %>%
  count(模型, 诊断) %>%
  spread(key = '诊断', value = 'n', fill = 0)
p2[, c(2:6)] <- p2[, c(2:6)] / rowSums(p2[, c(2:6)])
# 鉴别得分
data2 <- data1 %>%
  group_by(专家, 病例, 模型) %>%
  summarize(score = mean(鉴别), .groups = 'rowwise')
xtabs(~专家 + 病例, data2)
data2.1 <- data2 %>%
  filter(专家 == '1')
data2.2 <- data2 %>%
  filter(专家 == '2')
data2.3 <- data2 %>%
  filter(专家 == '3')
data2.4 <- data2 %>%
  filter(专家 == '4')
data2 <- merge(data2.1, data2.2, by = c('病例', '模型'))
data3 <- merge(data2.3, data2.4, by = c('病例', '模型'))
data2 <- merge(data2, data3, by = c('病例', '模型'))
cohen.kappa(x = data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")])
KendallW(data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")],
         TRUE, test = TRUE)
kruskal.test(鉴别 ~ 模型,
             data = data1)
test <- dunnTest(鉴别 ~ 模型,
                 data = data1, method = 'bh')
p <- test2p(test)
ggplot(data1, aes(x = 模型, y = 鉴别)) +
  geom_violin(color = 'black', fill = 'lightblue') +
  geom_boxplot(width = 0.2, fill = 'white')+ theme_classic() + 
  ylab('Score') + xlab('Model')
p2 <- data1 %>%
  count(模型, 鉴别) %>%
  spread(key = '鉴别', value = 'n', fill = 0)
p2[, c(2:6)] <- p2[, c(2:6)] / rowSums(p2[, c(2:6)])
# 禁忌得分
data2 <- data1 %>%
  group_by(专家, 病例, 模型) %>%
  summarize(score = mean(禁忌), .groups = 'rowwise')
xtabs(~专家 + 病例, data2)
data2.1 <- data2 %>%
  filter(专家 == '1')
data2.2 <- data2 %>%
  filter(专家 == '2')
data2.3 <- data2 %>%
  filter(专家 == '3')
data2.4 <- data2 %>%
  filter(专家 == '4')
data2 <- merge(data2.1, data2.2, by = c('病例', '模型'))
data3 <- merge(data2.3, data2.4, by = c('病例', '模型'))
data2 <- merge(data2, data3, by = c('病例', '模型'))
cohen.kappa(x = data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")])
KendallW(data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")],
         TRUE, test = TRUE)
kruskal.test(禁忌 ~ 模型,
             data = data1)
test <- dunnTest(禁忌 ~ 模型,
                 data = data1, method = 'bh')
p <- test2p(test)
ggplot(data1, aes(x = 模型, y = 禁忌)) +
  geom_violin(color = 'black', fill = 'lightblue') +
  geom_boxplot(width = 0.2, fill = 'white') + theme_classic() + 
  ylab('Score') + xlab('Model')
p2 <- data1 %>%
  count(模型, 禁忌) %>%
  spread(key = '禁忌', value = 'n', fill = 0)
p2[, c(2:6)] <- p2[, c(2:6)] / rowSums(p2[, c(2:6)])

# 初步治疗方案
# 打分
data1 <- data %>% 
  select(c("1、评价专家编号", "2、评价病例编号", "3、生成回答模型编号", "4、评价任务编号", 
           "17、实际治疗方案在回答所给三个方案中的第几个？", "26、对回答进行总体评分")) %>%
  filter(`4、评价任务编号` == "初步治疗方案任务") %>%
  filter(`2、评价病例编号` != "003010165") %>%
  filter(`2、评价病例编号` != "000498774") 
colnames(data1) <- c('专家', "病例", "模型", "任务", "方案排序", '评分')
data1 <- data1 %>%
  filter(!duplicated(cbind(专家, 病例, 模型)))
data1 <- data1 %>%
  filter(模型 != 'Google Gemini')
data1$评分 <- as.numeric(data1$评分)
data1$模型 <- factor(data1$模型)
xtabs(~专家 + 病例, data1)

data2 <- data1 %>%
  group_by(专家, 病例, 模型) %>%
  summarize(score = mean(评分), .groups = 'rowwise')
xtabs(~专家 + 病例, data2)
data2.1 <- data2 %>%
  filter(专家 == '1')
data2.2 <- data2 %>%
  filter(专家 == '2')
data2.3 <- data2 %>%
  filter(专家 == '3')
data2.4 <- data2 %>%
  filter(专家 == '4')
data2 <- merge(data2.1, data2.2, by = c('病例', '模型'))
data3 <- merge(data2.3, data2.4, by = c('病例', '模型'))
data2 <- merge(data2, data3, by = c('病例', '模型'))
KendallW(data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")],
         TRUE, test = TRUE)
cohen.kappa(x = data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")])

kruskal.test(评分 ~ 模型,
             data = data1)
test <- dunnTest(评分~模型,
                 data = data1, method = 'bh')
p <- test2p(test)
ggplot(data1, aes(x = 模型, y = 评分)) +
  geom_violin(color = 'black', fill = 'lightblue') +
  geom_boxplot(width = 0.2, fill = 'white')  + theme_classic() + 
  ylab('Score') + xlab('Model')
p2 <- data1 %>%
  count(模型, 评分) %>%
  spread(key = '评分', value = 'n', fill = 0)
p2[, c(2:6)] <- p2[, c(2:6)] / rowSums(p2[, c(2:6)])
# 排名统计
data1 <- data %>% 
  select(c("1、评价专家编号", "2、评价病例编号", "3、生成回答模型编号", "4、评价任务编号", 
           "17、实际治疗方案在回答所给三个方案中的第几个？", "26、对回答进行总体评分")) %>%
  filter(`4、评价任务编号` == "初步治疗方案任务") %>%
  filter(`2、评价病例编号` != "003010165") %>%
  filter(`2、评价病例编号` != "000498774") 
colnames(data1) <- c('专家', "病例", "模型", "任务", "方案排序", '评分')
data1 <- data1 %>%
  filter(!duplicated(cbind(专家, 病例, 模型)))
data1 <- data1 %>%
  filter(模型 != 'Google Gemini')
data1$评分 <- as.numeric(data1$评分)
data1$模型 <- factor(data1$模型)
data1$方案排序 <- factor(data1$方案排序) %>%
  as.numeric()
xtabs(~模型 + 方案排序, data1)
kruskal.test(方案排序 ~ 模型,
             data = data1)
test <- dunnTest(方案排序 ~ 模型,
                 data = data1, method = 'bh')
p <- test2p(test)
p$y.position <- c(4.5, 5)
data2 <- as.numeric(data1$方案排序)
ggplot(data1, aes(x = 模型, y = 方案排序)) +
  geom_violin(color = 'black', fill = 'lightblue') +
  geom_boxplot(width = 0.2, fill = 'white')  + theme_classic() + 
  ylab('Rank') + xlab('Model') + stat_pvalue_manual(p, label = 'signif')
p2 <- data1 %>%
  count(模型, 方案排序) %>%
  spread(key = '方案排序', value = 'n', fill = 0)
p2[, c(2:5)] <- p2[, c(2:5)] / rowSums(p2[, c(2:5)])

data2 <- data1
data2$方案排序 <- ifelse(data2$方案排序 > 1, 2, 1)
data2$方案排序 <- factor(data2$方案排序)
p <- xtabs(~模型 + 方案排序, data2)
fisher.test(p, simulate.p.value = T)
library(fmsb)
pairwise.fisher.test(p)

data3 <- data1
data3$方案排序 <- ifelse(data3$方案排序 < 4 , 1, 4)
data3$方案排序 <- factor(data3$方案排序)
p <- xtabs(~模型 + 方案排序, data3)
fisher.test(p, simulate.p.value = T)
library(fmsb)
pairwise.fisher.test(p)

# 详细治疗方案
data1 <- data %>% 
  select(c("1、评价专家编号", "2、评价病例编号", "3、生成回答模型编号", "4、评价任务编号", 
           "18、详细治疗方案中，回答是否遵循了指南？", 
           "19、详细治疗方案中，回答是否结合了患者情况？",
           "26、对回答进行总体评分")) %>%
  filter(`4、评价任务编号` == "详细治疗方案任务") %>%
  filter(`2、评价病例编号` != "003010165") %>%
  filter(`2、评价病例编号` != "000498774") 
colnames(data1) <- c('专家', "病例", "模型", "任务", "遵循指南情况", 
                     "遵循患者情况", '评分') 
data1 <- data1 %>%
  filter(!duplicated(cbind(专家, 病例, 模型)))
data1 <- data1 %>%
  filter(模型 != 'Google Gemini')
data1$评分 <- as.numeric(data1$评分)
data1$模型 <- factor(data1$模型)
xtabs(~专家 + 病例, data1)
# 总体打分
data2 <- data1 %>%
  group_by(专家, 病例, 模型) %>%
  summarize(score = mean(评分), .groups = 'rowwise')
xtabs(~专家 + 病例, data2)
data2.1 <- data2 %>%
  filter(专家 == '1')
data2.2 <- data2 %>%
  filter(专家 == '2')
data2.3 <- data2 %>%
  filter(专家 == '3')
data2.4 <- data2 %>%
  filter(专家 == '4')
data2 <- merge(data2.1, data2.2, by = c('病例', '模型'))
data3 <- merge(data2.3, data2.4, by = c('病例', '模型'))
data2 <- merge(data2, data3, by = c('病例', '模型'))
KendallW(data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")],
         TRUE, test = TRUE)
cohen.kappa(x = data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")])

kruskal.test(评分 ~ 模型,
             data = data1)
test <- dunnTest(评分~模型,
                 data = data1, method = 'bh')
p <- test2p(test)
ggplot(data1, aes(x = 模型, y = 评分)) +
  geom_violin(color = 'black', fill = 'lightblue') +
  geom_boxplot(width = 0.2, fill = 'white')+ theme_classic() + 
  ylab('Score') + xlab('Model')
p2 <- data1 %>%
  count(模型, 评分) %>%
  spread(key = '评分', value = 'n', fill = 0)
p2[, c(2:6)] <- p2[, c(2:6)] / rowSums(p2[, c(2:6)])
# 指南打分
data1$遵循指南情况 <- as.numeric(data1$遵循指南情况)
data2 <- data1 %>%
  group_by(专家, 病例, 模型) %>%
  summarize(score = mean(遵循指南情况), .groups = 'rowwise')
xtabs(~专家 + 病例, data2)
data2.1 <- data2 %>%
  filter(专家 == '1')
data2.2 <- data2 %>%
  filter(专家 == '2')
data2.3 <- data2 %>%
  filter(专家 == '3')
data2.4 <- data2 %>%
  filter(专家 == '4')
data2 <- merge(data2.1, data2.2, by = c('病例', '模型'))
data3 <- merge(data2.3, data2.4, by = c('病例', '模型'))
data2 <- merge(data2, data3, by = c('病例', '模型'))
KendallW(data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")],
         TRUE, test = TRUE)
cohen.kappa(x = data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")])

kruskal.test(遵循指南情况 ~ 模型,
             data = data1)
test <- dunnTest(遵循指南情况 ~ 模型,
                 data = data1, method = 'bh')
p <- test2p(test)
ggplot(data1, aes(x = 模型, y = 遵循指南情况)) +
  geom_violin(color = 'black', fill = 'lightblue') +
  geom_boxplot(width = 0.2, fill = 'white')+ theme_classic() + 
  ylab('Score') + xlab('Model')
p2 <- data1 %>%
  count(模型, 遵循指南情况) %>%
  spread(key = '遵循指南情况', value = 'n', fill = 0)
p2[, c(2:5)] <- p2[, c(2:5)] / rowSums(p2[, c(2:5)])
# 患者
data1$遵循患者情况 <- as.numeric(data1$遵循患者情况)
data2 <- data1 %>%
  group_by(专家, 病例, 模型) %>%
  summarize(score = mean(遵循患者情况), .groups = 'rowwise')
xtabs(~专家 + 病例, data2)
data2.1 <- data2 %>%
  filter(专家 == '1')
data2.2 <- data2 %>%
  filter(专家 == '2')
data2.3 <- data2 %>%
  filter(专家 == '3')
data2.4 <- data2 %>%
  filter(专家 == '4')
data2 <- merge(data2.1, data2.2, by = c('病例', '模型'))
data3 <- merge(data2.3, data2.4, by = c('病例', '模型'))
data2 <- merge(data2, data3, by = c('病例', '模型'))
KendallW(data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")],
         TRUE, test = TRUE)
cohen.kappa(x = data2[, c("score.x.x", "score.y.x", "score.x.y", "score.y.y")])

kruskal.test(遵循患者情况 ~ 模型,
             data = data1)
test <- dunnTest(遵循患者情况 ~ 模型,
                 data = data1, method = 'bh')
p <- test2p(test)
ggplot(data1, aes(x = 模型, y = 遵循患者情况)) +
  geom_violin(color = 'black', fill = 'lightblue') +
  geom_boxplot(width = 0.2, fill = 'white') + theme_classic() + 
  ylab('Score') + xlab('Model')
p2 <- data1 %>%
  count(模型, 遵循患者情况) %>%
  spread(key = '遵循患者情况', value = 'n', fill = 0)
p2[, c(2:6)] <- p2[, c(2:6)] / rowSums(p2[, c(2:6)])

# 知识性打分
data1 <- data %>% 
  select(c("1、评价专家编号", "2、评价病例编号", "3、生成回答模型编号", "4、评价任务编号", 
           "23、回答知识性问题打分扣分制，满分5分，出现一处错误扣1分，扣完为止；如果出现对患者有害的回答，一票否决，计为0分。")) %>%
  filter(`2、评价病例编号` != "003010165") %>%
  filter(`2、评价病例编号` != "000498774") 
colnames(data1) <- c('专家', "病例", "模型", "任务", "评分") 
data1 <- data1 %>%
  filter(!duplicated(cbind(专家, 病例, 模型, 任务)))
data1 <- data1 %>%
  filter(模型 != 'Google Gemini')
data1$模型 <- factor(data1$模型)
xtabs(~专家 + 病例, data1)
# 有害信息
data2 <- data1
data2$评分 <- ifelse(data1$评分 != "有害信息", "无害", "有害")
p <- xtabs(~模型 + 评分, data2)
fisher.test(p, simulate.p.value = T)
# 无害信息
data2 <- data1 %>%
  filter(评分 != "有害信息")
data2$评分 <- as.numeric(data2$评分)
data2 <- data2 %>%
  group_by(专家, 病例, 模型) %>%
  summarize(score = mean(评分), .groups = 'rowwise')
xtabs(~专家 + 病例, data2)
data2.1 <- data2 %>%
  filter(专家 == '1')
data2.2 <- data2 %>%
  filter(专家 == '2')
data2.3 <- data2 %>%
  filter(专家 == '3')
data2.4 <- data2 %>%
  filter(专家 == '4')
data2 <- merge(data2.1, data2.2, by = c('病例', '模型'))
data3 <- merge(data2.3, data2.4, by = c('病例', '模型'))
data2 <- data3
KendallW(data2[, c("score.x", "score.y")],
         TRUE, test = TRUE)
data2 <- data1 %>%
  filter(专家 > 2) %>%
  filter(评分 != "有害信息")
data2$评分 <- as.numeric(data2$评分)
kruskal.test(评分 ~ 模型,
             data = data2)
test <- dunnTest(评分 ~ 模型,
                 data = data2, method = 'bh')
p <- test2p(test)
ggplot(data2, aes(x = 模型, y = 评分)) +
  geom_violin(color = 'black', fill = 'lightblue') +
  geom_boxplot(width = 0.2, fill = 'white')+ theme_classic() + 
  ylab('Score') + xlab('Model')
p2 <- data1 %>%
  count(模型, 评分) %>%
  spread(key = '评分', value = 'n', fill = 0)
p2[, c(2:8)] <- p2[, c(2:8)] / rowSums(p2[, c(2:8)])

