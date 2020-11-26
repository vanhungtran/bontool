---
title: "Test"
author: "TRAN VAN HUNG"
date: "11/26/2020"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
load("~/bontool/data/data1.RData")



library(pROC)



library(tidyverse)
data2 <- data1 %>% select(contains(" - "))
data2$Status = ifelse(data1$Statut=='Endo',1,0)




DFALL3 = data2
DFALL3_Ctrl = DFALL3 %>% filter(Status==0)
DFALL3_Endo = DFALL3 %>% filter(Status==1)





library(tidyr)

DFALL3_Ctrl_c = data.frame(
  sapply(
    DFALL3_Ctrl,
    function(x) ifelse(is.na(x),
                       median(x, na.rm = TRUE),
                       x)))
DFALL3_Endo_c = data.frame(
  sapply(
    DFALL3_Endo,
    function(x) ifelse(is.na(x),
                       median(x, na.rm = TRUE),
                       x)))

DFALL4 = rbind.data.frame(DFALL3_Ctrl_c, DFALL3_Endo_c)

library(dplyr)
pacman::p_load(caTools)


y = DFALL4$Status




X= DFALL4[,-1]

AUC = t(colAUC(X, y, plotROC=FALSE, alg=c("Wilcoxon","ROC")))
colnames(AUC) = 'valueAUC'
#AUC = AUC %>% arrange(valueAUC)
AUC1 = cbind.data.frame(AUC, rownames(AUC))
colnames(AUC1) = c('valueAUC','Marker')
AUC1 = AUC1 %>% arrange(desc(valueAUC)) %>% mutate(N = 1:length(Marker))
AUC1 %>% knitr::kable()



DFALL5 = DFALL4
colnames(DFALL5) = NULL
colnames(DFALL5)[which(colnames(DFALL4)=='Status')] <- "Status"




for (i in (1:length(colnames(DFALL5))-1)){
  colnames(DFALL5)[i] <- paste0('Marker_',i-1)
}









fit1=glm(Status~Marker_49,data = DFALL5, family='binomial')
fit2=glm(Status~Marker_1,data = DFALL5, family='binomial')
fit3=glm(Status~Marker_1 + Marker_3 +
           Marker_5 + Marker_49 + Marker_56+ ,data = DFALL5, family='binomial')


Status = DFALL5$Status
preds=predict(fit1)
roc1=roc(Status ~ preds)

preds2=predict(fit2)
roc2=roc(Status ~ preds2)

preds3=predict(fit3)
roc3=roc(Status ~ preds3)



plot(roc1)
plot(roc2, add=TRUE, col='red')
plot(roc3, add=TRUE, col='blue')

```

