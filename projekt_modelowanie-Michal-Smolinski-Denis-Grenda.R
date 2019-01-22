library(data.table)
library(corrplot)
library(corrgram)
library(leaps)
library(gam)
library(plotROC)

load("~/KrukUWr2018.RData")

Cases=data.table(cases)
Events=data.table(events)

#podzial na zbior testowy i uczacy
set.seed(1410)

idx =  sample(seq(1, 3), size = nrow(Cases), replace = TRUE, prob = c(0.3, 0.6, 0.1))
Cases[, Set := idx]

Train = Cases[Set == 1]
Test = Cases[which(idx==2),]
Ref = Cases[which(idx==3),]

TR = Cases[which(idx==1 | idx ==2),]

setkey(Cases,CaseId)
setkey(Events,CaseId)
setkey(TR,CaseId)
setkey(Ref,CaseId)

Events[is.na(PaymentAmount), PaymentAmount := 0]


#wylaczamy firmy z analizy ze wzgledu na podobne wolumeny
Cases[Age==-1, Age:=NA]

# _________________________________

v = Cases[Set<3 & !is.na(LoanAmount) & Principal != 0, (LoanAmount/Principal)]
v = v[!v %in% boxplot.stats(v)$out]
mean(v) #1.247484
var(v) # 0.1317789
Cases[Set < 3 & is.na(LoanAmount), LoanAmount:=Principal*rnorm(1,mean(v), sqrt(var(v)) )]
  

v = Cases[Set == 3 & !is.na(LoanAmount) & Principal != 0, (LoanAmount/Principal)]
v = v[!v %in% boxplot.stats(v)$out]
mean(v) #1.258678
var(v) # 0.1424521
Cases[Set == 3 & is.na(LoanAmount), LoanAmount:=Principal*rnorm(1,mean(v), sqrt(var(v)) )]

Cases[LoanAmount<Principal + Interest + ifelse(is.na(Other), 0, Other), LoanAmount := Principal + Interest  + ifelse(is.na(Other), 0, Other)]

#___________________________________________________________

Cases[Set<3,CreditCard := ifelse(Product=="Credit card",1,0)]
Cases[Set<3,Female := ifelse(Gender=="FEMALE",1,0)]

Cases[Set==3,CreditCard := ifelse(Product=="Credit card",1,0)]
Cases[Set==3,Female := ifelse(Gender=="FEMALE",1,0)]

#___________________________________________________________

Cases[Set<3 & is.na(Female),Female:= ifelse(runif(1,0,1)<Cases[Set<3,mean(Female,na.rm=TRUE)],1,0)]
Cases[Set<3 & is.na(Bailiff),Bailiff:= ifelse(runif(1,0,1)<Cases[Set<3,mean(Bailiff,na.rm=TRUE)],1,0)]
Cases[Set<3 & is.na(ExternalAgency), ExternalAgency:= ifelse(runif(1,0,1)<Cases[Set<3,mean(ExternalAgency,na.rm=TRUE)],1,0)]

Cases[Set==3 & is.na(Female),Female:= ifelse(runif(1,0,1)<Cases[Set==3,mean(Female,na.rm=TRUE)],1,0)]
Cases[Set==3 & is.na(Bailiff),Bailiff:= ifelse(runif(1,0,1)<Cases[Set==3,mean(Bailiff,na.rm=TRUE)],1,0)]
Cases[Set==3 & is.na(ExternalAgency), ExternalAgency:= ifelse(runif(1,0,1)<Cases[Set==3,mean(ExternalAgency,na.rm=TRUE)],1,0)]

#______________________________________________________________

Cases[Set<3 & is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0]
Cases[Set<3 & is.na(ClosedExecution), ClosedExecution:= ifelse(runif(1,0,1)<Cases[Set<3,mean(ClosedExecution,na.rm=TRUE)],1,0)]

Cases[Set==3 & is.na(ClosedExecution) & Bailiff==0, ClosedExecution:= 0]
Cases[Set==3 & is.na(ClosedExecution), ClosedExecution:= ifelse(runif(1,0,1)<Cases[Set==3, mean(ClosedExecution,na.rm=TRUE)],1,0)]

#____________________________________________________________________

Variables = c(         "Land",
                       "D_ContractDateToImportDate",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate"

)

for(v in Variables)
{
  for( r in c("<", "=="))
  {
    non_na_set = eval( parse( text=sprintf("Cases[Set %s 3 & !is.na(%s), %s]", r,v,v) ) )
    size_nas = eval(parse( text=sprintf("Cases[Set %s 3, sum(is.na(%s))]", r, v) ) ) 
    var_dist = eval(parse( text=sprintf("prop.table(table(Cases[Set %s 3 & !is.na(%s), %s]))", r, v, v) ))
    sampled = sample(unique(non_na_set), size = size_nas, prob = var_dist, replace=TRUE)
    
    eval(parse( text=sprintf("Cases[Set %s 3 & is.na(%s), %s := sampled]", r, v ,v) ))
  }
}


land_stats = unique(cases[!is.na(Land),.(Land,GDPPerCapita, MeanSalary)])[order(Land)]

Cases[, GDPPerCapita := land_stats[Land,2]]
Cases[,MeanSalary := land_stats[Land,3]]


#___________________

sum12 =  Events[,.(SumOfPayments = sum(PaymentAmount)), by=.(CaseId)]
sumofpay = sum12[, .SD, .SDcols = c("CaseId", "SumOfPayments")]
setkey(sumofpay, CaseId)
Cases = Cases[sumofpay, nomatch=0, on = "CaseId"]

# _________________________________

#principal, interest i other skalujemy wzgledem toa
#takze skalujemy loan amount, by usunac korelacje (mimo mniej naturalnej interpretacji)
#wtedy other mozna wyrzucic bo po przerobce, other = 1-principal-interest
#od  "D_ContractDateToImportDate" odejmujemy DPD, tworzac nowa zmienna "ContractDateToDefault"

Cases[, External := Bailiff + ExternalAgency + ClosedExecution]
Cases[, Principal := Principal/TOA]
Cases[,Interest:= Interest / TOA]
Cases[,LoanAmount:= LoanAmount / TOA]
Cases[, D_ContractDateToImportDate := ifelse(D_ContractDateToImportDate<DPD, 0, D_ContractDateToImportDate-DPD)]
#Cases[,M_LastPaymentToImportDate:=ifelse(M_LastPaymentToImportDate>DPD,0,DPD-M_LastPaymentToImportDate)]
names(Cases)[names(Cases) == "D_ContractDateToImportDate"] = "ContractDateToDefault"



Variables = c(         "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "ContractDateToDefault",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "GDPPerCapita",
                       "MeanSalary",
                       "External",
                       "CreditCard",
                       "Female"
)

corrplot(cor(Cases[Set == 1,.SD,.SDcols = Variables]), order = "hclust", tl.col='black', tl.cex=.75)

vif <- data.table()
for (i in 1:dim(Cases[Set==1,.SD,.SDcols = Variables])[2]) {
  
  model_lm <- lm(paste(Variables[i],paste(Variables[-i], collapse="+"),sep="~"), data = Cases[Set==1, ])
  vif <- rbind(vif ,data.table(Variable = Variables[i], AdjR2 = summary(model_lm)$adj.r.squared, VIF = 1/(1-summary(model_lm)$adj.r.squared)))
}       

#pkb per capita i srednia pensja sa skorelowane z populacja w miescie i maja duzy vif - nalezy je wywalic
print(vif)

###############################################################



vars = c(              "LoanAmount",
                       "TOA",
                       "Principal",
                       "Interest",
                       "ContractDateToDefault",
                       "DPD",
                       "PopulationInCity",
                       "Age",
                       "LastPaymentAmount",
                       "M_LastPaymentToImportDate",
                       "External"
)

# __________________________________

train_means =  Cases[Set == 1, sapply(.SD, function(x) mean(x, na.rm=TRUE)), .SDcols = vars]
train_vars =  Cases[Set == 1, sapply(.SD, function(x) var(x, na.rm=TRUE)), .SDcols = vars]

CasesStd = data.table(cbind(CaseId=Cases[,CaseId],scale(Cases[,.SD,.SDcols = vars], center = train_means, scale = sqrt(train_vars)), 
                             CreditCard=Cases[,CreditCard], Female=Cases[,Female], SumOfPayments = Cases[, SumOfPayments], Set=Cases[,Set]))

# dodajemy zmienna dobry/zly
# dobry kilent to taki ktory dokonal jakiejkolwiek wplaty
Cases[, IsGood := ifelse(SumOfPayments > 0, 1, 0)]
CasesStd[, IsGood := ifelse(SumOfPayments > 0, 1, 0)]

#________________________________________________________


sum12 =  Events[,.(SumOfPayments = sum(PaymentAmount)), by=.(CaseId)]
for(i in 1:12)
{
  colname = paste0("payment", i)
  payments = Events[Month==i,  .(payment = sum(PaymentAmount) ), by=.(CaseId)]
  setkey(payments, CaseId)
  sum12 = sum12[payments]
  sum12[, payment := ifelse(SumOfPayments == 0, 0, payment/SumOfPayments)]
  names(sum12)[names(sum12) == "payment"] = colname 
}

payment_distribution = c()
for(i in 1:12)
{
  colname = paste0("payment", i)
  p = sum12[which(idx==1)][SumOfPayments > 0, .SD, .SDcols=colname][[1]]
  payment_distribution = c(payment_distribution, mean(p))
}

interest_rate = 0.015 # stopa referencyjna nbp
discount_rate = 1/(1 + interest_rate/12) # stopa dyskontowa
  
# wartosc obecna netto to suma_platnosci * d
d = sum(payment_distribution * 0.8*(discount_rate^(1:12))) # 0.7932205


#________________________________________
#lista formul - do selekcji krokowej

formulas = lapply(setdiff(vars, c("Female", "External", "CreditCard")), 
                  function(x) as.formula(paste("~1+", x, "+s(", x, ",2)", "+s(", x, ",3)", sep = "")))
formulas = c(formulas, 
             lapply(c("Female", "External", "CreditCard"), function(x) (as.formula(paste("~1+", x,sep = "")))))

#________________________________________
# model 1 - jednoetapowy - suma wp³at

CasesStd[, SuccessRate := CasesStd[, SumOfPayments] / Cases[,TOA]]
CasesStd[SuccessRate<0, SuccessRate := 0]
CasesStd[SumOfPayments <0, SumOfPayments := 0]
Train = CasesStd[Set==1,]; Test = CasesStd[Set==2,]; Ref = CasesStd[Set==3,]


sum_model = glm(as.formula(paste("round(SumOfPayments)+1~",paste(vars, collapse="+"))), data=Train, family=quasipoisson)

payment_predict = predict(sum_model, Test, type="response")
sum_predict = sum(payment_predict) #suma = 41045641
payment_test = Cases[Set==2][, sum(SumOfPayments)] #suma = 38709312

abs(sum_predict - payment_test)/payment_test


#__________________________________
# model 2 - jednoetapowy
# modelowanie skutecznosci

sr_model =  gam(as.formula(paste("round(10000*SuccessRate)~",paste(vars, collapse="+"))), data=Train, family=poisson)

sr_predict = predict.Gam(sr_model, Test, type="response")/1000
payment_predict2 = sr_predict * Cases[Set==2][,TOA]
sum_predict2 = sum(payment_predict2)

abs(sum_predict2 - payment_test)/payment_test


#_______________________________________
# model 3 - dwuetapowy

# 3a - selekcja klientow dobrych

class = gam(as.formula(paste("IsGood~",paste(vars, collapse="+"))), data=Train,family=binomial )

gam_predict = predict.Gam(class, Test, type="response")


#rysujemy krzywa ROC
roc1 = data.frame(M = gam_predict, D = Test[,IsGood])
rocplot = ggplot(roc1, aes(m=M, d=D)) + plotROC::geom_roc(labelround=2, labelsize = 3.88, cutoffs.at = c(0.1,0.2,0.25,0.3,0.35,0.4,0.5,0.6)) + geom_abline(intercept = 0, slope = 1) + xlab("1- specifity") + ylab("sensitivity") + ggtitle("ROC Curve")
rocplot


#ustalamy najlepszy cutoff
#kryterium - najwieksza suma sens+spec
sensspec = list(c(),c())
sensspec[[1]] = sapply(0:1000, function(x) {sum((gam_predict>x/1000) & Test[,IsGood])/sum(Test[,IsGood])})
sensspec[[2]] = sapply(0:1000, function(x) {sum((gam_predict<x/1000) & !Test[,IsGood])/sum(!Test[,IsGood])})

sensspec = sensspec[[1]] + sensspec[[2]]
cutoff = which(sensspec == max(sensspec))/1000
cutoff

#ostateczna predykcja
predicted = (gam_predict > cutoff) * 1
predicted = data.table(IsGood = Test[, IsGood], PredictedIsGood = predicted)

tp = dim(predicted[PredictedIsGood == 1 & IsGood == 1])[1]
tn = dim(predicted[PredictedIsGood == 0 & IsGood == 0])[1]
fp = dim(predicted[PredictedIsGood == 1 & IsGood == 0])[1]
fn = dim(predicted[PredictedIsGood == 0 & IsGood == 1])[1]

acc = (tp+tn)/(tp+tn+fp+fn); acc #0.639
sens = tp/(tp+fn); sens #0.558
spec = tn/(tn+fp); spec #0.680


# 3b - obliczenie skutecznosci zakladajac ze klient jest dobry

good_set = Train[IsGood==1,]

sr_model_good = gam(as.formula(paste("round(10000*SuccessRate)~",paste(vars, collapse="+"))), data=good_set, family=poisson)

sr_predict_good = predict(sr_model_good, Test, type="response")/10000
payment_predict3 = sr_predict_good * Cases[Set==2][,TOA]

sum_predict3 = sum(payment_predict3 * predicted[,PredictedIsGood])

abs(sum_predict3 - payment_test)/payment_test

#____________________________________

# model 4
# predykcja wp³at = P(klient dobry) * skutecznoœæ * TOA

payment_predict4 = payment_predict3 * gam_predict
sum_predict4 = sum(payment_predict4)

abs(sum_predict4 - payment_test)/payment_test

#____________________________________

#predykcja na zbiorze referencyjnym

ref_npv = Ref[, sum(SumOfPayments)] * d; ref_npv # 4324062
ref_toas = Cases[Set==3][,TOA]

# model 1

recovery_predict1 = predict.Gam(sum_model, Ref, type="response")
npv1 = sum(recovery_predict1) * d; npv1
error1 = abs(npv1 - ref_npv) / ref_npv; error1 


recovery_predict2 = predict.Gam(sr_model, Ref, type="response")/10000
npv2 = sum(recovery_predict2 * ref_toas) * d; npv2
error2 = abs(npv2 - ref_npv) / ref_npv; error2 


class3 = predict.Gam(class, Ref, type="response")
class3 = (class3 > cutoff) * 1

recovery_predict3_good = predict(sr_model_good, Ref, type="response") * ref_toas / 10000
npv3 = sum(recovery_predict3_good * class3) * d; npv3 
error3 = abs(npv3 - ref_npv) / ref_npv; error3 


prop4 = predict.Gam(class, Ref, type="response")
npv4 = sum(recovery_predict3_good * prop4) * d; npv4
error4 = abs(npv4 - ref_npv) / ref_npv; error4


options(scipen=999)
options(digits=3)
mt = matrix(c(npv1,npv2,npv3,npv4,error1,error2,error3,error4), 2,4, byrow=TRUE)
rownames(mt) = c("Predicted NPV", "Relative Error")
colnames(mt) = c("Model 1", "Model 2", "Model 3", "Model 4")
mt


#                    Model 1     Model 2     Model 3      Model 4
#Predicted NPV  4519102.2261 4521438.872 5488789.318 4468302.5716
#Relative Error       0.0245       0.024       0.185       0.0354