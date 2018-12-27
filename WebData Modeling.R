df<-read.table("C:/Users/simon/Downloads/Model_Data.csv",sep = ",",header = T)
df<-df[complete.cases(df),]


df$Class1<-ifelse(df$Per_Change_T1>.02,"Increase",ifelse(df$Per_Change_T1<(-.02),"Decrease","No_Change"))
df$Class5<-ifelse(df$Per_Change_T5>.02,"Increase",ifelse(df$Per_Change_T5<(-.02),"Decrease","No_Change"))
df$Class30<-ifelse(df$Per_Change_T30>.01,"Increase",ifelse(df$Per_Change_T30<(-.02),"Decrease","No_Change"))
# df$Class

fun <- function(stri){
  as.integer(gsub(" ","",gsub("d","",gsub("a","",substr(stri,1,4)))))
}

df$Delta <- fun(df$TimeDelta)


df$log_F_P_1<-log((df$Finance.First.Paragraph+1))
df$log_G_P_1<-log((df$General.First.Paragraph+1))
df$log_F_P_L<-log((df$Finance.Last.Paragraph+1))
df$log_G_P_L<-log((df$General.Last.Paragraph+1))
df$log_F_H<-log((df$Finance.Headline+1))
df$log_G_H<-log((df$General.Headline+1))
df$log_F_C<-log(df$Finance.Combined+1)
df$log_G_C<-log(df$General.Combined+1)

df$mkvaltq<-log(df$mkvaltq)

library(caret)

set.seed(1995)

tetr<-createDataPartition(df$Per_Change_T1,p = .75,list = F)
tr<-df[tetr,]
te<-df[-tetr,]

##############

mod<-lm(Per_Change_T1~Finance.Headline+Finance.First.Paragraph+
          Finance.Last.Paragraph+mkvaltq+Delta
        ,data = tr)
summary(mod)
sqrt(mean((te$Per_Change_T1-predict(mod,newdata = te))^2))

modgent1<-lm(Per_Change_T1~General.Headline+General.First.Paragraph+
          General.Last.Paragraph+mkvaltq+Delta
        ,data = tr)
summary(modgent1)
sqrt(mean((te$Per_Change_T1-predict(modgent1,newdata = te))^2))

sqrt(abs(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared))


mod<-lm(Per_Change_T5~Industry+Finance.Headline+Finance.First.Paragraph+
          Finance.Last.Paragraph+mkvaltq+Delta
        ,data = tr)

summary(mod)
sqrt(mean((te$Per_Change_T5-predict(mod,newdata = te))^2))

modgen<-lm(Per_Change_T5~General.Headline+General.First.Paragraph+
             General.Last.Paragraph+mkvaltq+Delta
           ,data = tr)
summary(modgen)
sqrt(mean((te$Per_Change_T5-predict(modgen,newdata = te))^2))

sqrt(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared)

mod<-lm(Per_Change_T30~Industry+Finance.Headline+Finance.First.Paragraph+
          Finance.Last.Paragraph+mkvaltq+Delta
        ,data = tr)
summary(mod)
sqrt(mean((te$Per_Change_T30-predict(mod,newdata = te))^2))


modgen<-lm(Per_Change_T30~General.Headline+General.First.Paragraph+
             General.Last.Paragraph+mkvaltq+Delta
           ,data = tr)
summary(modgen)
sqrt(mean((te$Per_Change_T30-predict(modgen,newdata = te))^2))

sqrt(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared)

#######################################################
mod<-lm(Per_Change_T1~Industry+Finance.Combined+mkvaltq+Delta,data = tr)
summary(mod)
sqrt(mean((te$Per_Change_T1-predict(mod,newdata = te))^2))


modgen<-lm(Per_Change_T1~General.Combined+mkvaltq+Delta
           ,data = tr)
summary(modgen)
sqrt(mean((te$Per_Change_T1-predict(modgen,newdata = te))^2))

sqrt(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared)


#################
mod<-lm(Per_Change_T5~Industry+Finance.Combined+mkvaltq+Delta,data = tr)
summary(mod)
sqrt(mean((te$Per_Change_T5-predict(mod,newdata = te))^2))


modgenT5<-lm(Per_Change_T5~General.Combined+mkvaltq+Delta
           ,data = tr)
summary(modgenT5)
sqrt(mean((te$Per_Change_T5-predict(modgen,newdata = te))^2))

sqrt(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared)


#########
mod<-lm(Per_Change_T30~Industry+Finance.Combined+mkvaltq+Delta,data = tr)
summary(mod)
sqrt(mean((te$Per_Change_T30-predict(mod,newdata = te))^2))


modgen<-lm(Per_Change_T30~General.Combined+mkvaltq+Delta
           ,data = tr)
summary(modgen)
sqrt(mean((te$Per_Change_T30-predict(modgen,newdata = te))^2))

sqrt(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared)









### General Sentiments of Financial Documents are -Not Significant-
# mod<-lm(Per_Change_T1~Industry+General.Headline+General.First.Paragraph+
#           General.Last.Paragraph+mkvaltq
#         ,data = tr)
# summary(mod)
#######
#T1
#######

library(gmodels)

tr$up<-ifelse(tr$Class1=="Increase",1,0)

logmod<-glm(up~Industry+Finance.Headline+Finance.First.Paragraph+
              Finance.Last.Paragraph+mkvaltq,data = tr,family = "binomial")
summary(logmod)

CrossTable(ifelse(predict(logmod,newdata = df,"respons")>.3,1,0),ifelse(df$Class1=="Increase",1,0))

logmod<-glm(up~Industry+General.Headline+General.First.Paragraph+
              General.Last.Paragraph+mkvaltq,data = tr,family = "binomial")
summary(logmod)

CrossTable(ifelse(predict(logmod,newdata = te,"respons")>.5,1,0),ifelse(te$Class1=="Increase",1,0))

logmod<-glm(up~Industry+Finance.Combined+mkvaltq,data = tr,family = "binomial")
summary(logmod)

CrossTable(ifelse(predict(logmod,newdata = te,"respons")>.5,1,0),ifelse(te$Class1=="Increase",1,0))

logmod<-glm(up~Industry+General.Combined+mkvaltq,data = tr,family = "binomial")
summary(logmod)

CrossTable(ifelse(predict(logmod,newdata = te,"respons")>.5,1,0),ifelse(te$Class1=="Increase",1,0))
#######
#T5
#######
tr$up<-ifelse(tr$Class5=="Increase",1,0)

logmod<-glm(up~Industry+Finance.Headline+Finance.First.Paragraph+
              Finance.Last.Paragraph+mkvaltq,data = tr,family = "binomial")
summary(logmod)

logmod<-glm(up~Industry+General.Headline+General.First.Paragraph+
              General.Last.Paragraph+mkvaltq,data = tr,family = "binomial")
summary(logmod)

logmod<-glm(up~Industry+Finance.Combined+mkvaltq,data = tr,family = "binomial")
summary(logmod)

logmod<-glm(up~Industry+General.Combined+mkvaltq,data = tr,family = "binomial")
summary(logmod)
#######
#T30
#######
tr$up<-ifelse(tr$Class30=="Increase",1,0)

logmod<-glm(up~Industry+Finance.Headline+Finance.First.Paragraph+
              Finance.Last.Paragraph+mkvaltq,data = tr,family = "binomial")
summary(logmod)

logmod<-glm(up~Industry+General.Headline+General.First.Paragraph+
              General.Last.Paragraph+mkvaltq,data = tr,family = "binomial")
summary(logmod)

logmod<-glm(up~Industry+Finance.Combined+mkvaltq,data = tr,family = "binomial")
summary(logmod)

logmod<-glm(up~Industry+General.Combined+mkvaltq,data = tr,family = "binomial")
summary(logmod)




###############################################
###   LOGIT  
###############################################



mod<-lm(Per_Change_T1~log_F_H+log_F_P_1+log_F_P_L+mkvaltq+Delta
        ,data = tr)
summary(mod)
prod(1.1^mod$coefficients[2:4])


modgen<-lm(Per_Change_T1~log_G_H+log_G_P_1+log_G_P_L+mkvaltq+Delta
           ,data = tr)
summary(modgen)
prod(1.1^modgen$coefficients[2:4])


sqrt(abs(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared))


mod<-lm(Per_Change_T5~log_F_H+log_F_P_1+log_F_P_L+mkvaltq+Delta
        ,data = tr)

summary(mod)
prod(1.1^mod$coefficients[2:4])

modgen<-lm(Per_Change_T5~log_G_H+log_G_P_1+log_G_P_L+mkvaltq+Delta
           ,data = tr)
summary(modgen)
(prod(1.1^modgen$coefficients[2:4])-1)*100

sqrt(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared)

mod<-lm(Per_Change_T30~log_F_H+log_F_P_1+log_F_P_L+mkvaltq+Delta
        ,data = tr)
summary(mod)
(prod(1.1^mod$coefficients[2:4])-1)*100


modgen<-lm(Per_Change_T30~log_G_H+log_G_P_1+log_G_P_L+mkvaltq+Delta
           ,data = tr)
summary(modgen)

(prod(1.1^modgen$coefficients[2:4])-1)*100


sqrt(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared)

#######################################################
mod<-lm(Per_Change_T1~Industry+log_F_C+mkvaltq+Delta,data = tr)
summary(mod)



modgen<-lm(Per_Change_T1~log_G_C+mkvaltq+Delta
           ,data = tr)
summary(modgen)


sqrt(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared)
1.1^mod$coefficients[3:5]
1.1^modgen$coefficients[3:5]

#################
mod<-lm(Per_Change_T5~Industry+log_F_C+mkvaltq+Delta,data = tr)
summary(mod)



modgen<-lm(Per_Change_T5~Industry+log_G_C+mkvaltq+Delta
           ,data = tr)
summary(modgen)


sqrt(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared)
1.1^mod$coefficients[3:5]
1.1^modgen$coefficients[3:5]
#########
mod<-lm(Per_Change_T30~Industry+log_F_C+mkvaltq+Delta,data = tr)
summary(mod)



modgen<-lm(Per_Change_T30~Industry+log_G_C+mkvaltq+Delta
           ,data = tr)
summary(modgen)


sqrt(summary(mod)$adj.r.squared-summary(modgen)$adj.r.squared)
1.1^mod$coefficients[3:5]
1.1^modgen$coefficients[3:5]
