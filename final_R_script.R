library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(hrbrthemes)
library(grid)
library(gridExtra)
setwd("C:/Users/juhig/OneDrive/Desktop/BU/course content s1/PM/assignment")
callcentre<-read_excel("callcentre_40.xlsx")
hrm<-read_excel("HRM_40.xlsx")

##merging both datasets
combineddata_1<-merge(callcentre, hrm, by = "agent")

##Removing NAs: Data cleaning##
combineddata_2<-na.omit(combineddata_1)
combineddata<-combineddata_2 %>% filter(combineddata_2$qualification %in% c("university","apprenticeship","some college"))


#customer satisfaction by agent
cust_1<-combineddata %>% group_by(agent) %>% summarise(mean_custsat=mean(customer_satisfaction)) %>% arrange(-mean_custsat)#customer satisfaction by agent

##Customer satisfaction throughout calls recieved
overall<-combineddata %>% summarise(mean_customer_satisfaction=mean(customer_satisfaction))
overall_plot<-boxplot(combineddata$customer_satisfaction,main = "customer satisfaction and call volume",ylab="customer_satisfaction")

##customer satisfaction and waiting time
new_1<-ggplot(combineddata_2, aes(x=waiting,y=customer_satisfaction))+
  geom_line(stat = "identity",position = "identity",color="#69b3a2")+scale_y_continuous(breaks = seq(1, 10, by = 1))+theme_ipsum()+geom_smooth(method = "lm",size=1, se=FALSE)

##mean customer satisfaction per hour
exp_1<-aggregate(customer_satisfaction~time,combineddata,mean)
exp_1_plot <- ggplot(exp_1, aes(x=time,y=customer_satisfaction))+
  geom_line(stat = "identity",position = "identity",color="#69b3a2")+scale_y_continuous(labels = scales::comma)+ 
  scale_x_continuous(breaks = seq(0, 25, by = 5))+theme_ipsum()+geom_smooth(method = "lm", se=FALSE)+xlab("Hour of the day")+
  theme(axis.title.x = element_text(hjust = .5))+ylab("mean customer satisfaction")

##mean no. of calls per hour
exp_4<-combineddata_1 %>% group_by(time) %>% count(time)
exp_4_plot<-ggplot(exp_4, aes(x=time,y=n))+
  geom_line(stat = "identity",position = "identity",color="red")+scale_x_continuous(breaks = seq(0, 25, by = 5))+theme_ipsum()+
  xlab("Hour of the day")+theme(axis.title.x = element_text(hjust = .5),axis.title.y = element_text(hjust = .5))+ylab("mean no.of calls")

##mean waiting time per hour
exp_3<-aggregate(waiting~time,combineddata_1,mean)
exp_3_plot<-ggplot(exp_3, aes(x=time,y=waiting))+
  geom_line(stat = "identity",position = "identity",color="#69b3a2")+scale_y_continuous(labels = scales::comma)+ 
  scale_x_continuous(breaks = seq(0, 25, by = 5))+theme_ipsum()+geom_smooth(method = "lm", se=FALSE)+xlab("Hour of the day")+
  theme(axis.title.x = element_text(hjust = .5),axis.title.y = element_text(hjust = .5))+ylab("mean waiting time")

##Agents worked per hour
exp_56<-combineddata %>% group_by(time) %>% summarise(count = n_distinct(agent))
exp_56_plot<-ggplot(exp_56, aes(x=time,y=count))+
  geom_line(stat = "identity",position = "identity",color="red")+scale_x_continuous(breaks = seq(0, 25, by = 5))+theme_ipsum()+
  xlab("Hour of the day")+theme(axis.title.x = element_text(hjust = .5),axis.title.y = element_text(hjust = .5))+ylab("no.of agents worked")

##Problem analysis
prob_1<-combineddata %>% group_by(forwarded) %>% count(problem)
prob_1_plot<-ggplot(prob_1, aes(x=problem,y=n,fill=forwarded))+
  geom_bar(stat="identity",colour="blue",width = .5)+ylab("call per problem")

prob_2_plot<-ggplot(prob_2, aes(x=problem,y=mean_customer_satisfaction,fill= "green3"))+
  geom_bar(stat="identity",colour="green3",width =.5)
prob_1_plot+prob_2_plot

##Top performer analysis

cust_2<-combineddata %>% group_by(agent,ethnicity,qualification,gender,tenure,age) %>% 
  summarise(sum_length=sum(length),mean_length=mean(length)) %>% arrange(-sum_length) ## call length per agent
cust_3<-combineddata %>% count(agent)
cust_4<-merge(cust_2,cust_3, by = "agent")
colnames(cust_4)[9]<-"call_per_agent"
cust_fin<-merge(cust_1,cust_4, by = "agent")
cust_fin_top_call_no<-cust_fin %>% slice_max(call_per_agent, n=5) %>% select("ethnicity","qualification","agent","age","call_per_agent")
cust_fin_top_cust_sat<-cust_fin %>% slice_max(mean_custsat, n=5)%>% select("ethnicity","qualification","agent","age","mean_custsat")
cust_fin_high_call_length<-cust_fin %>% slice_max(mean_length, n=5)%>% select("ethnicity","qualification","agent","age","mean_length")

##call per agent based on tenure
tenure_1<-ggplot(cust_4, aes(x=tenure,y=call_per_agent))+
  geom_line(stat="identity", colour="blue")
