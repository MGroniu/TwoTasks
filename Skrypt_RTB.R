library(rjson)
library(dplyr)
library(data.table)
setwd("")

#################################################
#TASK 1
#################################################

#-------------------------
#ZAD1
result <- fromJSON(file = "task1_data.json")
df  <-  as.data.frame(matrix(unlist(result), nrow=length(unlist(result[1]))))

names(df) <- names(result)

df$clicks <- as.numeric(as.character(df$clicks))
df$conversions <- as.numeric(as.character(df$conversions))
df$conversion_value <- as.numeric(as.character(df$conversion_value))
df$ctr <- as.numeric(as.character(df$ctr))
df$cost <- as.numeric(as.character(df$cost))
df <- df[!is.na(df$cost),]

aggregate(as.numeric(df$conversion_value), by = list(Category=df$business_unit), FUN=sum)

conversion_value_df <- df %>% group_by(business_unit) %>% summarise(conversion_value = sum(as.numeric(conversion_value))) %>% data.frame()
conversion_value_df[order(conversion_value_df$conversion_value, decreasing = TRUE),] %>% head(5)

#-------------------------
#ZAD2
df$impres <- df$clicks/df$ctr
df[!is.finite(df$impres),'impres'] <- 0


ctr_df <- df %>% group_by(country) %>% summarise(ctr = sum(clicks, na.rm = TRUE)/sum(impres, na.rm = TRUE)) %>% data.frame()
ctr_df[!is.finite(ctr_df$ctr),'ctr'] <- 0


#The Cost of Campaign = Total Impressions/1000 x CPM
cpm_df <- df %>% group_by(country) %>% summarise(cpm = sum(cost,na.rm = TRUE)*1000/sum(impres, na.rm = TRUE)) %>% data.frame()
cpm_df[!is.finite(cpm_df$cpm),'cpm'] <- 0


df$cpm <- df$cost*1000/df$impres
df[!is.finite(df$cpm),'cpm'] <- 0
#Conversion RAte https://support.google.com/google-ads/answer/2684489?hl=en
cr_df <- df %>% group_by(country) %>% summarise(cr = sum(conversions, na.rm = TRUE)/sum(clicks, na.rm = TRUE)) %>% data.frame()
cr_df[!is.finite(cr_df$cr),'cr'] <- 0

df$cr <- df$conversions/ df$clicks
df[!is.finite(df$cr),'cr'] <- 0

calculated_df <- ctr_df
calculated_df <- left_join(calculated_df, cpm_df)
calculated_df <- left_join(calculated_df, cr_df)
quantile(calculated_df$ctr,probs = c(0.2, 0.8))
quantile(calculated_df$cpm,probs = c(0.2, 0.8))
quantile(calculated_df$cr,probs = c(0.2, 0.8))

#-------------------------
#ZAD3
df %>% group_by(category, business_unit) %>%
  summarize(q25cr = quantile(cr, probs = c(0.25)),
            q50cr = quantile(cr, probs = c(0.5)),
            q75cr = quantile(cr, probs = c(0.75)),
            q25cr = quantile(ctr, probs = c(0.25)),
            q50cr = quantile(ctr, probs = c(0.5)),
            q75cr = quantile(ctr, probs = c(0.75)),
            q25cpm = quantile(cpm, probs = c(0.25)),
            q50cpm = quantile(cpm, probs = c(0.5)),
            q75cpm = quantile(cpm, probs = c(0.75)),
            q25impres = quantile(impres, probs = c(0.25)),
            q50impres = quantile(impres, probs = c(0.5)),
            q75impres = quantile(impres, probs = c(0.75)))



#-------------------------
#ZAD5
calculated_df[order(calculated_df$cr, decreasing = TRUE),]
df_business_unit <- df %>%group_by(business_unit) %>% summarise(ctr = sum(clicks, na.rm = TRUE)/sum(impres, na.rm = TRUE)) %>% data.frame() 
df_business_unit <- left_join(df_business_unit, df %>% group_by(business_unit) %>% summarise(cpm = sum(cost,na.rm = TRUE)*1000/sum(impres, na.rm = TRUE)) %>% data.frame())
df_business_unit <- left_join(df_business_unit, df %>% group_by(business_unit) %>% summarise(cr = sum(conversions, na.rm = TRUE)/sum(clicks, na.rm = TRUE)) %>% data.frame())

df_business_unit %>% arrange(desc(ctr))
df_business_unit %>% arrange(desc(cpm))
df_business_unit %>% arrange(desc(cr))

#################################################
#TASK 2
#################################################

#-------------------------
#ZAD1
task2_df <- read.csv("task2_data.csv")
task2_df$last_viewed <- as.POSIXct(as.numeric(task2_df$last_viewed)/1000, origin='1970-01-01', tz = "UTC")

last_viewed_df <- task2_df %>% group_by(user) %>% summarise(last_viewed = max(last_viewed)) %>% data.frame()
last_viewed_df <- left_join(last_viewed_df,task2_df)
setnames(last_viewed_df, "product", "last_product")

second_last_viewd_df <- task2_df %>% group_by(user) %>% summarise(second_last_viewed = max( last_viewed[last_viewed!=max(last_viewed)] )) %>% data.frame()
second_last_viewd_df <- left_join(second_last_viewd_df,task2_df, by = c("user"="user", "second_last_viewed"="last_viewed"))
setnames(second_last_viewd_df, "product", "second_last_product")

viewed_df <- left_join(last_viewed_df,second_last_viewd_df[,c("user","second_last_product")], by = "user")

nrow(viewed_df[viewed_df$last_product=="product 2" & viewed_df$second_last_product=="product 1",])/length(unique(task2_df$user))

#-------------------------
#ZAD2
nrow(viewed_df[viewed_df$last_product=="product 2" & viewed_df$second_last_product=="product 1",])/nrow(last_viewed_df[last_viewed_df$last_product=="product 1",])

#-------------------------
#ZAD3
viewed_df[viewed_df$last_product == "product 2",]


#-------------------------
#ZAD4
task2_df[task2_df$product== "product 2", ] 
distinct(task2_df[task2_df$product== "product 2", c("user","product")] )

task2_4_df <- left_join(distinct(task2_df[task2_df$product== "product 2", c("user","product")] ),
               distinct(task2_df[task2_df$product== "product 1", c("user","product")] ),
               by = "user" )

nrow(task2_4_df[complete.cases(task2_4_df),])/nrow(distinct(task2_df[task2_df$product== "product 2", c("user","product")] ))


#-------------------------
#ZAD5
table(last_viewed_df$last_product)
table(second_last_viewd_df$second_last_product)
table(task2_df$product)
