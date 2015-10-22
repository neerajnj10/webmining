```
library(shinyapps)
library(rvest)
library(RCurl)
library(stringr)
library(XML)
library(snowfall)
library(ggplot2)
library(xtable)
library(ggvis)
library(knitr)
library(lattice)
options(warn=-1)


mh <- html("https://www.gov.uk/government/publications/suspended-licences-for-manufacturers-and-wholesalers-of-medicines")
mhr <- mh%>% html_nodes("span a")%>% html_attr(name="href")
mhr
mhrg <- str_c("https://www.gov.uk",mhr)
mhrl <- grep("https://www.gov.uk/.*csv$",mhrg)
mhr_sheet<- mhrg[mhrl]
mhr_sheet
mhr_get <- vector("list", length= (length(mhr_sheet)))

for(i in 1:length(mhr_sheet)){
  print(i)
  require(RCurl)
  mhr_sheet[i] <- getURL(mhr_sheet[i])
  mhr_get[[i]]<-read.csv(textConnection(mhr_sheet[i]))
}
m1<- mhr_get[[1]]
m1$Reason <- ""
m1$Action <- "Suspended"
m1$`Report Date` <- ""
colnames(m1) <- c("Company", "License","Date","Period","Reason",
                  "Action","Report Date")
m2<- mhr_get[[2]]
m2$`Report Date`<-""
m2$Action <- "Revoked"
m2$Period <- ""
colnames(m2) <- c("Company","License","Date","Reason","Report Date",
                  "Action","Period")
mhra_final <-rbind(m1, m2)

###
h1 <- html("http://www.hpra.ie/homepage/medicines/safety-notices?page=1&type=3&orderby=DATE_NEWEST")
h2<- html("http://www.hpra.ie/homepage/medicines/safety-notices?page=2&type=3&orderby=DATE_NEWEST")
h<- h2%>% html_nodes("td a")%>% html_attr(name="href")
hp <- h1%>% html_nodes("td a")%>% html_attr(name="href")
hpr <- c(h,hp)
hprg <- str_c("http://www.hpra.ie/homepage/medicines/",hpr)

Notice <- vector("list", length=length(hprg))
for(i in 1:length(hprg)){
  print(i)
  Notice[i] <- html(hprg[[i]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblnoticetype")%>%html_text()
}
Notice = do.call("rbind", lapply(Notice, "[[", 1))
Notice

Date <- vector("list", length=length(hprg))
for(j in 1:length(hprg)){
  print(j)
  Date[j] <- html(hprg[[j]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lbldate")%>%html_text()
}
Date = do.call("rbind", lapply(Date, "[[", 1))
Date

Reason <- vector("list", length=length(hprg))
for(k in 1:length(hprg)){
  print(k)
  Reason[k] <- html(hprg[[k]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblbody")%>%html_text()
}
Reason = do.call("rbind", lapply(Reason, "[[", 1))
Reason

Product_name <- vector("list", length=length(hprg))
for(l in 1:length(hprg)){
  print(l)
  Product_name[l]<- html(hprg[[l]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblproductnameortype")%>%html_text()
}
Product_name = do.call("rbind", lapply(Product_name, "[[", 1))
Product_name

Refernce <- vector("list", length=length(hprg))
for(m in 1:length(hprg)){
  print(m)
  Refernce[m]<- html(hprg[[m]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblreference")%>%html_text()
}
Refernce = do.call("rbind", lapply(Refernce, "[[", 1))
Refernce

Authorization_number <- vector("list", length=length(hprg))
for(n in 1:length(hprg)){
  print(n)
  Authorization_number[n]<- html(hprg[[n]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblauthorisationnumber")%>%html_text()
}  
Authorization_number = do.call("rbind", lapply(Authorization_number, "[[", 1))
Authorization_number

Active_Substance<- vector("list", length=length(hprg))
for(o in 1:length(hprg)){
  print(o)
  Active_Substance[o] <-  html(hprg[[o]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblactivesubstance")%>%html_text()
}
Active_Substance = do.call("rbind", lapply(Active_Substance, "[[", 1))
Active_Substance

Product_Classification<-  vector("list", length=length(hprg))
for(p in 1:length(hprg)){
  print(p)
  Product_Classification[p]<- html(hprg[[p]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblproductclassification")%>%html_text()
}
Product_Classification = do.call("rbind", lapply(Product_Classification, "[[", 1))
Product_Classification

Manufacturer<-vector("list", length=length(hprg))
for(q in 1:length(hprg)){  
  print(q)
  Manufacturer[q]<- html(hprg[[q]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblmanufacturerorsupplier")%>%html_text()
}
Manufacturer = do.call("rbind", lapply(Manufacturer, "[[", 1))
Manufacturer

Expiry_date<- vector("list", length=length(hprg))
for(r in 1:length(hprg)){   
  print(r)
  Expiry_date[r]<- html(hprg[[r]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblserialorbatchnumberandexpirydate")%>%html_text()
}
Expiry_date = do.call("rbind", lapply(Expiry_date, "[[", 1))


Authorization_holder <- vector("list", length=length(hprg))
for(s in 1:length(hprg)){
  print(s)
  Authorization_holder[s]<- html(hprg[[s]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblauthorisationholder")%>%html_text()
}  
Authorization_holder = do.call("rbind", lapply(Authorization_holder, "[[", 1))
Authorization_holder

Prescription_req <- vector("list", length=length(hprg))
for(t in 1:length(hprg)){
  print(t)
  Prescription_req[t]<- html(hprg[[t]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblprescriptionrequired")%>%html_text()
}
Prescription_req = do.call("rbind", lapply(Prescription_req, "[[", 1))
Prescription_req

Recall_class <-  vector("list", length=length(hprg))
for(u in 1:length(hprg)){
  print(u)
  Recall_class[u]<- html(hprg[[u]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblrecallclassification")%>%html_text()
}
Recall_class = do.call("rbind", lapply(Recall_class, "[[", 1))
Recall_class

Target_audience <- vector("list", length=length(hprg))
for(v in 1:length(hprg)){
  print(v)
  Target_audience[v]<- html(hprg[[v]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lbltargetaudience")%>%html_text()
}
Target_audience = do.call("rbind", lapply(Target_audience, "[[", 1))
Target_audience

Problem <-  vector("list", length=length(hprg))
for(w in 1:length(hprg)){ 
  print(w)
  Problem[w]<- html(hprg[[w]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblproblemorissue")%>%html_text()
}
Problem = do.call("rbind", lapply(Problem, "[[", 1))
Problem

Background_info <- vector("list", length=length(hprg))
for(x  in 1:length(hprg)){
  print(x)
  Background_info[x]<- html(hprg[[x]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblbackgroundinformationorrelateddocuments")%>%html_text()
}
Background_info = do.call("rbind", lapply(Background_info, "[[", 1))
Background_info 

Actions <-  vector("list", length=length(hprg))
for(y in 1:length(hprg)){
  print(y)
  Actions[y]<- html(hprg[[y]])%>%
    html_nodes("#ContentPlaceHolderBody_C001__lblactionstobetaken")%>%html_text()
}
Actions = do.call("rbind", lapply(Actions, "[[", 1))
Actions

hpra_final <- data.frame(Notice,Date,Reason,Product_name,Refernce,Authorization_number,
                         Active_Substance,Manufacturer,Expiry_date, Background_info,Product_Classification,
                         Authorization_holder,Prescription_req,Recall_class,
                         Target_audience, Problem, Actions)
hpra_final$links <- hprg
hpra_final$Notice <- gsub("^Notice type: ", "", hpra_final$Notice)
hpra_final$Date <- gsub("^Date: ", "", hpra_final$Date)
hpra_final$Product_name <- gsub("^Product name or type:", "", hpra_final$Product_name)
hpra_final$Refernce <- gsub("^Reference:", "", hpra_final$Refernce)
hpra_final$Authorization_number <- gsub("^Authorisation Number:", "", hpra_final$Authorization_number)
hpra_final$Active_Substance <- gsub("^Active Substance:", "", hpra_final$Active_Substance)
hpra_final$Manufacturer <- gsub("Manufacturer Or Supplier:","", hpra_final$Manufacturer)
hpra_final$Expiry_date <- gsub("Serial Or Batch Number And Expiry Date:","", hpra_final$Expiry_date)
hpra_final$Background_info <- gsub("^Background Information Or Related Documents:", "", hpra_final$Background_info)
hpra_final$Product_Classification <- gsub("^Product Classification:", "", hpra_final$Product_Classification)
hpra_final$Authorization_holder <- gsub("^Authorisation Holder:", "", hpra_final$Authorization_holder)
hpra_final$Prescription_req <- gsub("^Prescription Required:", "", hpra_final$Prescription_req)
hpra_final$Recall_class <- gsub("^Recall Classification:", "", hpra_final$Recall_class)
hpra_final$Target_audience <- gsub("^Target Audience:", "", hpra_final$Target_audience)
hpra_final$Problem <- gsub("Problem Or Issue:","",hpra_final$Problem)
hpra_final$Actions <- gsub("Actions To Be Taken:","",hpra_final$Actions)
hpra_final$Indication <- NA
hpra_final$`Lab operator` <- NA
hpra_final$`Reporting Origin` <- NA
hpra_final$`Specific Comments` <- NA
colnames(hpra_final)[9] <- "Expiry_date_Batch_no."
names(hpra_final)
###
an_main <- html("http://www.ansm.sante.fr/S-informer/Informations-de-securite-Ruptures-de-stock-des-medicaments")
an <- an_main%>% html_nodes("div a")%>%html_attr(name="href")

ang <- grep("^/S-informer/Informations-de-securite-Ruptures-de-stock-des-medicaments.*$", an)
anm <- an[ang]
anm <- str_c("http://www.ansm.sante.fr",anm)
an_sm <- anm[-c(1,2)]

an_m <- html("http://www.ansm.sante.fr/S-informer/Informations-de-securite-Ruptures-de-stock-des-medicaments/ALKONATREM-150-mg-gelule-Chlorhydrate-de-demeclocycline-Rupture-de-stock")%>%
  html_nodes("tr th")%>% html_text()
an_main <-as.data.frame(t(an_m),header=FALSE)

Indications <- vector("list", length = (length(an_sm)))
for(i in 1:length(an_sm)){
  print(i)
  # error handling - skips to next URL if it gets an error
  result <- try(
    Indications[i] <- html(an_sm[[i]])%>% html_nodes(".online_editor th+td")%>% html_text()
  ); if(class(result) == "try-error") next;
}
Indications = do.call("rbind", lapply(Indications, "[[", 1))


Lab_operator <- vector("list", length = (length(an_sm)))
for(i in 1:length(an_sm)){
  print(i)
  # error handling - skips to next URL if it gets an error
  result <- try(
    Lab_operator[i] <- html(an_sm[[i]])%>% html_nodes("tr~ tr+ tr td") %>% html_text()
  ); if(class(result) == "try-error") next;
}
Lab_operator = do.call("rbind", lapply(Lab_operator, "[[", 1))
Lab_operator <- gsub("\n","",Lab_operator)

Reporting_origin <- vector("list", length = (length(an_sm)))
for(i in 1:length(an_sm)){
  print(i)
  # error handling - skips to next URL if it gets an error
  result <- try(
    Reporting_origin[i] <- html(an_sm[[i]])%>% html_nodes("tr:nth-child(3) td") %>% html_text()
  ); if(class(result) == "try-error") next;
}
Reporting_origin = do.call("rbind", lapply(Reporting_origin, "[[", 1))
Reporting_origin <- gsub("\n","",Reporting_origin)

Date <- vector("list", length = (length(an_sm)))
for(i in 1:length(an_sm)){
  print(i)
  # error handling - skips to next URL if it gets an error
  result <- try(
    Date[i] <- html(an_sm[[i]])%>% html_nodes("tr:nth-child(4) td") %>% html_text()
  ); if(class(result) == "try-error") next;
}
Date = do.call("rbind", lapply(Date, "[[", 1))
Date <- sub("^.+([0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]).+$","\\1",Date)

Specific_comments <- vector("list", length = (length(an_sm)))
for(i in 1:length(an_sm)){
  print(i)
  # error handling - skips to next URL if it gets an error
  result <- try(
    Specific_comments[i] <- html(an_sm[[i]])%>% html_nodes("td div") %>% html_text()
  ); if(class(result) == "try-error") next;
}
Specific_comments = do.call("rbind", lapply(Specific_comments, "[[", 1))
Specific_comments <- gsub("\n","",Specific_comments)

#Now combining all the parts together and structuring them in tabular form


ansm_full <- cbind(Indications,Lab_operator,Reporting_origin,Date,Specific_comments)
ansm_view <- rbind(an_main,ansm_full)
colnames(ansm_view) <- c("Indication","Lab operator", "Reporting Origin",
                         "Date","Specific Comments")
ansm_view <- ansm_view[-1,]
ansm_view$links <- an_sm
ansm_view$Notice <- NA
ansm_view$Reason <- NA
ansm_view$Product_name <- NA
ansm_view$Refernce <- NA
ansm_view$Manufacturer <- NA
ansm_view$Actions <- NA
ansm_view$Expiry_date_Batch_no. <- NA
ansm_view$Problem <- NA
ansm_view$Authorization_number <- NA
ansm_view$Active_Substance <- NA
ansm_view$Background_info <- NA
ansm_view$Product_Classification <- NA
ansm_view$Authorization_holder <- NA
ansm_view$Prescription_req <- NA
ansm_view$Recall_class <- NA
ansm_view$Target_audience <- NA
Recalls <- rbind(hpra_final,ansm_view)

##ashp
##main
library(rvest)
as_c <- html('http://www.ashp.org/menu/DrugShortages/CurrentShortages')
as_main_current <- as_c%>% html_node("table")%>% html_table
#View(as_main_current)

as_r<-html("http://www.ashp.org/menu/DrugShortages/ResolvedShortages")
as_main_resolved<- as_r %>% html_node("table")%>% html_table
as_no <- html('http://www.ashp.org/menu/DrugShortages/NoPresentations')
as_main_no <- as_no%>% html_node("table")%>% html_table
as_d <- html("http://www.ashp.org/menu/DrugShortages/DrugsNoLongerAvailable")
as_main_dis <- as_d%>%html_node("table")%>% html_table
##now the sub-parts, from hyperlinks
#current
library(stringr)
as_get_link_current <- as_c%>% html_nodes("td a")%>% html_attr(name="href")
as_get_link_current <- str_c("http://www.ashp.org",as_get_link_current)
as_main_current$links <-as_get_link_current
as_main_current$Status <- "Shortage"
#resolved
as_get_link_resolved <- as_r%>% html_nodes("td a")%>% html_attr(name="href")
as_get_link_resolved <- str_c("http://www.ashp.org",as_get_link_resolved)
as_main_resolved$links <- as_get_link_resolved
as_main_resolved$Status <- "Resolved"
#no preparation
as_get_link_noprep <- as_no%>% html_nodes("td a")%>% html_attr(name="href")
as_get_link_noprep <- str_c("http://www.ashp.org",as_get_link_noprep)
as_main_no$links <- as_get_link_noprep
as_main_no$Status <- "No preparation"
#discontinued
as_get_link_dis <- as_d%>% html_nodes("td a")%>% html_attr(name="href")
as_get_link_dis <- str_c("http://www.ashp.org",as_get_link_dis)
as_main_dis$links <- as_get_link_dis
as_main_dis$Status <- "Discontinued"
as_f <- rbind(as_main_current,as_main_resolved, as_main_no,as_main_dis)
##getting data now

as_product <- vector("list",length=length(as_f$links))
for(i in 1:length(as_f$links)){
  print(i)
  as_product[[i]] <- html(as_f$links[i])%>%html_nodes("#ctl00_ContentPlaceHolder1_lblProducts") %>% html_text("/n")
 # Sys.sleep(1)
}
Product_description <- do.call("rbind", lapply(as_product, "[", 1))

as_reason <- vector("list",length=length(as_f$links))
for(i in 1:length(as_f$links)){
  print(i)
  as_reason[[i]] <- html(as_f$links[i])%>%html_nodes("#ctl00_ContentPlaceHolder1_lblReason") %>% html_text("/n")
  #Sys.sleep(1)
}
Reason_for_Shortage <- do.call("rbind", lapply(as_reason, "[[", 1))

as_available <- vector("list",length=length(as_f$links))
for(i in 1:length(as_f$links)){
  print(i)
  as_available[[i]] <- html(as_f$links[i])%>%html_nodes("#ctl00_ContentPlaceHolder1_lblAvailable") %>% html_text("/n")
#  Sys.sleep(1)
}
library (snowfall)
# initialize cluster
sfInit (parallel=TRUE , cpus=4)
# parallel computing
Available_Products <- do.call("rbind", sfLapply(as_available, "[", 1))
sfStop ()


as_resupply <- vector("list",length=length(as_f$links))
for(i in 1:length(as_f$links)){
  print(i)
  as_resupply[[i]] <- html(as_f$links[i])%>%html_nodes("#ctl00_ContentPlaceHolder1_lblResupply") %>% html_text("/n")
  #Sys.sleep(1)
}
sfInit (parallel=TRUE , cpus=4)
Resupply <- do.call("rbind", sfLapply(as_resupply, "[[", 1))
sfStop()

as_bind <- cbind(Product_description,Reason_for_Shortage,Available_Products,
                 Resupply)
ashp_final <-data.frame(as_bind, as_f)
names(ashp_final) <- c("Affected Product Description", "Reason for Shortage", 
                       "Available Products","Resupply", "Generic Name", "Revision Date",
                       "Links", "Status")
#import alerts
#fda
library(stringr)
imp<- html("http://www.accessdata.fda.gov/scripts/ImportRefusals/ir_selection.cfm?DYear=2012&DMonth=12&CountryCode=CA")%>%
  html_nodes("td a")%>% html_attr(name= "href")
imp <- str_c("http://www.accessdata.fda.gov/scripts/ImportRefusals/", imp)
imp_tb <- vector("list", length = (length(imp)))
for(i in 1:length(imp)){
  print(i)
  # error handling - skips to next URL if it gets an error
  result <- try(
    imp_tb[[i]] <- html(imp[i])%>% html_nodes("tr td")%>% html_text()
  ); if(class(result) == "try-error") next;
}
full1 = do.call("rbind", lapply(imp_tb, "[[", 1))
full2 = do.call("rbind", lapply(imp_tb, "[[", 2))
full3 = do.call("rbind", lapply(imp_tb, "[[", 3))
full4 = do.call("rbind", lapply(imp_tb, "[[", 4))
full5 = do.call("rbind", lapply(imp_tb, "[[", 5))
full6 = do.call("rbind", lapply(imp_tb, "[[", 6))
full7 = do.call("rbind", lapply(imp_tb, "[[", 7))
full8 = do.call("rbind", lapply(imp_tb, "[[", 8))
full9 = do.call("rbind", lapply(imp_tb, "[[", 9))
full10 = do.call("rbind", lapply(imp_tb, "[[", 10))
full11 = do.call("rbind", lapply(imp_tb, "[[", 11))
full12 = do.call("rbind", lapply(imp_tb, "[[", 12))
full13 = do.call("rbind", lapply(imp_tb, "[[", 13))
full14 = do.call("rbind", lapply(imp_tb, "[[", 14))
full <- cbind(full1,full2,full3,full4,full5,full6,full7,full8,full9,full10,full11,full12,full13,full14)
y_main<- html("http://www.accessdata.fda.gov/scripts/ImportRefusals/ir_detail.cfm?EntryId=UPS-8677355-9&DocId=1&LineId=1&SfxId=")%>% html_nodes("tr th")%>% html_text()
d_main <-as.data.frame(t(y_main),header=FALSE)
d_main<- d_main[,-c(15:18)]
omg <- rbind(d_main,full)
##

library(XML)
library(dplyr)
library(lubridate)
library(data.table)

CEP <- "http://www.blue-inspection.com/english/aktuelles/cep.html?sortby=manufacturer&desc=1"
CEP <- readHTMLTable(CEP, stringAsFactor=FALSE, which =1)
CEP<-as.data.frame(CEP)
colnames(CEP) <- c("Substance name","Manufacturer","Country","CEP Number","Date","Action","Reason")
d <- as.Date( CEP$Date, "%d.%m.%Y")
d<- strftime(d, "%m/%d/%Y")
CEP$Date <- d
Lines <- grep("CEP ....-...", CEP[,"CEP Number"])
CEP[,"CEP Number"][Lines]
CEP[,"CEP Number"]<-(gsub("^.*(CEP ....-...).*$", "\\1", CEP[,"CEP Number"][Lines]))
CEP[,"CEP Number"]
CEP <- as.data.frame(CEP)
names(CEP)
CEP1 <- "http://www.edqm.eu/en/CEP-suspensions-withdrawals-restorations-1536.html#CEPinspection"
CEP1 <- readHTMLTable(CEP1, which = 1, stringAsFactor=FALSE)
colnames(CEP1) <- c("Date","Substance name","CEP Number")
d1 <- as.Date(CEP1$Date, "%d/%m/%y")
d1<- strftime(d1, "%m/%d/%Y")
CEP1$Date <- d1
CEP1

CEP2 <- "http://www.edqm.eu/en/CEP-suspensions-withdrawals-restorations-1536.html#CEPinspection"
CEP2 <- readHTMLTable(CEP2, which = 2, stringAsFactor=FALSE)
colnames(CEP2) <- c("Date","Substance name","CEP Number")
d2 <- as.Date(CEP2$Date, "%d/%m/%y")
d2 <- strftime(d2, "%m/%d/%Y")
CEP2$Date <- d2
CEP2

CEP3 <- "http://www.edqm.eu/en/CEP-suspensions-withdrawals-restorations-1536.html#CEPinspection"
CEP3 <- readHTMLTable(CEP3, which = 3, stringAsFactor=FALSE)
colnames(CEP3) <- c("Date","Substance name","CEP Number")
d3 <- as.Date(CEP3$Date, "%d/%m/%y")
d3<- strftime(d3, "%m/%d/%Y")
CEP3$Date <- d3
CEP3

CEP4 <- "http://www.edqm.eu/en/CEP-suspensions-withdrawals-restorations-1536.html#CEPinspection"
CEP4 <- readHTMLTable(CEP4, which = 4, stringAsFactor=FALSE)
colnames(CEP4) <- c("Date","Substance name","CEP Number")
d4 <- as.Date(CEP4$Date, "%d/%m/%y")
d4 <- strftime(d4, "%m/%d/%Y")
CEP4$Date <- d4
CEP4$Date[[4]] <- "11/03/14"
CEP4

CEP5 <- "http://www.edqm.eu/en/CEP-suspensions-withdrawals-restorations-1536.html#CEPinspection"
CEP5 <- readHTMLTable(CEP5, which = 5, stringAsFactor=FALSE)
colnames(CEP5) <- c("Date","Substance name","CEP Number")
d5 <- as.Date(CEP5$Date, "%d/%m/%y")
d5 <- strftime(d5, "%m/%d/%Y")
CEP5$Date <- d5
CEP5$Date[[7]] <- "11/13/14"

CEP6 <- "http://www.edqm.eu/en/CEP-suspensions-withdrawals-restorations-1536.html#CEPinspection"
CEP6 <- readHTMLTable(CEP6, which = 6, stringAsFactor=FALSE)
colnames(CEP6) <- c("Date","Substance name","CEP Number")
d6 <- as.Date(CEP6$Date, "%d/%m/%y")
d6 <- strftime(d6, "%m/%d/%Y")
CEP6$Date <- d6
CEP6

Suspension<-as.data.frame(rbind(CEP1,CEP2,CEP3))
Withdrawal<-as.data.frame(rbind(CEP4, CEP5))
Restoration <- as.data.frame(CEP6)
#Withdrawal[,"Date"] <- gsub("<U+00A0>", "", Withdrawal[,"Date"])
#tablehead <- gsub("<U+00A0>", "", tablehead)
CEP_EDQM <- as.data.frame(rbind(Suspension,Withdrawal,Restoration))
CEP_EDQM$Manufacturer <- NA
CEP_EDQM$Country      <- NA
CEP_EDQM$Action       <- NA
CEP_EDQM$Reason       <- NA
nm <- c("Manufacturer", "Country","Reason", "Action")
CEP_EDQM[nm] <- lapply(nm, function(x) CEP[[x]][match(CEP_EDQM$`CEP Number`, CEP$`CEP Number`)])
cp <-rbind(CEP, CEP_EDQM)
cp$Reasons <- NA
cp$Reasons[cp$Reason ==1] <- "By EDQM, as a result of an inspection of the manufacturing site(s)"
cp$Reasons[cp$Reason ==2] <- "By EDQM, upon request from the holder, due to a temporary inability to produce the API under the approved conditions "
cp$Reasons[cp$Reason ==3] <- "By EDQM, due to a failure to fulfill the requirements of the CEP procedure with regards to updating the application and complying with GMP"
cp$Reasons[cp$Reason ==4] <- "By EDQM, following inspection of the manufacturing site(s)"
cp$Reasons[cp$Reason ==5] <- "By EDQM, due to a failure to comply with a declaration of willingness to be inspected and/or to operate according to EU GMP (e.g. refusal, of inspection, reconstruction/restoration of site(s), to achieve GMP level, and temporary closure)"
cp$Reason <- NULL
cp[!duplicated(cp), ]
cp

##canada inspection
library(snowfall)
library(stringr)
library(XML)
library(stringi)
library(RCurl)
library(rvest)
sfInit (parallel=TRUE , cpus=4)
##subparts.

f <- html("http://www.hc-sc.gc.ca/dhp-mps/pubs/compli-conform/tracker-suivi-eng.php")%>%
  html_nodes("td a" ) %>% html_attr(name="href")
ci <- vector("list", length=length(f))
for (i in 1:length(f)){
  print(i)
  result <- try(
    ci[[i]] <-html(f[i])%>% html_nodes("#awr_details_header_container")%>%html_text);
  if(class(result) == "try-error") next;
}
ci <- gsub("\n|\t", "", ci)
ci <- do.call("rbind", sfLapply(ci,"[[", 1))
startdate <- str_extract(ci, "Starting date:.* Posting")
startdate <- gsub("Starting date:|Posting","", startdate)

Postingdate <- str_extract(ci, "Posting.* Type")
Postingdate <- gsub("Posting date:|Type", "", Postingdate)

Typeofcommunication <- str_extract(ci, "Type.* Subcategory")
Typeofcommunication <- gsub("Type of communication:|Subcategory","", Typeofcommunication)

Subcategories <- str_extract(ci, "Subcategory:.* Source")
Subcategories <- gsub("Subcategory:| Hazard classification:Type .* ","",Subcategories)

Classifications <- str_extract(ci, "Hazard.* Source")
Classifications <- gsub("Hazard classification:|Source","", Classifications)

Recallsource <- str_extract(ci, "Source of recall:.* Issue")
Recallsource <- gsub("Source of recall:|Issue","", Recallsource)

Issues <- str_extract(ci, "Issue:.* Audience")
Issues <- gsub("Issue:|Audience","",Issues)

Audiences <- str_extract(ci, "Audience:.* Identification")
Audiences <- gsub("Audience:|Identification:","",Audiences)

Identificationnumber <- str_extract(ci, "Identification number:.*")
Identificationnumber <- gsub("Identification number:","",Identificationnumber)

healthcanada <- cbind(startdate,Postingdate,Typeofcommunication,Subcategories,Classifications,
                      Recallsource,Issues,Audiences,Identificationnumber)
colnames(healthcanada) <- c("Start date", "Posting date","type of communication",
                            "Sub categories", "classification","recall source",
                            "issues","audience","identification no.")

healthcanada<- as.data.frame(healthcanada)

r <- htmlParse("http://www.hc-sc.gc.ca/dhp-mps/pubs/compli-conform/tracker-suivi-eng.php")
q1 <- readHTMLTable(r,which=1, stringAsFactor=FALSE)
q2 <- readHTMLTable(r,which=2, stringAsFactor=FALSE)
q <- rbind(q1, q2)
healthcanadamain <- q

sfStop()
```
