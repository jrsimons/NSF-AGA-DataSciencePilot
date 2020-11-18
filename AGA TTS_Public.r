#setwd("C:/Users/mcasey001.LOCALAD/OneDrive - Guidehouse/Documents/Project/NSF/Single Audit Review/Version for review")
setwd("C:/Users/mcasey001.LOCALAD/OneDrive - Guidehouse/Documents/Project/NSF/Single Audit Review")

suppressWarnings(library(dplyr))
suppressWarnings(library(readxl))
suppressWarnings(library(lubridate))
suppressWarnings(library(aod))
suppressWarnings(library(plm))
suppressWarnings(library(stargazer))
suppressWarnings(library(corrplot))
suppressWarnings(library(caret))
suppressWarnings(library(ROCR))
suppressWarnings(library(png))

CFDA_Entry<-c('47.')

#summary_reports<- readxl_example("Summary_Reports.xlsx")
CFDA_Data<-read_excel("Summary_Reports.xlsx", sheet = "CFDA INFO")
#CFDA_Data1<-("Summary_Reports (1).xlsx", sheet = "CFDA INFO")
#CFDA_Data<-bind_rows(CFDA_Data,CFDA_Data1)
Finding_Data<-read_excel("Summary_Reports.xlsx", sheet = "FINDINGS")
General_Info <- read_excel("Summary_Reports.xlsx", sheet = "GENERAL INFO")
head(CFDA_Data)
head(Finding_Data)
head(General_Info)

#read in by award, FY13-FY17
#CFDAData<-read.csv("fac_cfda_findings.csv", stringsAsFactors = F)
#read in by award, FY18-FY19
#CFDAData1<-read.csv("fac_cfda_findings_2018_2019.csv", stringsAsFactors = F)
#FindingData<-read.csv("fac_audit_findings.csv", stringsAsFactors = F)
#bind rows, since columns are the same to have a full set of award data FY2013-FY2019
#CFDAData<-bind_rows(CFDAData,CFDAData1)
#FAC General Data
#FAC_General<-read.csv("fac_summary_data_72.csv", stringsAsFactors = F)
#head(CFDAData)

FindingData_working<- FindingData %>% mutate(Compliance_Allowable_Activity= ifelse(grepl("A",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Allowable_Costs= ifelse(grepl("B",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Cash_Management= ifelse(grepl("C",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_D= ifelse(grepl("D",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Eligibility= ifelse(grepl("E",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Equipment_Real_Property= ifelse(grepl("F",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Matching_Level_of_Effort= ifelse(grepl("G",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Period_of_Performance= ifelse(grepl("H",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Procurement= ifelse(grepl("I",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Program_Income= ifelse(grepl("J",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_K= ifelse(grepl("K",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Reporting= ifelse(grepl("L",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Subrecipient= ifelse(grepl("M",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Special_Tests_Provisions= ifelse(grepl("N",TYPEREQUIREMENT),1,0))
FindingData_working<- FindingData_working %>% mutate(Compliance_Other= ifelse(grepl("P",TYPEREQUIREMENT),1,0))


Agency_only<- CFDA_Data %>% filter(grepl(CFDA_Entry,CFDA))
colnames(Agency_only)
head(Agency_only)

CFDA_Finding_Data<-left_join(Agency_only,FindingData_working, by = "ELECAUDITSID")
colnames(CFDA_Finding_Data)
head(CFDA_Finding_Data)

CFDA_Finding_Data_to_merge<-CFDA_Finding_Data %>% mutate(modified_opinion=ifelse(MODIFIEDOPINION=="Y",1,0))
CFDA_Finding_Data_to_merge<-CFDA_Finding_Data_to_merge %>% mutate(other_matters=ifelse(OTHER.MATTERS=="Y",1,0))
CFDA_Finding_Data_to_merge<-CFDA_Finding_Data_to_merge %>% mutate(material_weakness=ifelse(MATERIALWEAKNESS=="Y",1,0))
CFDA_Finding_Data_to_merge<-CFDA_Finding_Data_to_merge %>% mutate(significant_deficiency=ifelse(SIGNIFICANTDEFICIENCY=="Y",1,0))
CFDA_Finding_Data_to_merge<-CFDA_Finding_Data_to_merge %>% mutate(other_findings=ifelse(OTHERFINDINGS=="Y",1,0))
CFDA_Finding_Data_to_merge<-CFDA_Finding_Data_to_merge %>% mutate(q_costs=ifelse(QCOSTS=="Y",1,0))
CFDA_Finding_Data_to_merge<-CFDA_Finding_Data_to_merge %>% mutate(repeat_finding=ifelse(REPEATFINDING=="Y",1,0))

CFDA_Finding_Data_to_merge<- CFDA_Finding_Data_to_merge %>% replace(is.na(.),0)

head(CFDA_Finding_Data_to_merge)
#head(CFDA_Finding_Data)

#Creating the aggregation to remove duplicate findings per award
CFDA_Finding_Data1<-CFDA_Finding_Data_to_merge %>% group_by(AUDITYEAR,DBKEY.x,CFDA,ELECAUDITSID,FEDERALPROGRAMNAME,AMOUNT,ELECAUDITSID) %>% summarize(Findings=mean(FINDINGSCOUNT),Modified_Opinion=sum(modified_opinion),Other_Matters=sum(other_matters),Material_Weakness=sum(material_weakness),Significant_Deficiency=sum(significant_deficiency),Other_Findings=sum(other_findings),Questioned_Costs=sum(q_costs), Repeat_Findings=sum(repeat_finding), Allowable_Activity=sum(Compliance_Allowable_Activity),Allowable_Costs=sum(Compliance_Allowable_Costs),Cash_Management=sum(Compliance_Cash_Management),ComplianceD=sum(Compliance_D),Eligibility=sum(Compliance_Eligibility),Equipment_Real_Property=sum(Compliance_Equipment_Real_Property),Matching_Level_of_Effort=sum(Compliance_Matching_Level_of_Effort),Period_of_Performance=sum(Compliance_Period_of_Performance),Procurement=sum(Compliance_Procurement),Program_Income=sum(Compliance_Program_Income),ComplianceK=sum(Compliance_K),Reporting=sum(Compliance_Reporting),Subrecipient=sum(Compliance_Subrecipient),Special_Tests_Provisions=sum(Compliance_Special_Tests_Provisions),Other=sum(Compliance_Other))


Combined_Data_Final<-left_join(CFDA_Finding_Data1,General_Info, by=c("AUDITYEAR"="AUDITYEAR","DBKEY.x"="DBKEY"))
Combined_Data_Final<-rename(Combined_Data_Final,DBKEY=DBKEY.x)
Combined_Data_Final<-rename(Combined_Data_Final,SIGNIFICANTDEFICIENCY="REPORTABLECONDITION/SIGNIFICANTDEFICIENCY")
head(Combined_Data_Final)
names(Combined_Data_Final)

#To add general findings at the award level, this will provide insight into if there are failures for awards that aren't associated with the agency.
Finding_Data_to_merge<-Combined_Data_Final %>% mutate(Significant_Deficiency_Overall=ifelse(SIGNIFICANTDEFICIENCY=="Y",1,0))
Finding_Data_to_merge<-Finding_Data_to_merge %>% mutate(Material_Weakness_Overall=ifelse(MATERIALWEAKNESS=="Y",1,0))
Finding_Data_to_merge<-Finding_Data_to_merge %>% mutate(Material_NonCompliance_Overall=ifelse(MATERIALNONCOMPLIANCE=="Y",1,0))
Finding_Data_to_merge<-Finding_Data_to_merge %>% mutate(Questioned_Costs_Overall=ifelse(QCOSTS=="Y",1,0))
Finding_Data_to_merge<-Finding_Data_to_merge %>% mutate(Going_Concern_Overall=ifelse(GOINGCONCERN=="Y",1,0))

#Remove the null values since in this case we know them to be 0s, since they are the result of not being able to match the award to the findings data.
Finding_Data_to_merge_no_na<- Finding_Data_to_merge %>% replace(is.na(.),0)

#Collapse to institution level by year, means are taken for the overall variables since in each observation they are the total for the institution
Agency_Data_Final_Institution_Yearly<-Finding_Data_to_merge_no_na %>% group_by(AUDITYEAR,EIN,MULTIPLEEINS,EINSUBCODE,AUDITEENAME,CITY,STATE,ZIPCODE,COG_OVER,COGAGENCY,OVERSIGHTAGENCY,TYPEREPORT_FS,TOTFEDEXPEND) %>% summarize(amount=sum(AMOUNT),fINDINGS_COUNT=sum(Findings),mODIFIED_OPINION=sum(Modified_Opinion),oTHER_MATTERS=sum(Other_Matters),mATERIAL_WEAKNESS=sum(Material_Weakness),significant_Deficiency=sum(Significant_Deficiency),oTHER_FINDINGS=sum(Other_Findings),qUESTIONED_COSTS=sum(Questioned_Costs),Repeat_Findings=sum(Repeat_Findings),allowable_Activity=sum(Allowable_Activity),allowable_Costs=sum(Allowable_Costs),cash_Management=sum(Cash_Management),complianceD=sum(ComplianceD),eligibility=sum(Eligibility),equipment_Real_Property=sum(Equipment_Real_Property),matching_Level_of_Effort=sum(Matching_Level_of_Effort),period_of_Performance=sum(Period_of_Performance),procurement=sum(Procurement),program_Income=sum(Program_Income),complianceK=sum(ComplianceK),reporting=sum(Reporting),subrecipient=sum(Subrecipient),special_Tests_Provisions=sum(Special_Tests_Provisions),other=sum(Other),sig_def_overall=mean(Significant_Deficiency_Overall),material_weakness_overall=mean(Material_Weakness_Overall),mat_non_compliance_overall=mean(Material_NonCompliance_Overall),q_costs_overall=mean(Questioned_Costs_Overall), g_concern_overall=mean(Going_Concern_Overall))
#write to csv
currentDate <- Sys.Date()
csvFileName <- paste(month(currentDate),"_",day(currentDate),"_",year(currentDate),"_Agency_Data_Final_Institution_Yearly.csv",sep="")
write.csv(Agency_Data_Final_Institution_Yearly, file=csvFileName)

#Agency_Data_Final_Institution_Yearly<-read.csv(" ")
head(Agency_Data_Final_Institution_Yearly)
names(Agency_Data_Final_Institution_Yearly)

Model_Run_One<-Agency_Data_Final_Institution_Yearly %>% mutate(Improper=ifelse(is.na(qUESTIONED_COSTS),0,
                                                        ifelse(qUESTIONED_COSTS>0,1,
                                                               ifelse(is.na(q_costs_overall),0,
                                                                      ifelse(is.na(mat_non_compliance_overall),0,
                                                                             ifelse((q_costs_overall>0 & mat_non_compliance_overall>0),1,0
                                                                                           ))))))
head(Model_Run_One)
sum(Model_Run_One$Improper)
names(Model_Run_One)

logistical_model <- glm(Improper~ Repeat_Findings +program_Income+ TOTFEDEXPEND, data=Model_Run_One, family = "binomial")
summary(logistical_model)

p <- ggplot(Model_Run_One, aes(amount, TOTFEDEXPEND))
p + geom_point(aes(colour = Improper)) + scale_colour_gradient(low = "green", high="red")


