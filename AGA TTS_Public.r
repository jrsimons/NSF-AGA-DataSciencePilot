#This code starts with a Single Audit Summary Report from the Federal Audit Clearinghouse website. 
#Please visit this website https://facdissem.census.gov/SearchA133.aspx to search for relevant audits then download the single audit summary document(s). 
#For this model we will want to pull all institutions with relevant CFDA prefixes. We can search by this prefix under "federal awards" in the search options. 
#Once we've selected the right prefix(es) and the relevant fiscal years we can search for single audit files. 

#On the search results tab there is an option to download the summary document that we will use. 
#The summary document provides a machine readable format of the single audit results, available from FY13 onward. 
#Once this document is downloaded, move it to the working directory for the code. 
#The FAC limits downloads to 10,000 institutions at one time, this means that data may need to be split across fiscal years. 
#In the data intake lines below there are lines that are currently inactive to navigate including multiple sheets.

#Lastly, this is an example and is not meant to be a definitive model ready for use. 
#The process has been simplified for demonstration purposes. 
#There are many assumptions underlying specific parts of the regression model that should be considered before using a tool like this for institutional use. 
#Please review the ReadMe available in the repository for more information.

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

#Relevant Agency CFDA numbers, add prefixes as necessary to the list, an example is included below. 
#CFDA_Entry<-c('47.','36.','10.')
#Prefixes here will need to correspond to the CFDA numbers selected in the original search from the Single Audit Database.

CFDA_Entry<-c('47.')


#Bring in all data from the Single Audit Database, if there are multiple entries for CFDA Information (for a large number of fiscal years or larger agencies this can occur), join using the additional excel read in with the optional bind_rows function before continuing.

#summary_reports<- readxl_example("Summary_Reports.xlsx")
CFDA_Data<-read_excel("Summary_Reports.xlsx", sheet = "CFDA INFO")
#CFDA_Data1<-("Summary_Reports (1).xlsx", sheet = "CFDA INFO")
#CFDA_Data<-bind_rows(CFDA_Data,CFDA_Data1)
Finding_Data<-read_excel("Summary_Reports.xlsx", sheet = "FINDINGS")
General_Info <- read_excel("Summary_Reports.xlsx", sheet = "GENERAL INFO")
head(CFDA_Data)
head(Finding_Data)
head(General_Info)

#This section takes the finding data and converts the dimensions into indicator variables so that they can be summed and so that multiple dimensions can be reviewed.

FindingData_working<- Finding_Data %>% mutate(Compliance_Allowable_Activity= ifelse(grepl("A",TYPEREQUIREMENT),1,0))
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

#This section creates a table with all awards limited by CFDA number. The overall table is still useful for other compliance dimensions and findings outside of the institution awards, but for some analyses we will want these separated.

Agency_only<- CFDA_Data %>% filter(grepl(CFDA_Entry,CFDA))
colnames(Agency_only)
head(Agency_only)

#This section merges the findings tab with the CFDA information for agency specific awards. Note there are duplicate columns based on the design of the single audit dataset, these will show up as .x or .y when there are duplicates.

CFDA_Finding_Data<-left_join(Agency_only,FindingData_working, by = "ELECAUDITSID")
colnames(CFDA_Finding_Data)
head(CFDA_Finding_Data)

#To aggregate this data further, we need to turn modified opinion and other findings into numeric values so that they can be summed. There is the option of using one Y to override all N, but this was done numerically since it gives more insight into the magnitude of the findings. In all cases, these can be treated as indicator variables as well, with any number greater than 0 representing a Y. A lack of findings in this instance is represented by a null value. We can then replace these with 0 to help with our numeric interpretation.

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

#Combined merge from the general information level to include individual award and finding data. Rename is to make sure the result is able to be deciphered

Combined_Data_Final<-left_join(CFDA_Finding_Data1,General_Info, by=c("AUDITYEAR"="AUDITYEAR","DBKEY.x"="DBKEY"))
Combined_Data_Final<-rename(Combined_Data_Final,DBKEY=DBKEY.x)
Combined_Data_Final<-rename(Combined_Data_Final,SIGNIFICANTDEFICIENCY="REPORTABLECONDITION/SIGNIFICANTDEFICIENCY")
head(Combined_Data_Final)
names(Combined_Data_Final)

#To review the data per institution, we will need to aggregate to an institution level. We'll use sum functions to maintain the degree of the magnitude of findings and to sum other relevant info.

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


#Model Data Set - can begin here

#Here we would join in agency specific data to inform the model. Here we will run based on the data simply provided in the Single Audit Database. These are just examples and a starting point, these are not useable in their current forms. There are implications and assumptions built into the construction of the regression models that should be considered in their entirety before using them for practical applications.


#Agency_Data_Final_Institution_Yearly<-read.csv(" ")
head(Agency_Data_Final_Institution_Yearly)
names(Agency_Data_Final_Institution_Yearly)

#One discussion in the literature is that a combination of questioned costs, and material non-compliance can be an indicator of improper payments. This dependent variable assumes that construction. In a given year, these dimensions report an improper payment when they are present.

Model_Run_One<-Agency_Data_Final_Institution_Yearly %>% mutate(Improper=ifelse(is.na(qUESTIONED_COSTS),0,
                                                        ifelse(qUESTIONED_COSTS>0,1,
                                                               ifelse(is.na(q_costs_overall),0,
                                                                      ifelse(is.na(mat_non_compliance_overall),0,
                                                                             ifelse((q_costs_overall>0 & mat_non_compliance_overall>0),1,0
                                                                                           ))))))
head(Model_Run_One)
sum(Model_Run_One$Improper)
names(Model_Run_One)

#This specification uses the following independent variables in a logistic construction. 
#This current construction treats each observation as independent. We should control for groups of institutions. A panel regression can take this into account. We can also use differences as another option to instead look at the change in improper over years. The key benefit is when there is more institutional information available that can shed background on what changed year to year to provide insight into where the dependent variable will land based on the other variables.

logistical_model <- glm(Improper~ Repeat_Findings +program_Income+ TOTFEDEXPEND, data=Model_Run_One, family = "binomial")
summary(logistical_model)

#We can also plot variables to see relationships.

p <- ggplot(Model_Run_One, aes(amount, TOTFEDEXPEND))
p + geom_point(aes(colour = Improper)) + scale_colour_gradient(low = "green", high="red")


