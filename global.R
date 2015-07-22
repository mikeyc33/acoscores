library(plyr)
library(dplyr)
library(DT)

acoinfo <- read.csv("./data/Medicare_Shared_Savings_Program_Accountable_Care_Organizations_with_coords.csv", stringsAsFactors = F)

acochars <- read.csv("./data/Medicare_Shared_Savings_Program_Accountable_Care_Organizations_Performance_Year_1_Results (1).csv", stringsAsFactors = F)

acoinfo2 <- acoinfo %>%
            mutate(aco_name = toupper(ACO.Legal.or.Name.Doing.Business.As),
                   addr = ACO.Address,
                   zip = substr(ACO.Address, nchar(ACO.Address)-4, nchar(ACO.Address)),
                   state = ifelse(nchar(ACO.Service.Area)==2, ACO.Service.Area, substr(ACO.Service.Area,1,2))) %>%
            select(aco_name, addr, lon, lat, state, zip, ACO.Service.Area)

acochars2 <- acochars %>%
             mutate(aco_name = toupper(ACO.Name..LBN.or.DBA..if.applicable..),
                    benes = Total.Assigned.Beneficiaries,
                    benchmark_exp = Total.Benchmark.Expenditures,
                    exp = Total.Expenditures,
                    Generated.Savings.Losses1.2 = ifelse(is.na(Generated.Savings.Losses1.2), "NA", Generated.Savings.Losses1.2),
                    bench_minus_assign_bene_exp = Total.Benchmark.Expenditures.Minus.Total.Assigned.Beneficiary.Expenditures) %>%
             select(aco_name, benes, benchmark_exp, exp, bench_minus_assign_bene_exp, ACO.1, ACO.2, ACO.3, ACO.4, ACO.5, ACO.6, ACO.7,
                    ACO.8., ACO.9., ACO.10., ACO.11, ACO.12, ACO.13, ACO.14, ACO.15, ACO.16, ACO.17, ACO.18, ACO.19, ACO.20, ACO.21, ACO.22,
                    ACO.23, ACO.24, ACO.25, ACO.26, ACO.27., ACO.28, ACO.29, ACO.30, ACO.31, Generated.Savings.Losses1.2,
                    Agreement.Start.Date)

acochars2 <- plyr::rename(acochars2, c("Generated.Savings.Losses1.2"="savings_losses"))
                    
df1 <- merge(acoinfo2, acochars2, by.x="aco_name", by.y="aco_name")

address_split <- strsplit(df1$addr,",")

city <- sapply(address_split, function(x) {
  if (length(x) < 4){
    city <- x[2]
  } else{
    city <- x[3]
  }
  return(city)
})

df2 <- cbind(df1, city)

df2$city <- as.character(df2$city)

num_vars <- c("benes","benchmark_exp", "exp", "bench_minus_assign_bene_exp")

#convert expenditure data to numeric format
df2[,num_vars] <- sapply(df2[,num_vars], function(x) as.numeric(gsub("[[:punct:]]",'',x)))

#calculate total 0-14 CAHPS quality points based on benchmarks

flat_bench <- function(x){
  score <- ifelse(x < 30, 0, ifelse(x>30 & x<=40, 1.1, ifelse(x>40 & x<=50, 1.25,
           ifelse(x>50 & x<=60, 1.4, ifelse(x>60 & x<=70, 1.55, ifelse(x>70 & x<=80, 1.70,
           ifelse(x>80 & x<=90, 1.85, 2)))))))
}

df3 <- df2 %>%
       mutate(c_access = flat_bench(df2$ACO.1),
              c_comm = flat_bench(df2$ACO.2),
              rate_md = flat_bench(df2$ACO.3),
              c_spec = flat_bench(df2$ACO.4),
              m_hlth_promo = ifelse(ACO.5<54.71, 0, ifelse(ACO.5>54.71 & ACO.5<=55.59, 1.1, ifelse(ACO.5>55.59 & ACO.5<=56.45, 1.25,
                             ifelse(ACO.5>56.45 & ACO.5<=57.63, 1.4, ifelse(ACO.5>57.63 & ACO.5<=58.22, 1.55, ifelse(ACO.5>58.22 & ACO.5<=59.09, 1.70,
                             ifelse(ACO.5>59.09 & ACO.5<=60.71, 1.85, 2))))))),
              m_sdm = ifelse(ACO.6<72.87, 0, ifelse(ACO.6>72.87 & ACO.6<=73.37, 1.1, ifelse(ACO.6>73.37 & ACO.6<=73.91, 1.25,
                      ifelse(ACO.6>73.91 & ACO.6<=74.51, 1.4, ifelse(ACO.6>74.51 & ACO.6<=75.25, 1.55, ifelse(ACO.6>75.25 & ACO.6<=75.82, 1.70,
                      ifelse(ACO.6>75.82 & ACO.6<=76.71, 1.85, 2))))))),
              CAHPS_score = c_access+c_comm+rate_md+c_spec+m_hlth_promo+m_sdm+2,
              rank = rank(-CAHPS_score, ties.method="max")) %>%
              #Remove 2 ACOs that have duplicate values for demo purposes
              filter(!aco_name %in% c("BAROMA HEALTH PARTNERS","MERCY ACO, LLC"))

allacos <- df3
allacos$aco <- allacos$aco_name
allacos$latitude <- jitter(allacos$lat)
allacos$longitude <- jitter(allacos$lon)
allacos$zipcode <- allacos$zip
row.names(allacos) <- allacos$aco

allacos <- subset(allacos, select=-c(lat,lon,aco_name,zip,addr,c_access,
                                     c_comm,rate_md,c_spec,m_hlth_promo,m_sdm))
#Legend titles for output
legend <- data.frame(var=names(allacos), legend_name=c("State","ACO Service Area", "No. of Assigned Benes",
                                                       "Total Benchmark Expenditures($)","Total Expenditures ($)",
                                                       "Tot. Benchmark - Total Assigned Bene. Exp ($)",
                                                       "Getting Timely Care (0-100)","Provider Communication (0-100)","Rating of Doctor (0-100)",
                                                       "Access to Specialists (0-100)","Health Promotion and Education (0-100)","Shared-Decision Making (0-100)",
                                                       "Health and Functional Status (0-100)","All Condition Readmissions","ASC Admission:COPD or Asthma",
                                                       "ASC Admission: Heart Failure","% of PCPs Qualified for EHR Incentive","Medication Reconciliation","Falls: Screening for Fall Risk",
                                                       "Influenza Immunization","Pneumococcal Vaccination","Adult Weight Screening","Tobacco Use/Cessation Intervention",
                                                       "Depression Screening","Colorectal Cancer Screening","Mammography Screening","Blood Pressure Screening within 2 years",
                                                       "Diabetes HbA1c Control","Diabetes LDL Control","Diabetes BP Control","Diabetes Tobacco Non-use","Diabetes Aspirin Use",
                                                       "% of Diab. Benes with poor HbA1c Control","% of Benes with BP < 140/90","% of Benes with IVD Lipid Profile and LDL Control",
                                                       "% of Benes with IVD who use Aspirin","Beta-Blocker Therapy for LVSD","Generated Savings/Losses ($)",
                                                       "ACO Start Date","City","Patient Experience (0-14)",
                                                       "Rank","ACO Name","Lat","Lng","Zip Code"))
