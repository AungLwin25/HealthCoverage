# Master script

# ... Diagnostic and age groups

# Load libraries
library(readxl)
library(tidyverse)

# Load Encounter data
df<- read_excel("Copy of UNICEF Data (12 July 2023).xlsx")

# Insert variable names
names(df) <- c("ID","eID","Date","Dx","OthDx")

# Clean date
df %>% mutate(Date= str_replace_all(Date,"/","-")) -> d1
d1$Date <- as.Date(d1$Date,format = "%m-%d-%Y")

# Summarize date @ Table 1
summary(d1$Date)

# Count beneficiaries @ Table 1
n_distinct(df$ID)->Ben

# Check data continuity $ data quality check
hist(d1$Date, breaks = "months", format = "%m %y", freq = TRUE, xlab = "Timeline")

# Diagnosis tables
table(d1$Dx) -> tbDx
write.csv(tbDx,"Dx.csv")

## missing diagnoses
sum(is.na(d1$Dx)) -> msDx
d1 %>% filter(is.na(Dx)) -> ms

# Dx is separated as another file
# Identification of AN care and nutritional deficiency diagnoses are coded by the analyst
Dx <- read_excel("Dx.xlsx")
Dx %>% group_by(Significance) %>%
  summarise(Case = sum(Freq)) -> DxGp
write.csv(DxGp,"DxGp.csv")
#c $ DxGp will serve for organizing dx groups in Excel -> code back to the primary data

#c Back to primary encounter data
# Strings to be detected
ANC <- c("pregnancy","Breech", "Multiparity","praevia",
         "previa", "oblique", "eclampsia","Maternal hypotension",
         "Fetal","eclampsia", "Polyhydra", "Fetal",
         "gravida","Hydatiform")
ANE <- c("anemia","Anaemia")
ANM <- c("TOF", "Down","Cleft","VSD","ASD")
ANP <- c("AEFI", "Anaphy")
BAC <- c("bacterial infection")
CBR <- c("childbirth")
CMD <- c("G6PD","thalas")
CVS <- c("hypertension","Hypotension","IHD","Heart",
         "blood pressure","Athero","heart")
DEV <- c("milestone","Development","development")
DTL <- c("Dental", "tooth", "denal")
GIT <- c("Gastr","Bloat","Constipa","Dysentry",
         "abdomen", "appendic", "Intuss", "GERD",
         "chronic diarrhoea", "Hirschsprung","Chole",
         "Peptic")
GYN <- c("Uterine", "vulva", "PID", "abortion","Breast",
         "Leukorrhoea","Puperal", "PID")
HEP <- c("Hepatic", "HCV")
IMB <- c("plegia")
IMN <- c("Immunization not")
IMD <- c("Measles")
INC <- c("Newborn", "newborn", "Labour", "labor",
         "PROM", "childbirth", "Preterm", "Antepartum",
         "Neonat","Abruption placenta", "Vomiting in newborn")
NEU <- c("palsy", "Myasthenia","nerve")
NRM <- c("No abnormal")
NUT <- c("Nutrit","B1","Iron", "Vitamin A",
         "Vitamin B", "Vitamin D", "B12", "B2")
PNC <- c("Neonatal skin", "Infant", "Retro exposed", "HBV exposed",
         "puerper")
PSY <- c("Anxiety", "Depress", "Mental", "mental")
RES <- c("Asthma","Tubercul","sinus", "rhini",
         "Haemoptysis","Nasal polyp", "COPD",
         "asthma","Pleural")
RTI <- c("respiratory infection","bronchiolitis", "Cold", "Cough",
         "Pneumonia", "Dengue","tonsilitis", "COVID",
         "tracheobronchitis")
SKN <- c("Scabies","Chickenpox", "Derma","Burns",
         "Skin","skin", "derma","Urticaria",
         "Impetigo", "Pruritus","Cellulitis","Lipoma",
         "Vitiligo","Herpes","Acne","Callos",
         "Nail","Psoriasis","burn")
VRS <- c("Viral infection")
WRM <- c("Helminth", "worm")

# Code diagnostic groups
d1 %>%
  mutate(DxGp = case_when(
    str_detect(Dx, paste("No abnormal", collapse = "|")) ~ "No abnormality",
    str_detect(Dx, paste(ANC, collapse = "|")) ~ "AN care",

    str_detect(Dx, paste(ANE, collapse = "|")) & str_detect(Dx, paste(ANC, collapse = "|"), negate = TRUE) ~ "Anemia",

    str_detect(Dx, paste(ANM, collapse = "|")) ~ "Congenital anomaly",
    str_detect(Dx, paste(ANP, collapse = "|")) ~ "Anaphylaxis",
    str_detect(Dx, paste(BAC, collapse = "|")) ~ "Bacterial infection",
    str_detect(Dx, paste(CMD, collapse = "|")) ~ "Congential metabolic deficiency",

    str_detect(Dx, paste(CVS, collapse = "|")) ~ "CVS",
    str_detect(Dx, paste(DEV, collapse = "|")) ~ "Developmental disorder",
    str_detect(Dx, paste(DTL, collapse = "|")) ~ "Dental care",
    str_detect(Dx, paste(GIT, collapse = "|")) ~ "GIT",

    str_detect(Dx, paste(GYN, collapse = "|")) & str_detect(Dx, paste(CBR, collapse = "|"), negate = TRUE) ~ "Gynecology care",

    str_detect(Dx, paste(IMB, collapse = "|")) ~ "Immobility",
    str_detect(Dx, paste(IMN, collapse = "|")) ~ "Immunization coverage",
    str_detect(Dx, paste(IMD, collapse = "|")) ~ "Immunization preventable disease",
    str_detect(Dx, paste(INC, collapse = "|")) ~ "IN care",

    str_detect(Dx, paste(NEU, collapse = "|")) ~ "Neurologic disorder",
    str_detect(Dx, paste(NRM, collapse = "|")) ~ "Normal",
    str_detect(Dx, paste(NUT, collapse = "|")) ~ "Nutritional care",

    str_detect(Dx, paste(PNC, collapse = "|")) & str_detect(Dx, paste(INC, collapse = "|"), negate = TRUE)  ~ "PN care",

    str_detect(Dx, paste(PSY, collapse = "|")) ~ "Psycological care",
    str_detect(Dx, paste(RES, collapse = "|")) ~ "Respiratory",
    str_detect(Dx, paste(RTI, collapse = "|")) ~ "Respiratory tract infection",
    str_detect(Dx, paste(SKN, collapse = "|")) ~ "Skin",

    str_detect(Dx, paste(VRS, collapse = "|")) ~ "Viral infection",
    str_detect(Dx, paste(WRM, collapse = "|")) ~ "Worm infestation",
    is.na(Dx) ~ "missing diagnoses",
    TRUE ~ "Other sickness and diseases")) -> d2

# Caveat: for case_when upper criteria dominate. Need to resolve by negating upper criterion/criteria as needed.

table(d2$DxGp) -> DxGp
write.csv(DxGp,"DxGp.csv")

write.csv(d2,"d2.csv")

# Load Member list data
Members <- read_excel("Members.xlsx")
names(Members)
Members[c(4,12,13)] -> mb1
names(mb1) <- c("ID","AgeMth", "AgeCat")

# merge with Encounter
merge(d2,mb1,by = "ID", all.x = TRUE) -> d3

# Explore age in months
quantile(d3$AgeMth, na.rm = TRUE)
boxplot(d3$AgeMth)
summary(d3$AgeMth)
hist(d3$AgeMth)

d3 %>% filter(AgeMth>144) -> temp
min(temp$AgeMth)
d3 %>% filter(AgeMth>156) -> temp
d3 %>% filter(AgeMth>156) -> temp

# Tabulation
table(d3$AgeCat)-> tb
write.csv(tb,"Agecat.csv")

table(d3$AgeCat,useNA = "always")

# Check missing age category
d3 %>% filter(is.na(AgeCat)) -> temp
head(temp)
sum(is.na(temp$AgeMth))

# Recode as women and children
names(d3)
d3 %>% mutate(Participant = case_when(
  str_detect(AgeCat,"Woman") ~ "woman",
  str_detect(AgeCat,"Child") ~ "child",
  TRUE ~ NA)) -> d4

# Count of women and children
table(d4$Participant,useNA = "always") -> tb

# Diagnostic theme by participant type
table(d4$DxGp,d4$Participant, useNA = "always") -> tb
write.csv(tb,"PersonDxGp.csv")

# Check inconsistencies:
## Child and AN care
d4 %>% filter(Participant == "child" & DxGp == "AN care") -> tp
write.csv(tp,"ChildAN.csv")

## Child and Gynecology care
d4 %>% filter(Participant == "child" & DxGp == "Gynecology care") -> tp
write.csv(tp,"ChildGY.csv")

## Child and puerperium
d4 %>% filter(Participant == "child" & Dx == "Normal puerperium") -> tp
write.csv(tp,"ChildGy.csv")

# Keep data
write.csv(d4,"d4.csv")

# ... All visits plot
library(readxl)
library(readr)
library(tidyverse)
library(fpp3)
library(lubridate)
library(reshape2)
library(ggthemes)

# prepare data
read_csv("d4.csv") -> df
names(df)
data <- df[-1]
names(data)
dim(data)

# time indexing
data$Year <- as.character(year(data$Date))
data$Month <- as.character(month(data$Date, label = TRUE))
data$YearMth <- paste(data$Year,data$Month)

# summarize
data %>% group_by(YearMth) %>% summarise(freq= n()) -> data2

# Time series indexing
data2 |>
  mutate(Month = yearmonth(YearMth)) |>
  as_tsibble(index = Month) %>% select(-1) -> data3

data3[c(2,1)] -> data4
write.csv(data4,"data4.csv")

# Basic plot
ggplot(prg5, aes(x=Month)) +
  geom_line(aes(y = Bacteria), color = "darkred") +
  geom_line(aes(y = GI), color="steelblue", linetype="twodash") +
  geom_line(aes(y = RTI), color="blue2", linetype="dashed") +
  geom_line(aes(y = Virus), color="chartreuse4", linetype="dotdash") +

  # Plot by data rearrangement
  Total <- ggplot(data4, aes(x = Month, y = freq)) +
  geom_line() +
  theme_economist_white() + ggtitle("All visits")

#,,, Common diseases' plot
library(readxl)
library(readr)
library(tidyverse)
library(fpp3)
library(lubridate)
library(reshape2)
library(ggthemes)

# prepare data
read_csv("d4.csv") -> df
names(df)
t1 <- df[-1]
names(t1)
dim(t1)

# time indexing
t1$Year <- as.character(year(t1$Date))
t1$Month <- as.character(month(t1$Date, label = TRUE))
t1$YearMth <- paste(t1$Year,t1$Month)

# RTI
names(t1)
t1 %>% filter(str_detect(DxGp,"Respiratory tract")) -> RTI
RTI %>% group_by(YearMth) %>% summarise(TotRTI = n()) -> RTIYrMth

# Various items
SelDx <- c("Respiratory tract", "GIT", "Viral", "Bacteria")
t1 %>% filter(str_detect(DxGp, paste(SelDx, collapse = "|" ))) -> sel
sel[c(12,6)] -> sel2
dcast(sel2,YearMth ~ DxGp) -> sel3
names(sel3) <- c("YearMth", "Bacteria", "GI","RTI","Virus")
write.csv(sel3,"sel3.csv")

# Time series indexing
sel3 |>
  mutate(Month = yearmonth(YearMth)) |>
  as_tsibble(index = Month) %>% select(-1) -> sel4

sel4[c(5,1:4)] -> sel5
write.csv(sel5,"sel5.csv")

# Basic plot
ggplot(sel5, aes(x=Month)) +
  geom_line(aes(y = Bacteria), color = "darkred") +
  geom_line(aes(y = GI), color="steelblue", linetype="twodash") +
  geom_line(aes(y = RTI), color="blue2", linetype="dashed") +
  geom_line(aes(y = Virus), color="chartreuse4", linetype="dotdash") +

  # Plot by data rearrangement
  sel6 <- sel5 %>%
  select(Month, Bacteria,Virus,RTI, GI) %>%
  gather(key = "variable", value = "value", -Month)

Diseases <- ggplot(sel6, aes(x = Month, y = value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  scale_color_manual(values = c("darkred", "darkorange","blue2","chartreuse4"))+
  theme_economist_white() + ggtitle("Bacteria, viruses, RTI, GI")

#... Maternal care plot
library(readxl)
library(readr)
library(tidyverse)
library(fpp3)
library(lubridate)
library(reshape2)
library(ggthemes)

# prepare data
read_csv("d4.csv") -> df
names(df)
p1 <- df[-1]
names(p1)
dim(p1)

# time indexing
p1$Year <- as.character(year(p1$Date))
p1$Month <- as.character(month(p1$Date, label = TRUE))
p1$YearMth <- paste(p1$Year,p1$Month)

# Various items
prgDx <- c("AN care", "IN care", "PN care")
p1 %>% filter(str_detect(DxGp, paste(prgDx, collapse = "|" ))) -> prg
prg[c(12,6)] -> prg2
dcast(prg2,YearMth ~ DxGp) -> prg3
names(prg3) <- c("Month","AN", "IN", "PN")
write.csv(prg3,"prg3.csv")

# Time series indexing
prg3 |>
  mutate(Month = yearmonth(Month)) |>
  as_tsibble(index = Month) %>% select(-1) -> prg4

prg4[c(4,1:3)] -> prg5
write.csv(prg5,"prg5.csv")

# Basic plot
ggplot(prg5, aes(x=Month)) +
  geom_line(aes(y = Bacteria), color = "darkred") +
  geom_line(aes(y = GI), color="steelblue", linetype="twodash") +
  geom_line(aes(y = RTI), color="blue2", linetype="dashed") +
  geom_line(aes(y = Virus), color="chartreuse4", linetype="dotdash") +

  # Plot by data rearrangement
  prg6 <- prg5 %>%
  select(Month, AN, IN, PN) %>%
  gather(key = "variable", value = "value", -Month)

dev.new()

Maternal <-ggplot(prg6, aes(x = Month, y = value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  scale_color_manual(values = c("red", "cyan","black")) +
  theme_economist_white() + ggtitle("Maternal care")

# ... No abnormality plot
library(readxl)
library(readr)
library(tidyverse)
library(fpp3)
library(lubridate)
library(reshape2)
library(ggthemes)

# prepare data
read_csv("d4.csv") -> df
names(df)
n1 <- df[-1]
names(n1)
dim(n1)

# time indexing
n1$Year <- as.character(year(n1$Date))
n1$Month <- as.character(month(n1$Date, label = TRUE))
n1$YearMth <- paste(n1$Year,n1$Month)

# Various items
nmDx <- c("No abnormal")
n1 %>% filter(str_detect(DxGp, paste(nmDx, collapse = "|" ))) -> nm
nm[c(12,6)] -> nm2
dcast(nm2,YearMth ~ DxGp) -> nm3
names(nm3) <- c("Month","freq")
write.csv(nm3,"nm3.csv")

# Time series indexing
nm3 |>
  mutate(Month = yearmonth(Month)) |>
  as_tsibble(index = Month) %>% select(-1) -> nm4

nm4[c(2:1)] -> nm5
write.csv(nm5,"nm5.csv")

# Basic plot
ggplot(nm5, aes(x=Month)) +
  geom_line(aes(y = Bacteria), color = "darkred") +
  geom_line(aes(y = GI), color="steelblue", linetype="twodash") +
  geom_line(aes(y = RTI), color="blue2", linetype="dashed") +
  geom_line(aes(y = Virus), color="chartreuse4", linetype="dotdash") +

  # Plot by data rearrangement
  Normal <- ggplot(nm5, aes(x = Month, y = freq)) +
  geom_line(color= "darkmagenta") +
  theme_economist_white() + ggtitle("Normal")

#... Combined grid
library(gridExtra)
dev.new()
grid.arrange(Total,Maternal,Diseases,Normal, ncol=2, nrow=2)




