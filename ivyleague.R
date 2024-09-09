# Loading packages
library(tidyverse)
library(ggthemes)
library(viridis)

# Importing data
ivy <-read.csv("new_ebulako.csv", header = TRUE)

# Data preview
dim(ivy); str(ivy)
head(ivy, 7); tail(ivy, 7)

# Duplicates
ivy<-ivy[!duplicated(ivy), ]; dim(ivy) # No duplicates present

# Missing values 
ivy[!complete.cases(ivy), ] # missing value identified 
                        # at row 178, SSRE column (column 5)

# Handling missing value at (178,5)
ceiling(mean(ivy$SSRE, na.rm=TRUE))->ivy[178,5]

# Data type conversion
# 'stream' character to factor
table(ivy$stream)
ivy$stream<-factor(ivy$stream, levels =c("P", "A", "I"),
                               labels =c("P", "A", "I")     
                  ); str(ivy$stream)

                                 
                                ### ANALYSIS PER CLASS
                                
# ATLANTA 
atl<-ivy[ivy$stream == "A", ]
str(atl)

# Descriptive analysis
# Customised descriptive functions 
COV<-function(vec){
  sd(vec)*100/mean(vec)
}

RANGE<-function(vec){
  max(vec)-min(vec)
}

uq<-function(vec){
  quantile(vec, probs=c(0.75))
}

lq<-function(vec){
  quantile(vec, probs=c(0.25))
}

# 1. English
# A. Descriptive summaries
desc.atl.eng <- data.frame(minimum = c(min(atl$english)),
              lower.q = c(lq(atl$english)),
              Median = c(median(atl$english)),
             upper.q = c(uq(atl$english)),
            maximum = c(max(atl$english)),
            average = c(mean(atl$english)),
            standard.dev = c(sd(atl$english)),
                   c.o.v=c(COV(atl$english))
    )

# B. Distribution
boxplot(atl$english, col='royalblue4', pch=16,horizontal = T,
     col.main="midnightblue",border="royalblue1", las=1
     ,main="ATLANTA: ENGLISH", xlab="English score")


custom.breaks<-c(0,10,20,30,40,50,60,70,80,90,100)
custom.labels<-c("0-10","11-20","21-30","31-40","41-50",
                 "51-60","61-70","71-80","81-90","91-100")

cat.atl.eng<-cut(atl$english,
                 breaks=custom.breaks, 
                 labels=custom.labels)

barplot(table(cat.atl.eng)*100/nrow(atl), tcl=65, las=1,border="cyan", ylim=c(0,30))

# Density plot
ggplot(atl, aes(x=english))+
  geom_density(fill = "dodgerblue4", col= "dodgerblue4", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Atlanta",
       subtitle="English")+
  geom_vline(xintercept = mean(atl$english))+
  theme_gdocs()

# 2. Kiswahili
# A. Descriptive summaries
desc.atl.kis <- data.frame(minimum = c(min(atl$kiswahili)),
                           lower.q = c(lq(atl$kiswahili)),
                           Median = c(median(atl$kiswahili)),
                           upper.q = c(uq(atl$kiswahili)),
                           maximum = c(max(atl$kiswahili)),
                           average = c(mean(atl$kiswahili)),
                           standard.dev = c(sd(atl$kiswahili)),
                           c.o.v=c(COV(atl$kiswahili))
); print(desc.atl.kis)

# B. Distribution
boxplot(atl$kiswahili, col='royalblue4', pch=16,
col.main="midnightblue",border="royalblue1", las=1, horizontal = T,
main="ATLANTA: KISWAHILI", xlab="Kiswahili score")


cat.atl.kis<-cut(atl$kiswahili,
                 breaks=custom.breaks, 
                 labels=custom.labels);print(cat.atl.kis)


barplot(table(cat.atl.kis)*100/nrow(atl), tcl=65, las=1,border="cyan", ylim=c(0,40))

# Density plot
ggplot(atl, aes(x=kiswahili))+
  geom_density(fill = "dodgerblue4", col= "dodgerblue4", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Atlanta",
       subtitle="Kiswahili")+
  geom_vline(xintercept = mean(atl$kiswahili))+
  theme_gdocs()

# 3. Mathematics
# A. Descriptive summaries
desc.atl.math <- data.frame(minimum = c(min(atl$math)),
                           lower.q = c(lq(atl$math)),
                           Median = c(median(atl$math)),
                           upper.q = c(uq(atl$math)),
                           maximum = c(max(atl$math)),
                           average = c(mean(atl$math)),
                           standard.dev = c(sd(atl$math)),
                           c.o.v=c(COV(atl$math))
); print(desc.atl.math)

# B. Distribution
boxplot(atl$math, col='royalblue4', pch=16,
     col.main="midnightblue",border="royalblue1", las=1,horizontal=T
     ,main="ATLANTA: MATHEMATICS", xlab="Mathematics score")

cat.atl.math<-cut(atl$math,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.atl.math)

barplot(table(cat.atl.math)*100/nrow(atl), tcl=65, las=1,border="cyan", ylim=c(0,30))

# Density plot
ggplot(atl, aes(x=math))+
  geom_density(fill = "dodgerblue4", col= "dodgerblue4", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Atlanta",
       subtitle="Mathematics")+
  theme_gdocs()

# 4. Science
# A. Descriptive summaries
desc.atl.scie <- data.frame(minimum = c(min(atl$science)),
                            lower.q = c(lq(atl$science)),
                            Median = c(median(atl$science)),
                            upper.q = c(uq(atl$science)),
                            maximum = c(max(atl$science)),
                            average = c(mean(atl$science)),
                            standard.dev = c(sd(atl$science)),
                            c.o.v=c(COV(atl$scie))
); print(desc.atl.scie)

# B. Distribution
boxplot(atl$science, col='royalblue4', pch=16,
     col.main="midnightblue",border="royalblue1", las=1,horizontal = T
     ,main="ATLANTA: SCIENCE", xlab="Science score")

cat.atl.scie<-cut(atl$science,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.atl.scie)

barplot(table(cat.atl.scie)*100/nrow(atl), tcl=65, las=1,border="cyan", ylim=c(0,30))

# Density plot
ggplot(atl, aes(x=science))+
  geom_density(fill = "dodgerblue4", col= "dodgerblue4", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Atlanta:",
       subtitle="Science")+
  theme_gdocs()

# 5. SSRE
# A. Descriptive summaries
desc.atl.ssre <- data.frame(minimum = c(min(atl$SSRE)),
                            lower.q = c(lq(atl$SSRE)),
                            Median = c(median(atl$SSRE)),
                            upper.q = c(uq(atl$SSRE)),
                            maximum = c(max(atl$SSRE)),
                            average = c(mean(atl$SSRE)),
                            standard.dev = c(sd(atl$SSRE)),
                            c.o.v=c(COV(atl$SSRE))
); print(desc.atl.ssre)

# B. Distribution
boxplot(atl$SSRE, col='royalblue4', pch=16,
     col.main="midnightblue",border="royalblue1", las=1,horizontal=T
     ,main="ATLANTA: SOCIAL STUDIES & RELIGIOUS EDUCATION", xlab="SSRE score")

cat.atl.ssre<-cut(atl$SSRE,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.atl.ssre)

barplot(table(cat.atl.ssre)*100/nrow(atl), tcl=65, las=1,border="cyan", ylim=c(0,60))

# Density plot
ggplot(atl, aes(x=SSRE))+
  geom_density(fill = "dodgerblue4", col= "dodgerblue4", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Atlanta:",
       subtitle="SSRE")+
  theme_gdocs()


# 6. Total
# Adding the totals column
atl$total<-atl$english+atl$kiswahili+atl$math+atl$science+atl$SSRE
atl$total

# A. Descriptive summaries
desc.atl.total <- data.frame(minimum = c(min(atl$total)),
                            lower.q = c(lq(atl$total)),
                            Median = c(median(atl$total)),
                            upper.q = c(uq(atl$total)),
                            maximum = c(max(atl$total)),
                            average = c(mean(atl$total)),
                            standard.dev = c(sd(atl$total)),
                            c.o.v=c(COV(atl$total))
); print(desc.atl.total)

# B. Distribution
boxplot(atl$total, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="ATLANTA: TOTAL MARKS", xlab="Total exam score")

cat.atl.total<-cut(atl$total,
                  breaks=c(0,100,200,300,400,500), 
                  labels=c("0-100","101-200","201-300","301-400","401-500"));print(cat.atl.total)

barplot(table(cat.atl.total)*100/nrow(atl),
        tcl=65, las=1,border="cyan",
        ylim=c(0,60), col="royalblue4")

# Density plot
ggplot(atl, aes(x=total))+
  geom_density(fill = "dodgerblue4", col= "dodgerblue4", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Atlanta:",
       subtitle="Total")+
  theme_gdocs()

# INDIANA 
ind<-ivy[ivy$stream == "I", ]
str(ind)
ind



# 1. English
# A. Descriptive summaries
desc.ind.eng <- data.frame(minimum = c(min(ind$english)),
                           lower.q = c(lq(ind$english)),
                           Median = c(median(ind$english)),
                           upper.q = c(uq(ind$english)),
                           maximum = c(max(ind$english)),
                           average = c(mean(ind$english)),
                           standard.dev = c(sd(ind$english)),
                           c.o.v=c(COV(ind$english))
); print(desc.ind.eng)

# B. Distribution
boxplot(ind$english, col='royalblue4', pch=16,horizontal = T,
        col.main="midnightblue",border="royalblue1", las=1
        ,main="INDIANNA: ENGLISH", xlab="English score")

cat.ind.eng<-cut(ind$english,
                 breaks=custom.breaks, 
                 labels=custom.labels);print(cat.ind.eng)

barplot(table(cat.ind.eng)*100/nrow(ind), tcl=65, las=1,border="cyan", ylim=c(0,40))

# Density plot
ggplot(ind, aes(x=english))+
  geom_density(fill = "mediumseagreen", col= "mediumseagreen", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Indianna:",
       subtitle="English")+
  theme_gdocs()

# 2. Kiswahili
# A. Descriptive summaries
desc.ind.kis <- data.frame(minimum = c(min(ind$kiswahili)),
                           lower.q = c(lq(ind$kiswahili)),
                           Median = c(median(ind$kiswahili)),
                           upper.q = c(uq(ind$kiswahili)),
                           maximum = c(max(ind$kiswahili)),
                           average = c(mean(ind$kiswahili)),
                           standard.dev = c(sd(ind$kiswahili)),
                           c.o.v=c(COV(ind$kiswahili))
); print(desc.ind.kis)

# B. Distribution
boxplot(ind$kiswahili, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1, horizontal = T,
        main="INDIANNA: KISWAHILI", xlab="Kiswahili score")


cat.ind.kis<-cut(ind$kiswahili,
                 breaks=custom.breaks, 
                 labels=custom.labels);print(cat.ind.kis)


barplot(table(cat.ind.kis)*100/nrow(ind), tcl=65, las=1,
        border="cyan", ylim = c(0,40))

# Density plot
ggplot(ind, aes(x=kiswahili))+
  geom_density(fill = "mediumseagreen", col= "mediumseagreen", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Indianna:",
       subtitle="Kiswahili")+
  theme_gdocs()

# 3. Mathematics
# A. Descriptive summaries
desc.ind.math <- data.frame(minimum = c(min(ind$math)),
                            lower.q = c(lq(ind$math)),
                            Median = c(median(ind$math)),
                            upper.q = c(uq(ind$math)),
                            maximum = c(max(ind$math)),
                            average = c(mean(ind$math)),
                            standard.dev = c(sd(ind$math)),
                            c.o.v=c(COV(ind$math))
); print(desc.ind.math)

# B. Distribution
boxplot(ind$math, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="INDIANNA: MATHEMATICS", xlab="Mathematics score")

cat.ind.math<-cut(ind$math,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.ind.math)

barplot(table(cat.ind.math)*100/nrow(ind), tcl=65, las=1,
        border="cyan", ylim = c(0,30))

# Density plot
ggplot(ind, aes(x=math))+
  geom_density(fill = "mediumseagreen", col= "mediumseagreen", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Indianna:",
       subtitle="Mathematics")+
  theme_gdocs()

# 4. Science
# A. Descriptive summaries
desc.ind.scie <- data.frame(minimum = c(min(ind$science)),
                            lower.q = c(lq(ind$science)),
                            Median = c(median(ind$science)),
                            upper.q = c(uq(ind$science)),
                            maximum = c(max(ind$science)),
                            average = c(mean(ind$science)),
                            standard.dev = c(sd(ind$science)),
                            c.o.v=c(COV(ind$scie))
); print(desc.ind.scie)

# B. Distribution
boxplot(ind$science, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal = T
        ,main="INDIANNA: SCIENCE", xlab="Science score")

cat.ind.scie<-cut(ind$science,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.ind.scie)

barplot(table(cat.ind.scie)*100/nrow(ind), tcl=65
        , las=1,border="cyan", ylim = c(0,30))

# Density plot
ggplot(ind, aes(x=science))+
  geom_density(fill = "mediumseagreen", col= "mediumseagreen", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Indianna:",
       subtitle="Science")+
  theme_gdocs()

# 5. SSRE
# A. Descriptive summaries
desc.ind.ssre <- data.frame(minimum = c(min(ind$SSRE)),
                            lower.q = c(lq(ind$SSRE)),
                            Median = c(median(ind$SSRE)),
                            upper.q = c(uq(ind$SSRE)),
                            maximum = c(max(ind$SSRE)),
                            average = c(mean(ind$SSRE)),
                            standard.dev = c(sd(ind$SSRE)),
                            c.o.v=c(COV(ind$SSRE))
); print(desc.ind.ssre)

# B. Distribution
boxplot(ind$SSRE, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="INDIANNA: SOCIAL STUDIES & RELIGIOUS EDUCATION", xlab="SSRE score")

cat.ind.ssre<-cut(ind$SSRE,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.ind.ssre)

barplot(table(cat.ind.ssre)*100/nrow(ind), tcl=65, 
        las=1,border="cyan", ylim=c(0,35))


# Density plot
ggplot(ind, aes(x=SSRE))+
  geom_density(fill = "mediumseagreen", col= "mediumseagreen", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Indianna:",
       subtitle="SSRE")+
  theme_gdocs()

# 6. Total
# Adding the totals column
ind$total<-ind$english+ind$kiswahili+ind$math+ind$science+ind$SSRE
ind$total

# A. Descriptive summaries
desc.ind.total <- data.frame(minimum = c(min(ind$total)),
                             lower.q = c(lq(ind$total)),
                             Median = c(median(ind$total)),
                             upper.q = c(uq(ind$total)),
                             maximum = c(max(ind$total)),
                             average = c(mean(ind$total)),
                             standard.dev = c(sd(ind$total)),
                             c.o.v=c(COV(ind$total))
); print(desc.ind.total)

# B. Distribution
boxplot(ind$total, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="INDIANNA: TOTAL MARKS", xlab="Total exam score")

cat.ind.total<-cut(ind$total,
                   breaks=c(0,100,200,300,400,500), 
                   labels=c("0-100","101-200","201-300","301-400","401-500"))
;print(cat.ind.total)

barplot(table(cat.ind.total)*100/nrow(ind),
        tcl=65, las=1,border="cyan",
        ylim=c(0,50), col="royalblue4")


# Density plot
ggplot(ind, aes(x=total))+
  geom_density(fill = "mediumseagreen", col= "mediumseagreen", alpha = 0.5)+
  labs(x="score",
       y="probability",
       title="Indianna:",
       subtitle="Total")+
  theme_gdocs()

# PACIFICA 
pac<-ivy[ivy$stream == "P", ]
str(pac)
pac



# 1. English
# A. Descriptive summaries
desc.pac.eng <- data.frame(minimum = c(min(pac$english)),
                           lower.q = c(lq(pac$english)),
                           Median = c(median(pac$english)),
                           upper.q = c(uq(pac$english)),
                           maximum = c(max(pac$english)),
                           average = c(mean(pac$english)),
                           standard.dev = c(sd(pac$english)),
                           c.o.v=c(COV(pac$english))
); print(desc.pac.eng)

# B. Distribution
boxplot(pac$english, col='royalblue4', pch=16,horizontal = T,
        col.main="midnightblue",border="royalblue1", las=1
        ,main="PACIFICA: ENGLISH", xlab="English score")

cat.pac.eng<-cut(pac$english,
                 breaks=custom.breaks, 
                 labels=custom.labels);print(cat.pac.eng)

barplot(table(cat.pac.eng)*100/nrow(pac), tcl=65,
        las=1,border="cyan", ylim = c(0,40))

# Density plot
ggplot(pac, aes(x=english))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Pacifica:",
       subtitle="English")+
  theme_gdocs()

# 2. Kiswahili
# A. Descriptive summaries
desc.pac.kis <- data.frame(minimum = c(min(pac$kiswahili)),
                           lower.q = c(lq(pac$kiswahili)),
                           Median = c(median(pac$kiswahili)),
                           upper.q = c(uq(pac$kiswahili)),
                           maximum = c(max(pac$kiswahili)),
                           average = c(mean(pac$kiswahili)),
                           standard.dev = c(sd(pac$kiswahili)),
                           c.o.v=c(COV(pac$kiswahili))
); print(desc.pac.kis)

# B. Distribution
boxplot(pac$kiswahili, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1, horizontal = T,
        main="PACIFICA: KISWAHILI", xlab="Kiswahili score")


cat.pac.kis<-cut(pac$kiswahili,
                 breaks=custom.breaks, 
                 labels=custom.labels);print(cat.pac.kis)


barplot(table(cat.pac.kis)*100/nrow(pac), tcl=65, 
        las=1, border="cyan", ylim = c(0,50))

# Density plot
ggplot(pac, aes(x=kiswahili))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Pacifica:",
       subtitle="Kiswahili")+
  theme_gdocs()

# 3. Mathematics
# A. Descriptive summaries
desc.pac.math <- data.frame(minimum = c(min(pac$math)),
                            lower.q = c(lq(pac$math)),
                            Median = c(median(pac$math)),
                            upper.q = c(uq(pac$math)),
                            maximum = c(max(pac$math)),
                            average = c(mean(pac$math)),
                            standard.dev = c(sd(pac$math)),
                            c.o.v=c(COV(pac$math))
); print(desc.pac.math)

# B. Distribution
boxplot(pac$math, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="PACIFICA: MATHEMATICS", xlab="Mathematics score")

cat.pac.math<-cut(pac$math,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.pac.math)

barplot(table(cat.pac.math)*100/nrow(pac), tcl=65,
        las=1,border="cyan", ylim = c(0,30))

ggplot(pac, aes(x=math))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Pacifica:",
       subtitle="Mathematics")+
  theme_gdocs()

# 4. Science
# A. Descriptive summaries
desc.pac.scie <- data.frame(minimum = c(min(pac$science)),
                            lower.q = c(lq(pac$science)),
                            Median = c(median(pac$science)),
                            upper.q = c(uq(pac$science)),
                            maximum = c(max(pac$science)),
                            average = c(mean(pac$science)),
                            standard.dev = c(sd(pac$science)),
                            c.o.v=c(COV(pac$scie))
); print(desc.pac.scie)

# B. Distribution
boxplot(pac$science, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal = T
        ,main="PACIFICA: SCIENCE", xlab="Science score")

cat.pac.scie<-cut(pac$science,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.pac.scie)

barplot(table(cat.pac.scie)*100/nrow(pac), tcl=65
        , las=1,border="cyan", ylim = c(0,30))

ggplot(pac, aes(x=science))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Pacifica:",
       subtitle="Science")+
  theme_gdocs()

# 5. SSRE
# A. Descriptive summaries
desc.pac.ssre <- data.frame(minimum = c(min(pac$SSRE)),
                            lower.q = c(lq(pac$SSRE)),
                            Median = c(median(pac$SSRE)),
                            upper.q = c(uq(pac$SSRE)),
                            maximum = c(max(pac$SSRE)),
                            average = c(mean(pac$SSRE)),
                            standard.dev = c(sd(pac$SSRE)),
                            c.o.v=c(COV(pac$SSRE))
); print(desc.pac.ssre)

# B. Distribution
boxplot(pac$SSRE, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="PACIFICA: SOCIAL STUDIES & RELIGIOUS EDUCATION", xlab="SSRE score")

cat.pac.ssre<-cut(pac$SSRE,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.pac.ssre)

barplot(table(cat.pac.ssre)*100/nrow(pac), tcl=65, 
        las=1,border="cyan", ylim=c(0,35))

ggplot(pac, aes(x=SSRE))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Pacifica:",
       subtitle="SSRE")+
  theme_gdocs()


# 6. Total
# Adding the totals column
pac$total<-pac$english+pac$kiswahili+pac$math+pac$science+pac$SSRE
pac$total

# A. Descriptive summaries
desc.pac.total <- data.frame(minimum = c(min(pac$total)),
                             lower.q = c(lq(pac$total)),
                             Median = c(median(pac$total)),
                             upper.q = c(uq(pac$total)),
                             maximum = c(max(pac$total)),
                             average = c(mean(pac$total)),
                             standard.dev = c(sd(pac$total)),
                             c.o.v=c(COV(pac$total))
); print(desc.pac.total)

# B. Distribution
boxplot(pac$total, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="PACIFICA: TOTAL MARKS", xlab="Total exam score")

cat.pac.total<-cut(pac$total,
                   breaks=c(0,100,200,300,400,500), 
                   labels=c("0-100","101-200","201-300","301-400","401-500"))
;print(cat.pac.total)

barplot(table(cat.pac.total)*100/nrow(pac),
        tcl=65, las=1,border="cyan",
        ylim=c(0,50), col="royalblue4")

ggplot(pac, aes(x=total))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Pacifica:",
       subtitle="Total")+
  theme_gdocs()

# COMBINED                           
# 1. English
# A. Descriptive summaries
desc.ivy.eng <- data.frame(minimum = c(min(ivy$english)),
                           lower.q = c(lq(ivy$english)),
                           Median = c(median(ivy$english)),
                           upper.q = c(uq(ivy$english)),
                           maximum = c(max(ivy$english)),
                           average = c(mean(ivy$english)),
                           standard.dev = c(sd(ivy$english)),
                           c.o.v=c(COV(ivy$english))
); print(desc.ivy.eng)

# B. Distribution
boxplot(ivy$english, col='royalblue4', pch=16,horizontal = T,
        col.main="midnightblue",border="royalblue1", las=1
        ,main="COMBINED: ENGLISH", xlab="English score")

cat.ivy.eng<-cut(ivy$english,
                 breaks=custom.breaks, 
                 labels=custom.labels);print(cat.ivy.eng)

barplot(table(cat.ivy.eng)*100/nrow(ivy), tcl=65,
        las=1,border="cyan", ylim = c(0,30))

# Density
ggplot(ivy, aes(x=english))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Combined:",
       subtitle="English")+
  theme_gdocs()

# 2. Kiswahili
# A. Descriptive summaries
desc.ivy.kis <- data.frame(minimum = c(min(ivy$kiswahili)),
                           lower.q = c(lq(ivy$kiswahili)),
                           Median = c(median(ivy$kiswahili)),
                           upper.q = c(uq(ivy$kiswahili)),
                           maximum = c(max(ivy$kiswahili)),
                           average = c(mean(ivy$kiswahili)),
                           standard.dev = c(sd(ivy$kiswahili)),
                           c.o.v=c(COV(ivy$kiswahili))
); print(desc.ivy.kis)

# B. Distribution
boxplot(ivy$kiswahili, col='royalblue4', pch=16,horizontal = T,
        col.main="midnightblue",border="royalblue1", las=1
        ,main="COMBINED: KISWAHILI", xlab="Kiswahili score")

cat.ivy.kis<-cut(ivy$kiswahili,
                 breaks=custom.breaks, 
                 labels=custom.labels);print(cat.ivy.kis)

barplot(table(cat.ivy.kis)*100/nrow(ivy), tcl=65,
        las=1,border="cyan", ylim = c(0,30))

# Density
ggplot(ivy, aes(x=kiswahili))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Combined:",
       subtitle="Kiswahili")+
  theme_gdocs()

# 3. Mathematics
# A. Descriptive summaries
desc.ivy.math <- data.frame(minimum = c(min(ivy$math)),
                            lower.q = c(lq(ivy$math)),
                            Median = c(median(ivy$math)),
                            upper.q = c(uq(ivy$math)),
                            maximum = c(max(ivy$math)),
                            average = c(mean(ivy$math)),
                            standard.dev = c(sd(ivy$math)),
                            c.o.v=c(COV(ivy$math))
); print(desc.ivy.math)

# B. Distribution
boxplot(ivy$math, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="COMBINED: MATHEMATICS", xlab="Mathematics score")

cat.ivy.math<-cut(ivy$math,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.ivy.math)

barplot(table(cat.ivy.math)*100/nrow(ivy), tcl=65,
        las=1,border="cyan", ylim = c(0,30))

# Density
ggplot(ivy, aes(x=math))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Combined:",
       subtitle="Mathematics")+
  theme_gdocs()

# 4. Science
# A. Descriptive summaries
desc.ivy.scie <- data.frame(minimum = c(min(ivy$science)),
                            lower.q = c(lq(ivy$science)),
                            Median = c(median(ivy$science)),
                            upper.q = c(uq(ivy$science)),
                            maximum = c(max(ivy$science)),
                            average = c(mean(ivy$science)),
                            standard.dev = c(sd(ivy$science)),
                            c.o.v=c(COV(ivy$scie))
); print(desc.ivy.scie)

# B. Distribution
boxplot(ivy$science, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal = T
        ,main="COMBINED: SCIENCE", xlab="Science score")

cat.ivy.scie<-cut(ivy$science,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.ivy.scie)

barplot(table(cat.ivy.scie)*100/nrow(ivy), tcl=65
        , las=1,border="cyan", ylim = c(0,30))

# Density
ggplot(ivy, aes(x=science))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Combined:",
       subtitle="Science")+
  theme_gdocs()

# 5. SSRE
# A. Descriptive summaries
desc.ivy.ssre <- data.frame(minimum = c(min(ivy$SSRE)),
                            lower.q = c(lq(ivy$SSRE)),
                            Median = c(median(ivy$SSRE)),
                            upper.q = c(uq(ivy$SSRE)),
                            maximum = c(max(ivy$SSRE)),
                            average = c(mean(ivy$SSRE)),
                            standard.dev = c(sd(ivy$SSRE)),
                            c.o.v=c(COV(ivy$SSRE))
); print(desc.ivy.ssre)

# B. Distribution
boxplot(ivy$SSRE, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="COMBINED: SOCIAL STUDIES & RELIGIOUS EDUCATION", xlab="SSRE score")

cat.ivy.ssre<-cut(ivy$SSRE,
                  breaks=custom.breaks, 
                  labels=custom.labels);print(cat.ivy.ssre)

barplot(table(cat.ivy.ssre)*100/nrow(ivy), tcl=65, 
        las=1,border="cyan", ylim=c(0,35))

# Density
ggplot(ivy, aes(x=SSRE))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Combined:",
       subtitle="SSRE")+
  theme_gdocs()

# 6. Total
# A. Descriptive summaries
ivy$total<-ivy$english+ivy$kiswahili+ivy$math+ivy$science+ivy$SSRE
ivy$total

desc.ivy.total <- data.frame(minimum = c(min(ivy$total)),
                             lower.q = c(lq(ivy$total)),
                             Median = c(median(ivy$total)),
                             upper.q = c(uq(ivy$total)),
                             maximum = c(max(ivy$total)),
                             average = c(mean(ivy$total)),
                             standard.dev = c(sd(ivy$total)),
                             c.o.v=c(COV(ivy$total))
); print(desc.ivy.total)

# B. Distribution
boxplot(ivy$total, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="COMBINED: TOTAL MARKS", xlab="Total exam score")

cat.ivy.total<-cut(ivy$total,
                   breaks=c(0,100,200,300,400,500), 
                   labels=c("0-100","101-200","201-300","301-400","401-500"))
print(cat.ivy.total)

barplot(table(cat.ivy.total)*100/nrow(ivy),
        tcl=65, las=1,border="cyan",
        ylim=c(0,50), col="royalblue4")


# Density
ggplot(ivy, aes(x=total))+
  geom_density(fill="orangered4",col="orangered",alpha=0.5)+
  labs(x="score",
       y="probability",
       title="Combined:",
       subtitle="Total")+
  theme_gdocs()



# English
par(mfrow=c(3,1))
boxplot(atl$english, col='royalblue4', pch=16,horizontal = T,
        col.main="midnightblue",border="royalblue1", las=1
        ,main="ATLANTA: ENGLISH", xlab="English score")
boxplot(ind$english, col='royalblue4', pch=16,horizontal = T,
        col.main="midnightblue",border="royalblue1", las=1
        ,main="INDIANNA: ENGLISH", xlab="English score")
boxplot(pac$english, col='royalblue4', pch=16,horizontal = T,
        col.main="midnightblue",border="royalblue1", las=1
        ,main="PACIFICA: ENGLISH", xlab="English score")

# Kiswahili
par(mfrow=c(3,1))
boxplot(atl$kiswahili, col='royalblue4', pch=16,horizontal = T,
        col.main="midnightblue",border="royalblue1", las=1
        ,main="ATLANTA: KISWAHILI", xlab="Kiswahili score")
boxplot(ind$kiswahili, col='royalblue4', pch=16,horizontal = T,
        col.main="midnightblue",border="royalblue1", las=1
        ,main="INDIANNA: KISWAHILI", xlab="Kiswahili score")
boxplot(pac$kiswahili, col='royalblue4', pch=16,horizontal = T,
        col.main="midnightblue",border="royalblue1", las=1
        ,main="PACIFICA: KISWAHILI", xlab="Kiswahili score")

# Mathematics
par(mfrow=c(3,1))
boxplot(atl$math, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="ATLANTA: MATHEMATICS", xlab="Mathematics score")
boxplot(ind$math, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="INDIANNA: MATHEMATICS", xlab="Mathematics score")
boxplot(pac$math, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="PACIFICA: MATHEMATICS", xlab="Mathematics score")

# Science
par(mfrow=c(3,1))
boxplot(atl$science, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal = T
        ,main="ATLANTA: SCIENCE", xlab="Science score")
boxplot(ind$science, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal = T
        ,main="INDIANNA: SCIENCE", xlab="Science score")
boxplot(pac$science, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal = T
        ,main="PACIFICA: SCIENCE", xlab="Science score")


# SSRE
par(mfrow=c(3,1))
boxplot(atl$SSRE, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="ATLANTA: SOCIAL STUDIES & RELIGIOUS EDUCATION", xlab="SSRE score")
boxplot(ind$SSRE, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="INDIANNA: SOCIAL STUDIES & RELIGIOUS EDUCATION", xlab="SSRE score")
boxplot(pac$SSRE, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="PACIFICA: SOCIAL STUDIES & RELIGIOUS EDUCATION", xlab="SSRE score")


# Total
par(mfrow=c(3,1))
boxplot(atl$total, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="ATLANTA: TOTAL MARKS", xlab="Total exam score")
boxplot(ind$total, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="INDIANNA: TOTAL MARKS", xlab="Total exam score")
boxplot(pac$total, col='royalblue4', pch=16,
        col.main="midnightblue",border="royalblue1", las=1,horizontal=T
        ,main="PACIFICA: TOTAL MARKS", xlab="Total exam score")

# ENGLISH
ivy %>% 
  group_by(stream) %>% 
  summarise(subject.mean=round(mean(english),2)) %>% 
  arrange(desc(subject.mean)) 

# KISWAHILI
ivy %>% 
  group_by(stream) %>% 
  summarise(subject.mean=round(mean(kiswahili),2)) %>% 
  arrange(desc(subject.mean))

# MATHEMATICS
ivy %>% 
  group_by(stream) %>% 
  summarise(subject.mean=round(mean(math),2)) %>% 
  arrange(desc(subject.mean))

# SCIENCE
ivy %>% 
  group_by(stream) %>% 
  summarise(subject.mean=round(mean(science),2)) %>% 
  arrange(desc(subject.mean))

# SSRE
ivy %>% 
  group_by(stream) %>% 
  summarise(subject.mean=round(mean(SSRE),2)) %>% 
  arrange(desc(subject.mean))

# TOTAL
ivy %>% 
  group_by(stream) %>% 
  summarise(subject.mean=round(mean(total),2)) %>% 
  arrange(desc(subject.mean))


