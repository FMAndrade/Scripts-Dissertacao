###########################
# Variaveis do modelo 1 ### 
###########################


rm(list=ls(all=T))

getwd()

setwd("C:\\Users\\User\\Felipe\\Bases diretores (banco antigo)\\Novas Bases (nov_2014)")

require(foreign)


data1<-read.spss("base_refeita_sem_duplicados_nov_2014_principal.sav", to.data.frame=T)

install.packages("nnet", dependencies=T)

require(nnet)




# ideb 2005_5 (nota padronizada)

mult1<-multinom(mult_06_08_09_nova ~ nota_padr_5_2005, data=data1) 
summary(mult1)

z1<-summary(mult1)$coefficients/summary(mult1)$standard.errors
z1

p1<-(1-pnorm(abs(z1), 0, 1)) * 2
p1


# ideb 2005_9 (nota padronizada)

mult2<-multinom(mult_06_08_09_nova ~ nota_padr_9_2005, data=data1) 
summary(mult2)

z2<-summary(mult2)$coefficients/summary(mult2)$standard.errors
z2

p2<-(1-pnorm(abs(z2), 0, 1)) * 2
p2


# ideb 2007_5 (nota padronizada)

mult3<-multinom(mult_06_08_09_nova ~ nota_padr_5_2007, data=data1) 
summary(mult3)

z3<-summary(mult3)$coefficients/summary(mult3)$standard.errors
z3

p3<-(1-pnorm(abs(z3), 0, 1)) * 2
p3


# ideb 2007_9 (nota padronizada)

mult4<-multinom(mult_06_08_09_nova ~ nota_padr_9_2007, data=data1) 
summary(mult4)

z4<-summary(mult4)$coefficients/summary(mult4)$standard.errors
z4

p4<-(1-pnorm(abs(z4), 0, 1)) * 2
p4


# ideb 2005_5 

mult5<-multinom(mult_06_08_09_nova ~ ideb_5_2005, data=data1) 
summary(mult5)

z5<-summary(mult5)$coefficients/summary(mult5)$standard.errors
z5

p5<-(1-pnorm(abs(z5), 0, 1)) * 2
p5


# ideb 2005_9 

mult6<-multinom(mult_06_08_09_nova ~ ideb_9_2005, data=data1) 
summary(mult6)

z6<-summary(mult6)$coefficients/summary(mult6)$standard.errors
z6

p6<-(1-pnorm(abs(z6), 0, 1)) * 2
p6


# ideb 2007_5 

mult7<-multinom(mult_06_08_09_nova ~ ideb_5_2007, data=data1) 
summary(mult7)

z7<-summary(mult7)$coefficients/summary(mult7)$standard.errors
z7

p7<-(1-pnorm(abs(z7), 0, 1)) * 2
p7


# ideb 2007_9 

mult8<-multinom(mult_06_08_09_nova ~ ideb_9_2007, data=data1) 
summary(mult8)

z8<-summary(mult8)$coefficients/summary(mult8)$standard.errors
z8

p8<-(1-pnorm(abs(z8), 0, 1)) * 2
p8


# indicador de favela (escola a 50 metros da favela)

mult9<-multinom(mult_06_08_09_nova ~ INDIC_Fav_50, data=data1) 
summary(mult9)

z9<-summary(mult9)$coefficients/summary(mult9)$standard.errors
z9

p9<-(1-pnorm(abs(z9), 0, 1)) * 2
p9


# indicador de favela (escola a 100 metros da favela)

mult10<-multinom(mult_06_08_09_nova ~ INDIC_Fav_100, data=data1) 
summary(mult10)

z10<-summary(mult10)$coefficients/summary(mult10)$standard.errors
z10

p10<-(1-pnorm(abs(z10), 0, 1)) * 2
p10


# indicador de carencia 2007 AI

mult11<-multinom(mult_06_08_09_nova ~ Carencia_perct_5_07_AI, data=data1) 
summary(mult11)

z11<-summary(mult11)$coefficients/summary(mult11)$standard.errors
z11

p11<-(1-pnorm(abs(z11), 0, 1)) * 2
p11


# indicador de carencia 2007 AF

mult12<-multinom(mult_06_08_09_nova ~ carencia_2007_AF, data=data1) 
summary(mult12)

z12<-summary(mult12)$coefficients/summary(mult12)$standard.errors
z12

p12<-(1-pnorm(abs(z12), 0, 1)) * 2
p12



# indicador de lideranca 2007 AI

mult13<-multinom(mult_06_08_09_nova ~ Liderenca_perct_5_07_AI, data=data1) 
summary(mult13)

z13<-summary(mult13)$coefficients/summary(mult13)$standard.errors
z13

p13<-(1-pnorm(abs(z13), 0, 1)) * 2
p13


# indicador de lideranca 2007 AF

mult14<-multinom(mult_06_08_09_nova ~ lideranca_2007_AF, data=data1) 
summary(mult14)

z14<-summary(mult14)$coefficients/summary(mult14)$standard.errors
z14

p14<-(1-pnorm(abs(z14), 0, 1)) * 2
p14


# indicador de colaboracao 2007 AI

mult15<-multinom(mult_06_08_09_nova ~ Colaboracao_perct_5_07_AI, data=data1) 
summary(mult15)

z15<-summary(mult15)$coefficients/summary(mult15)$standard.errors
z15

p15<-(1-pnorm(abs(z15), 0, 1)) * 2
p15


# indicador de colaboracao 2007 AF

mult16<-multinom(mult_06_08_09_nova ~ colaboracao_2007_9_AF, data=data1) 
summary(mult16)

z16<-summary(mult16)$coefficients/summary(mult16)$standard.errors
z16

p16<-(1-pnorm(abs(z16), 0, 1)) * 2
p16


# indicador de experiencia 2006

mult17<-multinom(mult_06_08_09_nova ~ exp_2006, data=data1) 
summary(mult17)

z17<-summary(mult17)$coefficients/summary(mult17)$standard.errors
z17

p17<-(1-pnorm(abs(z17), 0, 1)) * 2
p17


# indicador de experiencia 2007

mult18<-multinom(mult_06_08_09_nova ~ exp_2007, data=data1) 
summary(mult18)

z18<-summary(mult18)$coefficients/summary(mult18)$standard.errors
z18

p18<-(1-pnorm(abs(z18), 0, 1)) * 2
p18


# indicador de experiencia 2008

mult19<-multinom(mult_06_08_09_nova ~ exp_2008, data=data1) 
summary(mult19)

z19<-summary(mult19)$coefficients/summary(mult19)$standard.errors
z19

p19<-(1-pnorm(abs(z19), 0, 1)) * 2
p19


# % de alunos brancos 2006

mult20<-multinom(mult_06_08_09_nova ~ cor_recode_06, data=data1) 
summary(mult20)

z20<-summary(mult20)$coefficients/summary(mult20)$standard.errors
z20

p20<-(1-pnorm(abs(z20), 0, 1)) * 2
p20


# % de alunos brancos 2007

mult21<-multinom(mult_06_08_09_nova ~ cor_recode_07, data=data1) 
summary(mult21)

z21<-summary(mult21)$coefficients/summary(mult21)$standard.errors
z21

p21<-(1-pnorm(abs(z21), 0, 1)) * 2
p21

# % de alunos brancos 2008

mult22<-multinom(mult_06_08_09_nova ~ cor_recode_08, data=data1) 
summary(mult22)

z22<-summary(mult22)$coefficients/summary(mult22)$standard.errors
z22

p22<-(1-pnorm(abs(z22), 0, 1)) * 2
p22


# % de pais com alta escolaridade 2006

mult23<-multinom(mult_06_08_09_nova ~ max_educ_06, data=data1) 
summary(mult23)

z23<-summary(mult23)$coefficients/summary(mult23)$standard.errors
z23

p23<-(1-pnorm(abs(z23), 0, 1)) * 2
p23


# % de pais com alta escolaridade 2007

mult24<-multinom(mult_06_08_09_nova ~ max_educ_07, data=data1) 
summary(mult24)

z24<-summary(mult24)$coefficients/summary(mult24)$standard.errors
z24

p24<-(1-pnorm(abs(z24), 0, 1)) * 2
p24


# % de pais com alta escolaridade 2008

mult25<-multinom(mult_06_08_09_nova ~ max_educ_08, data=data1) 
summary(mult25)

z25<-summary(mult25)$coefficients/summary(mult25)$standard.errors
z25

p25<-(1-pnorm(abs(z25), 0, 1)) * 2
p25


# indicativo de pressao da CRE 2005 (escolas com menores desempenhos AI)

mult26<-multinom(mult_06_08_09_nova ~ ideb_5_05_new, data=data1) 
summary(mult26)

z26<-summary(mult26)$coefficients/summary(mult26)$standard.errors
z26

p26<-(1-pnorm(abs(z26), 0, 1)) * 2
p26


# indicativo de pressao da CRE 2005 (escolas com menores desempenhos AF)

mult27<-multinom(mult_06_08_09_nova ~ ideb_9_05_new, data=data1) 
summary(mult27)

z27<-summary(mult27)$coefficients/summary(mult27)$standard.errors
z27

p27<-(1-pnorm(abs(z27), 0, 1)) * 2
p27


# indicativo de pressao da CRE 2007 (escolas com menores desempenhos AI)

mult28<-multinom(mult_06_08_09_nova ~ ideb_5_07_new, data=data1) 
summary(mult28)

z28<-summary(mult28)$coefficients/summary(mult28)$standard.errors
z28

p28<-(1-pnorm(abs(z28), 0, 1)) * 2
p28


# indicativo de pressao da CRE 2007 (escolas com menores desempenhos AF)

mult29<-multinom(mult_06_08_09_nova ~ ideb_9_07_new, data=data1) 
summary(mult29)

z29<-summary(mult29)$coefficients/summary(mult29)$standard.errors
z29

p29<-(1-pnorm(abs(z29), 0, 1)) * 2
p29


# complexidade escolar 2007

mult30<-multinom(mult_06_08_09_nova ~ complexidade_esc_07, data=data1) 
summary(mult30)

z30<-summary(mult30)$coefficients/summary(mult30)$standard.errors
z30

p30<-(1-pnorm(abs(z30), 0, 1)) * 2
p30


# complexidade escolar 2008

mult31<-multinom(mult_06_08_09_nova ~ complexidade_esc_08, data=data1) 
summary(mult31)

z31<-summary(mult31)$coefficients/summary(mult31)$standard.errors
z31

p31<-(1-pnorm(abs(z31), 0, 1)) * 2
p31
