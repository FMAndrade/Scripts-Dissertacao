###########################
# Variaveis do modelo 2 ### 
###########################


rm(list=ls(all=T))

getwd()

setwd("C:\\Users\\User\\Felipe\\Bases diretores (banco antigo)\\Novas Bases (nov_2014)")

require(foreign)


data1<-read.spss("base_refeita_sem_duplicados_nov_2014_principal.sav", to.data.frame=T)

install.packages("nnet", dependencies=T)

require(nnet)



# ideb 2009_5 (nota padronizada)

mult1<-multinom(mult_09_11_12_nova ~ nota_padr_5_2009, data=data1) 
summary(mult1)

z1<-summary(mult1)$coefficients/summary(mult1)$standard.errors
z1

p1<-(1-pnorm(abs(z1), 0, 1)) * 2
p1


# ideb 2009_9 (nota padronizada)

mult2<-multinom(mult_09_11_12_nova ~ nota_padr_9_2009, data=data1) 
summary(mult2)

z2<-summary(mult2)$coefficients/summary(mult2)$standard.errors
z2

p2<-(1-pnorm(abs(z2), 0, 1)) * 2
p2


# ideb 2011_5 (nota padronizada)

mult3<-multinom(mult_09_11_12_nova ~ nota_padr_5_2011, data=data1) 
summary(mult3)

z3<-summary(mult3)$coefficients/summary(mult3)$standard.errors
z3

p3<-(1-pnorm(abs(z3), 0, 1)) * 2
p3


# ideb 2011_9 (nota padronizada)

mult4<-multinom(mult_09_11_12_nova ~ nota_padr_9_2011, data=data1) 
summary(mult4)

z4<-summary(mult4)$coefficients/summary(mult4)$standard.errors
z4

p4<-(1-pnorm(abs(z4), 0, 1)) * 2
p4


# ideb 2009_5 

mult5<-multinom(mult_09_11_12_nova ~ ideb_5_2009, data=data1) 
summary(mult5)

z5<-summary(mult5)$coefficients/summary(mult5)$standard.errors
z5

p5<-(1-pnorm(abs(z5), 0, 1)) * 2
p5


# ideb 2009_9 

mult6<-multinom(mult_09_11_12_nova ~ ideb_9_2009, data=data1) 
summary(mult6)

z6<-summary(mult6)$coefficients/summary(mult6)$standard.errors
z6

p6<-(1-pnorm(abs(z6), 0, 1)) * 2
p6


# ideb 2011_5 

mult7<-multinom(mult_09_11_12_nova ~ ideb_5_2011, data=data1) 
summary(mult7)

z7<-summary(mult7)$coefficients/summary(mult7)$standard.errors
z7

p7<-(1-pnorm(abs(z7), 0, 1)) * 2
p7


# ideb 2011_9 

mult8<-multinom(mult_09_11_12_nova ~ ideb_9_2011, data=data1) 
summary(mult8)

z8<-summary(mult8)$coefficients/summary(mult8)$standard.errors
z8

p8<-(1-pnorm(abs(z8), 0, 1)) * 2
p8


# indicador de favela (escola a 50 metros da favela)

mult9<-multinom(mult_09_11_12_nova ~ INDIC_Fav_50, data=data1) 
summary(mult9)

z9<-summary(mult9)$coefficients/summary(mult9)$standard.errors
z9

p9<-(1-pnorm(abs(z9), 0, 1)) * 2
p9


# indicador de favela (escola a 100 metros da favela)

mult10<-multinom(mult_09_11_12_nova ~ INDIC_Fav_100, data=data1) 
summary(mult10)

z10<-summary(mult10)$coefficients/summary(mult10)$standard.errors
z10

p10<-(1-pnorm(abs(z10), 0, 1)) * 2
p10


# indicador de Premio (PAD) 2009

mult11<-multinom(mult_09_11_12_nova ~ PREMIO_2009, data=data1) 
summary(mult11)

z11<-summary(mult11)$coefficients/summary(mult11)$standard.errors
z11

p11<-(1-pnorm(abs(z11), 0, 1)) * 2
p11


# indicador de Premio (PAD) 2010

mult12<-multinom(mult_09_11_12_nova ~ PREMIO_2010, data=data1) 
summary(mult12)

z12<-summary(mult12)$coefficients/summary(mult12)$standard.errors
z12

p12<-(1-pnorm(abs(z12), 0, 1)) * 2
p12


# indicador de Premio (PAD) 2011 AI

mult13<-multinom(mult_09_11_12_nova ~ PREMIO_2011_AI, data=data1) 
summary(mult13)

z13<-summary(mult13)$coefficients/summary(mult13)$standard.errors
z13

p13<-(1-pnorm(abs(z13), 0, 1)) * 2
p13


# indicador de Premio (PAD) 2011 AF

mult14<-multinom(mult_09_11_12_nova ~ PREMIO_2011_AF, data=data1) 
summary(mult14)

z14<-summary(mult14)$coefficients/summary(mult14)$standard.errors
z14

p14<-(1-pnorm(abs(z14), 0, 1)) * 2
p14


# carencia 2009 AI

mult15<-multinom(mult_09_11_12_nova ~ Carencia_perct_5_09_AI, data=data1) 
summary(mult15)

z15<-summary(mult15)$coefficients/summary(mult15)$standard.errors
z15

p15<-(1-pnorm(abs(z15), 0, 1)) * 2
p15


# carencia 2009 AF

mult16<-multinom(mult_09_11_12_nova ~ carencia_2009_AF, data=data1) 
summary(mult16)

z16<-summary(mult16)$coefficients/summary(mult16)$standard.errors
z16

p16<-(1-pnorm(abs(z16), 0, 1)) * 2
p16


# carencia 2011 AI

mult17<-multinom(mult_09_11_12_nova ~ Carencia_perct_5_11_AI, data=data1) 
summary(mult17)

z17<-summary(mult17)$coefficients/summary(mult17)$standard.errors
z17

p17<-(1-pnorm(abs(z17), 0, 1)) * 2
p17


# carencia 2011 AF

mult18<-multinom(mult_09_11_12_nova ~ carencia_2011_AF, data=data1) 
summary(mult18)

z18<-summary(mult18)$coefficients/summary(mult18)$standard.errors
z18

p18<-(1-pnorm(abs(z18), 0, 1)) * 2
p18


# lideranca 2009 AI

mult19<-multinom(mult_09_11_12_nova ~ Liderenca_perct_5_09_AI, data=data1) 
summary(mult19)

z19<-summary(mult19)$coefficients/summary(mult19)$standard.errors
z19

p19<-(1-pnorm(abs(z19), 0, 1)) * 2
p19


# lideranca 2009 AF

mult20<-multinom(mult_09_11_12_nova ~ lideranca_2009_AF, data=data1) 
summary(mult20)

z20<-summary(mult20)$coefficients/summary(mult20)$standard.errors
z20

p20<-(1-pnorm(abs(z20), 0, 1)) * 2
p20


# lideranca 2011 AI

mult21<-multinom(mult_09_11_12_nova ~ Liderenca_perct_5_11_AI, data=data1) 
summary(mult21)

z21<-summary(mult21)$coefficients/summary(mult21)$standard.errors
z21

p21<-(1-pnorm(abs(z21), 0, 1)) * 2
p21


# lideranca 2011 AF

mult22<-multinom(mult_09_11_12_nova ~ lideranca_2011_AF, data=data1) 
summary(mult22)

z22<-summary(mult22)$coefficients/summary(mult22)$standard.errors
z22

p22<-(1-pnorm(abs(z22), 0, 1)) * 2
p22


# colaboracao 2009 AI

mult23<-multinom(mult_09_11_12_nova ~ Colaboracao_perct_5_09_AI, data=data1) 
summary(mult23)

z23<-summary(mult23)$coefficients/summary(mult23)$standard.errors
z23

p23<-(1-pnorm(abs(z23), 0, 1)) * 2
p23


# colaboracao 2009 AF

mult24<-multinom(mult_09_11_12_nova ~ colaboracao_2009_9_AF, data=data1) 
summary(mult24)

z24<-summary(mult24)$coefficients/summary(mult24)$standard.errors
z24

p24<-(1-pnorm(abs(z24), 0, 1)) * 2
p24


# colaboracao 2011 AI

mult25<-multinom(mult_09_11_12_nova ~ Colaboracao_perct_5_11_AI, data=data1) 
summary(mult25)

z25<-summary(mult25)$coefficients/summary(mult25)$standard.errors
z25

p25<-(1-pnorm(abs(z25), 0, 1)) * 2
p25


# colaboracao 2011 AF

mult26<-multinom(mult_09_11_12_nova ~ colaboracao_2011_9_AF, data=data1) 
summary(mult26)

z26<-summary(mult26)$coefficients/summary(mult26)$standard.errors
z26

p26<-(1-pnorm(abs(z26), 0, 1)) * 2
p26



# nota padronizada 2009 3

mult27<-multinom(mult_09_11_12_nova ~ nota_padr_3_2009, data=data1) 
summary(mult27)

z27<-summary(mult27)$coefficients/summary(mult27)$standard.errors
z27

p27<-(1-pnorm(abs(z27), 0, 1)) * 2
p27



# nota padronizada 2009 7

mult28<-multinom(mult_09_11_12_nova ~ nota_padr_7_2009, data=data1) 
summary(mult28)

z28<-summary(mult28)$coefficients/summary(mult28)$standard.errors
z28

p28<-(1-pnorm(abs(z28), 0, 1)) * 2
p28



# nota padronizada 2010 3

mult29<-multinom(mult_09_11_12_nova ~ nota_padr_3_2010, data=data1) 
summary(mult29)

z29<-summary(mult29)$coefficients/summary(mult29)$standard.errors
z29

p29<-(1-pnorm(abs(z29), 0, 1)) * 2
p29


# nota padronizada 2010 4

mult30<-multinom(mult_09_11_12_nova ~ nota_padr_4_2010, data=data1) 
summary(mult30)

z30<-summary(mult30)$coefficients/summary(mult30)$standard.errors
z30

p30<-(1-pnorm(abs(z30), 0, 1)) * 2
p30


# nota padronizada 2010 7

mult31<-multinom(mult_09_11_12_nova ~ nota_padr_7_2010, data=data1) 
summary(mult31)

z31<-summary(mult31)$coefficients/summary(mult31)$standard.errors
z31

p31<-(1-pnorm(abs(z31), 0, 1)) * 2
p31


# nota padronizada 2010 8

mult32<-multinom(mult_09_11_12_nova ~ nota_padr_8_2010, data=data1) 
summary(mult32)

z32<-summary(mult32)$coefficients/summary(mult32)$standard.errors
z32

p32<-(1-pnorm(abs(z32), 0, 1)) * 2
p32


# nota padronizada 2011 3

mult33<-multinom(mult_09_11_12_nova ~ nota_padr_3_2011, data=data1) 
summary(mult33)

z33<-summary(mult33)$coefficients/summary(mult33)$standard.errors
z33

p33<-(1-pnorm(abs(z33), 0, 1)) * 2
p33


# nota padronizada 2011 4

mult34<-multinom(mult_09_11_12_nova ~ nota_padr_4_2011, data=data1) 
summary(mult34)

z34<-summary(mult34)$coefficients/summary(mult34)$standard.errors
z34

p34<-(1-pnorm(abs(z34), 0, 1)) * 2
p34


# nota padronizada 2011 7

mult35<-multinom(mult_09_11_12_nova ~ nota_padr_7_2011, data=data1) 
summary(mult35)

z35<-summary(mult35)$coefficients/summary(mult35)$standard.errors
z35

p35<-(1-pnorm(abs(z35), 0, 1)) * 2
p35


# nota padronizada 2011 8

mult36<-multinom(mult_09_11_12_nova ~ nota_padr_8_2011, data=data1) 
summary(mult36)

z36<-summary(mult36)$coefficients/summary(mult36)$standard.errors
z36

p36<-(1-pnorm(abs(z36), 0, 1)) * 2
p36


# indicador de experiencia 2009

mult37<-multinom(mult_09_11_12_nova ~ exp_2009, data=data1) 
summary(mult37)

z37<-summary(mult37)$coefficients/summary(mult37)$standard.errors
z37

p37<-(1-pnorm(abs(z37), 0, 1)) * 2
p37


# indicador de experiencia 2010

mult38<-multinom(mult_09_11_12_nova ~ exp_2010, data=data1) 
summary(mult38)

z38<-summary(mult38)$coefficients/summary(mult38)$standard.errors
z38

p38<-(1-pnorm(abs(z38), 0, 1)) * 2
p38


# indicador de experiencia 2011

mult39<-multinom(mult_09_11_12_nova ~ exp_2011, data=data1) 
summary(mult39)

z39<-summary(mult39)$coefficients/summary(mult39)$standard.errors
z39

p39<-(1-pnorm(abs(z39), 0, 1)) * 2
p39


# % de alunos brancos 2009

mult40<-multinom(mult_09_11_12_nova ~ cor_recode_09, data=data1) 
summary(mult40)

z40<-summary(mult40)$coefficients/summary(mult40)$standard.errors
z40

p40<-(1-pnorm(abs(z40), 0, 1)) * 2
p40


# % de alunos brancos 2010

mult41<-multinom(mult_09_11_12_nova ~ cor_recode_10, data=data1) 
summary(mult41)

z41<-summary(mult41)$coefficients/summary(mult41)$standard.errors
z41

p41<-(1-pnorm(abs(z41), 0, 1)) * 2
p41


# % de alunos brancos 2011

mult42<-multinom(mult_09_11_12_nova ~ cor_recode_11, data=data1) 
summary(mult42)

z42<-summary(mult42)$coefficients/summary(mult42)$standard.errors
z42

p42<-(1-pnorm(abs(z42), 0, 1)) * 2
p42


# % de pais com alta escolaridade 2009

mult43<-multinom(mult_09_11_12_nova ~ max_educ_09, data=data1) 
summary(mult43)

z43<-summary(mult43)$coefficients/summary(mult43)$standard.errors
z43

p43<-(1-pnorm(abs(z43), 0, 1)) * 2
p43


# % de pais com alta escolaridade 2010

mult44<-multinom(mult_09_11_12_nova ~ max_educ_10, data=data1) 
summary(mult44)

z44<-summary(mult44)$coefficients/summary(mult44)$standard.errors
z44

p44<-(1-pnorm(abs(z44), 0, 1)) * 2
p44


# % de pais com alta escolaridade 2011

mult45<-multinom(mult_09_11_12_nova ~ max_educ_11, data=data1) 
summary(mult45)

z45<-summary(mult45)$coefficients/summary(mult45)$standard.errors
z45

p45<-(1-pnorm(abs(z45), 0, 1)) * 2
p45


# iderio 2009 3

mult46<-multinom(mult_09_11_12_nova ~ Iderio_2009_3, data=data1) 
summary(mult46)

z46<-summary(mult46)$coefficients/summary(mult46)$standard.errors
z46

p46<-(1-pnorm(abs(z46), 0, 1)) * 2
p46


# iderio 2009 7

mult47<-multinom(mult_09_11_12_nova ~ Iderio2009_7, data=data1) 
summary(mult47)

z47<-summary(mult47)$coefficients/summary(mult47)$standard.errors
z47

p47<-(1-pnorm(abs(z47), 0, 1)) * 2
p47



# iderio 2010 3

mult48<-multinom(mult_09_11_12_nova ~ Iderio_2010_3, data=data1) 
summary(mult48)

z48<-summary(mult48)$coefficients/summary(mult48)$standard.errors
z48

p48<-(1-pnorm(abs(z48), 0, 1)) * 2
p48


# iderio 2010 7

mult49<-multinom(mult_09_11_12_nova ~ Iderio2010_7, data=data1) 
summary(mult49)

z49<-summary(mult49)$coefficients/summary(mult49)$standard.errors
z49

p49<-(1-pnorm(abs(z49), 0, 1)) * 2
p49


# indicativo de pressao da CRE ideb 2009 5

mult50<-multinom(mult_09_11_12_nova ~ ideb_5_09_new, data=data1) 
summary(mult50)

z50<-summary(mult50)$coefficients/summary(mult50)$standard.errors
z50

p50<-(1-pnorm(abs(z50), 0, 1)) * 2
p50


# indicativo de pressao da CRE ideb 2009 9

mult51<-multinom(mult_09_11_12_nova ~ ideb_9_09_new, data=data1) 
summary(mult51)

z51<-summary(mult51)$coefficients/summary(mult51)$standard.errors
z51

p51<-(1-pnorm(abs(z51), 0, 1)) * 2
p51


# indicativo de pressao da CRE ideb 2011 5

mult52<-multinom(mult_09_11_12_nova ~ ideb_5_11_new, data=data1) 
summary(mult52)

z52<-summary(mult52)$coefficients/summary(mult52)$standard.errors
z52

p52<-(1-pnorm(abs(z52), 0, 1)) * 2
p52


# indicativo de pressao da CRE ideb 2011 9

mult53<-multinom(mult_09_11_12_nova ~ ideb_9_11_new, data=data1) 
summary(mult53)

z53<-summary(mult53)$coefficients/summary(mult53)$standard.errors
z53

p53<-(1-pnorm(abs(z53), 0, 1)) * 2
p53


# indicativo de pressao da CRE iderio 2009 3

mult54<-multinom(mult_09_11_12_nova ~ Iderio_2009_3_new, data=data1) 
summary(mult54)

z54<-summary(mult54)$coefficients/summary(mult54)$standard.errors
z54

p54<-(1-pnorm(abs(z54), 0, 1)) * 2
p54


# indicativo de pressao da CRE iderio 2009 7

mult55<-multinom(mult_09_11_12_nova ~ Iderio2009_7_new, data=data1) 
summary(mult55)

z55<-summary(mult55)$coefficients/summary(mult55)$standard.errors
z55

p55<-(1-pnorm(abs(z55), 0, 1)) * 2
p55


# indicativo de pressao da CRE iderio 2010 3

mult56<-multinom(mult_09_11_12_nova ~ Iderio_2010_3_new, data=data1) 
summary(mult56)

z56<-summary(mult56)$coefficients/summary(mult56)$standard.errors
z56

p56<-(1-pnorm(abs(z56), 0, 1)) * 2
p56


# indicativo de pressao da CRE iderio 2010 7

mult57<-multinom(mult_09_11_12_nova ~ Iderio2010_7_new, data=data1) 
summary(mult57)

z57<-summary(mult57)$coefficients/summary(mult57)$standard.errors
z57

p57<-(1-pnorm(abs(z57), 0, 1)) * 2
p57


# complexidade escolar 2009

mult58<-multinom(mult_09_11_12_nova ~ complexidade_esc_09, data=data1) 
summary(mult58)

z58<-summary(mult58)$coefficients/summary(mult58)$standard.errors
z58

p58<-(1-pnorm(abs(z58), 0, 1)) * 2
p58


# complexidade escolar 2010

mult59<-multinom(mult_09_11_12_nova ~ complexidade_esc_10, data=data1) 
summary(mult59)

z59<-summary(mult59)$coefficients/summary(mult59)$standard.errors
z59

p59<-(1-pnorm(abs(z59), 0, 1)) * 2
p59


# complexidade escolar 2011

mult60<-multinom(mult_09_11_12_nova ~ complexidade_esc_11, data=data1) 
summary(mult60)

z60<-summary(mult60)$coefficients/summary(mult60)$standard.errors
z60

p60<-(1-pnorm(abs(z60), 0, 1)) * 2
p60
