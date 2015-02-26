#################################
# Novas analises por Mandato 1 ##
#################################


# As analises abaixos compreendem as variaveis de entrada do diretor, ou seja, no mandato de 2006 a 2008. Sao inseridas
# as variaveis de 2006, e no mandato de 2009 a 2011, as variaveis de 2009


rm(list=ls(all=T))

getwd()

setwd("C:\\Users\\User\\Felipe\\Bases diretores (banco antigo)\\Novas Bases (nov_2014)")

require(foreign)


data1<-read.spss("base_refeita_sem_duplicados_nov_2014_principal.sav", to.data.frame=T)

install.packages("nnet", dependencies=T)

require(nnet)
require(stargazer)


# Modelo de 2006 a 2008 - 2009



# Modelo 1 (2006-08/09) Desempenho


multn0 <- multinom(mult_06_08_09_nova ~ nota_padr_5_2007 + nota_padr_9_2007 + ideb_5_07_new + ideb_9_07_new, data=data1)

confint(multn0)

#  Modelo 2 (2006-08/09) Contexto Escolar e caracteristica do diretor

multn1 <- multinom(mult_06_08_09_nova ~ Carencia_perct_5_07_AI + carencia_2007_AF + Liderenca_perct_5_07_AI + lideranca_2007_AF + 
                     Colaboracao_perct_5_07_AI + colaboracao_2007_9_AF + exp_2006 + cor_recode_06 + max_educ_06 + complexidade_esc_07,
                   data=data1)



# Modelo 3 (2006-08/09) Modelo Cheio

multn2 <- multinom(mult_06_08_09_nova ~ nota_padr_5_2007 + nota_padr_9_2007 + Carencia_perct_5_07_AI + carencia_2007_AF +
                     Liderenca_perct_5_07_AI + lideranca_2007_AF + Colaboracao_perct_5_07_AI + colaboracao_2007_9_AF + 
                     exp_2006 + cor_recode_06 + max_educ_06 + ideb_5_07_new + ideb_9_07_new + complexidade_esc_07, data=data1)

multn2

znew2<-summary(multn2)$coefficients/summary(multn2)$standard.errors
znew2

pnew2<-(1-pnorm(abs(znew2), 0, 1)) * 2
pnew2


logLik (multn0)
logLik (multn1)
logLik (multn2)


stargazer (multn0, multn1, multn2)


# Modelo 2009 a 2011 - 2012


# Modelo 1 (2009-11/12) Desempenho


multn3 <- multinom(mult_09_11_12_nova ~ nota_padr_5_2009 + nota_padr_9_2009 + PREMIO_2009 + nota_padr_3_2009 + nota_padr_7_2009 + 
                     ideb_5_09_new + ideb_9_09_new + Iderio_2009_3_new + Iderio2009_7_new, data=data1) 


# Modelo 2 (2009-11/12) Contexto Escolar e caracteristica do diretor

multn4 <- multinom(mult_09_11_12_nova ~ Carencia_perct_5_09_AI + carencia_2009_AF + Liderenca_perct_5_09_AI + lideranca_2009_AF + 
                     Colaboracao_perct_5_09_AI + colaboracao_2009_9_AF + exp_2009 + cor_recode_09 + max_educ_09 + complexidade_esc_09,
                   data=data1)


# Modelo 3 (2009-11/12) Modelo cheio

multn5 <- multinom(mult_09_11_12_nova ~ nota_padr_5_2009 + nota_padr_9_2009 + PREMIO_2009 + Carencia_perct_5_09_AI + carencia_2009_AF + 
                     Liderenca_perct_5_09_AI + lideranca_2009_AF + Colaboracao_perct_5_09_AI + colaboracao_2009_9_AF + nota_padr_3_2009 +
                     nota_padr_7_2009 + exp_2009 + cor_recode_09 + max_educ_09 + ideb_5_09_new + ideb_9_09_new + Iderio_2009_3_new + 
                     Iderio2009_7_new + complexidade_esc_09, data=data1)


# Modelo 4 (2009-11/12) somente com as variaveis significativas

multn6<-multinom(mult_09_11_12_nova ~ complexidade_esc_09 + nota_padr_9_2009 + ideb_5_09_new + carencia_2009_AF + Colaboracao_perct_5_09_AI +
                   exp_2009, data=data1)
stargazer(multn6)



stargazer(multn3, multn4, multn5)


# Calculando o loglikelyhood dos modelos 

logLik(multn3)
logLik(multn4)
logLik(multn5)



# Probabilidade predita do modelo


# Valores mantidos na media

p1<-predict (multn1, data.frame(nota_padr_5_2007 = mean(data1$nota_padr_5_2007, na.rm=T),
                            nota_padr_9_2007 = mean(data1$nota_padr_9_2007, na.rm=T),
                            Carencia_perct_5_07_AI = mean(data1$Carencia_perct_5_07_AI, na.rm=T),
                            carencia_2007_AF = mean(data1$carencia_2007_AF, na.rm=T),
                            Liderenca_perct_5_07_AI = mean(data1$Liderenca_perct_5_07_AI, na.rm=T),
                            lideranca_2007_AF = mean(data1$lideranca_2007_AF, na.rm=T),
                            Colaboracao_perct_5_07_AI = mean(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                            colaboracao_2007_9_AF = mean(data1$colaboracao_2007_9_AF, na.rm=T),
                            exp_2006 = mean(data1$exp_2006, na.rm=T),
                            cor_recode_06 = mean(data1$cor_recode_06,na.rm=T),
                            max_educ_06 = mean(data1$max_educ_06, na.rm=T),
                            ideb_5_07_new = mean(data1$ideb_5_07_new, na.rm=T),
                            ideb_9_07_new = mean(data1$ideb_9_07_new, na.rm=T),
                            complexidade_esc_07 = mean(data1$complexidade_esc_07, na.rm=T)),
         type="probs")
                      
p1

# Variavies que valores altos sao "boas" no max e variaveis com valores baixo que sao "ruins" no min

p2<-predict (multn2, data.frame(nota_padr_5_2007 = max(data1$nota_padr_5_2007, na.rm=T),
                                nota_padr_9_2007 = max(data1$nota_padr_9_2007, na.rm=T),
                                Carencia_perct_5_07_AI = min(data1$Carencia_perct_5_07_AI, na.rm=T),
                                carencia_2007_AF = min(data1$carencia_2007_AF, na.rm=T),
                                Liderenca_perct_5_07_AI = max(data1$Liderenca_perct_5_07_AI, na.rm=T),
                                lideranca_2007_AF = max(data1$lideranca_2007_AF, na.rm=T),
                                Colaboracao_perct_5_07_AI = max(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                                colaboracao_2007_9_AF = max(data1$colaboracao_2007_9_AF, na.rm=T),
                                exp_2006 = max(data1$exp_2006, na.rm=T),
                                cor_recode_06 = max(data1$cor_recode_06,na.rm=T),
                                max_educ_06 = max(data1$max_educ_06, na.rm=T),
                                ideb_5_07_new = min(data1$ideb_5_07_new, na.rm=T),
                                ideb_9_07_new = min(data1$ideb_9_07_new, na.rm=T),
                                complexidade_esc_07 = min(data1$complexidade_esc_07, na.rm=T)),
             type="probs")


p2

# Variaveis que os valores altos sao "boas" no min e variaveis que sao "ruins" no max

p3<-predict (multn2, data.frame(nota_padr_5_2007 = min(data1$nota_padr_5_2007, na.rm=T),
                                nota_padr_9_2007 = min(data1$nota_padr_9_2007, na.rm=T),
                                Carencia_perct_5_07_AI = max(data1$Carencia_perct_5_07_AI, na.rm=T),
                                carencia_2007_AF = max(data1$carencia_2007_AF, na.rm=T),
                                Liderenca_perct_5_07_AI = min(data1$Liderenca_perct_5_07_AI, na.rm=T),
                                lideranca_2007_AF = min(data1$lideranca_2007_AF, na.rm=T),
                                Colaboracao_perct_5_07_AI = min(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                                colaboracao_2007_9_AF = min(data1$colaboracao_2007_9_AF, na.rm=T),
                                exp_2006 = min(data1$exp_2006, na.rm=T),
                                cor_recode_06 = min(data1$cor_recode_06,na.rm=T),
                                max_educ_06 = min(data1$max_educ_06, na.rm=T),
                                ideb_5_07_new = max(data1$ideb_5_07_new, na.rm=T),
                                ideb_9_07_new = max(data1$ideb_9_07_new, na.rm=T),
                                complexidade_esc_07 = max(data1$complexidade_esc_07, na.rm=T)),
             type="probs")


p3


# segundo momento (2009-2011/2012)

# Valores mantidos na media

p4<-predict (multn5, data.frame(nota_padr_5_2009 = mean(data1$nota_padr_5_2009, na.rm=T),
                                nota_padr_9_2009 = mean(data1$nota_padr_9_2009, na.rm=T),
                                PREMIO_2009 = mean(data1$PREMIO_2009, na.rm=T),
                                Carencia_perct_5_09_AI = mean(data1$Carencia_perct_5_09_AI, na.rm=T),
                                carencia_2009_AF = mean(data1$carencia_2009_AF, na.rm=T),
                                Liderenca_perct_5_09_AI = mean(data1$Liderenca_perct_5_09_AI, na.rm=T),
                                lideranca_2009_AF = mean(data1$lideranca_2009_AF, na.rm=T),
                                Colaboracao_perct_5_09_AI = mean(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                                colaboracao_2009_9_AF = mean(data1$colaboracao_2009_9_AF, na.rm=T),
                                nota_padr_3_2009 = mean(data1$nota_padr_3_2009, na.rm=T),
                                nota_padr_7_2009 = mean(data1$nota_padr_7_2009, na.rm=T),
                                exp_2009 = mean(data1$exp_2009, na.rm=T),
                                cor_recode_09 = mean(data1$cor_recode_09,na.rm=T),
                                max_educ_09 = mean(data1$max_educ_09, na.rm=T),
                                ideb_5_09_new = mean(data1$ideb_5_09_new, na.rm=T),
                                ideb_9_09_new = mean(data1$ideb_9_09_new, na.rm=T),
                                Iderio_2009_3_new = mean(data1$Iderio_2009_3_new, na.rm=T),
                                Iderio2009_7_new = mean(data1$Iderio2009_7_new, na.rm=T),
                                complexidade_esc_09 = mean(data1$complexidade_esc_09, na.rm=T)),
             type="probs")

p4

# Variavies que valores altos sao "boas" no max e variaveis com valores baixo que sao "ruins" no min

p5<-predict (multn5, data.frame(nota_padr_5_2009 = max(data1$nota_padr_5_2009, na.rm=T),
                                nota_padr_9_2009 = max(data1$nota_padr_9_2009, na.rm=T),
                                PREMIO_2009 = max(data1$PREMIO_2009, na.rm=T),
                                Carencia_perct_5_09_AI = min(data1$Carencia_perct_5_09_AI, na.rm=T),
                                carencia_2009_AF = min(data1$carencia_2009_AF, na.rm=T),
                                Liderenca_perct_5_09_AI = max(data1$Liderenca_perct_5_09_AI, na.rm=T),
                                lideranca_2009_AF = max(data1$lideranca_2009_AF, na.rm=T),
                                Colaboracao_perct_5_09_AI = max(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                                colaboracao_2009_9_AF = max(data1$colaboracao_2009_9_AF, na.rm=T),
                                nota_padr_3_2009 = max(data1$nota_padr_3_2009, na.rm=T),
                                nota_padr_7_2009 = max(data1$nota_padr_7_2009, na.rm=T),
                                exp_2009 = max(data1$exp_2009, na.rm=T),
                                cor_recode_09 = max(data1$cor_recode_09,na.rm=T),
                                max_educ_09 = max(data1$max_educ_09, na.rm=T),
                                ideb_5_09_new = min(data1$ideb_5_09_new, na.rm=T),
                                ideb_9_09_new = min(data1$ideb_9_09_new, na.rm=T),
                                Iderio_2009_3_new = min(data1$Iderio_2009_3_new, na.rm=T),
                                Iderio2009_7_new = min(data1$Iderio2009_7_new, na.rm=T),
                                complexidade_esc_09 = min(data1$complexidade_esc_09, na.rm=T)),
             type="probs")

p5

# Variaveis que os valores altos sao "boas" no min e variaveis que sao "ruins" no max

p6<-predict (multn5, data.frame(nota_padr_5_2009 = min(data1$nota_padr_5_2009, na.rm=T),
                                nota_padr_9_2009 = min(data1$nota_padr_9_2009, na.rm=T),
                                PREMIO_2009 = min(data1$PREMIO_2009, na.rm=T),
                                Carencia_perct_5_09_AI = max(data1$Carencia_perct_5_09_AI, na.rm=T),
                                carencia_2009_AF = max(data1$carencia_2009_AF, na.rm=T),
                                Liderenca_perct_5_09_AI = min(data1$Liderenca_perct_5_09_AI, na.rm=T),
                                lideranca_2009_AF = min(data1$lideranca_2009_AF, na.rm=T),
                                Colaboracao_perct_5_09_AI = min(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                                colaboracao_2009_9_AF = min(data1$colaboracao_2009_9_AF, na.rm=T),
                                nota_padr_3_2009 = min(data1$nota_padr_3_2009, na.rm=T),
                                nota_padr_7_2009 = min(data1$nota_padr_7_2009, na.rm=T),
                                exp_2009 = min(data1$exp_2009, na.rm=T),
                                cor_recode_09 = min(data1$cor_recode_09,na.rm=T),
                                max_educ_09 = min(data1$max_educ_09, na.rm=T),
                                ideb_5_09_new = max(data1$ideb_5_09_new, na.rm=T),
                                ideb_9_09_new = max(data1$ideb_9_09_new, na.rm=T),
                                Iderio_2009_3_new = max(data1$Iderio_2009_3_new, na.rm=T),
                                Iderio2009_7_new = max(data1$Iderio2009_7_new, na.rm=T),
                                complexidade_esc_09 = max(data1$complexidade_esc_09, na.rm=T)),
             type="probs")

p6


# Tabela dos valores preditos

tabel1<-data.frame(p1, p2, p3)
tabel2<-data.frame(p4, p5, p6)


colnames(tabel1)[1] <- "M1"
colnames(tabel1)[2] <- "M2"
colnames(tabel1)[3] <- "M3"

colnames(tabel2)[1] <- "M1"
colnames(tabel2)[2] <- "M2"
colnames(tabel2)[3] <- "M3"

tabel<-rbind(tabel1, tabel2)





stargazer(tabel)

# Exponencial dos coeficientes

b1<-exp(coef(multn3))
b2<-exp(coef(multn4))
b3<-exp(coef(multn5))

