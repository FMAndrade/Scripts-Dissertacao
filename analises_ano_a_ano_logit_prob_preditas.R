########################
# Analises ano a ano 1 #
########################


rm(list=ls(all=T))

getwd()

setwd("C:\\Users\\User\\Felipe\\Bases diretores (banco antigo)\\Novas Bases (nov_2014)")

require(foreign)


data1<-read.spss("base_refeita_sem_duplicados_nov_2014_principal.sav", to.data.frame=T)



# Analises bivariadas de troca ano a ano

# 2006-2007 Como as variaveis sao de troca de um ano ao outro, nao faz sentido add variaveis do ano seguinte, so do ano base

#logit

m1 <- glm(troca_06_07 ~ exp_2006 + cor_recode_06 + max_educ_06 + Carencia_perct_5_07_AI + carencia_2007_AF +
            Liderenca_perct_5_07_AI + lideranca_2007_AF + Colaboracao_perct_5_07_AI + colaboracao_2007_9_AF +
            complexidade_esc_07, data = data1,
              family = binomial (link = "logit"))

summary(m1)
plot(m1)

# 2007-2008 (resultado da prova brasil so saiu no meio de 2008)

m2 <- glm(troca_07_08 ~ exp_2006 + cor_recode_06 + max_educ_06 + Carencia_perct_5_07_AI + carencia_2007_AF +
            Liderenca_perct_5_07_AI + lideranca_2007_AF + Colaboracao_perct_5_07_AI + colaboracao_2007_9_AF +
            complexidade_esc_07, data = data1,
          family = binomial (link = "logit"))

summary(m2)


# 2008-2009

m3 <- glm(troca_08_09 ~ nota_padr_5_2007 + nota_padr_9_2007 + Carencia_perct_5_07_AI + carencia_2007_AF +
            Liderenca_perct_5_07_AI + lideranca_2007_AF + Colaboracao_perct_5_07_AI + colaboracao_2007_9_AF + 
            exp_2006 + cor_recode_06 + max_educ_06 + ideb_5_07_new + ideb_9_07_new + complexidade_esc_07, data = data1,
          family = binomial (link = "logit"))

summary(m3)


# 2009-2010


m4 <- glm(troca_09_10 ~ Carencia_perct_5_09_AI + carencia_2009_AF + Liderenca_perct_5_09_AI + lideranca_2009_AF + 
            Colaboracao_perct_5_09_AI + colaboracao_2009_9_AF + exp_2009 + cor_recode_09 + max_educ_09 + complexidade_esc_09
          , data = data1, family = binomial (link = "logit"))

summary(m4)


# 2010-2011

m5 <- glm(troca_10_11 ~ nota_padr_5_2009 + nota_padr_9_2009 + PREMIO_2009 + Carencia_perct_5_09_AI + carencia_2009_AF + 
            Liderenca_perct_5_09_AI + lideranca_2009_AF + Colaboracao_perct_5_09_AI + colaboracao_2009_9_AF + nota_padr_3_2009 +
            nota_padr_7_2009 + exp_2009 + cor_recode_09 + max_educ_09 + ideb_5_09_new + ideb_9_09_new + Iderio_2009_3_new + 
            Iderio2009_7_new + complexidade_esc_09, data=data1, family = binomial (link = "logit"))


summary(m5)


# 2011-2012

m6 <- glm(troca_11_12 ~ nota_padr_5_2009 + nota_padr_9_2009 + PREMIO_2009 + Carencia_perct_5_09_AI + carencia_2009_AF + 
            Liderenca_perct_5_09_AI + lideranca_2009_AF + Colaboracao_perct_5_09_AI + colaboracao_2009_9_AF + nota_padr_3_2009 +
            nota_padr_7_2009 + exp_2009 + cor_recode_09 + max_educ_09 + ideb_5_09_new + ideb_9_09_new + Iderio_2009_3_new + 
            Iderio2009_7_new + complexidade_esc_09, data=data1, family = binomial (link = "logit"))


summary(m6)


# Probabilidades preditas em cada modelo

# Todas as Variaveis mantidas na media

# 2006-2007

pp1 <- predict (m1, data.frame(exp_2006 = mean (data1$exp_2006, na.rm=T),
                            cor_recode_06 = mean (data1$cor_recode_06, na.rm=T),
                            max_educ_06 = mean (data1$max_educ_06, na.rm=T),
                            Carencia_perct_5_07_AI = mean (data1$Carencia_perct_5_07_AI, na.rm=T),
                            carencia_2007_AF = mean (data1$carencia_2007_AF, na.rm=T),
                            Liderenca_perct_5_07_AI = mean (data1$Liderenca_perct_5_07_AI, na.rm=T),
                            lideranca_2007_AF = mean (data1$lideranca_2007_AF, na.rm=T),
                            Colaboracao_perct_5_07_AI = mean(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                            colaboracao_2007_9_AF = mean (data1$colaboracao_2007_9_AF, na.rm=T),
                            complexidade_esc_07 = mean (data1$complexidade_esc_07, na.rm=T)),
         type="response")

pp1


# 2007-2008

pp2 <- predict (m2, data.frame(exp_2006 = mean (data1$exp_2006, na.rm=T),
                               cor_recode_06 = mean (data1$cor_recode_06, na.rm=T),
                               max_educ_06 = mean (data1$max_educ_06, na.rm=T),
                               Carencia_perct_5_07_AI = mean (data1$Carencia_perct_5_07_AI, na.rm=T),
                               carencia_2007_AF = mean (data1$carencia_2007_AF, na.rm=T),
                               Liderenca_perct_5_07_AI = mean (data1$Liderenca_perct_5_07_AI, na.rm=T),
                               lideranca_2007_AF = mean (data1$lideranca_2007_AF, na.rm=T),
                               Colaboracao_perct_5_07_AI = mean(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                               colaboracao_2007_9_AF = mean (data1$colaboracao_2007_9_AF, na.rm=T),
                               complexidade_esc_07 = mean (data1$complexidade_esc_07, na.rm=T)),
                type="response")

pp2


# 2008 - 2009


pp3 <- predict (m3, data.frame(exp_2006 = mean (data1$exp_2006, na.rm=T),
                               cor_recode_06 = mean (data1$cor_recode_06, na.rm=T),
                               max_educ_06 = mean (data1$max_educ_06, na.rm=T),
                               Carencia_perct_5_07_AI = mean (data1$Carencia_perct_5_07_AI, na.rm=T),
                               carencia_2007_AF = mean (data1$carencia_2007_AF, na.rm=T),
                               Liderenca_perct_5_07_AI = mean (data1$Liderenca_perct_5_07_AI, na.rm=T),
                               lideranca_2007_AF = mean (data1$lideranca_2007_AF, na.rm=T),
                               Colaboracao_perct_5_07_AI = mean(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                               colaboracao_2007_9_AF = mean (data1$colaboracao_2007_9_AF, na.rm=T),
                               complexidade_esc_07 = mean (data1$complexidade_esc_07, na.rm=T),
                               nota_padr_5_2007 = mean (data1$nota_padr_5_2007, na.rm=T),
                               nota_padr_9_2007 = mean (data1$nota_padr_9_2007, na.rm=T),
                               ideb_5_07_new  = mean (data1$ideb_5_07_new , na.rm=T),
                               ideb_9_07_new  = mean (data1$ideb_9_07_new , na.rm=T)),
                type="response")

pp3

# 2009-2010

pp4 <- predict (m4, data.frame(exp_2009 = mean (data1$exp_2009, na.rm=T),
                               cor_recode_09 = mean (data1$cor_recode_09, na.rm=T),
                               max_educ_09 = mean (data1$max_educ_09, na.rm=T),
                               Carencia_perct_5_09_AI = mean (data1$Carencia_perct_5_09_AI, na.rm=T),
                               carencia_2009_AF = mean (data1$carencia_2009_AF, na.rm=T),
                               Liderenca_perct_5_09_AI = mean (data1$Liderenca_perct_5_09_AI, na.rm=T),
                               lideranca_2009_AF = mean (data1$lideranca_2009_AF, na.rm=T),
                               Colaboracao_perct_5_09_AI = mean(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                               colaboracao_2009_9_AF = mean (data1$colaboracao_2009_9_AF, na.rm=T),
                               complexidade_esc_09 = mean (data1$complexidade_esc_09, na.rm=T)),
                type="response")

pp4

# 2010-2011

pp5 <- predict (m5, data.frame(exp_2009 = mean (data1$exp_2009, na.rm=T),
                               cor_recode_09 = mean (data1$cor_recode_09, na.rm=T),
                               max_educ_09 = mean (data1$max_educ_09, na.rm=T),
                               Carencia_perct_5_09_AI = mean (data1$Carencia_perct_5_09_AI, na.rm=T),
                               carencia_2009_AF = mean (data1$carencia_2009_AF, na.rm=T),
                               Liderenca_perct_5_09_AI = mean (data1$Liderenca_perct_5_09_AI, na.rm=T),
                               lideranca_2009_AF = mean (data1$lideranca_2009_AF, na.rm=T),
                               Colaboracao_perct_5_09_AI = mean(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                               colaboracao_2009_9_AF = mean (data1$colaboracao_2009_9_AF, na.rm=T),
                               complexidade_esc_09 = mean (data1$complexidade_esc_09, na.rm=T),
                               nota_padr_5_2009 = mean (data1$nota_padr_5_2009, na.rm=T),
                               nota_padr_9_2009 = mean (data1$nota_padr_9_2009, na.rm=T),
                               ideb_5_09_new  = mean (data1$ideb_5_09_new , na.rm=T),
                               ideb_9_09_new  = mean (data1$ideb_9_09_new , na.rm=T),
                               PREMIO_2009  = mean (data1$PREMIO_2009 , na.rm=T),
                               nota_padr_3_2009  = mean (data1$nota_padr_3_2009 , na.rm=T),
                               nota_padr_7_2009  = mean (data1$nota_padr_7_2009 , na.rm=T),
                               Iderio_2009_3_new  = mean (data1$Iderio_2009_3_new , na.rm=T),
                               Iderio2009_7_new  = mean (data1$Iderio2009_7_new , na.rm=T)),
                type="response")

pp5
                
# 2011- 2012

pp6 <- predict (m6, data.frame(exp_2009 = mean (data1$exp_2009, na.rm=T),
                               cor_recode_09 = mean (data1$cor_recode_09, na.rm=T),
                               max_educ_09 = mean (data1$max_educ_09, na.rm=T),
                               Carencia_perct_5_09_AI = mean (data1$Carencia_perct_5_09_AI, na.rm=T),
                               carencia_2009_AF = mean (data1$carencia_2009_AF, na.rm=T),
                               Liderenca_perct_5_09_AI = mean (data1$Liderenca_perct_5_09_AI, na.rm=T),
                               lideranca_2009_AF = mean (data1$lideranca_2009_AF, na.rm=T),
                               Colaboracao_perct_5_09_AI = mean(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                               colaboracao_2009_9_AF = mean (data1$colaboracao_2009_9_AF, na.rm=T),
                               complexidade_esc_09 = mean (data1$complexidade_esc_09, na.rm=T),
                               nota_padr_5_2009 = mean (data1$nota_padr_5_2009, na.rm=T),
                               nota_padr_9_2009 = mean (data1$nota_padr_9_2009, na.rm=T),
                               ideb_5_09_new  = mean (data1$ideb_5_09_new , na.rm=T),
                               ideb_9_09_new  = mean (data1$ideb_9_09_new , na.rm=T),
                               PREMIO_2009  = mean (data1$PREMIO_2009 , na.rm=T),
                               nota_padr_3_2009  = mean (data1$nota_padr_3_2009 , na.rm=T),
                               nota_padr_7_2009  = mean (data1$nota_padr_7_2009 , na.rm=T),
                               Iderio_2009_3_new  = mean (data1$Iderio_2009_3_new , na.rm=T),
                               Iderio2009_7_new  = mean (data1$Iderio2009_7_new , na.rm=T)),
                               type="response")

pp6



# Variaveis favoraveis no maximo e desfavoraveis no minimo

# 2006-2007

mm1 <- predict (m1, data.frame(exp_2006 = max (data1$exp_2006, na.rm=T),
                               cor_recode_06 = max (data1$cor_recode_06, na.rm=T),
                               max_educ_06 = max (data1$max_educ_06, na.rm=T),
                               Carencia_perct_5_07_AI = min (data1$Carencia_perct_5_07_AI, na.rm=T),
                               carencia_2007_AF = min (data1$carencia_2007_AF, na.rm=T),
                               Liderenca_perct_5_07_AI = max (data1$Liderenca_perct_5_07_AI, na.rm=T),
                               lideranca_2007_AF = max (data1$lideranca_2007_AF, na.rm=T),
                               Colaboracao_perct_5_07_AI = max(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                               colaboracao_2007_9_AF = max (data1$colaboracao_2007_9_AF, na.rm=T),
                               complexidade_esc_07 = min (data1$complexidade_esc_07, na.rm=T)),
                type="response")

mm1


# 2007-2008

mm2 <- predict (m2, data.frame(exp_2006 = max (data1$exp_2006, na.rm=T),
                               cor_recode_06 = max (data1$cor_recode_06, na.rm=T),
                               max_educ_06 = max (data1$max_educ_06, na.rm=T),
                               Carencia_perct_5_07_AI = min (data1$Carencia_perct_5_07_AI, na.rm=T),
                               carencia_2007_AF = min (data1$carencia_2007_AF, na.rm=T),
                               Liderenca_perct_5_07_AI = max (data1$Liderenca_perct_5_07_AI, na.rm=T),
                               lideranca_2007_AF = max (data1$lideranca_2007_AF, na.rm=T),
                               Colaboracao_perct_5_07_AI = max(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                               colaboracao_2007_9_AF = max (data1$colaboracao_2007_9_AF, na.rm=T),
                               complexidade_esc_07 = min (data1$complexidade_esc_07, na.rm=T)),
                type="response")

mm2


# 2008 - 2009


mm3 <- predict (m3, data.frame(exp_2006 = max (data1$exp_2006, na.rm=T),
                               cor_recode_06 = max (data1$cor_recode_06, na.rm=T),
                               max_educ_06 = max (data1$max_educ_06, na.rm=T),
                               Carencia_perct_5_07_AI = min (data1$Carencia_perct_5_07_AI, na.rm=T),
                               carencia_2007_AF = min (data1$carencia_2007_AF, na.rm=T),
                               Liderenca_perct_5_07_AI = max (data1$Liderenca_perct_5_07_AI, na.rm=T),
                               lideranca_2007_AF = max (data1$lideranca_2007_AF, na.rm=T),
                               Colaboracao_perct_5_07_AI = max(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                               colaboracao_2007_9_AF = max (data1$colaboracao_2007_9_AF, na.rm=T),
                               complexidade_esc_07 = min (data1$complexidade_esc_07, na.rm=T),
                               nota_padr_5_2007 = max (data1$nota_padr_5_2007, na.rm=T),
                               nota_padr_9_2007 = max (data1$nota_padr_9_2007, na.rm=T),
                               ideb_5_07_new  = min (data1$ideb_5_07_new , na.rm=T),
                               ideb_9_07_new  = min (data1$ideb_9_07_new , na.rm=T)),
                type="response")

mm3

# 2009-2010

mm4 <- predict (m4, data.frame(exp_2009 = max (data1$exp_2009, na.rm=T),
                               cor_recode_09 = max (data1$cor_recode_09, na.rm=T),
                               max_educ_09 = max (data1$max_educ_09, na.rm=T),
                               Carencia_perct_5_09_AI = min (data1$Carencia_perct_5_09_AI, na.rm=T),
                               carencia_2009_AF = min (data1$carencia_2009_AF, na.rm=T),
                               Liderenca_perct_5_09_AI = max (data1$Liderenca_perct_5_09_AI, na.rm=T),
                               lideranca_2009_AF = max (data1$lideranca_2009_AF, na.rm=T),
                               Colaboracao_perct_5_09_AI = max(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                               colaboracao_2009_9_AF = max (data1$colaboracao_2009_9_AF, na.rm=T),
                               complexidade_esc_09 = min (data1$complexidade_esc_09, na.rm=T)),
                type="response")

mm4

# 2010-2011

mm5 <- predict (m5, data.frame(exp_2009 = max (data1$exp_2009, na.rm=T),
                               cor_recode_09 = max (data1$cor_recode_09, na.rm=T),
                               max_educ_09 = max (data1$max_educ_09, na.rm=T),
                               Carencia_perct_5_09_AI = min (data1$Carencia_perct_5_09_AI, na.rm=T),
                               carencia_2009_AF = min (data1$carencia_2009_AF, na.rm=T),
                               Liderenca_perct_5_09_AI = max (data1$Liderenca_perct_5_09_AI, na.rm=T),
                               lideranca_2009_AF = max (data1$lideranca_2009_AF, na.rm=T),
                               Colaboracao_perct_5_09_AI = max(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                               colaboracao_2009_9_AF = max (data1$colaboracao_2009_9_AF, na.rm=T),
                               complexidade_esc_09 = min (data1$complexidade_esc_09, na.rm=T),
                               nota_padr_5_2009 = max (data1$nota_padr_5_2009, na.rm=T),
                               nota_padr_9_2009 = max (data1$nota_padr_9_2009, na.rm=T),
                               ideb_5_09_new  = min (data1$ideb_5_09_new , na.rm=T),
                               ideb_9_09_new  = min (data1$ideb_9_09_new , na.rm=T),
                               PREMIO_2009  = max (data1$PREMIO_2009 , na.rm=T),
                               nota_padr_3_2009  = max (data1$nota_padr_3_2009 , na.rm=T),
                               nota_padr_7_2009  = max (data1$nota_padr_7_2009 , na.rm=T),
                               Iderio_2009_3_new  = min (data1$Iderio_2009_3_new , na.rm=T),
                               Iderio2009_7_new  = min (data1$Iderio2009_7_new , na.rm=T)),
                type="response")

mm5

# 2011- 2012

mm6 <- predict (m6, data.frame(exp_2009 = max (data1$exp_2009, na.rm=T),
                               cor_recode_09 = max (data1$cor_recode_09, na.rm=T),
                               max_educ_09 = max (data1$max_educ_09, na.rm=T),
                               Carencia_perct_5_09_AI = min (data1$Carencia_perct_5_09_AI, na.rm=T),
                               carencia_2009_AF = min (data1$carencia_2009_AF, na.rm=T),
                               Liderenca_perct_5_09_AI = max (data1$Liderenca_perct_5_09_AI, na.rm=T),
                               lideranca_2009_AF = max (data1$lideranca_2009_AF, na.rm=T),
                               Colaboracao_perct_5_09_AI = max(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                               colaboracao_2009_9_AF = max (data1$colaboracao_2009_9_AF, na.rm=T),
                               complexidade_esc_09 = min (data1$complexidade_esc_09, na.rm=T),
                               nota_padr_5_2009 = max (data1$nota_padr_5_2009, na.rm=T),
                               nota_padr_9_2009 = max (data1$nota_padr_9_2009, na.rm=T),
                               ideb_5_09_new  = min (data1$ideb_5_09_new , na.rm=T),
                               ideb_9_09_new  = min (data1$ideb_9_09_new , na.rm=T),
                               PREMIO_2009  = max (data1$PREMIO_2009 , na.rm=T),
                               nota_padr_3_2009  = max (data1$nota_padr_3_2009 , na.rm=T),
                               nota_padr_7_2009  = max (data1$nota_padr_7_2009 , na.rm=T),
                               Iderio_2009_3_new  = min (data1$Iderio_2009_3_new , na.rm=T),
                               Iderio2009_7_new  = min (data1$Iderio2009_7_new , na.rm=T)),
                type="response")

mm6


# Variaveis favoraveis no minimo e desfavoraveis no maximo

# 2006-2007

bb1 <- predict (m1, data.frame(exp_2006 = min (data1$exp_2006, na.rm=T),
                               cor_recode_06 = min (data1$cor_recode_06, na.rm=T),
                               max_educ_06 = min (data1$max_educ_06, na.rm=T),
                               Carencia_perct_5_07_AI = max (data1$Carencia_perct_5_07_AI, na.rm=T),
                               carencia_2007_AF = max (data1$carencia_2007_AF, na.rm=T),
                               Liderenca_perct_5_07_AI = min (data1$Liderenca_perct_5_07_AI, na.rm=T),
                               lideranca_2007_AF = min (data1$lideranca_2007_AF, na.rm=T),
                               Colaboracao_perct_5_07_AI = min(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                               colaboracao_2007_9_AF = min (data1$colaboracao_2007_9_AF, na.rm=T),
                               complexidade_esc_07 = max (data1$complexidade_esc_07, na.rm=T)),
                type="response")

bb1


# 2007-2008

bb2 <- predict (m2, data.frame(exp_2006 = min (data1$exp_2006, na.rm=T),
                               cor_recode_06 = min (data1$cor_recode_06, na.rm=T),
                               max_educ_06 = min (data1$max_educ_06, na.rm=T),
                               Carencia_perct_5_07_AI = max (data1$Carencia_perct_5_07_AI, na.rm=T),
                               carencia_2007_AF = max (data1$carencia_2007_AF, na.rm=T),
                               Liderenca_perct_5_07_AI = min (data1$Liderenca_perct_5_07_AI, na.rm=T),
                               lideranca_2007_AF = min (data1$lideranca_2007_AF, na.rm=T),
                               Colaboracao_perct_5_07_AI = min(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                               colaboracao_2007_9_AF = min (data1$colaboracao_2007_9_AF, na.rm=T),
                               complexidade_esc_07 = max (data1$complexidade_esc_07, na.rm=T)),
                type="response")

bb2


# 2008 - 2009


bb3 <- predict (m3, data.frame(exp_2006 = min (data1$exp_2006, na.rm=T),
                               cor_recode_06 = min (data1$cor_recode_06, na.rm=T),
                               max_educ_06 = min (data1$max_educ_06, na.rm=T),
                               Carencia_perct_5_07_AI = max (data1$Carencia_perct_5_07_AI, na.rm=T),
                               carencia_2007_AF = max (data1$carencia_2007_AF, na.rm=T),
                               Liderenca_perct_5_07_AI = min (data1$Liderenca_perct_5_07_AI, na.rm=T),
                               lideranca_2007_AF = min (data1$lideranca_2007_AF, na.rm=T),
                               Colaboracao_perct_5_07_AI = min(data1$Colaboracao_perct_5_07_AI, na.rm=T),
                               colaboracao_2007_9_AF = min (data1$colaboracao_2007_9_AF, na.rm=T),
                               complexidade_esc_07 = max (data1$complexidade_esc_07, na.rm=T),
                               nota_padr_5_2007 = min (data1$nota_padr_5_2007, na.rm=T),
                               nota_padr_9_2007 = min (data1$nota_padr_9_2007, na.rm=T),
                               ideb_5_07_new  = max (data1$ideb_5_07_new , na.rm=T),
                               ideb_9_07_new  = max (data1$ideb_9_07_new , na.rm=T)),
                type="response")

bb3

# 2009-2010

bb4 <- predict (m4, data.frame(exp_2009 = min (data1$exp_2009, na.rm=T),
                               cor_recode_09 = min (data1$cor_recode_09, na.rm=T),
                               max_educ_09 = min (data1$max_educ_09, na.rm=T),
                               Carencia_perct_5_09_AI = max (data1$Carencia_perct_5_09_AI, na.rm=T),
                               carencia_2009_AF = max (data1$carencia_2009_AF, na.rm=T),
                               Liderenca_perct_5_09_AI = min (data1$Liderenca_perct_5_09_AI, na.rm=T),
                               lideranca_2009_AF = min (data1$lideranca_2009_AF, na.rm=T),
                               Colaboracao_perct_5_09_AI = min(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                               colaboracao_2009_9_AF = min (data1$colaboracao_2009_9_AF, na.rm=T),
                               complexidade_esc_09 = max (data1$complexidade_esc_09, na.rm=T)),
                type="response")

bb4

# 2010-2011

bb5 <- predict (m5, data.frame(exp_2009 = min (data1$exp_2009, na.rm=T),
                               cor_recode_09 = min (data1$cor_recode_09, na.rm=T),
                               max_educ_09 = min (data1$max_educ_09, na.rm=T),
                               Carencia_perct_5_09_AI = max (data1$Carencia_perct_5_09_AI, na.rm=T),
                               carencia_2009_AF = max (data1$carencia_2009_AF, na.rm=T),
                               Liderenca_perct_5_09_AI = min (data1$Liderenca_perct_5_09_AI, na.rm=T),
                               lideranca_2009_AF = min (data1$lideranca_2009_AF, na.rm=T),
                               Colaboracao_perct_5_09_AI = min(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                               colaboracao_2009_9_AF = min (data1$colaboracao_2009_9_AF, na.rm=T),
                               complexidade_esc_09 = max (data1$complexidade_esc_09, na.rm=T),
                               nota_padr_5_2009 = min (data1$nota_padr_5_2009, na.rm=T),
                               nota_padr_9_2009 = min (data1$nota_padr_9_2009, na.rm=T),
                               ideb_5_09_new  = max (data1$ideb_5_09_new , na.rm=T),
                               ideb_9_09_new  = max (data1$ideb_9_09_new , na.rm=T),
                               PREMIO_2009  = min (data1$PREMIO_2009 , na.rm=T),
                               nota_padr_3_2009  = min (data1$nota_padr_3_2009 , na.rm=T),
                               nota_padr_7_2009  = min (data1$nota_padr_7_2009 , na.rm=T),
                               Iderio_2009_3_new  = max (data1$Iderio_2009_3_new , na.rm=T),
                               Iderio2009_7_new  = max (data1$Iderio2009_7_new , na.rm=T)),
                type="response")

bb5

# 2011- 2012

bb6 <- predict (m6, data.frame(exp_2009 = min (data1$exp_2009, na.rm=T),
                               cor_recode_09 = min (data1$cor_recode_09, na.rm=T),
                               max_educ_09 = min (data1$max_educ_09, na.rm=T),
                               Carencia_perct_5_09_AI = max (data1$Carencia_perct_5_09_AI, na.rm=T),
                               carencia_2009_AF = max (data1$carencia_2009_AF, na.rm=T),
                               Liderenca_perct_5_09_AI = min (data1$Liderenca_perct_5_09_AI, na.rm=T),
                               lideranca_2009_AF = min (data1$lideranca_2009_AF, na.rm=T),
                               Colaboracao_perct_5_09_AI = min(data1$Colaboracao_perct_5_09_AI, na.rm=T),
                               colaboracao_2009_9_AF = min (data1$colaboracao_2009_9_AF, na.rm=T),
                               complexidade_esc_09 = max (data1$complexidade_esc_09, na.rm=T),
                               nota_padr_5_2009 = min (data1$nota_padr_5_2009, na.rm=T),
                               nota_padr_9_2009 = min (data1$nota_padr_9_2009, na.rm=T),
                               ideb_5_09_new  = max (data1$ideb_5_09_new , na.rm=T),
                               ideb_9_09_new  = max (data1$ideb_9_09_new , na.rm=T),
                               PREMIO_2009  = min (data1$PREMIO_2009 , na.rm=T),
                               nota_padr_3_2009  = min (data1$nota_padr_3_2009 , na.rm=T),
                               nota_padr_7_2009  = min (data1$nota_padr_7_2009 , na.rm=T),
                               Iderio_2009_3_new  = max (data1$Iderio_2009_3_new , na.rm=T),
                               Iderio2009_7_new  = max (data1$Iderio2009_7_new , na.rm=T)),
                type="response")

bb6


# Base de dados com as probabilidades preditas


new1 <- data.frame(pp1, pp2, pp3, pp4, pp5, pp6)
new2 <- data.frame(mm1, mm2, mm3, mm4, mm5, mm6)
new3 <- data.frame(bb1, bb2, bb3, bb4, bb5, bb6)

total<-rbind(new1, new2, new3)

colnames(new1)[1]<-"2006-2007"
colnames(new1)[2]<-"2007-2008"
colnames(new1)[3]<-"2008-2009"
colnames(new1)[4]<-"2009-2010"
colnames(new1)[5]<-"2010-2011"
colnames(new1)[6]<-"2011-2012"

colnames(new2)[1]<-"2006-2007"
colnames(new2)[2]<-"2007-2008"
colnames(new2)[3]<-"2008-2009"
colnames(new2)[4]<-"2009-2010"
colnames(new2)[5]<-"2010-2011"
colnames(new2)[6]<-"2011-2012"

colnames(new3)[1]<-"2006-2007"
colnames(new3)[2]<-"2007-2008"
colnames(new3)[3]<-"2008-2009"
colnames(new3)[4]<-"2009-2010"
colnames(new3)[5]<-"2010-2011"
colnames(new3)[6]<-"2011-2012"


# Fazendo relatorios


library(stargazer)

stargazer(m1, m2, m3, m4, m5, m6)
stargazer(m1, m2, m3)
stargazer(m4, m5, m6)


# Exponencial dos resultados

exp(coef(m1))
exp(coef(m2))
exp(coef(m3))
exp(coef(m4))
exp(coef(m5))
exp(coef(m6))

