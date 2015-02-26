########################
# Analises ano a ano 2 #
########################


rm(list=ls(all=T))

getwd()

setwd("C:\\Users\\User\\Felipe\\Bases diretores (banco antigo)\\Novas Bases (nov_2014)")

require(foreign)
require(car)

data1<-read.spss("base_refeita_sem_duplicados_nov_2014_principal.sav", to.data.frame=T)


#####################################################################

# Dividindo as análises em duas partes (anos inciais e anos finais)

#####################################################################



#####################
# Anos Iniciais #####
#####################

# logit

# 2006-2007

m1 <- glm(troca_06_07 ~ exp_2006 + cor_recode_06 + max_educ_06 + Carencia_perct_5_07_AI + Colaboracao_perct_5_07_AI + 
            complexidade_esc_07 + nota_padr_5_2005 + ideb_5_05_new, data = data1,
          family = binomial (link = "logit"))

summary(m1)

vif(m1)

plot(m1)

# 2007-2008 (resultado da prova brasil so saiu no meio de 2008)

m2 <- glm(troca_07_08 ~ exp_2007 + cor_recode_07 + max_educ_07 + Carencia_perct_5_07_AI +
           Colaboracao_perct_5_07_AI + complexidade_esc_07 + nota_padr_5_2005 + ideb_5_05_new, 
          data = data1,family = binomial (link = "logit"))

summary(m2)
vif(m2)

# 2008-2009

m3 <- glm(troca_08_09 ~ nota_padr_5_2007 + Carencia_perct_5_07_AI +
            Colaboracao_perct_5_07_AI + exp_2008 + cor_recode_08 + max_educ_08 + ideb_5_07_new +
            complexidade_esc_07, data = data1, family = binomial (link = "logit"))

summary(m3)
vif(m3)

# 2009-2010


m4 <- glm(troca_09_10 ~ Carencia_perct_5_09_AI + Colaboracao_perct_5_09_AI + exp_2009 + cor_recode_09
          + max_educ_09 + complexidade_esc_09 + nota_padr_5_2007 + ideb_5_07_new,  
          data = data1, family = binomial (link = "logit"))

summary(m4)
vif(m4)

# 2010-2011

m5 <- glm(troca_10_11 ~ nota_padr_5_2009 + PREMIO_2009 + Carencia_perct_5_09_AI + pior_ind_ideb_iderio_09_10_AI +
            Colaboracao_perct_5_09_AI + nota_padr_3_2010 + exp_2010 + cor_recode_10 + max_educ_10 + 
             + complexidade_esc_10 + pior_ideb_iderio_09_09_AI + nota_padr_3_2009,
          data=data1, family = binomial (link = "logit"))


summary(m5)
vif(m5)

# 2011-2012

m6 <- glm(troca_11_12 ~ nota_padr_5_2009 + PREMIO_2009 + Carencia_perct_5_09_AI + pior_ind_ideb_iderio_09_10_AI +
           Colaboracao_perct_5_09_AI + nota_padr_3_2010 + exp_2011 + cor_recode_11 + max_educ_11
          + complexidade_esc_11 + pior_ideb_iderio_09_09_AI + nota_padr_3_2009,
          data=data1, family = binomial (link = "logit"))


summary(m6)
vif(m6)




########################
# Anos Finais ##########
########################


#logit

# 2006-2007

m7 <- glm(troca_06_07 ~ exp_2006 + cor_recode_06 + max_educ_06 + carencia_2007_AF + colaboracao_2007_9_AF +
            complexidade_esc_07 + nota_padr_9_2005 + ideb_9_05_new,
          data = data1,family = binomial (link = "logit"))

summary(m7)
vif(m7)


# 2007-2008 (resultado da prova brasil so saiu no meio de 2008)

m8 <- glm(troca_07_08 ~ exp_2007 + cor_recode_07 + max_educ_07 + carencia_2007_AF + colaboracao_2007_9_AF +
            complexidade_esc_07 + nota_padr_9_2005 + ideb_9_05_new,
          data = data1,family = binomial (link = "logit"))

summary(m8)
vif(m8)

# 2008-2009

m9 <- glm(troca_08_09 ~ nota_padr_9_2007 + carencia_2007_AF + colaboracao_2007_9_AF + 
            exp_2008 + cor_recode_08 + max_educ_08 + ideb_9_07_new + complexidade_esc_07, data = data1,
          family = binomial (link = "logit"))

summary(m9)
vif(m9)

# 2009-2010


m10 <- glm(troca_09_10 ~ carencia_2009_AF +  colaboracao_2009_9_AF + exp_2009 + cor_recode_09
          + max_educ_09 + complexidade_esc_09 + nota_padr_9_2007 + ideb_9_07_new,
          data = data1, family = binomial (link = "logit"))

summary(m10)
vif(m10)

# 2010-2011

m11 <- glm(troca_10_11 ~ nota_padr_9_2009 + PREMIO_2009 + carencia_2009_AF + colaboracao_2009_9_AF +
        nota_padr_7_2010 + exp_2010 + cor_recode_10 + max_educ_10 + complexidade_esc_10 +
          nota_padr_7_2009 + pior_ind_ideb_iderio_09_10_AF + pior_ideb_iderio_09_09_AF,
          data=data1, family = binomial (link = "logit"))


summary(m11)
vif(m11)


# 2011-2012

m12 <- glm(troca_11_12 ~ nota_padr_9_2009 + PREMIO_2009 + carencia_2009_AF + colaboracao_2009_9_AF +
            nota_padr_7_2010 + exp_2011 + cor_recode_11 + max_educ_11 + pior_ind_ideb_iderio_09_10_AF +
            complexidade_esc_11 + nota_padr_7_2009 + pior_ideb_iderio_09_09_AF,
           data=data1, family = binomial (link = "logit"))


summary(m12)
vif(m12)




##################################
# Relatorios dos modelos #########
##################################

require(stargazer)


###################
# Momento 1 e 2 ###
###################


stargazer (m1, m2, m3)
stargazer (m4, m5, m6)
stargazer(m7, m8, m9)
stargazer(m10, m11, m12)
