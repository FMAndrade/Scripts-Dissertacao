################################
# Analises ano a ano 2 #########
# Modelos Finais ###############
################################


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

m1 <- glm(troca_06_07 ~ exp_2006 + max_educ_06 + Colaboracao_cat_07 + Carencia_cat_07 + 
            complexidade_esc_07 + nota_padr_5_2005 + ideb_5_05_new, data = data1,
          family = binomial (link = "logit"))

summary(m1)

vif(m1)

plot(m1)

# 2007-2008 (resultado da prova brasil so saiu no meio de 2008)

m2 <- glm(troca_07_08 ~ exp_2007 + max_educ_07 + Carencia_cat_07  +
            Colaboracao_cat_07 + complexidade_esc_07 + nota_padr_5_2005 + ideb_5_05_new, 
          data = data1,family = binomial (link = "logit"))

summary(m2)
vif(m2)

# 2008-2009

m3 <- glm(troca_08_09 ~ media_np_05_07_AI + Carencia_cat_07 +
            Colaboracao_cat_07 + exp_2008 + max_educ_08 + ideb_05_07_AI_AF_pior +
            complexidade_esc_07, data = data1, family = binomial (link = "logit"))

summary(m3)
vif(m3)

# 2009-2010


m4 <- glm(troca_09_10 ~ Colaboracao_cat_09 + Carencia_cat_09 + exp_2009
          + max_educ_09 + complexidade_esc_09 + media_np_05_07_AI + ideb_05_07_AI_AF_pior,  
          data = data1, family = binomial (link = "logit"))

summary(m4)
vif(m4)

# 2010-2011

m5 <- glm(troca_10_11 ~ nota_padr_5_2009 + nota_padr_3_2010 + nota_padr_3_2009 + PREMIO_2009 +
            Colaboracao_cat_09 + exp_2010 + max_educ_10 + Carencia_cat_09 
            + complexidade_esc_10 + sum_ideb_iderio_09_10_pior,
          data=data1, family = binomial (link = "logit"))


summary(m5)
vif(m5)

# 2011-2012

m6 <- glm(troca_11_12 ~ nota_padr_5_2009 + nota_padr_3_2010 + nota_padr_3_2009 + PREMIO_2009 +
            Colaboracao_cat_09 + exp_2011 + max_educ_11 + Carencia_cat_09 
          + complexidade_esc_11 + sum_ideb_iderio_09_10_pior,
          data=data1, family = binomial (link = "logit"))


summary(m6)
vif(m6)




########################
# Anos Finais ##########
########################




# 2006-2007

m7 <- glm(troca_06_07 ~ exp_2006 + max_educ_06 + Carencia_cat_07 + Colaboracao_cat_07 +
            complexidade_esc_07 + nota_padr_9_2005 + ideb_9_05_new,
          data = data1,family = binomial (link = "logit"))

summary(m7)
vif(m7)


# 2007-2008 (resultado da prova brasil so saiu no meio de 2008)

m8 <- glm(troca_07_08 ~ exp_2007 + max_educ_07 + Carencia_cat_07 + Colaboracao_cat_07 +
            complexidade_esc_07 + nota_padr_9_2005 + ideb_9_05_new,
          data = data1,family = binomial (link = "logit"))

summary(m8)
vif(m8)

# 2008-2009

m9 <- glm(troca_08_09 ~ media_np_05_07_AF + Carencia_cat_07 + Colaboracao_cat_07 + 
            exp_2008 + max_educ_08 + complexidade_esc_07 + ideb_05_07_AI_AF_pior, data = data1,
          family = binomial (link = "logit"))

summary(m9)
vif(m9)

# 2009-2010


m10 <- glm(troca_09_10 ~ Carencia_cat_09 +  Colaboracao_cat_09 + exp_2009
           + max_educ_09 + complexidade_esc_09 + media_np_05_07_AF + ideb_05_07_AI_AF_pior,
           data = data1, family = binomial (link = "logit"))

summary(m10)
vif(m10)

# 2010-2011

m11 <- glm(troca_10_11 ~ nota_padr_9_2009 + nota_padr_7_2010 + nota_padr_7_2009 + PREMIO_2009
              + exp_2010 + max_educ_10 + complexidade_esc_10 + Carencia_cat_09 +  Colaboracao_cat_09
              + sum_ideb_iderio_09_10_pior, data=data1, family = binomial (link = "logit"))


summary(m11)
vif(m11)


# 2011-2012

m12 <- glm(troca_11_12 ~ nota_padr_9_2009 + nota_padr_7_2010 + nota_padr_7_2009 + PREMIO_2009
           + exp_2011 + max_educ_11 + complexidade_esc_11 + Carencia_cat_09 +  Colaboracao_cat_09
           + sum_ideb_iderio_09_10_pior, data=data1, family = binomial (link = "logit"))

summary(m12)
vif(m12)



######################################
# Relatórios ano a ano ###############
######################################


# Anos Iniciais


library(stargazer)


stargazer(m1,m2,m3)
stargazer(m4,m5,m6)



# Anos Finais

stargazer(m7,m8,m9)
stargazer(m10,m11,m12)





