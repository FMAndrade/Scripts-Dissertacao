###########################################
# Novas analises por Mandato 3 ############
# Variaveis Novas (Felipe e Mariane) ######
# media do ideb 2005 e 2007 ###############
# var de pior ind para os dois seg ########
###########################################
###########################################


# As analises abaixos compreendem as variaveis de entrada do diretor, ou seja, no mandato de 2006 a 2008. Sao inseridas
# as variaveis de 2006, e no mandato de 2009 a 2011, as variaveis de 2009.


rm(list=ls(all=T))

getwd()

setwd("C:\\Users\\User\\Felipe\\Bases diretores (banco antigo)\\Novas Bases (nov_2014)")

require(foreign)
require(car)
require(nnet)


data1<-read.spss("base_refeita_sem_duplicados_nov_2014_principal.sav", to.data.frame=T)


# Como as mudancas ocorreram somente no desempenho e nos indicadores, os modelos de contexto escolar nao sao rodados 
# novamente.

# Verificando como se comporta a variavel de maxima educacao (modelo so com ela)


multn1.1 <- multinom(mult_06_08_09_nova ~ max_educ_06, data=data1)
multn1.2 <- multinom(mult_06_08_09_nova ~ max_educ_09, data=data1)

stargazer(multn1.1)
stargazer(multn1.2)


# Modelo 2 (Desempenho)

# Primeiro segmento

multn2 <- multinom(mult_06_08_09_nova ~  media_np_05_07_AI + ideb_05_07_AI_AF_pior, data=data1)

# Segundo Segmento

multn2.1 <- multinom(mult_06_08_09_nova ~  media_np_05_07_AF + ideb_05_07_AI_AF_pior, data=data1)



# No modelo cheio, foram utilizados os indicadores de carencia e colaboracao, calculados pelo metodo 1

# Modelo 3 (Full Model)

# Metodo 3.1

# Primeiro segmento

multn3.1 <- multinom(mult_06_08_09_nova ~ exp_2006 + cor_recode_06 + max_educ_06 + Colaboracao_cat_07 + Carencia_cat_07
                     + complexidade_esc_07 + media_np_05_07_AI +ideb_05_07_AI_AF_pior, data=data1)


# Segundo segmento

multn3.2 <- multinom(mult_06_08_09_nova ~ exp_2006 + cor_recode_06 + max_educ_06 + Colaboracao_cat_07 + Carencia_cat_07
                     + complexidade_esc_07 + media_np_05_07_AF +ideb_05_07_AI_AF_pior, data=data1)



#####################################################################################################


# Segundo ciclo de mandato (2009-2011/2012)


# Modelo 5 (Desempenho)

#Primeiro segmento

multn5.1 <- multinom(mult_09_11_12_nova ~  nota_padr_5_2009 + nota_padr_3_2009 + nota_padr_3_2010 +
                       PREMIO_2009 + sum_ideb_iderio_09_10_pior, data=data1)


# Segundo segmento

multn5.2 <- multinom(mult_09_11_12_nova ~  nota_padr_9_2009 + nota_padr_7_2009 + nota_padr_7_2010 + 
                       PREMIO_2009 + sum_ideb_iderio_09_10_pior, data=data1)


# Modelo cheio

# Metodo 6.1

# Primeiro segmento

multn6.1 <- multinom(mult_09_11_12_nova ~ exp_2009 + cor_recode_09 + max_educ_09 + Colaboracao_cat_09 + Carencia_cat_09
                     + complexidade_esc_09 + nota_padr_5_2009 + PREMIO_2009 + nota_padr_3_2010
                        + nota_padr_3_2009 + sum_ideb_iderio_09_10_pior, data=data1)


# Segundo segmento

multn6.2 <- multinom(mult_09_11_12_nova ~ exp_2009 + cor_recode_09 + max_educ_09 + Colaboracao_cat_09 + Carencia_cat_09
                     + complexidade_esc_09 + nota_padr_9_2009 + PREMIO_2009 +
                       nota_padr_7_2010 + nota_padr_7_2009 + sum_ideb_iderio_09_10_pior, data=data1)



########################################
# Relatorios ###########################
########################################

# Primeiro Ciclo

stargazer(multn2)
stargazer(multn2.1)
stargazer(multn3.1)
stargazer(multn3.2)

# Segundo ciclo

stargazer(multn5.1)
stargazer(multn5.2)
stargazer(multn6.1)
stargazer(multn6.2)
  
  
  
  


