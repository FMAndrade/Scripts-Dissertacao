###########################################
# Novas analises por Mandato 2 ############
# Variaveis Novas (Mariane) ###############
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
View(data1)


######################################################
# Analises divididas por ciclo de mandato ############
######################################################



# Primeiro ciclo de mandato (2006-2008/2009)


# Modelo 1 (Contexto Escolar)

# Modelo 1.1 (metodo 1-analise fatorial com as medidas categóricas e depois fiz um aggregate com a média fatores por escola)



multn1.1 <- multinom(mult_06_08_09_nova ~ exp_2006 + cor_recode_06 + max_educ_06 + Colaboracao_cat_07 +
                       Carencia_cat_07 + complexidade_esc_07, data=data1)

rm(multn1.1)

require(stargazer)

stargazer(multn1.1)

# Modelo 1.2 (transformei as variáveis em dummies, e calculei a porcentagem de professores por escola que responderam 
# concordo ou sim é grave, e edepois analise fatorial)

multn1.2 <- multinom(mult_06_08_09_nova ~ exp_2006 + cor_recode_06 + max_educ_06 + Colaboracao_perct_07 +
                       Carencia_perct_07 + complexidade_esc_07, data=data1)


# Modelo 1.3 (indicadores comparaveis por escola, com metodologia do primeiro metodo)

multn1.3 <- multinom(mult_06_08_09_nova ~ exp_2006 + cor_recode_06 + max_educ_06 + Colaboracao_cat_07_comp +
                       Carencia_cat_07_comp + complexidade_esc_07, data=data1)



# Modelo 1.4 (indicadores comparaveis por escola, com metodologia do segundo metodo)


multn1.4 <- multinom(mult_06_08_09_nova ~ exp_2006 + cor_recode_06 + max_educ_06 + Colaborcao_perct_07_comp +
                       Carencia_perct_07_comp + complexidade_esc_07, data=data1)



# Modelo 2 (Desempenho)


multn2 <- multinom(mult_06_08_09_nova ~  nota_padr_5_2005 + nota_padr_9_2005 + nota_padr_5_2007 + nota_padr_9_2007 +
                       ideb_5_05_new + ideb_9_05_new + ideb_5_07_new + ideb_9_07_new, data=data1)



# Modelo 3 (Full Model)

# Metodo 3.1

multn3.1 <- multinom(mult_06_08_09_nova ~ exp_2006 + cor_recode_06 + max_educ_06 + Colaboracao_cat_07 + Carencia_cat_07
                     + complexidade_esc_07 + nota_padr_5_2005 + nota_padr_9_2005 + nota_padr_5_2007 + nota_padr_9_2007 +
                       ideb_5_05_new + ideb_9_05_new + ideb_5_07_new + ideb_9_07_new, data=data1)


# Metodo 3.2


multn3.2 <- multinom(mult_06_08_09_nova ~ exp_2006 + cor_recode_06 + max_educ_06 + Colaboracao_perct_07 + Carencia_perct_07
                     + complexidade_esc_07 + nota_padr_5_2005 + nota_padr_9_2005 + nota_padr_5_2007 + nota_padr_9_2007 +
                       ideb_5_05_new + ideb_9_05_new + ideb_5_07_new + ideb_9_07_new, data=data1)



# Metodo 3.3 (indicador comparavel por escola, metodo 1)


multn3.3 <- multinom(mult_06_08_09_nova ~ exp_2006 + cor_recode_06 + max_educ_06 + Colaboracao_cat_07_comp + Carencia_cat_07_comp
                     + complexidade_esc_07 + nota_padr_5_2005 + nota_padr_9_2005 + nota_padr_5_2007 + nota_padr_9_2007 +
                       ideb_5_05_new + ideb_9_05_new + ideb_5_07_new + ideb_9_07_new, data=data1)


# Metodo 3.4 (indicador comparavel por escola, metodo 2)


multn3.4 <- multinom(mult_06_08_09_nova ~ exp_2006 + cor_recode_06 + max_educ_06 + Colaborcao_perct_07_comp + Carencia_perct_07_comp
                     + complexidade_esc_07 + nota_padr_5_2005 + nota_padr_9_2005 + nota_padr_5_2007 + nota_padr_9_2007 +
                       ideb_5_05_new + ideb_9_05_new + ideb_5_07_new + ideb_9_07_new, data=data1)



#####################################################################################################


# Segundo ciclo de mandato (2009-2011/2012)



# Modelo 4 (Contexto Escolar)

# Modelo 4.1 (metodo 1-analise fatorial com as medidas categóricas e depois fiz um aggregate com a média fatores por escola)


multn4.1 <- multinom(mult_09_11_12_nova ~ exp_2009 + cor_recode_09 + max_educ_09 + Colaboracao_cat_09 +
                       Carencia_cat_09 + complexidade_esc_09, data=data1)



# Modelo 4.2 (transformei as variáveis em dummies, e calculei a porcentagem de professores por escola que responderam 
# concordo ou sim é grave, e edepois analise fatorial)

multn4.2 <- multinom(mult_09_11_12_nova ~ exp_2009 + cor_recode_09 + max_educ_09 + Colaboracao_perct_09 +
                       Carencia_perct_09 + complexidade_esc_09, data=data1)


# Modelo 4.3 (indicadores comparaveis por escola, com metodologia do primeiro metodo)

multn4.3 <- multinom(mult_09_11_12_nova ~ exp_2009 + cor_recode_09 + max_educ_09 + Colaboracao_cat_09_comp +
                       Carencia_cat_09_comp + complexidade_esc_09, data=data1)



# Modelo 4.4 (indicadores comparaveis por escola, com metodologia do segundo metodo)


multn4.4 <- multinom(mult_09_11_12_nova ~ exp_2009 + cor_recode_09 + max_educ_09 + Colaborcao_perct_09_comp +
                       Carencia_perct_09_comp + complexidade_esc_09, data=data1)


# Modelo 5 (Desempenho)


multn5 <- multinom(mult_09_11_12_nova ~  nota_padr_5_2009 + nota_padr_9_2009 + PREMIO_2009 + nota_padr_3_2010 +
                     nota_padr_7_2010 + nota_padr_3_2009 + nota_padr_7_2009 + pior_ideb_iderio_09_09_AI +
                     pior_ind_ideb_iderio_09_10_AI + pior_ideb_iderio_09_09_AF + pior_ind_ideb_iderio_09_10_AF, data=data1)


# Modelo 6 (Full Model)

# Metodo 6.1

multn6.1 <- multinom(mult_09_11_12_nova ~ exp_2009 + cor_recode_09 + max_educ_09 + Colaboracao_cat_09 + Carencia_cat_09
                     + complexidade_esc_09 + nota_padr_5_2009 + nota_padr_9_2009 + PREMIO_2009 + nota_padr_3_2010 +
                       nota_padr_7_2010 + nota_padr_3_2009 + nota_padr_7_2009 + pior_ideb_iderio_09_09_AI +
                       pior_ind_ideb_iderio_09_10_AI + pior_ideb_iderio_09_09_AF + pior_ind_ideb_iderio_09_10_AF, data=data1)


# Metodo 6.2


multn6.2 <- multinom(mult_09_11_12_nova ~ exp_2009 + cor_recode_09 + max_educ_09 + Colaboracao_perct_09 + Carencia_perct_09
                     + complexidade_esc_09 + nota_padr_5_2009 + nota_padr_9_2009 + PREMIO_2009 + nota_padr_3_2010 +
                       nota_padr_7_2010 + nota_padr_3_2009 + nota_padr_7_2009 + pior_ideb_iderio_09_09_AI +
                       pior_ind_ideb_iderio_09_10_AI + pior_ideb_iderio_09_09_AF + pior_ind_ideb_iderio_09_10_AF, data=data1)


# Metodo 6.3 (indicador comparavel por escola, metodo 1)


multn6.3 <- multinom(mult_09_11_12_nova ~ exp_2009 + cor_recode_09 + max_educ_09 + Colaboracao_cat_09_comp + Carencia_cat_09_comp
                     + complexidade_esc_09 + nota_padr_5_2009 + nota_padr_9_2009 + PREMIO_2009 + nota_padr_3_2010 +
                       nota_padr_7_2010 + nota_padr_3_2009 + nota_padr_7_2009 + pior_ideb_iderio_09_09_AI +
                       pior_ind_ideb_iderio_09_10_AI + pior_ideb_iderio_09_09_AF + pior_ind_ideb_iderio_09_10_AF, data=data1)


# Metodo 6.4 (indicador comparavel por escola, metodo 2)


multn6.4 <- multinom(mult_09_11_12_nova ~ exp_2009 + cor_recode_09 + max_educ_09 + Colaborcao_perct_09_comp + Carencia_perct_09_comp
                     + complexidade_esc_09 + nota_padr_5_2009 + nota_padr_9_2009 + PREMIO_2009 + nota_padr_3_2010 +
                       nota_padr_7_2010 + nota_padr_3_2009 + nota_padr_7_2009 + pior_ideb_iderio_09_09_AI +
                       pior_ind_ideb_iderio_09_10_AI + pior_ideb_iderio_09_09_AF + pior_ind_ideb_iderio_09_10_AF, data=data1)




##########################################
# Relatorios dos resultados ##############
##########################################

require(stargazer)


# Primeiro ciclo
stargazer(multn1.1)
stargazer(multn1.2)
stargazer(multn1.3)
stargazer(multn1.4)
stargazer(multn2)
stargazer(multn3.1) 
stargazer(multn3.2)
stargazer(multn3.3)
stargazer(multn3.4)


# Segundo ciclo
stargazer(multn4.1)
stargazer(multn4.2)
stargazer(multn4.3)
stargazer(multn4.4)
stargazer(multn5)
stargazer(multn6.1)
stargazer(multn6.2)
stargazer(multn6.3)
stargazer(multn6.4)


#############################################################
# Maneira de utilizar o predict para valores especificos ####
#############################################################

# Passo a passo

# Inserir uma linha especifica na base de dados
# rodar a regressao novamente
# criar uma nova base de dados somente com os valores preditos
# acessar a linha especifica que foi criada


b2<-predict(multn1.1,data1, "probs")

b2[6,]

