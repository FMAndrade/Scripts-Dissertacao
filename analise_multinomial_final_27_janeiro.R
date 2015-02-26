###########################################
# Novas analises por Mandato 4 ############
# Variaveis Novas (Felipe e Mariane) ######
# Modelos sugeridos por Mariane ###########
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
require(stargazer)

data1<-read.spss("base_refeita_sem_duplicados_nov_2014_principal.sav", to.data.frame=T)


###############################################################
# Modelos sugeridos por Mariane (final)- Gracas ao bom senhor!#
###############################################################


##############################
# Modelos 2006-2008/2009 #####
##############################


# Modelo 1 (Condicoes de oferta/clima escolar)

multn1 <- multinom(mult_06_08_09_nova ~ max_educ_06 + Colaboracao_cat_07 + Carencia_cat_07 + complexidade_esc_07,
                   data=data1)


multn2 <- multinom(mult_06_08_09_nova ~ max_educ_06 + Colaboracao_cat_07 + Carencia_cat_07 + complexidade_esc_07 +
                     exp_2006, data=data1)


multn3 <- multinom(mult_06_08_09_nova ~ max_educ_06 + Colaboracao_cat_07 + Carencia_cat_07 + complexidade_esc_07 +
                     exp_2006 + pior_ind_05_07_one + pior_ind_05_07_two, data=data1)


##############################
# Modelo Cheio Anos Iniciais##
##############################

multn4 <- multinom(mult_06_08_09_nova ~ max_educ_06 + Colaboracao_cat_07 + Carencia_cat_07 + complexidade_esc_07 +
                     exp_2006 + pior_ind_05_07_one + pior_ind_05_07_two + media_np_05_07_AI, data=data1)


############################
# Modelo Cheio Anos Finais##
############################

multn5 <- multinom(mult_06_08_09_nova ~ max_educ_06 + Colaboracao_cat_07 + Carencia_cat_07 + complexidade_esc_07 +
                     exp_2006 + pior_ind_05_07_one + pior_ind_05_07_two + media_np_05_07_AF, data=data1)



##############################
# Modelos 2009-2011/2012 #####
##############################


multn6 <- multinom(mult_09_11_12_nova ~ max_educ_09 + Colaboracao_cat_09 + Carencia_cat_09 + complexidade_esc_09,
                   data=data1)


multn7 <- multinom(mult_09_11_12_nova ~ max_educ_09 + Colaboracao_cat_09 + Carencia_cat_09 + complexidade_esc_09 +
                     exp_2009, data=data1)



multn8 <- multinom(mult_09_11_12_nova ~ max_educ_09 + Colaboracao_cat_09 + Carencia_cat_09 + complexidade_esc_09 +
                     exp_2009 + pior_ind_09_10_one + pior_ind_09_10_two, data=data1)

###############################
# Modelo Cheio Anos Iniciais###
###############################

multn9 <- multinom(mult_09_11_12_nova ~ max_educ_09 + Colaboracao_cat_09 + Carencia_cat_09 + complexidade_esc_09 +
                     exp_2009 + pior_ind_09_10_one + pior_ind_09_10_two + media_ind_09_10_AI + PREMIO_2009, data=data1)


##############################
# Modelo Cheio Anos Finais####
##############################

multn10 <- multinom(mult_09_11_12_nova ~ max_educ_09 + Colaboracao_cat_09 + Carencia_cat_09 + complexidade_esc_09 +
                     exp_2009 + pior_ind_09_10_one + pior_ind_09_10_two + media_ind_09_10_AF + PREMIO_2009, data=data1)


# Mencionar que fiz modelos so com as variaveis de desempenho. Devo comentar os modelos, mas apresentar seus resultados
# somente como anexo do trabalho.



##########################################
# Modelos com variaveis de desempenho ####
##########################################


# Primeiro ciclo

multn11<-multinom(mult_06_08_09_nova ~ media_np_05_07_AI + pior_ind_05_07_one + pior_ind_05_07_two, data=data1)

multn12<-multinom(mult_06_08_09_nova ~ media_np_05_07_AF + pior_ind_05_07_one + pior_ind_05_07_two, data=data1)


# Segundo Ciclo

multn13<-multinom(mult_09_11_12_nova ~ media_ind_09_10_AI + pior_ind_09_10_one + pior_ind_09_10_two, data=data1)

multn14<-multinom(mult_09_11_12_nova ~ media_ind_09_10_AF + pior_ind_09_10_one + pior_ind_09_10_two, data=data1)


##############################
# Gerando os relatorios ######
##############################

# Como colocar a tabela no modelo paisagem
# \usepackage{pdflscape}
# \pagestyle{empty}
# \begin{landscape}


stargazer(multn1, multn2, multn3)
stargazer(multn4, multn5)


stargazer(multn6, multn7, multn8)
stargazer(multn9, multn10)


stargazer(multn11, multn12)
stargazer(multn13, multn14)

#######################################
# Probabilidades Preditas #############
#######################################

# Primeiro ciclo

#AI
predict(multn4, type = "probs")
#AF
predict(multn5, type = "probs")

# Segundo ciclo

#AI
predict(multn9, type = "probs")
#AF
predict(multn10, type = "probs")



# variavel experiencia e carencia

predict(multn2, type = "probs")
predict(multn7, type = "probs")




# LogLikelihood

logLik(multn1)
logLik(multn2)
logLik(multn3)
logLik(multn4)
logLik(multn5)
logLik(multn6)
logLik(multn7)
logLik(multn8)
logLik(multn9)
logLik(multn10)
logLik(multn11)
logLik(multn12)
logLik(multn13)
logLik(multn14)
