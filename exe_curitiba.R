### Death records from City of Curitiba ###

### random sample ###

rm(list = ls())
graphics.off()

library(tidyverse)
library(BSDA) # Z test
library(nortest) # Normality Test

# 1: Create table

obit <- c("F","F","M","M","M","F","F","F","M","M","M","M","M","F","M",
          "M","M","F","M","F","F","M","M","M","F","M","F","M",
          "M","M","F","F","M","M","F","M","F","F","F","F","M","M","M","F",
          "M","M","F","M","M","F","F","F","F","M","F","F","F","F",
          "F","M","F","F","F","M","M","F","M","F","M","F","M","F","M",
          "F","M","F","M","F","M","F","M","M","M","M","F","F","F","F",
          "F","M","F","F","M","M","M","M","F","F","F","F","F","M") %>%
  tibble(.) %>%
  mutate('occupation' = c("DO LAR","DO LAR","ORÇAMENTISTA","AUXILIAR PRODUCAO",
         "ZELADOR(A)","DO LAR","DO LAR","AUXILIAR LOGISTICA","LAVRADOR",
         "OPERADOR(A)","MILITAR","COMERCIANTE","TAXISTA","DO LAR",
         "TAXISTA","OUTROS","AUXILIAR SERVICOS GERAIS",
         "DO LAR","AUTONOMO(A)","OUTROS","DO LAR","MEDICO(A)",
         "ADVOGADO(A)","OUTROS","AUXILIAR LIMPEZA","PINTOR(A)",
         "DO LAR","CONTADOR(A)","EMPREITERO(A)","OUTROS","CAMAREIRA","DO LAR",
         "INSTALADOR","PEDREIRO","PROFESSOR(A)","PINTOR(A)",
         "DO LAR","DO LAR","DO LAR","DO LAR","AUTONOMO(A)","CARPINTEIRO(A)",
         "PEDREIRO","VENDEDOR(A)","PORTEIRO(A)","COMERCIANTE","DO LAR",
         "AUTONOMO","PINTOR(A)","OUTROS","DO LAR","MANICURE","AUXILIAR SERVICOS GERAIS",
         "DO LAR","DO LAR","PROFESSOR(A)","PROFESSOR(A)","DO LAR","SECRETARIA",
         "AUTONOMO","DO LAR","DIARISTA","DO LAR","MECANICO","PEDREIRO",
         "COZINHEIRO(A)","AUTONOMO","DO LAR","OUTROS","DO LAR","REPOSITOR(A)",
         "AUXILIAR","MONTADOR(A)","DO LAR","OUTROS","ASSISTENTE SOCIAL","LAVRADOR",
         "DO LAR","MILITAR","OUTROS","LAVRADOR","OUTROS","TAXISTA","ADVOGADO(A)",
         "COZINHEIRO(A)","PROFESSOR(A)","DO LAR","DO LAR","COSTUREIRO(A)","OUTROS",
         "DO LAR","OUTROS","OUTROS","COMERCIANTE","MOTORISTA","CONTADOR(A)","DO LAR",
         "PROFESSOR(A)","AUXILIAR","DO LAR","DO LAR","MOTORISTA"),
         'age'=c(58,83,76,85,76,72,83,31,81,53,79,83,98,88,90,45,57,49,51,
          69,45,68,80,79,56,69,83,88,84,76,88,64,69,59,80,67,73,72,
          63,85,68,96,35,36,39,78,68,69,67,55,84,71,76,62,89,90,
          72,53,66,52,71,77,38,20,57,66,68,100,76,79,51,34,55,85,108,
          48,75,96,51,86,89,61,70,61,82,88,34,78,91,75,88,84,90,
          64,46,71,68,68,45,74,92,79))


obit <- obit %>%
  rename('gender' = '.')


# 1.1: Analysing age variable
obit %>%
  select(age) %>%
  summary()

# histogram of age (save)
age.hist <- ggplot(data=obit, aes(x=age)) +
  geom_histogram(bins = 20,
                 binwidth = 3.5,
                 fill='brown',
                 colour='black') +
  theme_minimal() +
  xlab('age') +
  ylab('frequency') +
  ggtitle('Histogram of age')


# density plot of age
ggplot(data=obit,aes(x=age)) +
  geom_density(fill='deepskyblue4',
               colour='brown',
               linetype='dashed',
               linewidth = 1,
               adjust=0.6,
               alpha=0.6) +
  theme_minimal() +
  xlab('age') +
  ylab('frequency in %') +
  ggtitle('Density Plot of age')



# 2: normality test for age
shapiro.test(obit$age) # not normal distributed

lillie.test(obit$age) # not normal distributed

# 2.1: test normality according to gender

# boxplot of age per gender
ggplot(data=obit, aes(x=gender,
                      y=age)) +
  geom_boxplot(fill='grey',
               outlier.colour = 'red',
               outlier.size = 3) +
  theme_minimal() +
  xlab('gender') +
  ylab('age') +
  ggtitle('Boxplot of age per gender')

ggsave('/home/denis/aqui/first_project/boxplot_per_gender.pdf',
       plot = last_plot(),
       units = 'cm',
       height = 25,
       width = 50)  

# select male and female genders separately
only_m <- obit %>% filter(gender == "M")
only_f <- obit %>% filter(gender == "F")

only_m %>%
  select(age) %>%
  summary()

only_f %>%
  select(age) %>%
  summary()

  
shapiro.test(only_m$age) # normally distributed
lillie.test(only_m$age) # normally distributed

shapiro.test(only_f$age) # not normally distributed
lillie.test(only_f$age) # not normally distributed


# histogram only_m
ggplot(data=only_m, aes(x=age)) +
  geom_histogram(bins = 20,
                 binwidth = 4.5,
                 colour='black',
                 fill='brown') +
  theme_minimal() +
  xlab('age') +
  ylab('counts') +
  ggtitle('Histogram of age - male sex')

ggsave('/home/denis/aqui/first_project/male_histogram.pdf',
       plot = last_plot(),
       units = 'cm',
       height = 25,
       width = 50)  

# density plot only_m
ggplot(data=only_m,aes(x=age)) +
  geom_density(fill='deepskyblue4',
               colour='brown',
               linetype='dashed',
               linewidth = 1,
               adjust=0.6,
               alpha=0.6) +
  theme_minimal() +
  xlab('age') +
  ylab('frequency in %') +
  ggtitle('Density Plot of age - male only')


# 3: Submitt only_me to Hypothesis Test


# According to IBGE, male population life expectancy in Brazil
# is 73.1

# H0: mean of age for male population is equal to 73.1
# Ha: mean of age for male population is not equal to 73.1

only_m$age %>%
  sd()

z.test(only_m$age, mu = 73.1, sigma.x = 17.0268)

# Hypothesis test conclusion:
# Despite the sample mean of almost 68.92, 
# at a 95% confidence level we're not supposed to reject
# null hypothesis. Mean of age for male sex is therefore 
# equal to 70. In other words, mean sample interval contains 
# the 73.1 populational mean
# Z calc lies not in rejection standard curve area 
# Z calc not less than -1.96 or greater than 1.96
# Z calc: -0.1.6944
# p-value: 0.09019

# P.S: You may export and save the plots to your PC by typing the lines below:
#ggsave('/home/user/plot_name.pdf',
#       plot = last_plot())
