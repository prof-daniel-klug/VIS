## instalando/carregando pacotes ----
pck <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
    }
packages <- c("dplyr","Rcpp","lubridate", "ggplot2")
pck(packages)

## lendo dados ---- 
url = "https://dadosabertos.poa.br/dataset/b80abf76-d088-4199-9f7e-dbe692125eb8/resource/ffa97db0-495d-4972-9d7a-18bfcdc47321/download/gerint_solicitacoes.csv"
data <- read.csv(url, sep = ";")

## manipulacao e limpeza ----
banco <- data %>%
  mutate(data_extracao = as.Date(substring(data_extracao,1,10)), # corrige data
         data_internacao = as.Date(substring(data_internacao,1,10)), # corrige data
         data_alta = as.Date(substring(data_alta,1,10)), # corrige data
         data_solicitacao = as.Date(substring(data_solicitacao,1,10)), # corrige data
         data_autorizacao = as.Date(substring(data_autorizacao,1,10)), # corrige data
         sexo = case_when(sexo == 'F' ~ "FEMININO", 
                          sexo == 'M' ~ "MASCULINO",
                          sexo == 'FEMININO' ~ "FEMININO",    # padroniza variavel sexo
                          sexo == 'MASCULINO' ~ "MASCULINO",
                          TRUE ~ "NAO INFORMADO"
                          ),
         tipo_internacao = case_when(tipo_internacao == 'PRÃ"PRIA' ~ "PROPRIA", # padroniza variavel tipo_internacao
                                     TRUE ~ "NAO PROPRIA"    
                                    ),
         dias_na_fila = horas_na_fila/24, # cria variavel dias na fila
         dias_internacao = as.numeric(data_alta - data_internacao) # cria variavel dias de internacao
  )

# % de casos com informacoes de horas_fila negativas
horas_fila_neg <- nrow(banco[banco$horas_na_fila<0,])/nrow(banco)*100 # 1,68%
banco <- banco %>% 
  filter(horas_na_fila >= 0) # remove linhas com horas_fila negativas


# % de casos com informacoes de dias_internacao negativas (ocorre por conta da variavel data_alta ter valor setado em anos anteriores a data de internacao)
dias_internacao_neg <- nrow(banco[banco$dias_internacao<0,])/nrow(banco)*100 # 1,61%
banco <- banco %>% 
  filter(dias_internacao >= 0) # remove linhas com dias_internacao negativas

# remove tres observacoes com data_solicitacao < 2011
data_solic <- nrow(banco[banco$data_solicitacao < '2011-01-01',])/nrow(banco)*100 
banco <- banco %>% 
  filter(data_solicitacao >= '2011-01-01') 

# remove casos onde situacao = "INTERNADA", porem existe data_alta
banco <- banco %>% 
  filter(!(situacao == 'INTERNADA' & !is.na(data_alta)))

# processo de limpeza eliminou cerca de 3,28% dos dados
subset <- nrow(banco)/nrow(data)*100 # 96,72%


## opcoes de graficos ----
# tempo medio de internação por carater de internacao #
banco %>%
  select(carater,dias_internacao) %>%
  group_by(carater) %>% 
  summarise(., mean(dias_internacao)) %>%
  ggplot(aes(x=reorder(carater, `mean(dias_internacao)`), y = `mean(dias_internacao)`))+
  geom_text(aes(label = round(`mean(dias_internacao)`,2)), hjust=-0.2, color="black",size=3.5) +
  geom_col(fill="#275b70") + 
  coord_flip() +
  xlab("")+
  ylab("")+
  labs(title = "Tempo médio de internação (dias)",
       subtitle = "Caráter de internação")

# tempo médio de permanencia dos pacientes na fila, por mes #
# selecao a partir de 2017, pois temos só dezembro 
banco <- banco %>% 
  mutate(year = year(data_autorizacao),
         month = month(data_autorizacao))

banco$month <- as.numeric(banco$month)
banco$dias_na_fila <- as.numeric(banco$dias_na_fila)
banco$year <- factor(banco$year)  

banco %>%
  select(month,year,dias_na_fila) %>%
  filter(year != 2017) %>% 
  group_by(year, month) %>% 
  summarise(., mean(dias_na_fila)) %>%
  na.omit() %>% 
  ggplot() +  
  geom_line(aes(x = month, 
            y = `mean(dias_na_fila)`, 
            color = year),size = 1.1) + 
  scale_x_continuous(breaks = 1:12)+
  xlab("Mês")+
  ylab("Tempo médio de fila")+
  labs(title = "Tempo médio de permanência dos pacientes na fila")+
  scale_colour_discrete(name = "Ano")

banco %>%
  select(idade,sexo) %>%
  filter(sexo != 'NAO INFORMADO') %>% 
  group_by(sexo) %>% 
  na.omit() %>% 
  ggplot() +  ggtitle("Comparação das idades dos grupos de sexo") +
  geom_boxplot(aes(x = sexo, y = idade), outlier.colour = "red")
  xlab("Sexo") +
  ylab("Idade")
  
  
  #####
table(banco$tipo_leito,banco$sexo)
table(banco$tipo_leito,banco$carater)
table(banco$year,banco$carater)
table(banco$executante, banco$carater)
table(banco$executante, banco$sexo)
table(banco$executante,banco$tipo_leito)
table(banco$executante)
  
