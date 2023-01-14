# Introdução

# O banco de dados que será apresetado  refere-se aos valores da produção de motociclos no País entre  janeiro de 1993 à setembro de 2022 com suas variáveis data e 
# valor(R$). Através desse banco de dados irei realizar uma análise de série temporal e apresentar o melhor modelo para previsão para os próximos passos(meses/ano).

# Pacotes  necessários 
library(TSA)
library(forecast)
library(fpp3)
library(stringr)
library(readr)

# Entrada do banco de dados 
dataset<-read_delim("bcdata.sgs.1377.csv", 
  delim = ";", escape_double = FALSE, col_types = cols(data = col_date(format = "%d/%m/%Y")),   trim_ws = TRUE)
head(dataset)

# Transformando a variável data em mensal 
dados <- dataset %>%
 mutate( ano_mes = str_sub( data, 1, 7 )  ) %>%
  group_by(ano_mes) %>% summarise( producao = sum(valor)  )
dados

# Indexando no tstible 

serie <- dados %>%
  mutate(data = yearmonth(ano_mes))%>%
  as_tsibble(index= data )
serie
tail(serie)


# Análisando o comportamento  da série

plot_serie = serie %>%
  autoplot(producao) +
  labs(title=" Producão de motociclos ",
       y=" R$ valor ")
plot_serie

# Gráfico ACF e PACF da série

acf_producao = serie %>% 
  ACF(producao, lag_max = 36) %>% 
  autoplot() + labs(title="ACF da variável valor")

pacf_producao = serie %>% 
  PACF(producao, lag_max = 36) %>% 
  autoplot() + labs(title= "PACF da variável valor")

gridExtra::grid.arrange(plot_serie,acf_producao, pacf_producao, layout_matrix=rbind(c(1,1),c(2,3) ))


# Verificando-se a série é estacionária
serie %>%
  features(producao, unitroot_kpss)
	

## Aplicando o operador de diferença para tornar a série estacionária
	
	# Gráfico do  operador de diferença na serie
plot_serieD = serie %>%
  autoplot( difference(producao) ) +
  labs(title="Produção de motociclos ",
       y="R$ valor")
plot_serieD

# Gráfico  ACF e PACF da serie

acf_D = serie %>%
  ACF(difference(producao), lag_max = 36) %>% 
  autoplot() + labs(title="ACF")
acf_D

pacf_D = serie %>%
  PACF(difference(producao), lag_max = 36) %>% 
  autoplot() + labs(title="PACF") 
pacf_D
gridExtra::grid.arrange(plot_serieD, acf_D, pacf_D,layout_matrix=rbind(c(1,1),c(2,3) ))

# Verificando-se  a serie está estacionária 
serie%>%
  features(difference(producao), unitroot_kpss)


# Definindo a base de dados em treinamento e teste.

train <- serie %>%
  filter_index("1993 Jan" ~ "2021 dec")
tail(train)
head(train)

test <- serie %>%
  filter_index("2022" ~ . );test

# Gráfico do Comportamento da serie em treino 

plot_serie = train %>%
  autoplot(producao) +
  labs(title="Produção de motociclos ",
       y="R$ valor  ")
plot_serie

#  Gráfico ACF e PACF do treino 
acf = train %>%
  ACF(producao, lag_max = 36) %>% 
  autoplot() + labs(title="ACF")
acf

pacf = train %>%
  PACF(producao, lag_max = 36) %>% 
  autoplot() + labs(title="PACF") 
pacf

gridExtra::grid.arrange(plot_serie,acf,pacf,layout_matrix=rbind(c(1,1),c(2,3) ))

# Verificando se a série em treino  é estacionária 
train %>%
  features(producao, unitroot_kpss)
	
## Aplicando o operador de diferença no treino 

# Gráfico do  operador de diferença no treinamento 
plot_serieD = train %>%
  autoplot( difference(producao) ) +
  labs(title=" ",
       y="R$ valor ")
plot_serieD
	
# Gráfico  ACF e PACF do operador diferença no treino 

acf_D = train %>%
  ACF(difference(producao), lag_max = 36) %>% 
  autoplot() + labs(title="ACF")
acf_D

pacf_D = train %>%
  PACF(difference(producao), lag_max = 36) %>% 
  autoplot() + labs(title="PACF") 
pacf_D

gridExtra::grid.arrange(plot_serieD, acf_D, pacf_D,layout_matrix=rbind(c(1,1),c(2,3) ))

# Estimar modelos especificados 

caf_fit <- train %>% model(
  ETS1 = ETS( producao ~ error("A") + trend("N") + season("N")),
  ETS2 = ETS( producao ~ error("A") + trend("A") + season("N")),
  ETS3 = ETS( producao ~ error("A") + trend("M") + season("N")),
  ETS4 = ETS( producao ~ error("A") + trend( "N" ) + season("A")),
  ETS5 = ETS( producao ~ error("A") + trend( "N" ) + season("M")),
  ARIMA011002 = ARIMA(producao ~ pdq(0,1,1) + PDQ(0,0,2)),
  ARIMA012022 = ARIMA(producao ~ pdq(0,1,2) + PDQ(0,0,2)),
  ARIMA012013 = ARIMA(producao ~ pdq(0,1,2) + PDQ(0,1,3)),
  auto = ARIMA(producao),
  auto1 = ETS(producao))
caf_fit

# Desempenho dos modelos

glance(caf_fit)

# Comportamento dos residuos
caf_fit %>%
  select(ARIMA012013) %>%
  gg_tsresiduals()

# Gerando uma  previsão h = 10 (passos)
caf_fc <- caf_fit %>% forecast(h = 10)
caf_fc

# Gráfico da serie com as previsões

caf_fc %>% filter( (.model == 'ARIMA012013') | (.model == c('auto', "ARIMA011002", "ARIMA012022"))) %>%
  autoplot(serie, level = NULL) +
  labs(
    y = "valores",
    title = "Previsão"
  ) +
  guides(colour = guide_legend(title = "Forecast"))
	
# Accracia do modelo 	
	accuracy(caf_fc, test)
	
# Já na accuracia o melhor modelo que deve ser ultilizado é o auto1 que é referente ao ETS,dado que, possui o menor ME,RMSE,MAE,MPE  MAPE entre todos os modelos. Para # isso, devemos refazer  a geração de passos no modelo indicado. 

caf_fit %>%
  select( auto1) %>%
  gg_tsresiduals()


# Gráfico da serie com as previsões

caf_fc %>% filter( (.model == 'auto1')) %>%
  autoplot(serie, level = NULL) +
  labs(
    y = "valores",
    title = "Previsão"
  ) +
  guides(colour = guide_legend(title = "Forecast"))
	

	
