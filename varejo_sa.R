
library(sidrar)
library(tidyverse)
setwd("C:/Users/cardo/Desktop/varejo")

#### ---- IPCA ---- ####

ipca<- get_sidra(api = '/t/1737/n1/all/v/69,2265/p/all/d/v69%202,v2265%202')

### IPCA anual
ipca %>% select(valor = "Valor",
                            data = "Mês (Código)",
                            tipo = "Variável") %>% 
  separate(data, into = c('ano', 'mes'), sep = 4) %>% 
  filter(ano >= 2014) %>% 
  pivot_wider(names_from = tipo, values_from = valor) %>% 
  rename('ipca_12m' = 4,
         'ipca_ano' = 3) %>% 
  filter(mes == 12) %>%
  ggplot(aes(x = as.integer(ano), y = ipca_ano))+
  geom_line(size = 1.5, color = '#0353a4')+
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, 2),
                     labels = \(x) paste0(x, '%'))+
  scale_x_continuous(breaks = seq(2014, 2021))+
  theme_minimal()+
  labs(title = 'IPCA',
       subtitle = 'Variação acumulada no ano',
       x = '',
       y = '',
       caption = 'Fonte: IBGE.')-> p1

### IPCA 12 meses
ipca %>% 
  select(valor = "Valor",
         data = "Mês (Código)",
         tipo = "Variável") %>% 
  separate(data, into = c('ano', 'mes'), sep = 4) %>% 
  filter(ano >= 2014) %>% 
  pivot_wider(names_from = tipo, values_from = valor) %>% 
  rename('ipca_12m' = 4,
         'ipca_ano' = 3) %>% 
  mutate(data = lubridate::make_date(year = ano,
                                     month = mes)) %>%  
  ggplot(aes(x = data))+
  geom_line(aes(y = ipca_12m), size = 1.5, color = '#0353a4')+
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, 2),
                     labels = \(x) paste0(x, '%'))+
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')+
  theme_minimal()+
  labs(title = 'IPCA',
       subtitle = 'Variação acumulada em 12 meses',
       x = '',
       y = '',
       caption = 'Fonte: IBGE.') -> p2

### unindo gráficos
library(patchwork)
p1/p2
ggsave('ipca.png')


### ipca bcb
ipca_bcb <- read_csv2('ipca.csv')

ipca_bcb %>% 
  rename(data = 1,
         meta = 2,
         observado = 3,
         max = 4,
         min = 5,
         focus = 6) -> ipca_bcb

###gráfico com xanflis
ipca_bcb %>%   
  pivot_longer(!data, names_to = 'tipo', values_to = 'valor') %>% 
  mutate(ano = lubridate::year(data)) %>% 
  filter(ano >= 2014,
         ano < 2024) %>% 
  drop_na() %>% 
  mutate(tipo = factor(tipo, 
                       levels = c('observado', 'focus',
                                  'min', 'meta', 'max'),
                       labels = c('Observado', 'Mediana Focus',
                                  'Limite Mínimo', 'Meta', 'Limite Máximo'))) %>% 
  ggplot(aes(x = data, y = valor, linetype = tipo, color = tipo, size = tipo))+
  geom_line()+
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')+
  scale_color_manual(values = c('#0353a4', '#0353a4', 'grey70', 'grey30', 'grey70'))+
  scale_linetype_manual(values = c(1, 4, 1, 2, 1))+
  scale_size_manual(values = c(1.5, 1.5, 1, 1, 1, 1))+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, 2),
                     labels = \(x) paste0(x, '%'))+
  labs(title = 'IPCA',
       subtitle = 'Variação acumulada em 12 meses e projeção Focus',
       x = '',
       y = '',
       caption = 'Fonte: Banco Central do Brasil e Relatório Focus 29/10/2021.',
       linetype = '',
       size = '',
       color = '')

### gráfico sem xanflis

ipca_bcb %>%   
  pivot_longer(!data, names_to = 'tipo', values_to = 'valor') %>% 
  filter(tipo %in% c('observado', 'focus')) %>% 
  mutate(tipo = factor(tipo,
                      levels = c('observado', 'focus'),
                      labels = c('Observado', 'Focus')),
         ano = lubridate::year(data)) %>% 
  filter(ano >= 2014) %>% 
  drop_na() %>% 
  ggplot(aes(x = data, y = valor, linetype = tipo, color = tipo))+
  geom_line(size = 1.5)+
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')+
  scale_linetype_manual(values = c(1, 1))+
  scale_color_manual(values = c('#0353a4', 'grey70'))+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  scale_y_continuous(limits = c(0, 12),
                     breaks = seq(0, 12, 2),
                     labels = \(x) paste0(x, '%'))+
  labs(title = 'IPCA',
       subtitle = 'Variação acumulada em 12 meses e projeção Focus',
       x = '',
       y = '',
       caption = 'Fonte: Banco Central do Brasil e Relatório Focus 29/10/2021.',
       linetype = '',
       color = '')


#### ---- PIB ---- ####

pib<- read_csv('gdp.csv')

# com vermelho para negativo e azul para positivo
pib %>% 
  select(1, 5:11) %>% 
  pivot_longer(!1, names_to = 'ano', values_to = 'var_pib') %>% 
  mutate(ano = as.integer(substr(ano, 1, 4)),
         cor = ifelse(var_pib > 0, 'positivo', 'negativo'),
         cor = factor(cor, levels = c('positivo', 'negativo'))) %>% 
  ggplot(aes(x = as.factor(ano), y = var_pib, fill = cor))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=paste0(round(var_pib,2), '%'),
                vjust = ifelse(var_pib > 0, -0.5, 1)))+
  scale_fill_manual(values = c('#0353a4', 'red2'))+
  theme_minimal()+
  theme(legend.position = 'null')+
  scale_y_continuous(limits = c(-5, 5),
                     breaks = seq(-5, 5, 2),
                     labels = \(x) paste0(x, '%'))+
  labs(title = 'PIB',
       subtitle = 'Variação anual',
       caption = 'Fonte: Banco Mundial.',
       x = '',
       y = '')

### adicionando expectativa focus
tibble(`Country Name` = 'Brazil',
       ano = seq(2021, 2024),
       var_pib = c(4.94, 1.20, 2.00, 2.20)) -> pib_focus

# grafico de linha
pib %>% 
  select(1, 5:11) %>% 
  pivot_longer(!1, names_to = 'ano', values_to = 'var_pib') %>% 
  mutate(ano = as.integer(substr(ano, 1, 4))) %>% 
  bind_rows(pib_focus) %>% 
  mutate(tipo = ifelse(ano <= 2020, 'observado', 'focus'),
         tipo = factor(tipo,
                       levels = c('observado', 'focus'),
                       labels = c('Observado', 'Mediana Focus'))) %>% 
  ggplot(aes(x = ano, y = var_pib, linetype = tipo))+
  geom_line(size = 1.5, color = '#0353a4')+
  scale_x_continuous(breaks = seq(2014, 2024))+
  scale_linetype_manual(values = c(1, 4))+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  scale_y_continuous(limits = c(-5, 5),
                     breaks = seq(-5, 5, 2),
                     labels = \(x) paste0(x, '%'))+
  labs(title = 'PIB',
       subtitle = 'Variação anual',
       caption = 'Fonte: Fonte: Banco Mundial e Relatório Focus 29/10/2021.',
       x = '',
       y = '',
       linetype = '')


# grafico de barras
pib %>% 
  select(1, 5:11) %>% 
  pivot_longer(!1, names_to = 'ano', values_to = 'var_pib') %>% 
  mutate(ano = as.integer(substr(ano, 1, 4))) %>% 
  bind_rows(pib_focus) %>% 
  mutate(tipo = ifelse(ano <= 2020, 'observado', 'focus'),
         tipo = factor(tipo,
                       levels = c('observado', 'focus'),
                       labels = c('Observado', 'Mediana Focus'))) %>% 
  ggplot(aes(x = ano, y = var_pib, fill = tipo))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=paste0(format(round(var_pib, 2), nsmall = 2), '%'),
                vjust = ifelse(var_pib > 0, -0.5, 1.2)))+
  scale_x_continuous(breaks = seq(2014, 2024))+
  scale_fill_manual(values = c('#0353a4', 'grey60'))+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  scale_y_continuous(limits = c(-5, 5),
                     breaks = seq(-5, 5, 2),
                     labels = \(x) paste0(x, '%'))+
  labs(title = 'PIB',
       subtitle = 'Variação anual',
       caption = 'Fonte: Banco Mundial e Relatório Focus 29/10/2021.',
       x = '',
       y = '',
       fill = '')
  


#### ---- SELIC ---- ####

selic<- read_csv2('selic.csv')

tibble(data = seq.Date(from = as.Date('2021-12-01'),
                       to = as.Date('2024-12-01'),
                       by = "month"),
       ano = lubridate::year(data),
       selic = case_when(
         ano == 2021 ~ 9.25,
         ano == 2022 ~ 10.25,
         ano == 2023 ~ 7.25,
         ano == 2024 ~ 6.75
       )) -> selic_focus
  

selic %>% 
  rename('data' = 1,
         'selic' = 2) %>% 
  mutate(ano = lubridate::year(data)) %>% 
  filter(ano >= 2014) %>% 
  bind_rows(selic_focus) %>% 
  mutate(tipo = ifelse(data < as.Date('2021-12-01'), 'observado', 'focus'),
         tipo = factor(tipo,
                       levels = c('observado', 'focus'),
                       labels = c('Observado', 'Mediana Focus'))) %>% 
  ggplot(aes(x = data, y = selic, color = tipo))+
  geom_line(size = 1.2)+
  scale_y_continuous(limits  = c(0, 16),
                     breaks = seq(0, 16, 2),
                     labels = \(x) paste0(x, '%'))+
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')+
  scale_color_manual(values = c('#0353a4', 'grey60'))+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  labs(title = 'Taxa Selic',
       subtitle = 'Observada e esperada',
       x = '',
       y = '',
       color = '',
       caption = 'Fonte: Banco Central do Brasil e Relatório Focus 29/10/2021.')

#### ---- EMPREGO E RENDA ---- ####

desemprego <- read.csv2('desemprego.csv')

desemprego %>% 
  mutate(taxa = as.numeric(taxa),
         mes = substr(trimestre, 1, 3),
         ano = substr(trimestre, 13, 16),
         data = seq.Date(as.Date('2012-03-01'),
                         as.Date('2021-08-01'),
                         by = 'month')) %>% 
  filter(ano >= 2014) %>% 
  ggplot(aes(x = data, y = taxa))+
  geom_line(size = 1, color = '#0353a4')+
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')+
  scale_y_continuous(limits = c(5, 15),
                     labels = \(x) paste0(x, '%'))+
  theme_minimal()+
  labs(title = 'Taxa de desemprego',
       x = '',
       y = '',
       caption = 'Fonte: PNADC/IBGE.') -> p1


renda <- read.csv2('rendimento.csv')

renda %>% 
  mutate(mes = substr(trimestre, 1, 3),
         ano = substr(trimestre, 13, 16),
         data = seq.Date(as.Date('2012-03-01'),
                         as.Date('2021-08-01'),
                         by = 'month')) %>% 
  filter(ano >= 2014) %>% 
  ggplot(aes(x = data, y = media))+
  geom_line(size = 1, color = '#0353a4')+
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')+
  scale_y_continuous(limits = c(2400, 2800),
                     labels = scales::dollar_format(prefix = 'R$ ',
                                                    big.mark = '.',
                                                    decimal.mark = ','))+
  theme_minimal()+
  labs(title = 'Rendimento médio do trabalho',
       x = '',
       y = '',
       caption = 'Fonte: PNADC/IBGE.') -> p2


library(patchwork)
p1/p2

#### ---- CAMBIO ---- ####

cambio <- read_csv2('cambio.csv')

cambio %>% 
  rename('data' = 1,
         'cambio' = 2) %>% 
  mutate(ano = lubridate::year(data)) %>% 
  filter(ano >= 2014) %>% 
  ggplot(aes(x = data, y = cambio))+
  geom_line(size = 1, color = '#0353a4')+
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')+
  scale_y_continuous(limits = c(2, 6))+
  theme_minimal()+
  labs(title = 'Taxa de Câmbio Nominal',
       subtitle = 'R$/US$, cotação de venda',
       x = '',
       y = '',
       caption = 'Fonte: Banco Central do Brasil.')

#### ---- CREDITO ---- ####

#variação

credito<-read_csv2('credito.csv')

credito %>% 
  rename('data' = 1,
         'pj' = 2,
         'pf' = 3) %>% 
  mutate(ano = lubridate::year(data)) %>% 
  filter(ano >= 2014) %>% 
  ggplot(aes(x = data, y = pf))+
  geom_line(size = 1, color = '#0353a4')+
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')+
  scale_y_continuous(limits = c(-25, 25),
                     breaks = seq(-25, 25, 10),
                     labels = \(x) paste0(x, '%'))+
  theme_minimal()+
  labs(title = 'Concessões de crédito não-rotativo',
       subtitle = 'Pessoa Física, var. % em 12 meses',
       x = '',
       y = '',
       caption = 'Fonte: Banco Central do Brasil.')


### volume

library(deflateBR)

credito_total <- read_csv2('credito_livre_direcionado.csv')

#breaks e limits para o gráfico

breaks <- \(x) {
  if (max(x) < 100) seq(20, 80, 20) else seq(200, 400, 50)
}

limits <- \(x) {
  if (max(x) < 100) c(20, 81) else c(200, 400)
}

# gráfico
credito_total %>% 
  slice(-94) %>% 
  rename('data' = 1,
         'livre_mi' = 2,
         'direcionado_mi' = 3) %>% 
  separate(data, into = c('mes', 'ano'), sep = '/') %>% 
  mutate(data = lubridate::make_date(year = ano,
                                     month = mes)) %>% 
  pivot_longer(!c(data, mes, ano), names_to = 'tipo', values_to = 'valor')  %>% 
  mutate(valor = as.numeric(str_replace(valor, '\\.', '')),
         valor_real = deflate(valor, data, '09/2021'),
         tipo = factor(tipo, 
                       levels = c('livre_mi',
                                  'direcionado_mi'),
                       labels = c('Livre',
                                  'Direcionado'))) %>% 
  ggplot(aes(x = data, y = valor_real/1000, color = tipo))+
  geom_line(size = 1.5, color = '#0353a4')+
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')+
  scale_y_continuous(breaks = breaks,
                     limits = limits,
                     labels = scales::dollar_format(prefix = 'R$ '))+
  theme_minimal()+
  theme(plot.margin = margin(.2, .5, .2, .2, unit = 'cm'))+
  facet_wrap(~tipo,
             scales = 'free_y')+
  theme(legend.position = 'bottom')+
  labs(title = 'Concessões mensais de Crédito com ajuste sazonal',
       subtitle = 'Em bilhões de reais, corrigidos para 09/2021 pelo IPCA',
       x = '',
       y = '',
       color = '',
       caption = 'Fonte: Banco Central do Brasil.')


#### ---- CONFIANÇA CONSUMIDOR (VER OUTRA FONTE) ---- ####

icc<- read.csv2('conf_consumidor.csv', encoding = 'latin1')


icc %>% 
  slice(-94) %>% 
  rename('data' = 1,
         'icc' = 2) %>% 
  separate(data, into = c('mes', 'ano'), sep = '/') %>% 
  mutate(data = lubridate::make_date(year = ano,
                                     month = mes)) %>% 
  filter(ano >= 2014) %>% 
  mutate(icc = as.numeric(str_replace(icc, '\\,', '\\.'))) %>% 
  ggplot(aes(x = data, y = icc))+
  geom_line(size = 1, color = '#0353a4')+
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')

#### ---- INDICADORES VAREJO ---- ####

varejo <- get_sidra(api = '/t/3416/n1/all/n3/all/v/all/p/last%2092/c11046/33534,90668,90669/d/v564%201,v565%201')

#variação acumulada no ano:
#tipos_de_indice == 'Variação acumulada no ano (base: igual período do ano anterior)'
#mes == 12

varejo %>% 
  select(5, 7, 9, 10, 13) %>% 
  janitor::clean_names() %>% 
  separate(mes_codigo, into = c('ano', 'mes'), sep = 4) %>% 
  mutate(data = lubridate::make_date(year = ano,
                                     month = mes),
         manter = ifelse(ano == '2021' & mes == '08', 'manter', ifelse(
           ano != '2021' & mes == '12', 'manter', 'descartar')),
         ano = ifelse(ano == '2021', '2021*', ano)) %>%
  filter(brasil_e_unidade_da_federacao %in% c('Brasil', 'Rio Grande do Sul',
                                              'Santa Catarina'),
         variavel == 'Índice de volume de vendas no comércio varejista',
         manter == 'manter',
         tipos_de_indice == 'Variação acumulada no ano (base: igual período do ano anterior)') %>% 
  ggplot(aes(x = ano, y = valor, fill = brasil_e_unidade_da_federacao))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_text(aes(label = paste0(round(valor, 2), '%'),
                group = brasil_e_unidade_da_federacao,
                vjust = ifelse(valor > 0, -0.5, 1)),
            position = position_dodge(width = .9),
            size = 2.5)+
  scale_fill_manual(values = c('#0353a4', '#001233', "#33415c"))+
  scale_y_continuous(limits = c(-6.2, 15))+
  theme_minimal()+
  theme(legend.position = 'bottom',
        plot.caption = element_text(lineheight = 1))+
  labs(title = 'Volume de vendas do comércio varejista',
       subtitle = 'Variação Anual',
       x = '',
       y = '',
       fill = '',
       caption = '*Até o mês de Agosto.
       Fonte: Pesquisa Mensal de Comércio/IBGE.')
  

?theme

#### ---- CAGED ---- ####
library(basedosdados)
set_billing_id('basedosdados-316822')

### geral
"SELECT ano,
       mes,
       sigla_uf, 
       SUM(admitidos) AS adm,
       SUM(desligados) AS desl,
       SUM(saldo_movimentacao) AS saldo 
FROM `basedosdados.br_me_caged.microdados_estabelecimentos`
GROUP BY ano, mes, sigla_uf 
ORDER BY sigla_uf, ano, mes" %>% 
  read_sql() %>% 
  mutate_if(bit64::is.integer64, as.integer)-> caged

caged %>% 
  group_by(ano, mes) %>% 
  summarise(adm = sum(adm),
            desl = sum(desl),
            saldo = sum(saldo)) %>%
  mutate(data = lubridate::make_date(year = ano,
                                     month = mes)) %>%
  pivot_longer(!c(ano, mes, data), values_to = 'total', names_to = 'tipo') %>% 
  filter(tipo == 'saldo') %>% 
  ggplot(aes(x = data,
             y = total/1000)) + 
  geom_bar(stat = 'identity', fill = '#0353a4')+
  scale_x_date(date_breaks = '2 month',
               date_labels = '%m/%Y')+
  scale_y_continuous(limits = c(-1000, 500),
                     breaks = seq(-1000, 500, 250))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))+
  labs(title = 'Saldo de empregos formais',
       subtitle = 'Em milhares',
       x = '',
       y = '',
       caption = 'Fonte: Novo Caged. Extraído por meio da Base dos Dados.')

# RS e SC

caged %>% 
  filter(sigla_uf %in% c('RS', 'SC')) %>% 
  group_by(ano, mes, sigla_uf) %>% 
  summarise(adm = sum(adm),
            desl = sum(desl),
            saldo = sum(saldo)) %>% 
  mutate(data = lubridate::make_date(year = ano,
                                     month = mes)) %>%
  pivot_longer(!c(ano, mes, data, sigla_uf),
               values_to = 'total', names_to = 'tipo') %>% 
  filter(tipo == 'saldo') %>% 
  ggplot(aes(x = data,
             y = total,
             fill = sigla_uf)) + 
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_x_date(date_breaks = '2 month',
               date_labels = '%m/%Y')+
  scale_fill_manual(values = c('#001233', "#33415c"))+
  scale_y_continuous(limits = c(-90000, 50000),
                     breaks = seq(-90000, 50000, 20000))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5),
        legend.position = 'bottom')+
  labs(title = 'Saldo de empregos formais',
       subtitle = 'Rio Grande do Sul e Santa Catarina',
       x = '',
       y = '',
       fill = '',
       caption = 'Fonte: Novo Caged. Extraído por meio da Base dos Dados.')


### setor de varejo

"SELECT ano,
       mes,
       sigla_uf, 
       cnae_2,
       SUM(admitidos) AS adm,
       SUM(desligados) AS desl,
       SUM(saldo_movimentacao) AS saldo 
FROM `basedosdados.br_me_caged.microdados_estabelecimentos`
GROUP BY ano, mes, sigla_uf, cnae_2 
HAVING cnae_2 = '47539'
ORDER BY sigla_uf, ano, mes" %>% 
  read_sql() %>% 
  mutate_if(bit64::is.integer64, as.integer)-> caged_cnae


caged_cnae %>% 
  group_by(ano, mes) %>% 
  summarise(adm = sum(adm),
            desl = sum(desl),
            saldo = sum(saldo)) %>% 
  mutate(data = lubridate::make_date(year = ano,
                                     month = mes)) %>%
  pivot_longer(!c(ano, mes, data), values_to = 'total', names_to = 'tipo') %>% 
  filter(tipo == 'saldo') %>% 
  ggplot(aes(x = data,
             y = total)) + 
  geom_bar(stat = 'identity', fill = '#0353a4')+
  scale_x_date(date_breaks = '2 month',
               date_labels = '%m/%Y')+
  scale_y_continuous(limits = c(-7000, 7000),
                     breaks = seq(-7000, 7000, 2000))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))+
  labs(title = 'Saldo de empregos formais',
       subtitle = 'Comércio varejista especializado de eletrodomésticos e equipamentos de áudio e vídeo',
       x = '',
       y = '',
       caption = 'Fonte: Novo Caged. Extraído por meio da Base dos Dados.')


# RS e SC

caged_cnae %>% 
  filter(sigla_uf %in% c('RS', 'SC')) %>% 
  group_by(ano, mes, sigla_uf) %>% 
  summarise(adm = sum(adm),
            desl = sum(desl),
            saldo = sum(saldo)) %>% 
  mutate(data = lubridate::make_date(year = ano,
                                     month = mes)) %>%
  pivot_longer(!c(ano, mes, data, sigla_uf),
               values_to = 'total', names_to = 'tipo') %>% 
  filter(tipo == 'saldo') %>% 
  ggplot(aes(x = data,
             y = total,
             fill = sigla_uf)) + 
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_x_date(date_breaks = '2 month',
               date_labels = '%m/%Y')+
  scale_fill_manual(values = c('#001233', "#33415c"))+
  scale_y_continuous(limits = c(-1000, 500))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5),
        legend.position = 'bottom')+
  labs(title = 'Saldo de empregos formais',
       subtitle = 'Comércio varejista especializado de eletrodomésticos e equipamentos de áudio e vídeo',
       x = '',
       y = '',
       fill = '',
       caption = 'Fonte: Novo Caged. Extraído por meio da Base dos Dados.')


