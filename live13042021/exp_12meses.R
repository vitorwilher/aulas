###################################################################
############ Expectativas de inflação vs. inflação ################


library(rbcb)
library(tidyverse)
library(lubridate)
library(latex2exp)

## Coleta de dados

expectativa = get_twelve_months_inflation_expectations('IPCA',
                                                       start_date = '2006-01-01') %>%
  filter(smoothed == 'S' & base == 0) %>%
  select(date, mean, median, min, max) %>%
  group_by(mes = floor_date(date, 'month')) %>%
  summarise(mean = mean(mean),
            median = mean(median),
            min = mean(min),
            max = mean(max)) %>%
  rename(date = mes) %>%
  mutate(exp_mean_12mesesantes = dplyr::lag(mean,12),
         exp_median_12mesesantes = dplyr::lag(median,12),
         exp_min_12mesesantes = dplyr::lag(min, 12),
         exp_max_12mesesantes = dplyr::lag(max,12)) %>%
  select(date, exp_mean_12mesesantes, exp_median_12mesesantes,
         exp_min_12mesesantes, exp_max_12mesesantes)
         
ipca = get_series(13522, start_date = '2007-01-01') %>%
  rename(inflacao = `13522`)

data = inner_join(ipca, expectativa, by='date')


## Visualização de dados

data %>%
  filter(date > '2014-01-01') %>%
  ggplot(aes(x=date))+
  geom_ribbon(aes(ymin = exp_min_12mesesantes, ymax = exp_max_12mesesantes, 
                  colour='Intervalo de Expectativas',
                  fill='Intervalo de Expectativas'),
              alpha=.8)+
  geom_line(aes(y=inflacao, colour='Inflação efetiva',
                fill='Inflação efetiva'), size=.8)+
  geom_line(aes(y=exp_mean_12mesesantes, colour='Expectativa 12 meses antes',
                fill='Expectativa 12 meses antes'), size=.8)+
  scale_colour_manual('', 
                      values = c('Intervalo de Expectativas'="#91b8bd",
                                 'Inflação efetiva'='#244747',
                                 'Expectativa 12 meses antes'='red'))+
  scale_fill_manual('',
                      values = c('Intervalo de Expectativas'="#91b8bd",
                                 'Inflação efetiva'='white',
                                 'Expectativa 12 meses antes'='white'))+
  theme(legend.position = c(.75,.85),
        panel.background = element_rect(colour='white', fill='white'),
        axis.line.x.bottom = element_line(colour='black', linetype = 'solid'),
        axis.line.y.left = element_line(colour='black', linetype = 'solid'),
        plot.title = element_text(size=11, face='bold'),
        plot.subtitle = element_text(size=10, face='italic'),
        plot.caption = element_text(face='bold'))+
  scale_x_date(breaks='1 year',
               date_labels = '%Y')+
  labs(x='', y='% a.a.',
       title='Inflação efetiva vs. Expectativa 12 meses antes',
       subtitle=TeX('$E_{t-12}\\[\\pi_{t}\\]$'),
       caption='Fonte: analisemacro.com.br com dados do BCB')
  
