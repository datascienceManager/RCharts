library(tidyverse)
library(ggplot2)
library(gghighlight)
library(ggtext)
library(showtext)

font_add_google("Source Sans Pro", "source_sans_pro")
font_add_google("Merriweather", "merriweather")
showtext_auto()
# 
# df <- read.csv("https://ourworldindata.org/grapher/time-spent-with-relationships-by-age-us.csv?v=1&csvType=full&useColumnShortNames=true")

# 
# df %>% ggplot(aes(x=age,y=minute()))

setwd('/Users/datascientist/ggplot')
dir()

df = read.csv("time-spent-with-relationships-by-age-us.csv")%>% as_tibble()%>%janitor::clean_names() %>% rename_with(
  ~c('Entitity', 'Code', 'Age', 'alone', 'friends', 'children', 'parents', 
     'partner', 'coworkers')
) %>% 
  pivot_longer(
    cols = alone:coworkers, 
    names_to = 'person',
    values_to = 'minutes'
  ) %>% 
  janitor::clean_names() %>% 
  filter(age <= 80) %>%
  select(age:minutes)




colors <- thematic::okabe_ito(2)
# title_text <- glue::glue(
#   'Around the age of 40, we spend less time with <span style = "color:{colors[1]}">**children**</span><br> and more time <span style = "color:{colors[2]}">**alone**</span>'
# )

title_text <- glue::glue(
  'Around the age of 40,we spend less time with
  <span style = "color:{colors[1]}">**children**</span><br> and more time <span style = "color:{colors[2]}">**alone**</span>'
)

title_text <- glue::glue(
  'Around the age of 40, we spend less time with<br>
   <span style="color:{colors[1]}"><b>children</b></span><br>
   and more time <span style="color:{colors[2]}"><b>alone</b></span>'
)


df %>%
  ggplot(aes(x = age, y = minutes, color = person)) +
  geom_line(linewidth = 1.5) +
  theme_minimal(
    base_size = 24,
    base_family = 'Source Sans Pro'
  ) +
  labs(
    x = 'Age',
    y = element_blank(),
    title = title_text,
    subtitle = 'Daily time spent with others (in minutes)',
    caption = 'Data: Our World in Data'
  ) +
  gghighlight(
    person %in% c('alone', 'children'), 
    age >= 38, 
    use_direct_label = F,
    use_group_by = FALSE,
    unhighlighted_params = list(color = 'grey80', linewidth = 1)
  ) +
  annotate(
    'text',
    x = 80,
    y = c(0, 15),
    size = 8,
    label = c('alone', 'children'),
    family = 'Source Sans Pro',
    fontface = 'bold',
    hjust = 1,
    color = thematic::okabe_ito(2)
  ) +
  annotate(
    'segment',
    x = 38,
    xend = 38,
    y = 0,
    yend = 10,
    linetype = 2,
    linewidth = 1,
    color = 'grey10'
  ) +
  scale_color_manual(values = thematic::okabe_ito(2)) +
  theme(
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.5, linetype = 2, color = 'grey85'),
    plot.title.position = 'plot',
    text = element_text(color = 'grey20'),
    axis.text = element_text(color = 'grey40'),
    plot.title =ggtext::element_markdown(
      color = 'grey20', family = 'Merriweather'
    ),
    plot.subtitle = element_text(
      color = 'grey30', 
      size = rel(0.8), 
      margin = margin(b = 8, unit = 'mm')
    ),
    plot.caption = element_text(size = rel(0.6), color = 'grey20')
  )






#============ archive =========



df%>% 
  ggplot(aes(x = age, y = minutes, color = person)) +
  geom_line(linewidth = 1.5) +
  theme_minimal(base_size = 24,base_family = 'source_sans_pro')+
  labs(x='Age',y=element_blank(),title ='Time with others as we get older',
       subtitle = 'Seond title')+
  # labs(x='Age',y=element_blank(),title =title_text, #'Time with others as we get older',
  #      subtitle = 'Seond title')+
  gghighlight(
    person %in% c('alone', 'children'), 
    age >= 38, 
    use_direct_label = F,
    use_group_by = FALSE,
    unhighlighted_params = list(color = 'grey80', linewidth = 1)
  )+
  annotate(
    'text',
    x = 80,#----- x axis units
    y = c(0, 10), #------- y axis units
    size = 8,
    label = c('alone', 'children'),
    family = 'Source Sans Pro',
    fontface = 'bold',
    hjust = 1,
    color = thematic::okabe_ito(2)
  ) +
  annotate(
    'segment',#--- Adding dash vertical line 
    x = 40,
    xend = 40,
    y = 0,
    yend=5,
    linetype=2,
    linewidth=1,
    color='grey10'
  )+
  scale_color_manual(values = thematic::okabe_ito(2))+
  theme(
    legend.position = 'none',
    panel.grid.major = element_blank(),
    panel.grid.minor = element_line(linewidth = 0.2,linetype = 1),#--- grid line fontsize
    plot.title.position = 'plot',#--- Title positioning 
    text = element_text(colour = 'grey20'),
    axis.text = element_text(colour = 'grey40'),
    plot.title = ggtext::element_markdown(colour = 'grey40',family = 'Merriweather'),
    plot.subtitle = element_text(
      color = 'grey30',
      size = rel(0.8),
      margin = margin(b=8,unit='mm')
    ),
    plot.caption = element_text(size=(0.6),colour = 'grey20')
  )


