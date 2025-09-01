library(tidyverse)
library(showtext)
library(ggtext)
library(colortools)
library(tmaptools)


# remotes::install_github("gmlang/colortools")

# install.packages("https://cran.r-project.org/src/contrib/Archive/colortools/colortools_0.1.5.tar.gz", repos = NULL, type = "source")


font_add_google("Libre Franklin", "franklin")
font_add_google("Gelasio", "gelasio")
showtext_opts(dpi = 300)
showtext_auto()


data <- tribble(
  ~category,~gain,~none,~lose,~group,
  "Total",21,37,41,"total",
  "Men",24,42,34,"gender",
  "Women",18,34,47,"gender",
  "White",24,43,33,"race",
  "Black",11,20,66,"race",
  "Hispanic",17,29,52,"race",
  "Asian*",15,38,47,"race",
  "Ages 18-29",22,30,47,"age",
  "30-49",22,34,42,"age",
  "50-64",21,42,37,"age",
  "65+",17,43,39,"age",
  "Postgrad",17,38,45,"education",
  "College grad",17,43,39,"education",
  "Some college",22,33,43,"education",
  "HS or less",23,37,40,"education",
  "Rep/Lean Rep",35,51,13,"politics",
  "<span style='color:gray50'>Conserv</span>",39,54,6,"politics",
  "<span style='color:gray50'>Mod/Lib</span>",29,47,23,"politics",
  "Dem/Lean Dem",7,23,69,"politics",
  "<span style='color:gray50'>Cons/Mod</span>",8,25,66,"politics",
  "<span style='color:gray50'>Liberal</span>",6,20,74,"politics"
) %>%
  mutate(group = factor(group,
                        levels = c("total", "gender", "race",
                                   "age", "education", "politics")),
         category = factor(category, levels = rev(category)))



data %>%
  pivot_longer(-c(category, group),
               names_to = "effect", values_to = "percent") %>%
  mutate(effect = factor(effect, levels = c("lose", "none", "gain"))) %>%
ggplot(aes(x = percent, y = category, fill = effect,label=percent))+
     geom_col()+
  geom_text(position = position_stack(vjust=0.5))+
  facet_grid(group~.,scales='free_y',space='free_y')+
  coord_cartesian(expand = FALSE,clip = 'off')+
  scale_fill_manual(
    breaks = c("gain","none","lose"),
    values = c("red","white","blue")
  )+
  theme(legend.position = "top", panel.grid = element_blank(),panel.background = element_blank())
  
  
  






# =========== color palette

# No margin
par(mar=c(0,0,1,0))

# Load RColorBrewer
library(RColorBrewer)

# Classic palette BuPu, with 4 colors
coul <- brewer.pal(4, "PuOr") 

# Add more colors to this palette :
coul <- colorRampPalette(coul)(25)

# Plot it
pie(rep(1, length(coul)), col = coul , main="") 
  
  

library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()
  
  
