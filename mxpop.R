library(tidyverse)
library(poissoned)
library(showtext)
library(extrafont)

font_import()


showtext_auto()


mx_pop <- read.csv("mex_pop.csv")


### Jittered plot

mx_pop %>%
        uncount(round(densidades/5)) %>%
        ggplot(aes(x = 0, y = 0)) +
        geom_jitter(fill = col_sea)+
        facet_wrap(~estado)



### Poissoned style

mx_poissoned <- mx_pop %>%
        rowwise() %>%
        mutate( 
                t = sqrt(densidades) ,
        pnts = list(poisson_disc(ncols = t, nrows = t, cell_size = 1/t))) %>%
        ungroup() %>%
        unnest(pnts) %>%
        rowwise() %>%
        mutate(angle = runif(1,0,360)) %>%
        ungroup()
        

# Fonts 

font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "montserrat", "Montserrat")



ggplot(data = mx_poissoned) +
        geom_tile(aes(0.5,0.5,width =1.07,height = 1.07), fill = col_sea, color = "grey97",size =0.5,stat = "unique")+
        geom_text(data = mx_pop,aes(0.5,-0.23, label = format(round(densidades), big.mark = ",")),alpha = 1,
                  family = "patua-one", fontface = "bold", size = 5, vjust = 0.2, color = "grey97")+
        geom_text(data = mx_poissoned,aes(x,y,label ="◀", angle = angle),size = 2.5, color = "#4589FF",family ="montserrat")+ 
        facet_wrap(~estado)+
        labs(title = "Population density by state in Mexico",
             subtitle = "Habitants/km²",
             caption = "Source: INEGI | Graph: Miguel HG | IG: _miguelhg")+
        theme_bw(base_size = 15, base_family = "montserrat")+
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "#5886a5"),
              strip.text = element_text(size = 12, family = f2, face = "bold"),
              strip.background = element_blank(),
              strip.text.x = element_text(color = "white"),
              strip.text.y = element_text(color = col_bg, angle = -90),
              plot.background = element_rect(fill = "#262626", color = NA),
              plot.title = element_text(size = 35, color = "white"),
              plot.subtitle = element_text(size = 20, color = "white"),
              plot.caption = element_text(color = "white"))
        
ggsave("mxpop.jpg",plot = last_plot(), height = 37,width = 43,units = "cm", dpi = 60)




ggplot(data = mx_poissoned) +
        geom_tile(aes(0.5,0.5,width =1.07,height = 1.07), fill = col_sea, color = "grey97",size =0.5,stat = "unique")+
        geom_text(data = mx_pop,aes(0.5,-0.23, label = format(round(densidades), big.mark = ",")),alpha = 1,
                  family = "patua-one", fontface = "bold", size = 5, vjust = 0.2, color = "grey97")+
        geom_text(data = mx_poissoned,aes(x,y,label ="◀", angle = angle),size = 2.5, color = "#4589FF",family ="montserrat")+ 
        facet_wrap(~estado)+
        labs(title = "Densidad de población por estado en México",
             subtitle = "Habitantes/km²",
             caption = "Fuente: INEGI | Gráfica: Miguel HG | IG: _miguelhg")+
        theme_bw(base_size = 15, base_family = "montserrat")+
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "#5886a5"),
              strip.text = element_text(size = 12, family = f2, face = "bold"),
              strip.background = element_blank(),
              strip.text.x = element_text(color = "white"),
              strip.text.y = element_text(color = col_bg, angle = -90),
              plot.background = element_rect(fill = "#262626", color = NA),
              plot.title = element_text(size = 35, color = "white"),
              plot.subtitle = element_text(size = 20, color = "white"),
              plot.caption = element_text(color = "white"))



ggsave("mxpop_esp.jpg",plot = last_plot(), height = 37,width = 43,units = "cm", dpi = 70)

        