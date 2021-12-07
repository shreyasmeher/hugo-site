library(rgdal) # plot map
library(leaflet) # plot map
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # plot
library(plotly) # interactive plot
library(shiny) # shiny
library(shinydashboard) # shiny dashboard

# general data
footprint <- read.csv("countries.csv")

## data cleaning


footprint1 <- footprint %>%
  mutate(Country = as.character(Country), GDP.per.Capita = as.numeric(gsub("[$,]",
                                                                           "", footprint$GDP.per.Capita)), HDI = round(HDI,
                                                                                                                       2), Countries.Required = round(Countries.Required,
                                                                                                                                                      2), Biocapacity.Deficit = as.factor(ifelse(Biocapacity.Deficit >
                                                                                                                                                                                                   0, "Reserve", "Deficit"))) %>%
  rename(Status = Biocapacity.Deficit) %>%
  select(-c(Data.Quality)) %>%
  drop_na()



# Ecological Footprint and Biocapacity plots

ef_region <- footprint %>%
  group_by(Region) %>%
  summarize(Ecological.Footprint = sum(Total.Ecological.Footprint)) %>%
  arrange(desc(Ecological.Footprint)) %>%
  mutate(text = paste0("Ecological Footprint: ",
                       Ecological.Footprint, " gha"))


ef_reg_plot <- ggplot(ef_region, aes(x = reorder(Region,
                                                 Ecological.Footprint), y = Ecological.Footprint,
                                     text = text)) + geom_col(aes(fill = Ecological.Footprint),
                                                              show.legend = F) + coord_flip() + labs(title = "Ecological Footprint by Region",
                                                                                                     y = "global hectares (gha)", x = NULL) + scale_y_continuous(limits = c(0,
                                                                                                                                                                            150), breaks = seq(0, 150, 25)) + scale_fill_gradient(low = "#F78181",
                                                                                                                                                                                                                                  high = "#3B0B0B") + theme(plot.title = element_text(face = "bold",
                                                                                                                                                                                                                                                                                      size = 14, hjust = 0.04), axis.ticks.y = element_blank(),
                                                                                                                                                                                                                                                            panel.background = element_rect(fill = "#ffffff"),
                                                                                                                                                                                                                                                            panel.grid.major.x = element_line(colour = "grey"),
                                                                                                                                                                                                                                                            axis.line.x = element_line(color = "grey"), axis.text = element_text(size = 10,
                                                                                                                                                                                                                                                                                                                                 colour = "black"))

ggplotly(ef_reg_plot, tooltip = "text")


b_region <- footprint %>%
  group_by(Region) %>%
  summarize(Biocapacity = sum(Total.Biocapacity)) %>%
  arrange(desc(Biocapacity)) %>%
  mutate(text = paste0("Biocapacity: ", Biocapacity,
                       " gha"))

b_reg_plot <- ggplot(b_region, aes(x = reorder(Region,
                                               Biocapacity), y = Biocapacity, text = text)) +
  geom_col(aes(fill = Biocapacity), show.legend = F) +
  coord_flip() + labs(title = "Biocapacity by Region",
                      y = "global hectares (gha)", x = NULL) + scale_y_continuous(limits = c(0,
                                                                                             275), breaks = seq(0, 250, 50)) + scale_fill_gradient(low = "#9AFE2E",
                                                                                                                                                   high = "#0B6121") + theme(plot.title = element_text(face = "bold",
                                                                                                                                                                                                       size = 14, hjust = 0.04), axis.ticks.y = element_blank(),
                                                                                                                                                                             panel.background = element_rect(fill = "#ffffff"),
                                                                                                                                                                             panel.grid.major.x = element_line(colour = "grey"),
                                                                                                                                                                             axis.line.x = element_line(color = "grey"), axis.text = element_text(size = 10,
                                                                                                                                                                                                                                                  colour = "black"))
ggplotly(b_reg_plot, tooltip = "text")

# Scatterplot between GDP and Ecological Footprint

scat_plot_data <- footprint1 %>%
  select(Country, Population.millions, GDP.per.Capita,
         HDI, Total.Ecological.Footprint, Status) %>%
  rename(Population.in.millions = Population.millions,
         Human.Development.Index = HDI, Ecological.Footprint = Total.Ecological.Footprint) %>%
  mutate(text = paste0("Country: ", Country, "<br>",
                       "HDI: ", Human.Development.Index, "<br>", "Ecological Footprint: ",
                       Ecological.Footprint, "<br>", "GDP per Capita: ",
                       "$", GDP.per.Capita))

scat_plot <- ggplot(scat_plot_data, aes(x = GDP.per.Capita,
                                        y = Ecological.Footprint, text = text)) + geom_smooth(col = "#61380B",
                                                                                              size = 0.7) + geom_point(aes(color = Status, size = GDP.per.Capita)) +
  scale_y_continuous(limits = c(0, 18)) + scale_color_manual(values = c("#DF0101",
                                                                        "#04B486")) + labs(title = "GDP on Ecological Footprint",
                                                                                           y = "Ecological Footprint", x = "GDP Per Capita") +
  theme(plot.title = element_text(face = "bold",
                                  size = 14, hjust = 0), panel.background = element_rect(fill = "#ffffff"),
        panel.grid.major.x = element_line(colour = "grey"),
        panel.grid.major.y = element_line(colour = "grey"),
        axis.line.x = element_line(color = "grey"),
        axis.line.y = element_line(color = "grey"),
        axis.text = element_text(size = 10, colour = "black"),
        legend.title = element_blank()) + theme(axis.title = element_text(family = "serif"),
    plot.title = element_text(family = "serif",
        size = 15, face = "plain", hjust = 0.5),
    legend.text = element_text(family = "serif"),
    legend.title = element_text(family = "serif"))

ggplotly(scat_plot, tooltip = "text") %>%
  layout(legend = list(orientation = "v", y = 1,
                       x = 0))
