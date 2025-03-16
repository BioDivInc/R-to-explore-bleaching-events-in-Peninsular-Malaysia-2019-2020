This is part II of my SQL project to play around with data collected in Malaysia in 2019/2020 by Szereday et al., 2024, investigating the effects of back-to-back thermal stress events on coral health (i.e., bleaching response). 

Here, I used the R package `dplyr` and `tidyr` to wrangle the data the same way I did in MS SQL Server and additionally visualized the results using the `ggplot2` package:

1) Bleaching Scores per site,
2) Bleaching Scores per genus,
3) Differences in Bleaching Scores between 2019 and 2020 for Morphotaxa.

Please find attached the raw data, provided by Szereday et al., 2024, the R file, and a .txt file for a quick look of the code if desired.


Additionally, the `readxl` package is helpful to load a .xlsx file as input.
``` R
#load essential libraries
library(readxl) #to read in the provided excel file
library(dplyr) #for data manipulation
library(ggplot2) #easy data visualization
```

Read in the raw data and prepare the dataframe for follow-up data wrangling
```R
#set working directory and load input file
setwd('NA') #use your own directory
data_raw <- read_excel("./Bleaching Data_Szereday et al. 2019-2020.xlsx") #raw data

#adjust df: drop columns, readjust order and rename columns
data_eval <- data_raw %>% select (-c(Wind:Habitat, Bleaching_Binary:max_DHW, avg_DHW:CRW_DHW)) #drop 'uninteresting' columns and create new df
data_eval <- data_eval[,c(1,2,6,3,4,7,5)] #reorders columns based on column number in df
data_eval <- data_eval %>% 
             rename(Depth=3, Bleaching_Score=7) %>%
             drop_na() #if there are NA cells, get rid of them
```

First, we can investigate a putative difference in bleaching scores per depth.
```R
#calculate the average Bleaching_Score per depth and round the value if numeric
depth <- data_eval %>%
         group_by(Depth) %>%
         summarize(avg_bleaching_score = mean(Bleaching_Score, na.rm=T)) %>%
         mutate_if(is.numeric, round, 1) %>%
         drop_na()
print(depth)
```

To include other interesting variables, such as Morphotaxa, Year, Depth and Site, we can run following code:
```R
#calculate the average Bleaching_Score per Morphotaxa, Year, Depth, Site
Morphotaxa <- data_eval %>%
              group_by(Morphotaxa, Year, Depth, Site) %>%
              summarize(avg_bleaching_score = mean(Bleaching_Score, na.rm=T)) %>% #calculate the average grouped bleaching score (group_by)
              mutate_if(is.numeric, round, 1) %>%
              drop_na() %>%
              arrange(avg_bleaching_score) #default ascending

#distinct df of top 20 entries
distinct_Morphotaxa_asc <- Morphotaxa %>% distinct(Morphotaxa, Year, Depth, Site, .keep_all = T) %>% arrange(avg_bleaching_score)
distinct_Morphotaxa_desc <- Morphotaxa %>% distinct(Morphotaxa, Year, Depth, Site, .keep_all = T) %>% arrange(desc(avg_bleaching_score))

print(n=20, distinct_Morphotaxa_asc)
print(n=20, distinct_Morphotaxa_desc)
```
The output shows the distinct, without duplicates, average bleaching scores in ascending and descening order.


To get a better idea of how the data looks like, we can visualize the results, depending on site and year, using the `ggplot2` package. Furthermore, to save a high resolution figure the `ggsave()`command is a good choice. 
```R
#visualize the bleaching score depending on site and year
data_eval$Year <- factor(data_eval$Year, levels = c('2019', '2020'))

site_plot <- ggplot(data_eval, aes(x=Year, y=Bleaching_Score, fill=Year))+
  geom_boxplot(width=0.7, lwd=0.5, fatten=2.5)+
  facet_grid(~Site, )+ #create facets using the sites
  scale_fill_manual(values= c("#0065b3", "#FFB74D"))+
  scale_y_continuous(limits=c(1, 6), breaks = seq(1, 6, by=1))+
  labs(y = "Bleaching Score")+
  theme_classic()+ #just an example of cleaning up the appearance a bit; theme_light()) is also great
  theme(
    axis.text.x = element_text(color = 'black', size = 12),
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title.x = element_text(color = 'black', size = 15),
    axis.title.y = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),  
    strip.text.x = element_text(color = 'black', size = 12),
    panel.spacing = unit(2, "lines"), #increase spacing between individual facets
    strip.background = element_blank(), #clean up the appearance; gets rid of the box around the facets' text
    legend.position = 'none'
  )
site_plot

#save figure in high resolution
ggsave("Bleaching_Score_Sites.tiff", units="cm", width=35, height=25, dpi=600, compression = 'lzw')
dev.off()
```

Same can be done for genus and year as depending variables.
```R
#visualize the bleaching score depending on genus, year
genus_plot <- ggplot(data_eval, aes(x=Genus, y=Bleaching_Score, fill=Year))+
  geom_boxplot(width=0.7, lwd=0.5, fatten=2.5)+
  scale_fill_manual(values= c("#0065b3", "#FFB74D"))+
  scale_y_continuous(limits=c(1, 6), breaks = seq(1, 6, by=1))+
  labs(y = "Bleaching Score")+
  theme_light()+ #just an example of cleaning up the appearance a bit; theme_classic() is also great
  theme(
    axis.text.x = element_text(color = 'black', size = 12, angle = 90, vjust = 0.35, hjust = .90), #adjust angle of axis text as well as vertical and horizontal justification
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title.x = element_text(color = 'black', size = 15),
    axis.title.y = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),  
    strip.text.x = element_text(color = 'black', size = 12),
    legend.position = 'right'
  )
genus_plot

#save figure in high resolution
ggsave("Bleaching_Score_Genus.tiff", units="cm", width=45, height=25, dpi=600, compression = 'lzw')
dev.off()
```

For a more intersting comparison, the difference in bleaching score between year 2019 and 2020 can be evaluated. This can help to visualize a putative difference in heat stress susceptibility between Morphotaxa and years. Here, a negative value corresponds to a decrease in the bleaching score in the year 2020 compared to 2019.
```R
#calculate the difference per morphotaxa first
relative_diff_2019 <- data_eval %>%
                 filter(Year == 2019) %>%
                 group_by(Morphotaxa, Site, Depth) %>% #group by parameters and use mutate to add IDs within selected groups
                 mutate(ID = row_number()) %>%
                 rename(Bleaching_Score_2019 = 7)

relative_diff_2020 <- data_eval %>%
                 filter(Year == 2020) %>%
                 group_by(Morphotaxa, Site, Depth) %>% #group by parameters and use mutate to add IDs within selected groups
                 mutate(ID = row_number()) %>%
                 rename(Bleaching_Score_2020 = 7)

relative_diff_total <- relative_diff_2019 %>%
                 inner_join(relative_diff_2020, by = c('ID', 'Morphotaxa', 'Depth', 'Site', 'Genus', 'Form')) %>% #inner join based on parameters that are present and of interested for us in both df
                 select(-c('Year.x', 'Year.y', 'ID')) %>% #deselect columns we created by joining
                 mutate(ΔBleaching_Score = Bleaching_Score_2019-Bleaching_Score_2020) #add the column to calculate the difference between 2019 and 2020

#distinct df of difference between 2019 and 2020 of top 20 entries
distinct_relative_diff_asc <- relative_diff_total %>% distinct(Site, Depth, Genus, Form, Morphotaxa, .keep_all = T) %>% arrange(ΔBleaching_Score)
distinct_relative_diff_desc <- relative_diff_total %>% distinct(Site, Depth, Genus, Form, Morphotaxa, .keep_all = T) %>% arrange(desc(ΔBleaching_Score))

print(n=20, distinct_relative_diff_asc)
print(n=20, distinct_relative_diff_desc)
```

Visualize the difference in bleaching scores per morphotaxa.
```R

#visualize the relative differences in bleaching scores between years; sorted alphabetically by default
relative_diff_plot <- ggplot(relative_diff_total, aes(x=ΔBleaching_Score, y=Morphotaxa, fill=Morphotaxa))+
  geom_boxplot(aes(x=ΔBleaching_Score, y=Morphotaxa), width = 0.7)+
  scale_x_continuous(limits=c(-6, 6), breaks = seq(-6, 6, by=1))+
  labs(x='ΔBleaching Score')+
  geom_vline(xintercept = 0)+ #vertical line to highlight Δ=0 -> no difference between 2019 and 2020
  theme_light()+ #just an example of cleaning up the appearance a bit; theme_classic() is also great
  scale_fill_manual(values = c ("black",#placeholder for only medians (+outliers)
                                "black",
                                "black",
                                "black",
                                "black",
                                "black",
                                "#FFB74D",
                                "black",
                                "cornflowerblue",
                                "#da7959",
                                "#C0CA33",
                                "black",
                                "#00BCD4",
                                "skyblue",
                                "black",
                                "#e2738c",
                                "#B39DDB",
                                "black",
                                "black", 
                                "#FFB74D",
                                "black",
                                "black",
                                "black",
                                "#9CCC65",
                                "#ba3c3c",
                                "black",
                                "darkseagreen",
                                "black",
                                "black",
                                "#00BCD4",
                                "black",
                                "black",
                                "#B1B3B3FF",
                                "#98B8D3",
                                "#f39f18",
                                "#7986CB",
                                "#FCDE70",
                                "black", 
                                "black",
                                "black",
                                "black"))+
  theme(
    axis.text.x = element_text(color = 'black', size = 12),
    axis.text.y = element_text(color = 'black', size = 12),
    axis.title.x = element_text(color = 'black', size = 15),
    axis.title.y = element_text(color = 'black', size = 15),
    axis.ticks = element_line(color = 'black'),  
    strip.text.x = element_text(color = 'black', size = 12),
    legend.position = 'none')
relative_diff_plot            

#save figure in high resolution
ggsave("Bleaching_Score_Differences.tiff", units="cm", width=25, height=35, dpi=600, compression = 'lzw')
dev.off()
```
