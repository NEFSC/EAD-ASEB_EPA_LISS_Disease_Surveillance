


# HYPOXIA PLOTS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

path.p              <- "C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "DOSummary_juv.csv$", recursive = FALSE)))) 


Summary.table            <- data.frame(matrix(nrow = 1, ncol = 6)) # create dataframe to save cumunalitively during for loop
colnames(Summary.table)  <- c('filename',
                              'type',
                              'threshold',
                              'numberFound',
                              'avgDurMinutes',
                              'avgAvgDO') # names in the for loop

library(dplyr)
for (m in 1:nrow(file.names.table)) {
  dat_loop              <- as.data.frame(read.csv(paste(path.p,'/',file.names.table[m,1], sep=''))) %>%  #reads in the data files
                              dplyr::mutate(filename = gsub("_Sonde.*","", file.names.table[m,1]))
  #summary_dat              <- summary_dat[!is.na(summary_dat$type),
  
  Summary.table <- rbind(dat_loop, Summary.table) #bind to a cumulative list dataframe
  # print(df_total)
}

Summary.table.om       <- Summary.table[!is.na(Summary.table$filename),] # omit NAs in filename 
Summary.table.final    <- Summary.table.om %>%
                            dplyr::rename(frequency = numberFound,
                                          duration = avgDurMinutes,
                                          magnitude = avgAvgDO) %>% 
                            dplyr::mutate(EQ = (as.numeric(frequency) * as.numeric(duration)) / as.numeric(magnitude) ) %>% 
                            mutate_all(funs(ifelse(is.na(.), 0, .)))  %>% # convert remaining NAs to 0
                            dplyr::mutate(Site = gsub(".*_","",filename),
                                          Date = gsub("_.*","",filename))
Summary.table.final  

library(ggplot2)
Duration_plot <- Summary.table.final %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(duration)/60, fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Duration of hypoxic events") +
  ylab("Duration of events (hours; minimum 1 hour)")# +
  # facet_wrap(~Date)
# Duration_plot

Frequency_plot <- Summary.table.final %>% 
  dplyr::filter(frequency >0) %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(frequency), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Frequency of hypoxic events") +
  ylab("Number of events") #+
  # facet_wrap(~Date)
# Frequency_plot

EQ_plot <- Summary.table.final %>% 
  dplyr::filter(!EQ == 'NaN') %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(EQ), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("(F*D)/M of hypoxic events") +
  ylab("[Freq*Duration/Mean_magnitude]")#  +
  # facet_wrap(~Date) 
# EQ_plot

library(ggpubr)
pdf("C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Events_plots/juvenile_growth_hypoxia.pdf", 
    height = 12, width =10)
ggarrange(Duration_plot,
          Frequency_plot,
          EQ_plot,
          nrow=3)
dev.off()







# Acidification PLOTS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





path.p              <- "C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "pHSummary_juv.csv$", recursive = FALSE)))) 


Summary.table            <- data.frame(matrix(nrow = 1, ncol = 6)) # create dataframe to save cumunalitively during for loop
colnames(Summary.table)  <- c('filename',
                              'type',
                              'threshold',
                              'numberFound',
                              'avgDurMinutes',
                              'avgAvgpH') # names in the for loop

library(dplyr)
for (m in 1:nrow(file.names.table)) {
  dat_loop              <- as.data.frame(read.csv(paste(path.p,'/',file.names.table[m,1], sep=''))) %>%  #reads in the data files
    dplyr::mutate(filename = gsub("_Sonde.*","", file.names.table[m,1]))
  #summary_dat              <- summary_dat[!is.na(summary_dat$type),
  
  Summary.table <- rbind(dat_loop, Summary.table) #bind to a cumulative list dataframe
  # print(df_total)
}

Summary.table.om       <- Summary.table[!is.na(Summary.table$filename),] # omit NAs in filename 
Summary.table.final    <- Summary.table.om %>%
  dplyr::rename(frequency = numberFound,
                duration = avgDurMinutes,
                magnitude = avgAvgpH) %>% 
  dplyr::mutate(EQ = (as.numeric(frequency) * as.numeric(duration)) / as.numeric(magnitude) ) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))  %>% # convert remaining NAs to 0
  dplyr::mutate(Site = gsub(".*_","",filename),
                Date = gsub("_.*","",filename))
Summary.table.final  

library(ggplot2)
Duration_plot <- Summary.table.final %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(duration)/60, fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Duration of acidifcation events") +
  ylab("Duration of events (hours; minimum 1 hour)")# +
  # facet_wrap(~Date)
# Duration_plot

Frequency_plot <- Summary.table.final %>% 
  dplyr::filter(frequency >0) %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(frequency), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Frequency of acidifcation events")  +
  ylab("Number of events") # +
  # facet_wrap(~Date)
# Frequency_plot

EQ_plot <- Summary.table.final %>% 
  dplyr::filter(!EQ == 'NaN') %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(EQ), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("(F*D)/M of acidifcation events") +
  ylab("[Freq*Duration/Mean_magnitude]") #+
  # facet_wrap(~Date)
# EQ_plot

library(ggpubr)
pdf("C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Events_plots/juvenile_growth_acidification.pdf", 
    height = 12, width =10)
ggarrange(Duration_plot,
          Frequency_plot,
          EQ_plot,
          nrow=3)
dev.off()





# Low Salinity PLOTS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





path.p              <- "C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "SalinitySummary_juv.csv$", recursive = FALSE)))) 


Summary.table            <- data.frame(matrix(nrow = 1, ncol = 6)) # create dataframe to save cumunalitively during for loop
colnames(Summary.table)  <- c('filename',
                              'type',
                              'threshold',
                              'numberFound',
                              'avgDurMinutes',
                              'avgAvgSalinity') # names in the for loop

library(dplyr)
for (m in 1:nrow(file.names.table)) {
  dat_loop              <- as.data.frame(read.csv(paste(path.p,'/',file.names.table[m,1], sep=''))) %>%  #reads in the data files
    dplyr::mutate(filename = gsub("_Sonde.*","", file.names.table[m,1]))
  #summary_dat              <- summary_dat[!is.na(summary_dat$type),
  
  Summary.table <- rbind(dat_loop, Summary.table) #bind to a cumulative list dataframe
  # print(df_total)
}

Summary.table.om       <- Summary.table[!is.na(Summary.table$filename),] # omit NAs in filename 
Summary.table.final    <- Summary.table.om %>%
  dplyr::rename(frequency = numberFound,
                duration = avgDurMinutes,
                magnitude = avgAvgSalinity) %>% 
  dplyr::mutate(EQ = (as.numeric(frequency) * as.numeric(duration)) / as.numeric(magnitude) ) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))  %>% # convert remaining NAs to 0
  dplyr::mutate(Site = gsub("_.*","",filename))
                #Date = gsub("_.*","",filename))
Summary.table.final  

library(ggplot2)
Duration_plot <- Summary.table.final %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(duration)/60, fill=as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Duration of low salinity  events") +
  ylab("Duration of events (hours; minimum 1 hour)") +
  xlab("Salinity threshold (PSU)")+
  scale_fill_discrete(name="Site")
  # facet_wrap(~Date)
 Duration_plot

Frequency_plot <- Summary.table.final %>% 
  dplyr::filter(frequency >0) %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(frequency), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Frequency of low salinity  events") +
  ylab("Number of events") +
  xlab("Salinity threshold (PSU)")+
  scale_fill_discrete(name="Site")
  # facet_wrap(~Date)
# Frequency_plot

EQ_plot <- Summary.table.final %>% 
  dplyr::filter(!EQ == 'NaN') %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(EQ), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("(F*D)/M of low salinity events") +
  ylab("[Freq*Duration/Mean_magnitude]") +
  xlab("Salinity threshold (PSU)")+
  scale_fill_discrete(name="Site")
  # facet_wrap(~Date)
# EQ_plot

library(ggpubr)
pdf("C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Events_plots/juvenile_growth_salinity.pdf", 
    height = 12, width =10)
ggarrange(Duration_plot,
          Frequency_plot,
          EQ_plot,
          nrow=3)
dev.off()



# Low temperature PLOTS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





path.p              <- "C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "LTempSummary_juv.csv$", recursive = FALSE)))) 


Summary.table            <- data.frame(matrix(nrow = 1, ncol = 6)) # create dataframe to save cumunalitively during for loop
colnames(Summary.table)  <- c('filename',
                              'type',
                              'threshold',
                              'numberFound',
                              'avgDurMinutes',
                              'avgAvgLTemp') # names in the for loop

library(dplyr)
for (m in 1:nrow(file.names.table)) {
  dat_loop              <- as.data.frame(read.csv(paste(path.p,'/',file.names.table[m,1], sep=''))) %>%  #reads in the data files
    dplyr::mutate(filename = gsub("_Sonde.*","", file.names.table[m,1]))
  #summary_dat              <- summary_dat[!is.na(summary_dat$type),
  
  Summary.table <- rbind(dat_loop, Summary.table) #bind to a cumulative list dataframe
  # print(df_total)
}

Summary.table.om       <- Summary.table[!is.na(Summary.table$filename),] # omit NAs in filename 
Summary.table.final    <- Summary.table.om %>%
  dplyr::rename(frequency = numberFound,
                duration = avgDurMinutes,
                magnitude = avgAvgLTemp) %>% 
  dplyr::mutate(EQ = (as.numeric(frequency) * as.numeric(duration)) / as.numeric(magnitude) ) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))  %>% # convert remaining NAs to 0
  dplyr::mutate(Site = gsub("_.*","",filename))
#Date = gsub("_.*","",filename))
Summary.table.final  

library(ggplot2)
Duration_plot <- Summary.table.final %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(duration)/60, fill=as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Duration of low temperature events") +
  ylab("Duration of events (hours; minimum 1 hour)") +
  xlab("Low temperature threshold (C)")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
Duration_plot

Frequency_plot <- Summary.table.final %>% 
  dplyr::filter(frequency >0) %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(frequency), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Frequency of low temperature events") +
  ylab("Number of events") +
  xlab("Low temperature threshold (C)")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
# Frequency_plot

EQ_plot <- Summary.table.final %>% 
  dplyr::filter(!EQ == 'NaN') %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(EQ), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("(F*D)/M of low temperature events") +
  ylab("[Freq*Duration/Mean_magnitude]") +
  xlab("Low temperature threshold (C)")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
# EQ_plot

library(ggpubr)
pdf("C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Events_plots/juvenile_growth_lowtemperature.pdf", 
    height = 12, width =10)
ggarrange(Duration_plot,
          Frequency_plot,
          EQ_plot,
          nrow=3)
dev.off()




# High temperature PLOTS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::





path.p              <- "C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "HTempSummary_juv.csv$", recursive = FALSE)))) 


Summary.table            <- data.frame(matrix(nrow = 1, ncol = 6)) # create dataframe to save cumunalitively during for loop
colnames(Summary.table)  <- c('filename',
                              'type',
                              'threshold',
                              'numberFound',
                              'avgDurMinutes',
                              'avgAvgHTemp') # names in the for loop

library(dplyr)
for (m in 1:nrow(file.names.table)) {
  dat_loop              <- as.data.frame(read.csv(paste(path.p,'/',file.names.table[m,1], sep=''))) %>%  #reads in the data files
    dplyr::mutate(filename = gsub("_Sonde.*","", file.names.table[m,1]))
  #summary_dat              <- summary_dat[!is.na(summary_dat$type),
  
  Summary.table <- rbind(dat_loop, Summary.table) #bind to a cumulative list dataframe
  # print(df_total)
}

Summary.table.om       <- Summary.table[!is.na(Summary.table$filename),] # omit NAs in filename 
Summary.table.final    <- Summary.table.om %>%
  dplyr::rename(frequency = numberFound,
                duration = avgDurMinutes,
                magnitude = avgAvgHTemp) %>% 
  dplyr::mutate(EQ = (as.numeric(frequency) * as.numeric(duration)) / as.numeric(magnitude) ) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))  %>% # convert remaining NAs to 0
  dplyr::mutate(Site = gsub("_.*","",filename))
#Date = gsub("_.*","",filename))
Summary.table.final  

library(ggplot2)
Duration_plot <- Summary.table.final %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(duration)/60, fill=as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Duration of high temperature events") +
  ylab("Duration of events (hours; minimum 1 hour)") +
  xlab("High temperature threshold (C)")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
Duration_plot

Frequency_plot <- Summary.table.final %>% 
  dplyr::filter(frequency >0) %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(frequency), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Frequency of high temperature events") +
  ylab("Number of events") +
  xlab("High temperature threshold (C)")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
# Frequency_plot

EQ_plot <- Summary.table.final %>% 
  dplyr::filter(!EQ == 'NaN') %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(EQ), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("(F*D)/M of high temperature events") +
  ylab("[Freq*Duration/Mean_magnitude]") +
  xlab("High temperature threshold (C)")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
# EQ_plot

library(ggpubr)
pdf("C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Events_plots/juvenile_growth_hightemperature.pdf", 
    height = 12, width =10)
ggarrange(Duration_plot,
          Frequency_plot,
          EQ_plot,
          nrow=3)
dev.off()




# Low chlorophyll-a PLOTS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




path.p              <- "C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "LChlaSummary_juv.csv$", recursive = FALSE)))) 


Summary.table            <- data.frame(matrix(nrow = 1, ncol = 6)) # create dataframe to save cumunalitively during for loop
colnames(Summary.table)  <- c('filename',
                              'type',
                              'threshold',
                              'numberFound',
                              'avgDurMinutes',
                              'avgAvgLChla') # names in the for loop

library(dplyr)
for (m in 1:nrow(file.names.table)) {
  dat_loop              <- as.data.frame(read.csv(paste(path.p,'/',file.names.table[m,1], sep=''))) %>%  #reads in the data files
    dplyr::mutate(filename = gsub("_Sonde.*","", file.names.table[m,1]))
  #summary_dat              <- summary_dat[!is.na(summary_dat$type),
  
  Summary.table <- rbind(dat_loop, Summary.table) #bind to a cumulative list dataframe
  # print(df_total)
}

Summary.table.om       <- Summary.table[!is.na(Summary.table$filename),] # omit NAs in filename 
Summary.table.final    <- Summary.table.om %>%
  dplyr::rename(frequency = numberFound,
                duration = avgDurMinutes,
                magnitude = avgAvgLChla) %>% 
  dplyr::mutate(EQ = (as.numeric(frequency) * as.numeric(duration)) / as.numeric(magnitude) ) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))  %>% # convert remaining NAs to 0
  dplyr::mutate(Site = gsub("_.*","",filename))
#Date = gsub("_.*","",filename))
Summary.table.final  

library(ggplot2)
Duration_plot <- Summary.table.final %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(duration)/60, fill=as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Duration of low Chlorophyll-a events") +
  ylab("Duration of events (hours; minimum 1 hour)") +
  xlab("Low Chlorophyll-A threshold")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
Duration_plot

Frequency_plot <- Summary.table.final %>% 
  dplyr::filter(frequency >0) %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(frequency), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Frequency of low Chlorophyll-a events") +
  ylab("Number of events") +
  xlab("Low Chlorophyll-a threshold")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
# Frequency_plot

EQ_plot <- Summary.table.final %>% 
  dplyr::filter(!EQ == 'NaN') %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(EQ), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("(F*D)/M of low Chlorophyll-a events") +
  ylab("[Freq*Duration/Mean_magnitude]") +
  xlab("Low Chlorophyll-a threshold")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
# EQ_plot

library(ggpubr)
pdf("C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Events_plots/juvenile_growth_lowchla.pdf", 
    height = 12, width =10)
ggarrange(Duration_plot,
          Frequency_plot,
          EQ_plot,
          nrow=3)
dev.off()



# High chlorophyll-a PLOTS ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::




path.p              <- "C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events" #the location of all your respirometry files 
file.names.table    <- data.frame(txt.files = (basename(list.files(path = paste(path.p,'/',sep=''), pattern = "HChlaSummary_juv.csv$", recursive = FALSE)))) 


Summary.table            <- data.frame(matrix(nrow = 1, ncol = 6)) # create dataframe to save cumunalitively during for loop
colnames(Summary.table)  <- c('filename',
                              'type',
                              'threshold',
                              'numberFound',
                              'avgDurMinutes',
                              'avgAvgHChla') # names in the for loop

library(dplyr)
for (m in 1:nrow(file.names.table)) {
  dat_loop              <- as.data.frame(read.csv(paste(path.p,'/',file.names.table[m,1], sep=''))) %>%  #reads in the data files
    dplyr::mutate(filename = gsub("_Sonde.*","", file.names.table[m,1]))
  #summary_dat              <- summary_dat[!is.na(summary_dat$type),
  
  Summary.table <- rbind(dat_loop, Summary.table) #bind to a cumulative list dataframe
  # print(df_total)
}

Summary.table.om       <- Summary.table[!is.na(Summary.table$filename),] # omit NAs in filename 
Summary.table.final    <- Summary.table.om %>%
  dplyr::rename(frequency = numberFound,
                duration = avgDurMinutes,
                magnitude = avgAvgHChla) %>% 
  dplyr::mutate(EQ = (as.numeric(frequency) * as.numeric(duration)) / as.numeric(magnitude) ) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))  %>% # convert remaining NAs to 0
  dplyr::mutate(Site = gsub("_.*","",filename))
#Date = gsub("_.*","",filename))
Summary.table.final  

library(ggplot2)
Duration_plot <- Summary.table.final %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(duration)/60, fill=as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Duration of high Chlorophyll-a events") +
  ylab("Duration of events (hours; minimum 1 hour)") +
  xlab("High Chlorophyll-A threshold")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
Duration_plot

Frequency_plot <- Summary.table.final %>% 
  dplyr::filter(frequency >0) %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(frequency), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("Frequency of high Chlorophyll-a events") +
  ylab("Number of events") +
  xlab("High Chlorophyll-a threshold")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
# Frequency_plot

EQ_plot <- Summary.table.final %>% 
  dplyr::filter(!EQ == 'NaN') %>% 
  ggplot(aes(x= as.factor(threshold), y = as.numeric(EQ), fill = as.factor(Site))) + 
  geom_bar(stat="identity", position = position_dodge2(preserve = "single")) + 
  theme_classic() + 
  ggtitle("(F*D)/M of high Chlorophyll-a events") +
  ylab("[Freq*Duration/Mean_magnitude]") +
  xlab("High Chlorophyll-a threshold")+
  scale_fill_discrete(name="Site")
# facet_wrap(~Date)
# EQ_plot

library(ggpubr)
pdf("C:/Users/kyra.lenderman/Documents/GitHub/EAD-ASEB_EPA_LISS_Disease_Surveillance/Sonde_Data/output/Juvenile_events/Events_plots/juvenile_growth_highchla.pdf", 
    height = 12, width =10)
ggarrange(Duration_plot,
          Frequency_plot,
          EQ_plot,
          nrow=3)
dev.off()



