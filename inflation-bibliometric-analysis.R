##################################################################################################################

##### Code for bibliometric analysis of Dimensions dataset

### Libraries
library(dplyr)
library(ggplot2)
library(bubbles)
### Data
data = read.csv("Dimensions-Publication.csv", sep = ",", header = TRUE)


### Data Cleaning Process

## Removing duplicates
length(unique(data$Title)) 
data = data[!duplicated(data$Title),]

## Pairing authors with universities

auts = function(num,autos){
  data[num,10] = autos
  return(data)
}

unis = function(num,univer){
  data[num,11] = univer
  return(data)
}

countr = function(num, paises){
  data[num,12] = paises
  return(data)
}

## example
# authors = "Lee, Cheng-Wen; Huruta, Andrian Dolfriandra "
# num = 427      
# univers = "Chung Yuan Christian University; Universitas Kristen Satya Wacana"
# countries = "China; Indonesia"
# 
# data = auts(num,authors)
# data = unis(num, univers)
# data = countr(numero,countries)
# 
# Fixing format

data$Authors = str_to_title(data$Authors)
data$`Cited references` = str_to_title(data$`Cited references`)
data = data %>% 
  mutate(Authors = str_replace_all(Authors, c(" Z;" = " Z.;"))) #for all the abc

# Removing duplicates
data$Title = tolower(data$Title)
length(unique(data$Title))
data = data[!duplicated(data$Title),]


##################################################################################################################
 
#### Data processing
# Extracting references
# Based on https://aurelien-goutsmedt.com/post/extracting-biblio-data-2/


references_extract <- data %>% 
  filter(! is.na("Cited references")) %>% 
  rename("citing_id" = "Publication ID") %>% 
  select("citing_id", "Cited references") %>% 
  separate_rows("Cited references", sep = ";(?=\\[)") %>%
  as_tibble

column_names <- c("authors", "author_id", "source","year", "volume", "issue",
                  "pagination", "doi", "publication_id","times_cited")

# Citations dataframe
dimensions_direct_citation <- references_extract %>% 
  separate(col = "Cited references", into = column_names, sep = "\\|")

citations <- dimensions_direct_citation %>% 
  add_count(publication_id) %>% 
  select(publication_id, n) %>% 
  unique

references_filtered <- dimensions_references %>% 
  left_join(citations) %>% 
  filter(n >= 5)

references_filtered <- references_filtered %>% 
  relocate(publication_id, .before = authors)  


#saveRDS(data, "data.Rdata")
#saveRDS(dimensions_direct_citation, "dimensions_direct_citation.Rdata")
#write_xlsx(data,"data.xlsx")

##################################################################################################################
#### Bibliometric Analysis

### Number of docs per year
data %>% 
  count(PubYear)

## Graph
data %>%
  count(PubYear) %>%
  ggplot(aes(x = PubYear, y = n, fill = n)) +
  geom_bar(stat = "sum", show.legend = FALSE, ) +
  scale_fill_viridis(option = "A") +
  ylab("Number of Documents per Year") + 
  xlab("Year") +
  scico::scale_fill_scico(palette = 'bam') 
theme_minimal() 

### Publications 
## Publications in last 15 years
tail(data.frame((table(data$PubYear))), 15)

 
## Top 15 journals
head(data.frame(sort(table(data$`Source title/Anthology title`), decreasing = TRUE)), 15)

## Most cited authors in references
head(data.frame(sort(table(na.omit(dimensions_direct_citation$authors)), decreasing = TRUE)), 15)


## Most productive authors
data_autores <- data %>% 
  select("Publication ID", "Authors") %>% 
  separate_rows("Authors", sep = "; ") %>%
  as_tibble

head(data.frame(sort(table(data_autores$Authors), decreasing = TRUE)), 15)

## graph
#devtools::install_github("jcheng5/bubbles")
bub = head(data.frame(sort(table(data_autores$Authors), decreasing = TRUE)), 100)
bubbles(value = bub$Freq, label = bub$Var1,
        color = sample(scico::scico(n = 100, palette = "bam")))

### Countries and regions analysis based on Bibliometrix package
library(bibliometrix)

M = convert2df("data.xlsx", dbsource = "dimensions", format = "excel")
results = biblioAnalysis(M, sep = ";")

top_co_colab = results$CountryCollaboration[1:15,]

to_co = function(df,k){
  xx=df[order(-(df$SCP+df$MCP)),] #Intra-country (SCP) and intercountry (MCP) collaboration indices
  xx1=cbind(top_co_colab[,1:2],rep("SCP",k))
  names(xx1)=c("Country","Freq","Collaboration")
  xx2=cbind(xx[,c(1,3)],rep("MCP",k))
  names(xx2)=c("Country","Freq","Collaboration")
  xx=rbind(xx2,xx1)
  xx$Country=factor(xx$Country,levels=xx$Country[1:dim(xx2)[1]])
  xx$Country = str_to_title(xx$Country)
  
  return(xx)
}
top_co_colab = to_co(top_co_colab,15)

top_co_colab %>% 
  ggplot(aes(x =  Country, y = Freq, fill = Collaboration)) +  
  geom_bar(stat="Identity", size = 4, show.legend = T)+
  scale_x_discrete(limits = rev(levels(top_co_colab$Country)))+
  xlab("Most productive countries") + ylab("Articles")+
  scico::scale_fill_scico_d(palette = 'bam') +
  coord_flip() +
  theme_minimal() 

