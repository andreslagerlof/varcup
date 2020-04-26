
res_hist %>% 
  count(fencer = fct_infreq(name), sort = TRUE)

ggplot(res_hist, aes(fct_infreq(name))) +
  geom_bar() +
  coord_flip() +
  scale_x_discrete()

res_hist <- read_csv2("./results/placeringar_2006-2019_v2.csv")

## Prepare the data

# Create a long dataset
res_hist <- res_hist %>% 
  pivot_longer(c("1:a", "2:a", "3:a"), 
               names_to = "place", values_to = "name")

# Separate name variable in res_hist df
res_hist <- res_hist %>% separate(name, into = c("firstname", "lastname"))

# Create datafreme with name, place and count of place
stat <- res_hist %>% group_by(firstname, place) %>% count() %>%arrange(desc(n)) %>%  arrange(place)


# Create df with name and no of entries
no_top3 <- res_hist2 %>% group_by(name) %>% count() %>% arrange(desc(n)) 

# Separate name variable in no_top3 df
no_top3 <- no_top3 %>% separate(name, into = c("firstname", "lastname"))

# Create barplot of top 3
no_top3 %>% 
  drop_na() %>% 
  ggplot( aes(x=reorder(firstname, n), n)) + 
  geom_bar(stat = "identity", fill = "chocolate") +
  theme_light() +
  coord_flip() +
  labs(title="Antal top 3 placeringar", 
       subtitle = "Perioden 2006 till 2019",
       caption = "Källa: Janne Tivenius och Andrés Gomez",
       x= "Fäktare", 
       y="Antal top 3") +
  geom_text(aes(label = n), 
            hjust = -0.25, 
            color = "#1380A1") 

# Third places
# Create barplot of third places
res_hist %>% filter(place == "3:a") %>% 
  ggplot( aes(x=reorder(firstname, n), n)) +
  geom_bar(stat = "identity", fill = "wheat4") +
  theme_minimal()+
  coord_flip() +
  labs(title="Antal 3:e platser per fäktare", 
       subtitle = "Perioden 2006 till 2019",
       caption = "Källa: Janne Tivenius och Andrés Gomez",
       x= "Fäktare", 
       y="Antal 3:e platser") +
  geom_text(aes(label = n), 
            hjust = -0.3)

tor <- hist %>% filter(`1:a` == "Tor Forsse")
christian <- hist %>% filter(`1:a` == "Christian Gustavsson")
last3years <- hist %>% filter(Year %in% c(2017, 2018, 2019))

# Create datafreme with name, place and count of place
stat1 <- res_hist %>% 
  group_by(name, place, Year) %>% 
  count() %>% arrange(desc(n)) %>%  
  arrange(place)


tor <- tor %>% rename(År = Year)

## Create nice tables with the gt package

tor_table <- gt(data = tor)
tor_table %>% 
  tab_header(title = "Tors segrar i Vårcupen")
  
last3_table <- gt(data = last3years)
last3_table %>% 
  tab_header(title = "Resultat de senaste 3 åren")
