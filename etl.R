Price_1year <- read.csv('Prices1yearsnoupfront.csv') %>%
  mutate(Term = "1year") %>%
  rename(OS = System,Convertible = Price, On_Demand = On.demand,Offering_Class = Offering.class)

Price_3year <- read.csv('Prices3yearsnoupfront.csv') %>%
  mutate(Term = "3years") %>%
  rename(OS = System,Convertible = Price, On_Demand = On.demand,Offering_Class = Offering.class)

Price <- merge(Price_1year,Price_3year,by=c("Region","OS","Type","Convertible","On_Demand","Offering_Class","Term"),all = TRUE)