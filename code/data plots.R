# Exploratory Plots
# one site at a time
test <-replicated  %>%
  filter(Area=="Corn", Year==1955, Season=="Spring") %>%
  group_by(Date) %>%
  mutate(Avg.Color = mean(Color))
  ungroup(test)
ggplot(test,aes(Date,Color)) +
  geom_point(aes(Date,Color, size=Count)) +
  geom_point(aes(Date,Avg.Color), color='red')


# All sites at once
Means <-replicated  %>%
  group_by(Area,Date, Year, Season) %>%
  mutate(Avg.Color = mean(Color))
ungroup(Means)

filter(Means,Season== "Spring",Year < '2010')
ggplot(Means,aes(Date,Color)) +
  geom_point(aes(Date,Color, size=Count)) +
  geom_point(aes(Date,Avg.Color), color='red') 