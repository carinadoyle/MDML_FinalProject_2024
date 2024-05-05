
resto_assess <- resto_assess %>% filter(GRADE!="N")
resto_assess <- resto_assess %>% filter(Neighborhood.Name!="park-cemetery-etc-Manhattan")
resto_assess <- resto_assess %>% select(-pests)

resto_pred <- predict(log_model, newdata=resto_assess, type="response")

resto_results <- cbind(resto_assess, resto_pred)

# predictions for the neighborhoods with highest average probability of having pest violations

resto_predictions <- resto_results %>% group_by(Neighborhood.Name) %>% 
  summarise(average.probability=mean(resto_pred)) %>% arrange(desc(average.probability))
resto_predictions

# let's keep the top 3 neighborhoods and graph them based on score

resto_graph <- as.data.frame(resto_results)
resto_graph_score <- resto_graph %>% select(nn=Neighborhood.Name, score=score, pred=resto_pred) %>% 
  group_by(nn, score) %>% mutate(average.pred=mean(pred)) %>% filter(nn=="East Village"|
                                                                       nn=="Chinatown"|
                                                                       nn=="West Village")
ggplot(data=resto_graph_score) + geom_line(mapping=aes(x=score, y=average.pred, color=nn)) 

resto_graph <- as.data.frame(resto_results)
resto_graph_gd <- resto_graph %>% select(nn=Neighborhood.Name, gd=GRADE.DAY, pred=resto_pred) %>% 
  group_by(nn, gd) %>% mutate(average.pred=mean(pred)) %>% filter(nn=="East Village"|
                                                                       nn=="Chinatown"|
                                                                       nn=="West Village")
ggplot(data=resto_graph_gd) + geom_bar(mapping=aes(x=gd, y=average.pred, color=nn)) 

