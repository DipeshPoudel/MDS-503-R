library(tidyverse)




## Plotting Using GGPLOT

ggplot(data = mpg)+geom_point(mapping = aes(x=displ,y=hwy))

ggplot(data=mpg,aes(x=displ,y=hwy,color=class))+geom_point()+geom_line()


ggplot(data=mpg,aes(x=displ,y=hwy,colour="blue"))+geom_point()


ggplot(data=mpg,aes(x=displ,y=hwy))+geom_point()+geom_line()
+facet_wrap(~class,nrow = 5)

ggplot(data=mpg,aes(x=displ,y=hwy),color=class)+geom_point()+geom_line()+facet_grid(~class)


ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))

ggplot(data=mpg,aes(x=displ,y=hwy))+geom_point()+geom_smooth()

ggplot(data=mpg,aes(x=displ,y=hwy))+geom_point()+geom_smooth(span=0.2)

ggplot(data=mpg,aes(x=displ,y=hwy))+geom_point()+geom_smooth(method = lm)

ggplot(data=mpg,aes(x=displ,y=hwy))+geom_point()
+geom_smooth(method = lm,se=FALSE)


ggplot(data = mpg,aes(hwy))+geom_histogram()

ggplot(data = mpg,aes(hwy))+geom_histogram(binwidth = 2.5)

ggplot(mpg,aes(displ,fill=drv))+geom_histogram(bindwidth=0.5)+facet_wrap(~drv,ncol=1)

drugs<-data.frame(drug=c("a","b","c"),effect=c(4.2,5.7,7.5))

ggplot(data = drugs,aes(drug,effect))+geom_bar(stat = "identity")


ggplot(mpg,aes(cty,hwy))+geom_point(alpha=1/3)

ggplot()

