
#We build a list of our files to read it in loop by a function
my.files = c('new_york_city.csv','washington.csv','chicago.csv')
#Load ggplot2 library for all visualizations
library(ggplot2)
#We build a general function to avoid rewriting the reading file, first summaries code
#Our function take file name in parameter
#The output of this general function will print the dimensions, the variables and summary on every data set
#This function can be used to read every file and
general_impression_data = function(filename){
      #Save object of file read in variable
      files.read=read.csv(file=filename)
      #Print this title to identify every data set of every state
      print(paste("Variables of",filename,"bike data",sep=" "))
      #Identify variables names of data set
      variable.names.studied = names(files.read)
      print(variable.names.studied)
      #Print title on Summary of data set
      title.data.summary = paste("Summary on",filename,"bike data",sep=" ")
      print(title.data.summary)
      #Compute summary on data set
      data.summary = summary(files.read)
      print(data.summary)
      #return object  of file read
      return (files.read)
}
length(my.files)







# Reading of chicago file data, print general information on data set
chi=general_impression_data(my.files[3])
#Identify the min and max of Trip.Duration varible
summary(chi$Trip.Duration)
#Distribution of user Gender in Chicago
table(chi$Gender)
#Statistics of Chicago Trip.Duration by Gender
by(chi$Trip.Duration,chi$Gender,summary)
#Histogramm of Trip duration by Gender
ggplot(aes(x=Trip.Duration), data=subset(chi,!Gender=='')) +
    geom_histogram(binwidth=200,color = 'black', fill = '#FFFFFF') +
    ggtitle('Histogram of Trip duration by Gender in Chicago') +
    labs(x = "Trip duration",y="Number of bike users system") +
    scale_x_continuous(limits=c(60,3600),breaks=seq(60,3600,600)) +
    facet_wrap(~Gender)

# Call our function to print general function on our data
wash=general_impression_data(my.files[2])
#How many user bike system by User.Type ?
summary(wash$User.Type)
table(wash$User.Type)
#Statistics to show the mean, the median of trip duration by User type
by(wash$Trip.Duration,wash$User.Type,summary)
#Represent these statistics of trip duration by User type with box plot
qplot(x = User.Type,
      y = Trip.Duration,data = subset(wash,!User.Type==''),
      geom = 'boxplot',
      ylim = c(60,3600),
      xlab='Users type in bike system',
      ylab='Bike trip duration')

# Print General Information by calling our function
ny=chi=general_impression_data(my.files[1])
#Distribution of user per birth Year
table(ny$Birth.Year)
#Identify statistics on Trip duration in New Yory
summary(ny$Trip.Duration)
#Statistics Summary to our question
by(ny$Trip.Duration,ny$Birth.Year,summary)
#Plot statistics to our question
ggplot(aes(x=Birth.Year,y=Trip.Duration),data=na.omit(ny))+
geom_point(alpha=1/20,color='orange',position=position_jitter(h=0))+
xlim(1885,2001)+
ylim(60,3600)+
geom_line(stat = 'summary',fun.y=mean)+
geom_line(stat = 'summary',fun.y=median,linetype=5)+
geom_line(stat = 'summary',fun.y=min,color='red',linetype=3)+
ggtitle('Representation of User trip duration per birth year in New York') +
    labs(x = "Birth year of User",y="Trip duration")

system('python -m nbconvert Explore_bikeshare_data.ipynb')
