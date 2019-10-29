# Grammar of Graphics -- Student Presentations

library(ggplot2)

data("diamonds")
force(diamonds)
# Here, we call our dataset, diamonds.

basic_graph = ggplot(diamonds, aes(x=cut))
# Now we have a variable that represents our foundation that we'll build the graph on top of.

basic_graph + geom_bar()
# Here is just a basic graph with no aesthetics.
# This graph is telling us the amount of each cut of the diamonds observered.
# See how we used our variable, 'basic_graph'
# This way, we don't have to retype "ggplot(diamonds, aes(x=cut))" each time.

basic_graph + geom_bar(fill="red", color="black", width=0.75)
# Here we have some basic aesthetics:
# 'fill' defines the color of the bars;
# 'color' defines the color of the outline of the bars; 
# 'width' defines the width of the bars.

graph_2 = ggplot(diamonds, aes(x=cut, y=carat, fill=color))
graph_2 + geom_bar(stat="identity", position="dodge")
# Here is an even more complicated graph that does some stuff we haven't done yet.
# This graph shows the carat of each diamond in the data set, by cut quality.
# Furthermore, it shows the different colors by cut quality, and the carats of those different colors.
# We allocate diamond color to each specific cut quality by using 'fill=color' in conjunciton with 'position="dodge"'
# This is a premium example of the perfect use case for a Bar Plot: 
# We have two sets of categorical data: Color and Cut, and one set of continuous data: Carat.


