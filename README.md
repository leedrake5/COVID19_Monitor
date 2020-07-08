
#Economic Impact


The early version of the model allows for a geographic display of things like college education, unemployment level, incomes, etc. You can click on a city to get info on which congressfolk are there, alongside ways to contact them.

Right now biggest needs are:

1) Demographic & economic data by zipcode, 
2) Ideas for how to integrate variables, and
3) How to make it look pretty & be useable

Thanks for reading this far, and if you have time take a look and let me know your thoughts.


You can run this demo with:
```
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("rstudio/leaflet")
shiny::runGitHub("leedrake5/congressImpact")
```

Data originally compiled for _Coming Apart: The State of White America, 1960–2010_ by Charles Murray (Crown Forum, 2012). This app was inspired by the Washington Post's interactive feature _[Washington: A world apart](http://www.washingtonpost.com/sf/local/2013/11/09/washington-a-world-apart/)_. It has been augmented with unemployment data and congressional data from the sunshine project by the author
