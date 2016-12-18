# H2O (h2o.ai) examples 

I have collected few examples in R and Python for using h2o, and some nice data science examples, like the one in `Telco_Customer_Churn_H2O.Rmd`.

In order to simply to run the above examples I have created a docker image.

Simply run the docker image:
```
> docker run -it -p 8787:8787 -p 54321:54321 aghorbani/rstudio-h2o
```
which provides you a web based r studio and h2o, and required packages to run the examples.
