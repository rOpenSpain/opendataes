
#EXAMPLE 1

# Get dataset url
# URL root: http://datos.gob.es/apidata/catalog/dataset
# Id: ea0001304-concesiones-de-obras-y-actividades-autorizadas-a-particulares1
# Extra parameters such as 'sort' and 'pageSize' are optional
x <- GET("http://datos.gob.es/apidata/catalog/dataset/ea0001304-concesiones-de-obras-y-actividades-autorizadas-a-particulares1?_sort=title&_pageSize=10&_page=0")

# Check class and http_type: "application/json"
class(x)
http_type(x)

# Parse content
cont <- content(x, "parse")

# Now in order to get the dataset que have to pass the response as first parameter and then type passed
# We have to deal with the encoding because otherwise an error is raised
out <- content(GET(cont$result$items[[1]]$distribution[[1]]$accessURL), type = "text/csv", encoding = "latin1")

# This retrieves a character vector of length 1 (delimite by ;)
out


#EXAMPLE 2

# Second attempt with another random dataset
# Get dataset url
x <- GET("http://datos.gob.es/apidata/catalog/dataset/l02000048-inversiones")

# Check class and http_type: "application/json"
class(x)
http_type(x)

# Parse content
cont <- content(x, "parse")

# Get data
# In this case the fifth accessURL evokes the csv (previous are json and xml, respectively)
out <- content(GET(cont$result$items[[1]]$distribution[[5]]$accessURL), type = "text/csv", encoding = "latin1")

# With this function (http_type) we can check the result
http_type(GET(cont$result$items[[1]]$distribution[[5]]$accessURL))

#In this case we get the dataframe because the delimiter is ',' instead of ';'
out


