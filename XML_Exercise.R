### This file contains some commands for a short "demo" of the XML package in R
### To run each command on your console, simply place the cursor on it
### and press Crtl + Enter

### let's try
print("Hello World")

### Let's call the Package "XML"
library(XML)

### It might produce some warnings, so better get rid of them
options(warn=-1)

### First thing: choose an XML file, and parse it
data <- xmlParse("corpus_ELTeC_it/ITBI0087_Svevo.xml")

### Show the contents of the variable "data"
data

### what is the class of the variable "data"?
class(data)

### define the namespace
namespace <- c(TEI="http://www.tei-c.org/ns/1.0")

### let's find a tag in the xml file!
# define the tag "body"
my_tag <- "//TEI:body"
# find all nodes in the document based on that tag
my_result <- xpathSApply(doc = data, path = my_tag, fun = xmlValue, namespaces = namespace)
# see the length of what I got...
length(my_result)
# see what I got
my_result

# define the tag "p" (under "body")
my_tag <- "//TEI:body//TEI:p"
# find all nodes in the document based on that tag
my_result <- xpathSApply(doc = data, path = my_tag, fun = xmlValue, namespaces = namespace)
# see the length of what I got...
length(my_result)
# see what I got
my_result

# define the tag "foreign"
my_tag <- "//TEI:body//TEI:foreign"
# find all nodes in the document based on that tag
my_result <- xpathSApply(doc = data, path = my_tag, fun = xmlValue, namespaces = namespace)
# see the length of what I got...
length(my_result)
# see what I got
my_result

# try another tag!!!
my_tag <- "//TEI:my_tag" # substitute "my_tag" with a tag in the document
# find all nodes in the document based on that tag
my_result <- xpathSApply(doc = data, path = my_tag, fun = xmlValue, namespaces = namespace)
# see the length of what I got...
length(my_result)
# see what I got
my_result

