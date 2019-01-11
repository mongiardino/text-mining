###General comments--------------------------------------------------------------------------------------------------

#Written by Nicolas Mongiardino Koch (Department of Geology & Geophysics, Yale University;
#nicolas.mongiardinokoch@yale.edu) as supplementary material to the manuscipt "The phylogenomic
#revolution and its conceptual innovations: a text mining approach" (Organisms Diversity & Evolution).
#This project originated as a personal challenge to learn text mining. I am not an expert on
#the topic, and it is very likely that there are easier ways to do everything that this script
#does. The code not only provides a way to fully reproduce all web scraping and analyses, but
#is intended to serve the purpose of a tutorial in mining and handling of text in tidy ways.
#Hopefully this will inspire other researchers to consider the article's text as data itself,
#its contents as evidence of the evolution of scientific disciplines.

#Although all of the scraping can possibly be done with just one approach, 3 different methods are
#used (R packages 'rvest' and 'RSelenium', as well as the program PhantomJS). Hopefully this will
#better show the range of possibilities that can be employed to scrape with R.

#CSS selectors were obtained using the amazing SelectorGadget software (http://selectorgadget.com/).
#Much of the code reflects things learned while reading Grolemund & Wickham (2017) -
#R for Data Science (http://r4ds.had.co.nz/) and Silge & Robinson (2017) - Text Mining with R
#(https://www.tidytextmining.com/).

#As a discipline changes and matures, the type of results and topics covered by different journals
#change as well. To incorporate as much diversity as possible 6 different journals were scraped.
#Four of these are journals specialize in phylogenetics and molecular biology, in which many of
#of the seminal papers in phylogenomics were published, thus having a strong influence on the field.
#The remaining two are journals that cover topics across the biological sciences; these increased the
#output of papers in phylogenomics as the methods became more mainstream. In all cases, searches 
#were restricted to research articles.

#Online content is dynamic, so it should be pointed out that subsequent analyses might not recover
#the exact same results (although big differences are also not expected). The raw data used to obtain
#the results are therefore also included as supplementary data (.Rda files).

#Sistematic Biology was mined on January 29, 2018
#Molecular Biology and Evolution was mined on January 29, 2018
#Plos One was mined on February 6th, 2018
#Molecular Phylogenetics and Evolution was mined on February 9th, 2018
#Genome Biology and Evolution was mined on February 12th, 2018
#Proceedings B was mined in February 17th, 2018

###Initial setup----------------------------------------------------------------------------------------------------

rm(list = ls())

#Modify this to set working directory
setwd(getwd())

#Get all required packages not already installed
packages <- c('rvest', 'RSelenium', 'tidyr', 'dplyr', 'tibble', 'stringr', 'tidytext', 'SnowballC', 'ggplot2', 'RColorBrewer')
new_packages <- packages[!packages %in% installed.packages()[,'Package']]
if(length(new_packages)) { install.packages(new_packages) }

#Web scraping
library(rvest)
library(RSelenium)

#Tydiverse
  #general
library(tidyr)
library(dplyr)
library(tibble)
  #text
library(tidytext)
library(stringr)
  #plotting
library(ggplot2)
library(RColorBrewer)

#Other text handling
library(SnowballC)


#Mining Plos One required using software PhantomJS, please download the program (execute the line below to
#go to the website and place the .exe file on your working directory
browseURL('http://phantomjs.org/')

#######Part 1: WEB SCRAPING------------------------------------------------------------------------------

###Systematic Biology----------------------------------------------------------------------------

#url for search of research articles in Systematic Biology with 'phylogenomics' in the Full Text
url = 'https://academic.oup.com/sysbio/search-results?page=1&f_ContentType=Journal+Article&f_ArticleTypeDisplayName=Research+Article&fl_SiteID=5349&qb=%7b%22FullText1%22%3a%22phylogenomics%22%7d&sort=Date+%E2%80%93+Oldest+First'
#Reading the HTML code from the website
systbiol_webpage = read_html(url)

#Setting some initial variables (empty vectors to store dois and publication dates, setting current page to 1,
#there are more results on other pages, and a vector of numbers used below to search and modify the url)
systbiol_doi = c()
systbiol_pubdates = c()
page = 1
more.results = T
which.page = c(0:9)

#The following will loop through all the pages of the query and extract the doi of the articles
while(more.results == T) {
  
  #The function html_nodes will extract a specific piece of the full HTML document that we previously
  #downloaded. In this case, '.al-citation-list a' is the entirety of doi links shown in the search page,
  #and '.al-pub-date' is the list of all publication dates. These are known as CSS selectors, and were 
  #identifies using SelectorGadget. The function html_text in turn extract the text from the specified node,
  #instead of other attributes it might have, such as links to other websites.
  systbiol_doi = c(systbiol_doi,html_text(html_nodes(systbiol_webpage,'.al-citation-list a')))
  systbiol_pubdates = c(systbiol_pubdates,html_text(html_nodes(systbiol_webpage,'.al-pub-date')))
  
  #Move to next page by modifying the text in object url
  page = page + 1
  num_in_url = which(strsplit(url,'')[[1]] %in% which.page) #find all the numbers in the url
  if(num_in_url[2] != num_in_url[1] + 1) { #if we are in pages < 10
    url = paste(substr(url,1,(num_in_url[1]-1)),page,substr(url,(num_in_url[1]+1),str_length(url)),sep='')
  }  else { #if we are in pages > 10
    url = paste(substr(url,1,(num_in_url[1]-1)),page,substr(url,(num_in_url[2]+1),str_length(url)),sep='')
  }
  systbiol_webpage = read_html(url)
  if(!identical(html_text(html_nodes(systbiol_webpage,'.error')), character(0))) {
    #Reached last page
    more.results = F 
  }
}

#For the publication dates, store only the year
for(i in 1:length(systbiol_pubdates)) {
  systbiol_pubdates[i] = strsplit(systbiol_pubdates[i], ' ')[[1]][4]
}
systbiol_pubdates = as.numeric(systbiol_pubdates)

#Eliminate papers from 2018 on, as well as the first paper and the last paper from 2017,
#which are not available as online text (at the time the data was gathered)
systbiol_doi = systbiol_doi[-c(1,299,which(systbiol_pubdates >= 2018))]
systbiol_pubdates = systbiol_pubdates[-c(1,299,which(systbiol_pubdates >= 2018))]

#Move on to scraping the titles and main texts
systbiol_articles = vector("list", length(systbiol_doi))
systbiol_titles = vector("list", length(systbiol_doi))

for(i in 1:length(systbiol_doi)) {
  url = systbiol_doi[i]
  systbiol_webpage = read_html(url)
  systbiol_articles[[i]] = html_text(html_nodes(systbiol_webpage,'.backacknowledgements-title , .-label , .section-title , p'))
  systbiol_titles[i] = html_text(html_nodes(systbiol_webpage,'.article-title-main'))
}

#Articles 258 & 269 are announcements although weren't tagged as such
#17,61,117 & 277 are symposium introductions
systbiol_articles = systbiol_articles[-c(17,61,117,258,269,277)]
systbiol_doi = systbiol_doi[-c(17,61,117,258,269,277)]
systbiol_pubdates = systbiol_pubdates[-c(17,61,117,258,269,277)]
systbiol_titles = systbiol_titles[-c(17,61,117,258,269,277)]

#Since that took a while, better to save the scraped articles
save(systbiol_articles, file = 'systbiol_articles.Rda')

#This contains acknowledgments, appendices, section titles and captions, which we are going to (try to) eliminate
#We'll do so by finding the first word that is usually present right after the main text ends,
#and eliminate everything that is after it. We'll also eliminate paragraphs of length < 50 words, as the majority
#of these are figure and table captions
stopwords = c('Acknowledgment','Aknowledgements','Acknowledgments','ACKNOWLEDGMENTS','Acknowledgements','AcknowledgmentS',
              'acknowledgment','ACKNOWLEDGEMENTS','A cknowledgments','SUPPLEMENTARY MATERIAL','FUNDING','Funding',
              'S UPPLEMENTARY M ATERIAL','Supplementary Material','S upplementary M aterial','A cknowledgements','S oftware availablity')

for(i in 1:length(systbiol_articles)) {
  to.eliminate = c()
  for(j in 1:length(systbiol_articles[[i]])) {
    #if you found the end of the article
    if(systbiol_articles[[i]][j] %in% stopwords || 
       paste(unlist(strsplit(systbiol_articles[[i]][j], ' '))[1:3], collapse = ' ') == 'Data available from' || 
       paste(unlist(strsplit(systbiol_articles[[i]][j], ' '))[1:2], collapse = ' ') == 'Supplementary material') {
      #eliminate everything that comes after it and move on to the next
      systbiol_articles[[i]] = systbiol_articles[[i]][-c(j:length(systbiol_articles[[i]]))]
      break
    } else {
      #also save a vector of the location of small paragraphs and eliminate them at the end
      if(length(strsplit(systbiol_articles[[i]][j], ' ')[[1]]) < 50) {
        to.eliminate = c(to.eliminate,j)
      }
    }
  }
  systbiol_articles[[i]] = systbiol_articles[[i]][-to.eliminate]
}

#Two articles did not match any stopping word, and so I am eliminating the ackowledgments by hand
#(which for these two are just the last paragraphs of each)
systbiol_articles[[12]] = systbiol_articles[[12]][-length(systbiol_articles[[12]])]
systbiol_articles[[46]] = systbiol_articles[[46]][-length(systbiol_articles[[46]])]

#Some articles include author, title, volume, issue, doi, etc as first paragraph, we'll remove them
#All of these have the journal's name in the first paragraph as well, so we are going to delete the first
#paragraph if we detect the journal's name in it
for(i in 1:length(systbiol_articles)) {
  if(str_detect(systbiol_articles[[i]][1], 'Systematic Biology')) {
    systbiol_articles[[i]] = systbiol_articles[[i]][-1]
  }
}

#Titles have backslash escape sequences and trailing spaces, we'll remove them
systbiol_titles = gsub("  ", "", systbiol_titles)
systbiol_titles = gsub("[\r\n]", "", systbiol_titles)

#Flatten the character vectors
for(i in 1:length(systbiol_articles)) {
  systbiol_articles[[i]] = paste(unlist(systbiol_articles[[i]]), sep = ' ', collapse = '')
}
systbiol_articles = unlist(systbiol_articles)

#That was a lot of work, let's combine everything and save it
systbiol = data.frame(Year = systbiol_pubdates, Link = systbiol_doi, Title = systbiol_titles, Article = systbiol_articles)
save(systbiol, file="systbiol.Rda")

###Mining Molecular Biology and Evolution----------------------------------------------------

#If code is not commented, it is doing the same thing that was already explained before
#url for search of research articles in Molecular Biology and Evolution with 'phylogenomics' in the Full Text
rm(list = ls())
url = 'https://academic.oup.com/mbe/search-results?page=1&f_ContentType=Journal+Article&f_ArticleTypeDisplayName=Research+Article&fl_SiteID=5325&qb=%7b%22FullText1%22%3a%22phylogenomics%22%7d&sort=Date+%E2%80%93+Oldest+First'
molbiolevol_webpage = read_html(url)

molbiolevol_doi = c()
molbiolevol_pubdates = c()
page = 1
more.results = T
which.page = c(0:9)

while(more.results == T) {
  molbiolevol_doi = c(molbiolevol_doi,html_text(html_nodes(molbiolevol_webpage,'.al-citation-list a')))
  molbiolevol_pubdates = c(molbiolevol_pubdates,html_text(html_nodes(molbiolevol_webpage,'.al-pub-date')))
  
  page = page + 1
  num_in_url = which(strsplit(url,'')[[1]] %in% which.page)
  if(num_in_url[2] != num_in_url[1] + 1) {
    url = paste(substr(url,1,(num_in_url[1]-1)),page,substr(url,(num_in_url[1]+1),str_length(url)),sep='')
  }  else {
    url = paste(substr(url,1,(num_in_url[1]-1)),page,substr(url,(num_in_url[2]+1),str_length(url)),sep='')
  }
  molbiolevol_webpage = read_html(url)
  if(!identical(html_text(html_nodes(molbiolevol_webpage,'.error')), character(0))) {
    more.results = F
  }
}

for(i in 1:length(molbiolevol_pubdates)) {
  molbiolevol_pubdates[i] = strsplit(molbiolevol_pubdates[i], ' ')[[1]][4]
}
molbiolevol_pubdates = as.numeric(molbiolevol_pubdates)

#Paper #1 has 'phylogenomics' only in the references, so we will eliminate it, #543 was not available, so idem
molbiolevol_doi = molbiolevol_doi[-c(1,543,which(molbiolevol_pubdates >= 2018))]
molbiolevol_pubdates = molbiolevol_pubdates[-c(1,543,which(molbiolevol_pubdates >= 2018))]

molbiolevol_articles = vector("list", length(molbiolevol_doi))
molbiolevol_titles = vector("list", length(molbiolevol_doi))

for(i in 1:length(molbiolevol_doi)) {
  url = molbiolevol_doi[i]
  molbiolevol_webpage = read_html(url)
  molbiolevol_articles[[i]] = html_text(html_nodes(molbiolevol_webpage,'.backacknowledgements-title , .section-title , p'))
  molbiolevol_titles[i] = html_text(html_nodes(molbiolevol_webpage,'.article-title-main'))
}

save(molbiolevol_articles, file="molbiolevol_articles.Rda")

stopwords = c('Acknowledgment','Acknowledgments','Supplementary Material','Supplementary data','Funding','Appendix')
eliminated = c()
i = 1

#The approach to select only the main text is slightly different for this journal
while(i <= length(molbiolevol_articles)) {
  to.eliminate = c()
  for(j in 1:length(molbiolevol_articles[[i]])) {
    #We'll eliminate some using stopwords as well
    if(molbiolevol_articles[[i]][j] %in% stopwords) {
      molbiolevol_articles[[i]] = molbiolevol_articles[[i]][-c(j:length(molbiolevol_articles[[i]]))]
      break
    }
    if(molbiolevol_articles[[i]][j] == '' && (j/length(molbiolevol_articles[[i]])) > 0.5) {
      #Other articles have an empty paragraph after the end of the main text and before everything
      #else we do not want to retain (some have it at the beginning as well, so we want to select
      #only the last instance of empty paragraphs, hence asking whether the empty paragraph is on
      #the second half of the article's length)
      molbiolevol_articles[[i]] = molbiolevol_articles[[i]][-c((j-1):length(molbiolevol_articles[[i]]))]
      break
    }
    if(length(strsplit(molbiolevol_articles[[i]][j], ' ')[[1]]) < 50) {
      to.eliminate = c(to.eliminate,j)
    }
  }
  
  molbiolevol_articles[[i]] = molbiolevol_articles[[i]][-to.eliminate]
  #Some articles are not available, these can be easily eliminated because they are really short
  #(less than 10 paragraphs). We'll save the list of eliminated articles for later.
  if(length(molbiolevol_articles[[i]]) <= 10) {
    molbiolevol_articles = molbiolevol_articles[-i]
    eliminated = c(eliminated,i)
  } else {
    i = i + 1
  }
}

#21 articles didn't match these patterns, we'll eliminate them by hand.
#Fortunately we only have to eliminate the last paragraph in each of these
#We will also remove the first paragraph for those in which it represents author, title, etc.
#For this we'll rely again on detecting the name of the journal.
exceptions = c(13,15,19,23,25,27,31,46,48,55,59,64,65,66,75,88,130,143,161,198,251)
for(i in 1:length(molbiolevol_articles)) {
  if(str_detect(molbiolevol_articles[[i]][1], 'Molecular Biology and Evolution')) {
    molbiolevol_articles[[i]] = molbiolevol_articles[[i]][-1]
  }
  if(i %in% exceptions) {
    molbiolevol_articles[[i]] = molbiolevol_articles[[i]][-length(molbiolevol_articles[[i]])]
  }
}

#Six articles were eliminated as they were too short, let's remove them from the other objects
molbiolevol_doi = molbiolevol_doi[-eliminated]
molbiolevol_pubdates = molbiolevol_pubdates[-eliminated]
molbiolevol_titles = molbiolevol_titles[-eliminated]

#Prepare everything to combine in one tibble
for(i in 1:length(molbiolevol_articles)) {
  molbiolevol_articles[[i]] = paste(unlist(molbiolevol_articles[[i]]), sep = ' ', collapse = '')
}

molbiolevol_articles = unlist(molbiolevol_articles)
molbiolevol_titles = unlist(molbiolevol_titles)

molbiolevol = data.frame(Year = molbiolevol_pubdates, Link = molbiolevol_doi, Title = molbiolevol_titles,
                         Article = molbiolevol_articles)
save(molbiolevol, file="molbiolevol.Rda")

###Mining PLoS One----------------------------------------------------
#Plos website seems to load an empty website after a query, and only load the results of it a little bit later.
#If mining is done with rvest, no results are found. Therefore, I used PhantomJS using a script that downloads
#the complete website after waiting a few seconds
rm(list = ls())

#Search for research articles in Plos One with 'phylogenomics' in the Body from 2000 to 2017
url = 'http://journals.plos.org/plosone/search?filterJournals=PLoSONE&filterArticleTypes=Research+Article&filterSections=Body&filterStartDate=2000-01-01&filterEndDate=2017-12-31&q=phylogenomics&page=1'
plos_doi = c()
plos_pubdates = c()

#The following will create a file with instructions for PhantomJS to download each of the
#pages from our search. After downloading it, it will read that file and extract what we are interested in
cat("var url ='http://journals.plos.org/plosone/search?filterJournals=PLoSONE&filterArticleTypes=Research+Article&filterSections=Body&filterStartDate=2000-01-01&filterEndDate=2017-12-31&q=phylogenomics&page=1';", "var page = new WebPage()", "var fs = require('fs');", "page.open(url, function (status) {", "        just_wait();", "});", "function just_wait() {", "    setTimeout(function() {", "               fs.write('1.html', page.content, 'w');", "            phantom.exit();", "    }, 15000);", "}", file="webscrape.js",sep="\n")
#We'll save the file in R as well
lines = readLines("webscrape.js")

#PhantomJS will download the webpage as file 1.html
system("phantomjs webscrape.js")

#And we can then read it and extract the information we are interested in
plos_webpage = read_html("1.html")
plos_doi = c(plos_doi,html_text(html_nodes(plos_webpage,'.search-results-doi a')))
plos_pubdates = c(plos_pubdates,html_text(html_nodes(plos_webpage,'.search-results-authors+ p')))
max.page = as.numeric(last(html_text(html_nodes(plos_webpage,'.text-color'))))

#This loop will modify the script used with PhantomJS and run it so that it moves through the search pages
for(i in 2:max.page) {
  if(i < 11) {
    url = paste(substr(url,1,(str_length(url)-1)),i,sep='')
  }  else {
    url = paste(substr(url,1,(str_length(url)-2)),i,sep='')
  }
  lines[1] = paste0("var url ='", url,"';")
  writeLines(lines, "webscrape.js")
  system("phantomjs webscrape.js")
  plos_webpage = read_html("1.html")
  plos_doi = c(plos_doi,html_text(html_nodes(plos_webpage,'.search-results-doi a')))
  plos_pubdates = c(plos_pubdates,html_text(html_nodes(plos_webpage,'.search-results-authors+ p')))
}

#Retain only the year of publication
for(i in 1:length(plos_pubdates)) {
  a = strsplit(plos_pubdates[i], '\n')[[1]]
  plos_pubdates[i] = last(strsplit(a[str_which(a, 'published')], ' ')[[1]])
}
plos_pubdates = as.numeric(plos_pubdates)

plos_articles = vector("list", length(plos_doi))
plos_titles = vector("list", length(plos_doi))

#Scrape the articles
for(i in 1:length(plos_doi)) {
  url = plos_doi[i]
  plos_webpage = read_html(url)
  plos_articles[[i]] = html_text(html_nodes(plos_webpage,'.toc-section p'))
  plos_titles[i] = html_text(html_nodes(plos_webpage,'#artTitle'))
}

save(plos_articles, file="plos_articles.Rda")

#Plos One has a highly standardized format, so if there is an acknowledgment and an author contribution
#section, these are always the last 2 paragraphs. Even if either one of these are missing, the last two paragraphs
#are never part of the main text, so we are safe to eliminate them for all
for(i in 1:length(plos_articles)) {
  to.eliminate = c()
  nparagraph = length(plos_articles[[i]])
  plos_articles[[i]] = plos_articles[[i]][-c((nparagraph-1):nparagraph)]
  for(j in 1:nparagraph) {
    if(length(strsplit(plos_articles[[i]][j], ' ')[[1]]) < 50) {
      to.eliminate = c(to.eliminate,j)
    }
  }
  plos_articles[[i]] = plos_articles[[i]][-to.eliminate]
}

for(i in 1:length(plos_articles)) {
  plos_articles[[i]] = paste(unlist(plos_articles[[i]]), sep = ' ', collapse = '')
}

plos_articles = as.character(plos_articles)
plos_titles = unlist(plos_titles)

#Save everything
plos = data.frame(Year = plos_pubdates, Link = plos_doi, Title = plos_titles, Article = plos_articles)
save(plos, file="plos.Rda")

###Mining Molecular Phylogenetics and Evolution------------------------------------------------------
#Neither rvest nor PhantomJS were successfull at getting the articles from this journal. I do not know the reason why,
#but both of them seemed to have problems with permissions, as the mined text only contained title and abstract, which 
#I guess is what is publicly available. In this case, RSelenium did the trick. We'll still use rvest to get the
#links to the papers, and then switch to RSelenium to scrape them
rm(list = ls())

#Search for research articles with the word phylogenomics
url = 'https://www.sciencedirect.com/search?qs=phylogenomics&pub=Molecular%20Phylogenetics%20and%20Evolution&origin=journal&zone=qSearch&cid=272322&articleTypes=FLA&show=25&offset=0'
molphyloevol_webpage = read_html(url)

molphyloevol_link = c()
molphyloevol_pubdates = c()
page = 0
more.results = T
while(more.results == T) {
  #Here the function html_attr will extract the website adress, rather the the text (as we've been doing with html_text)
  #that forms part of our CSS selector.
  molphyloevol_link = c(molphyloevol_link,paste('https://www.sciencedirect.com', html_attr(html_nodes(molphyloevol_webpage, '.text-s'), 'href')[-c(1:2)], sep = ''))
  molphyloevol_pubdates = c(molphyloevol_pubdates,html_text(html_nodes(molphyloevol_webpage, '.SubType')))
  
  page = page + 1
  if(page == 1) {
    url = paste(substr(url,1,(str_length(url)-1)),(page*25),sep='')
  } else {
    if(page < 5) {
      url = paste(substr(url,1,(str_length(url)-2)),(page*25),sep='')
    } else {
      url = paste(substr(url,1,(str_length(url)-3)),(page*25),sep='')
    }
  }
  molphyloevol_webpage = read_html(url)
  if(identical(html_text(html_nodes(molphyloevol_webpage,'.SubType')), character(0))) {
    more.results = F
  }
}

#some pubdates have issue number, others don't, but the year is always the previous to last object
#after separating by commas. Some were in press (and other might be when you run this!),
#but they will be easy to recognize as they will be NAs after running this loop.
for(i in 1:length(molphyloevol_pubdates)) {
  cut = strsplit(molphyloevol_pubdates[i], ', ')
  cut = cut[[1]][length(cut[[1]])-1]
  molphyloevol_pubdates[i] = unlist(strsplit(cut[[1]], ' '))[2]
}
molphyloevol_pubdates = as.numeric(molphyloevol_pubdates)

#Delete articles in press and thos from 2018 onwards
molphyloevol_link = molphyloevol_link[-c(which(molphyloevol_pubdates >= 2018), which(is.na(molphyloevol_pubdates)))]
molphyloevol_pubdates = molphyloevol_pubdates[-c(which(molphyloevol_pubdates >= 2018), which(is.na(molphyloevol_pubdates)))]

molphyloevol_articles = vector("list", length(molphyloevol_link))
molphyloevol_titles = vector("list", length(molphyloevol_link))

#start RSelenium
rD = rsDriver(browser = 'chrome')
remDr = rD[['client']]
i = 1

#More or less randomly, RSelenium will not wait until the article is fully loaded before scraping it.
#I am both setting an explicitly waiting time, and I am also asking it to retry until it gets the full
#article. Depending on your internet connection one of the two might be sufficient.
while(i <= length(molphyloevol_link)) {
  #navigate to the page
  remDr$navigate(molphyloevol_link[i])
  #wait a little bit
  Sys.sleep(5)
  #read the page html
  molphyloevol_webpage = read_html(remDr$getPageSource()[[1]])
  #There seems to be two different structures in which papers were organized. This will recognize each
  #format and scrape accordingly
  if(identical(html_text(html_nodes(molphyloevol_webpage,'.title-text')), character(0))) {
    molphyloevol_articles[[i]] = html_text(html_nodes(molphyloevol_webpage,'.svArticle , #frag_2 p'))
    molphyloevol_titles[i] = html_text(html_nodes(molphyloevol_webpage,'.svTitle'))
  } else {
    molphyloevol_articles[[i]] = html_text(html_nodes(molphyloevol_webpage,'#body-mathjax-container h2 , p'))
    molphyloevol_titles[i] = html_text(html_nodes(molphyloevol_webpage,'.title-text'))
  }
  if(length(molphyloevol_articles[[i]]) > 20) {
    #If you have it all, then move to the next one; otherwise retry
    i = i + 1
  }
}

#Don't pay attention to the errors popping up, titles were scraped correctly
#Let's close the browser, stop the selenium server and save the articles
remDr$close()
rD[["server"]]$stop()
save(molphyloevol_articles, file="molphyloevol_articles.Rda")

stopwords = c('Acknowledgments', 'Acknowledgements', 'Acknowledgement', 'Acknowledgment', 'Funding')

for(i in 1:length(molphyloevol_articles)) {
  to.eliminate = c()
  for(j in 1:length(molphyloevol_articles[[i]])) {
    if(molphyloevol_articles[[i]][j] %in% stopwords) {
      molphyloevol_articles[[i]] = molphyloevol_articles[[i]][-c(j:length(molphyloevol_articles[[i]]))]
      break
    }
    if(length(strsplit(molphyloevol_articles[[i]][j], ' ')[[1]]) < 50) {
      to.eliminate = c(to.eliminate,j)
    }
  }
  molphyloevol_articles[[i]] = molphyloevol_articles[[i]][-to.eliminate]
}

for(i in 1:length(molphyloevol_articles)) {
  molphyloevol_articles[[i]] = paste(unlist(molphyloevol_articles[[i]]), sep = ' ', collapse = '')
}

molphyloevol_articles = unlist(molphyloevol_articles)
molphyloevol_titles = unlist(molphyloevol_titles)

molphyloevol = data.frame(Year = molphyloevol_pubdates, Link = molphyloevol_link, Title = molphyloevol_titles, 
                          Article = molphyloevol_articles)
save(molphyloevol, file="molphyloevol.Rda")

###Mining Genome Biology and Evolution------------------------------------------------------------
rm(list = ls())

#url for search of research articles in Genome Biology and Evolution with 'phylogenomics' in the full text
url = 'https://academic.oup.com/gbe/search-results?page=1&sort=Date+%E2%80%93+Oldest+First&f_ContentType=Journal+Article&f_ArticleTypeDisplayName=Research+Article&f_TocHeadingTitle=Research+articleANDresearch+articles&fl_SiteID=5281&qb={%22FullText1%22:%22phylogenomics%22}'
#Reading the HTML code from the website
genomebiolevol_webpage = read_html(url)

genomebiolevol_doi = c()
genomebiolevol_pubdates = c()
page = 1
more.results = T
which.page = c(0:9)

#The following will loop through all the pages of the query and extract the doi of the articles
while(more.results == T) {
  genomebiolevol_doi = c(genomebiolevol_doi,html_text(html_nodes(genomebiolevol_webpage,'.al-citation-list a')))
  genomebiolevol_pubdates = c(genomebiolevol_pubdates,html_text(html_nodes(genomebiolevol_webpage,'.al-pub-date')))
  
  #Move to next page
  page = page + 1
  num_in_url = which(strsplit(url,'')[[1]] %in% which.page) #find all the numbers in url
  if(num_in_url[2] != num_in_url[1] + 1) { #if we are in pages < 10
    url = paste(substr(url,1,(num_in_url[1]-1)),page,substr(url,(num_in_url[1]+1),str_length(url)),sep='')
  }  else { #if we are in pages > 10
    url = paste(substr(url,1,(num_in_url[1]-1)),page,substr(url,(num_in_url[2]+1),str_length(url)),sep='')
  }
  genomebiolevol_webpage = read_html(url)
  if(!identical(html_text(html_nodes(genomebiolevol_webpage,'.error')), character(0))) {
    more.results = F #Reached last page
  }
}

#Store only the years
for(i in 1:length(genomebiolevol_pubdates)) {
  genomebiolevol_pubdates[i] = strsplit(genomebiolevol_pubdates[i], ' ')[[1]][4]
}
genomebiolevol_pubdates = as.numeric(genomebiolevol_pubdates)

#Remove articles from 2018 or younger
genomebiolevol_doi = genomebiolevol_doi[-c(which(genomebiolevol_pubdates >= 2018))]
genomebiolevol_pubdates = genomebiolevol_pubdates[-c(which(genomebiolevol_pubdates >= 2018))]

#Scraping titles and main text
genomebiolevol_articles = vector("list", length(genomebiolevol_doi))
genomebiolevol_titles = vector("list", length(genomebiolevol_doi))

#Some from 2017 weren't available still, so we'll have to eliminate them (they should be available now however..)
eliminated = c() 

for(i in 1:length(genomebiolevol_doi)) {
  url = genomebiolevol_doi[i]
  genomebiolevol_webpage = read_html(url)
  if(identical(html_text(html_nodes(genomebiolevol_webpage,'.article-title-main')), character(0))) {
    eliminated = c(eliminated, i)
  } else {
    genomebiolevol_articles[[i]] = html_text(html_nodes(genomebiolevol_webpage,'.backacknowledgements-title , .section-title , p'))
    genomebiolevol_titles[i] = html_text(html_nodes(genomebiolevol_webpage,'.article-title-main'))
  }
}

genomebiolevol_articles = genomebiolevol_articles[-eliminated]
genomebiolevol_doi = genomebiolevol_doi[-eliminated]
genomebiolevol_pubdates = genomebiolevol_pubdates[-eliminated]
genomebiolevol_titles = genomebiolevol_titles[-eliminated]

save(genomebiolevol_articles, file = 'genomebiolevol_articles.Rda')

#In this case, the stopwords approach works perfectly for the articles after number 58,
#For those before, the acknowledgment section is not titled, but it is always right before
#the supplementary data. Therefore, we have to use two different approaches
stopwords = c('Funding','Acknowledgments','Acknowledgements','Acknowledgment')

for(i in 1:length(genomebiolevol_articles)) {
  to.eliminate = c()
  for(j in 1:length(genomebiolevol_articles[[i]])) {
    if(i < 58 && genomebiolevol_articles[[i]][j] == 'Supplementary data') {
      genomebiolevol_articles[[i]] = genomebiolevol_articles[[i]][-c((j-1):length(genomebiolevol_articles[[i]]))]
      break
    } else {
      if(genomebiolevol_articles[[i]][j] %in% stopwords) {
        genomebiolevol_articles[[i]] = genomebiolevol_articles[[i]][-c(j:length(genomebiolevol_articles[[i]]))]
        break
      } else {
        if(length(strsplit(genomebiolevol_articles[[i]][j], ' ')[[1]]) < 50) {
          to.eliminate = c(to.eliminate,j)
        }
      }
    }
  }
  genomebiolevol_articles[[i]] = genomebiolevol_articles[[i]][-to.eliminate]
}

#Three articles did not match any approach, and so I am eliminating the ackowledgments by hand
#(which are always the last paragraph)
for(i in c(19,25,29)) {
  genomebiolevol_articles[[i]] = genomebiolevol_articles[[i]][-length(genomebiolevol_articles[[i]])]
}

#Titles have backslash escape sequences and trailing spaces, we'll remove them
genomebiolevol_titles = gsub("  ", "", genomebiolevol_titles)
genomebiolevol_titles = gsub("[\r\n]", "", genomebiolevol_titles)

for(i in 1:length(genomebiolevol_articles)) {
  genomebiolevol_articles[[i]] = paste(unlist(genomebiolevol_articles[[i]]), sep = ' ', collapse = '')
}

genomebiolevol_articles = unlist(genomebiolevol_articles)

genomebiolevol = data.frame(Year = genomebiolevol_pubdates, Link = genomebiolevol_doi, Title = genomebiolevol_titles, 
                            Article = genomebiolevol_articles)
save(genomebiolevol, file="genomebiolevol.Rda")

###Mining Proceedings B------------------------------------------------------------
rm(list = ls())

#url for search of research articles in Proceedings B with 'phylogenomics' up to (and including) 2017
url = 'http://rspb.royalsocietypublishing.org/search/phylogenomics%20jcode%3Aroyprsb%20limit_to%3A2017-12-31%20numresults%3A10%20sort%3Apublication-date%20direction%3Aascending%20format_result%3Astandard?page=0'
#Reading the HTML code from the website
procb_webpage = read_html(url)
#How many pages will there be
num_pages = ceiling(as.integer(str_split(gsub("[\t\n]", "", html_text(html_nodes(procb_webpage,'#page-title'))), ' ')[[1]][1])/10)
num_pages = num_pages

procb_link = c()
procb_pubdates = c()

#The following will loop through all the pages of the query and extract the doi of the articles
for(i in 1:num_pages) {
  procb_prelinks = html_nodes(procb_webpage,'.highwire-cite-title')
  procb_prelinks = procb_prelinks[str_detect(procb_prelinks, 'href')]
  procb_link = c(procb_link,paste('http://rspb.royalsocietypublishing.org', str_match(procb_prelinks, 'a href="(.*?)"')[,2], sep = ''))
  procb_pubdates = c(procb_pubdates,html_text(html_nodes(procb_webpage,'.highwire-cite-metadata-date')))
  
  #Move to next page
  if(i < 11) {
    url = paste(substr(url,1,(str_length(url)-1)),i,sep='')
  }  else {
    url = paste(substr(url,1,(str_length(url)-2)),i,sep='')
  }
  procb_webpage = read_html(url)
  i = i + 1
}

#Store only the years
for(i in 1:length(procb_pubdates)) {
  procb_pubdates[i] = strsplit(procb_pubdates[i], ' ')[[1]][4]
}
procb_pubdates = as.numeric(procb_pubdates)

#Scraping titles and main text
procb_articles = vector("list", length(procb_link))
procb_titles = vector("list", length(procb_link))

for(i in 1:length(procb_link)) {
  url = procb_link[i]
  procb_webpage = read_html(url)
  procb_articles[[i]] = html_text(html_nodes(procb_webpage,'.fulltext-view'))
  procb_titles[i] = html_text(html_nodes(procb_webpage,'.highwire-cite-title'))[1]
}

#Some of them are not available online (3).
to.eliminate = c()
for(i in 1:length(procb_link)) {
  if(length(procb_articles[[i]]) < 1) {
    to.eliminate = c(to.eliminate, i)
  }
}

#Another one wans't available either, but we scraped the abstract so it did not show up before.
#Another one is an editorial. These are the only two really short articles.
for(i in 1:length(procb_link)) {
  if(length(strsplit(procb_articles[[i]], ' ')[[1]]) < 2000) {
    to.eliminate = c(to.eliminate, i)
  }
}

procb_articles = procb_articles[-to.eliminate]
procb_link = procb_link[-to.eliminate]
procb_pubdates = procb_pubdates[-to.eliminate]
procb_titles = procb_titles[-to.eliminate]

save(procb_articles, file = 'procb_articles.Rda')

stopwords = c('Acknowledgments', 'Data accessibility', 'Funding statement', 'Ethics', "Authors' contributions", 
              'Competing interests', 'Funding', 'References')

for(i in 1:length(procb_articles)) {
  lengths = rep(0, length(stopwords))
  for(j in 1:length(lengths)) {
    lengths[j] = str_length(gsub(paste(stopwords[j], ".*", sep=''),"",procb_articles[[i]]))
  }
  procb_articles[[i]] = gsub(paste(stopwords[which(lengths == min(lengths))], ".*", sep=''),"",procb_articles[[i]])
}

procb_articles = unlist(procb_articles)
procb_titles = unlist(procb_titles)

procb = data.frame(Year = procb_pubdates, Link = procb_link, Title = procb_titles, Article = procb_articles)
save(procb, file="procb.Rda")

#######Part 2: TEXT MINING------------------------------------------------------------------------------

###Text handling and analyses---------------------------------------------------------------------------

rm(list=ls())

#If you are starting here, set the working directory to the folder containing the supplementary files of the paper
#by modifying the following
setwd(getwd())

#Load all of the data
load(file="systbiol.Rda")
load(file="molbiolevol.Rda")
load(file="plos.Rda")
load(file="molphyloevol.Rda")
load(file="genomebiolevol.Rda")
load(file="procb.Rda")

#Let's combine everything and tidy it up
all_articles = as.tibble(rbind(systbiol, molbiolevol, plos, molphyloevol, genomebiolevol, procb)) %>%
  mutate_if(is.numeric, as.integer) %>% mutate_if(is.factor, as.character) %>%
  add_column(Journal = c(rep('Systematic Biology', nrow(systbiol)), rep('Molecular Biology and Evolution', nrow(molbiolevol)), rep('Plos One', nrow(plos)), rep('Molecular Phylogenetics and Evolution', nrow(molphyloevol)), rep('Genome Biology and Evolution', nrow(genomebiolevol)), rep('Proceedings of the Royal Society B', nrow(procb)))) %>%
  mutate(Journal = as.factor(Journal)) %>% dplyr::select(Year, Journal, everything()) %>% arrange(Year)

#Plot papers by journal through time (Fig. S1)
ggplot(data = all_articles, mapping = aes(x = Year, fill = Journal)) + 
  geom_bar() + labs(y = 'Number of publications')

#Eliminate everything in parentheses (mostly just citations) and everything in square brackets (citation format for
#PLoS One), as well as all numbers. Just to leave things clean, let's also remove all double spaces we introduced in the
#process (although this is not really needed). Also, remove hanging periods and minuses that are left from removing numbers.
all_articles = all_articles %>% mutate(Article = gsub('\\([^\\)]+\\)', '', Article)) %>%
  mutate(Article = gsub('\\[[^]]+]', ' ', Article)) %>% mutate(Article = gsub('[0-9]', '', Article)) %>%
  mutate(Article = gsub('  ', ' ', Article)) %>% mutate(Article = gsub(' . ', ' ', Article)) %>%
  mutate(Article = gsub(' - ', ' ', Article))

#Tokenize (put each word in a row), and stem it
all_words = all_articles %>% unnest_tokens(Words, Article) %>% 
  anti_join(stop_words, by = c('Words' = 'word')) %>% mutate(Stems = wordStem(Words))

#What are the most common words overall?
all_words %>% count(Stems, sort = TRUE) %>% print(n = 20)

#Obtain count of words (stemmed) per year
freq_per_year_stem = all_words %>% count(Stems, Year)

#Obtain the total number of words published in each year...
years_covered = unlist(unique(all_words$Year), use.names = F)
total_words = vector(length = length(years_covered))
for(i in 1:length(years_covered)) {
  total_words[i] = nrow(all_words %>% filter(Year == years_covered[i]))
}
total_count = tibble(Year = years_covered, Total = total_words)

#... and plot it
ggplot(total_count, aes(Year, Total)) + geom_point() + geom_smooth() + labs(x = 'Year', y = 'Number of words')

#Create a column with stem frequency per year, dividing the count by the total number of words in a year
freq_per_year_stem = freq_per_year_stem %>% left_join(total_count, by = 'Year') %>%
  mutate(freq = n/Total) %>% dplyr::select(-Total)

#Let's also summarize per article (might be useful..)
freq_per_article_stem = all_words %>% count(Stems, Link)

#This contains lots of symbols, mispellings, as well as words used very sporadically
#We can get rid of all of these by keeping only those found in more than 2/3 of the covered years,
#as well as more than 10% of total articles.
#Having many observations will also make inferences on temporal dynamics more robust
#(This takes some time)
all_unique_stems = unique(freq_per_year_stem$Stems)
pos = 1
while(pos <= length(all_unique_stems)) {
  if(nrow(freq_per_year_stem %>% filter(Stems == all_unique_stems[pos])) < ceiling(length(years_covered)*(2/3))) {
    all_unique_stems = all_unique_stems[-pos]
  } else {
    if(nrow(freq_per_article_stem %>% filter(Stems == all_unique_stems[pos])) < ceiling(nrow(all_articles)*0.1)) {
      all_unique_stems = all_unique_stems[-pos]
    } else {
      pos = pos + 1
    }
  }
}

#Filter the datasets to retain only stems that fulfill those requirements
freq_per_year_stem = freq_per_year_stem %>% filter(Stems %in% all_unique_stems)
freq_per_article_stem = freq_per_article_stem %>% filter(Stems %in% all_unique_stems)

#Add ceros for the years with no mentions (very straightforward with function complete) and save
freq_per_year_stem = freq_per_year_stem %>% complete(Stems, Year, fill = list(n = 0, freq = 0)) %>% arrange(Stems, Year)
freq_per_article_stem = freq_per_article_stem %>% complete(Stems, Link, fill = list(n = 0)) %>% arrange(Stems, Link)
save(freq_per_year_stem, file="freq_per_year_stem.Rda")

#Check for correlation with time.
#Save the p-values and the correlation coefficients.
word = pval = rho = vector(length = length(all_unique_stems))
for(i in 1:length(all_unique_stems)) {
  cor = cor.test(as.numeric(unlist(freq_per_year_stem[freq_per_year_stem[,1] == all_unique_stems[i],2])), 
                 as.numeric(unlist(freq_per_year_stem[freq_per_year_stem[,1] == all_unique_stems[i],4])), 
                 method = 'spearman', exact = F)
  word[i] = all_unique_stems[i]
  pval[i] = cor$p.value
  rho[i] = cor$estimate
}

#Put it all in a tibble and order it by decreasing correlation coefficient
correlations_stem = data_frame(Word = word, Correlation = rho, Pvalue = pval) %>% arrange(desc(Correlation))

#188 words can be considered to have a significant positive trend with time
#after adjusting for multiple comparisons (a further 36 show a significant negative correlation)
which(p.adjust(unlist((correlations_stem)$Pvalue), method = 'BH') < 0.05)

#Let's just focus on the ones with a significant positive correlation
strong_correlations_stem = correlations_stem %>% slice(1:188)

#Print them and save them for later analyses
print(strong_correlations_stem, n = 188)
reduced_stems = freq_per_year_stem %>% filter(Stems %in% strong_correlations_stem$Word)

#For each stem, also find the words are feading into them (i.e., the inflected variants in which
#shape they show up in the articles). We will save them in the named list 'words'
correlated_stems = unique(strong_correlations_stem$Word)
words = list()
total = vector(length = length(correlated_stems))
for(i in 1:length(correlated_stems)) {
  words = c(words, list(sort(table(all_words[all_words$Stems == correlated_stems[i],5]), decreasing = T)))
  total[i] = sum(words[[i]])
}
names(words) = correlated_stems
words

#Estimate uncertainty in frequency values by bootstraping articles. This also takes a long time to run.
#First, let's define the number of replicates. The figure on the paper was produced using 1,000 but 100
#gives more or less the same answer and saves a lot of time.
replicates = 100
boot_words = vector(length = length(years_covered))

for(i in 1:replicates) {
  #Let's start by resampling articles with replacement
  boot_repl = all_articles %>% sample_frac(size = 1, replace = T) %>% unnest_tokens(Words, Article) %>%
    anti_join(stop_words, by = c('Words' = 'word')) %>% mutate(Stems = wordStem(Words)) %>% count(Stems, Year)
  
  #Calculate the total number of words in the bootstrapped pseudoreplicate
  for(j in 1:length(years_covered)) {
    boot_words[j] = nrow(boot_repl %>% filter(Year == years_covered[j]))
  }
  total_boot_count = tibble(Year = years_covered, Total = boot_words)
  
  #We will use this variable as column name
  varname = paste("repl", i , sep=".")
  
  #We don't need the entire list of words anymore, so let's reduce to the instances of observation of
  #the selected words, and calculate their frequency out of the total number of words per replicate.
  #The resulting object will contain the Stem, Year, and a column representing the frequency obtained
  #per replicate
  if(i == 1) {
    bootstrap_articles = boot_repl %>% filter(Stems %in% strong_correlations_stem$Word) %>% left_join(total_boot_count, by = 'Year') %>% 
      mutate(!!varname := n/Total) %>% select(-Total) %>% complete(Stems, Year) %>% arrange(Stems, Year) %>%
      select_('Stems', 'Year', varname)
  } else {
    bootstrap_articles = bootstrap_articles %>% mutate(!!varname := unname(unlist(boot_repl %>% filter(Stems %in% strong_correlations_stem$Word) %>%
      left_join(total_boot_count, by = 'Year') %>% mutate(!!varname := n/Total) %>% select(-Total) %>%
      complete(Stems, Year) %>% arrange(Stems, Year) %>% select_(varname))))
  }
}

#Replace all 0s at once and save
bootstrap_articles = bootstrap_articles %>% mutate_all(funs(replace(., is.na(.), 0)))
save(bootstrap_articles, file='boot.Rda')

#Calculate standard errors. Given the format of the object this requires operations by row. The tidyverse
#offers intuitive and fast ways of performing operations by columns, but doesn't work that well with rows.
#Just to show how this can be done anyways, here is a way of doing it with tidy functions, but this is 
#unnecessarily slow...
boot_sd = bootstrap_articles %>% select(starts_with('repl')) %>% rowwise() %>% 
  do(data.frame(., stdev = sd(unlist(.[str_c('repl.', c(1:replicates))])))) %>% select(stdev)

reduced_stems = reduced_stems %>% mutate(se = unlist(boot_sd, use.names = F)/sqrt(nrow(all_articles)))

#Plotting the results-----------------------------------------------------------------------------------

#For the three focus stems, generate the labels for the plot,
#including the stem and the three most common uses of it 
interesting_words = c('discord', 'suscept', 'discrimin')
labels = vector(length = length(interesting_words))
for(i in 1:length(interesting_words)) {
  different_uses = names(unlist(words[interesting_words[i]]))[1:3]
  different_uses = str_split(different_uses, '\\.')
  for(j in 1:length(which(!is.na(different_uses)))) {
    different_uses[j] = gsub(different_uses[[j]][1], "", different_uses[[j]][2])
  }
  if(length(which(str_length(different_uses) == 0)) + length(which(is.na(str_length(different_uses)))) < 3) {
    labels[i] = str_c(interesting_words[i], '[', str_c(different_uses, collapse = '/'), ']')
  } else {
    labels[i] = interesting_words[i]
  }
}

labels = str_replace(labels, '/]', ']')
labels = str_replace(labels, '//', '/')

#Recreate Fig. 1, using cubic regression splines to show the trend and +- 1 standard error as
#estimated using article bootstrapping
reduced_stems %>% filter(Stems %in% interesting_words) %>% mutate_if(is.character, as.factor) %>% 
  ggplot(aes(x=Year, y=freq, color=Stems, group=Stems)) + geom_point() + 
  geom_smooth(size = 2, method = 'lm', formula = y ~ splines::bs(x, 3), alpha = 0.1, se = F) +
  geom_errorbar(aes(ymin=freq-se, ymax=freq+se), width=.2, alpha = 0.8) +
  facet_wrap(~ Stems, scales = 'free_y', ncol = 3) + 
  theme(legend.position = 'bottom') + labs(y = 'Frequency') + 
  scale_color_manual('Words', values = brewer.pal(3, 'Set2'), labels = labels) + 
  theme(strip.background = element_blank(), strip.text = element_blank(), aspect.ratio = 1,
  legend.text = element_text(size = 12), panel.grid.major = element_line(colour="#cccccc",
  size = 0.1), panel.grid.minor = element_line(colour="#cccccc", size = 0.1), 
  panel.background = element_rect(fill = "white", color = 'black')) +
  scale_y_continuous(labels = scales::scientific)

#Save all instances of these words to files. This will generate one file per selected stem,
#each one including all sentences in which the stem is employed
all_sentences = all_articles %>% unnest_tokens(Sentences, Article, token = 'sentences')
words_to_search = words[names(words) %in% interesting_words]

for(i in 1:length(interesting_words)) {
  all_sentences %>% select(Year, Sentences) %>% 
    filter(str_detect(Sentences, paste(names(unlist(unname(words_to_search[i]))), collapse = '|'))) %>% 
    write.table(file = paste(names(words_to_search[i]), '.txt', sep=''), quote = F)
}

#You can modify this to have R print to the console a sample of sentences containing any of the words
#from the list of the significantly trending stems (in this case, we are using the stem 'trim' as example)
words_to_search = words[names(words) == 'trim']
cat(unlist(all_sentences %>% select(Sentences) %>% 
    filter(str_detect(Sentences, paste(names(unlist(unname(words_to_search))), collapse = '|'))) %>% 
    mutate(Sentences = paste(Sentences, '\n')) %>% sample_n(size = 100), use.names = F))

###Some concluding remarks---------------------------------------------------------------------------------------------

#Although some of the words selected are used almost exclusively in the sense expressed in the
#manuscript (e.g., 'discordance' refering to differences in topology obtained from different 
#data sources) others are used in different ways with different meanings.
#However, from the total number of sentences using the stem 'discrim' to express whether molecular
#data is strong enough to favor specific topologies, models or evolutionary scenarios, over 2/3 use it
#to express the inability to discriminate. 'suscept' is also used to express suceptibility rather
#than lack of it (e.g., 'more susceptible' is 2.3 times more common than 'less susceptible';
#'not suscepible' and 'not be susceptible' only occur once each, while 'is susceptible' and 
#'are suceptible' occur 42 times, and other bigrams such as 'highly susceptible' (17 times) and
#'particularly suceptible' (12 times) are also common). It is therefore valid to interpret an increase
#in the frequency of use of these three words (i.e., those in Fig. 1) as is done in the paper.
