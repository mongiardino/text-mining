# text-mining
Tutorial on web scraping and text mining with R. Contains code and data files supporting Mongiardino Koch (2019) "The phylogenomic revolution and its conceptual innovations".

The first half of the file text_mining_phylogenomics.R contains the code used to perform web scraping on 6 scientific journals. The text is heavily commented, so it can be used as a tutorial on difference ways of doing text mining with R. Three different approaches are explored, relying on R packages *rvest* and *RSelenium*, and the browser PhantomJS (http://phantomjs.org/). All CSS selector were identified with SelectorGadget (https://selectorgadget.com/), a very useful Chrome extension for web scraping.

The second half of the file contains the code to perform text mining. All text is handled using tidy data principles. Given that web content is dynamic, it is very likely that running the first half of the code today will result in different scraped content. Therefore, all the scraped text that was employed to write the manuscript can be found on the .Rda files. These should be loaded if results want to be replicated.
