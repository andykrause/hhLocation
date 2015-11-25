##### Introduction
*Please answer these introductory questions for your case study in a few sentences.*

1) Who are you and what is your research field? Include your name, affiliation, discipline, and the background or context of your overall research that is necessary specifically to introduce your specific case study.

Andy Krause, Lecturer in Property (Real Estate) at the University of Melbourne.  My research focuses on the spatial analysis of real estate markets, particularly in regards to valuation and location.  

2) Define what the term "reproducibility" means to you generally and/or in the particular context of your case study.

"Reproducibility" means that a subsequent researcher(s) can openly access the data, code, analytical workflow and data provenence and then re-create the identical results WITHOUT consulting the original researcher(s).

##### Workflow diagram

[Diagram](workflowDiagram.pdf)

##### Workflow narrative

This research analyzes the household location choices of American households in the largest 50 metropolitan areas in the United States.  Households are broken down by five-year age cohorts (based on the age of the head of the householder) and mapped against the household's distance (census block group level) from the central business district of the metropolitan area in which they reside.  In polycentric regions, such as Seattle (Tacoma, Bellevue and Everett as alternative CBDs), analyses are conducted on distance to core center as well as secondary centers. An initial paper reporting the results is currently under review.

All data, code and analytical workflow are hosted on-line.  Code and analytical workflow, including analytical script and custom function sets, are written in R and found on the project's [Github Repository](http:/github.com/andykrause/hhLocation).  The complete set of raw data is available through the U.S. Census (discussed below).  Users wishing to skip the data compiling and/or cleaning steps can download the compiled or cleaned data from the project's [Dataverse Repository](http://www.this-page-intentionally-left-blank.org/).  Finally, an interactive website that allows users to further explore the results is hosted by [ShinyApps.io](http://www.this-page-intentionally-left-blank.org/)

The *hhLocAnalysis.R* file in the main analysis script and the only file that needs to be executed.  Two key process parameters must be manually set:

1. **reBuildData** - do you want to go through the entire data compilation process?
2. **reCleanData** - do you want to re-clean data?

A third parameter -- the location of a suitable data directory -- must be set if **reBuildData** is set to TRUE. 

The user must also specify the directory where the [Code Repository](http:/github.com/andykrause/hhLocation) was cloned (**codeDir**). Additional parameters regarding the location of the downloaded intermediate data (see below) -- **rawDataFile** and **cleanDataFile** -- and a path to output the figues to (**figurePath**) must also be set. This is the extent of manual operation. All other processes run automatically.  If the data is fully built from scratch this process may take multiple hours. Additionally, the use may change a number of the optional paramters that handle the distance scaling, overall number of metro-regions to analyze, maximum distance from CBD center to include in the data and whether or not computational progress is reported. 

The raw data for this study comes primarily from the U.S. Census's FTP site.  Data files for every county in the fifty largest metropolitan areas are downloaded, unzipped, cleaned and written out as a standardized .csv file.  Custom functions to handle the data acquisition process were written in R and are found in the *buildHHData.R* and *buildCBSAData.R* files in the repository.  This time consuming (and data storage intensive) process of compilation can be avoided by setting **reBuildData** to FALSE. If either **reBuildData** or **reCleanData** are set to FALSE then the intermediate data sets (compiled and/or cleaned) must be downloaded from the [Dataverse Repository](http://www.this-page-intentionally-left-blank.org/).  Two additional files containing city and CBSA (metro region) names are also needed for the analysis.  These files are available in .csv form in the code repository. 

After the cleaned data has been compiled and built (or downloaded from Dataverse),
the main analysis script then calculated the location quotient distance profiles and plots a variety of different visualizations of the results. Final results, both tabular and visual, are then combined in an RStudio/Knitr file along with the narrative to create the final document (compiled in LaTeX).  The full data provenance is described and hosted on the code repository via a Markdown file.  Finally, the raw R objects along with RStudio Shiny code are uploaded to ShinyApps.io to create the interactive online website.  Also note that the collaborative website [Authorea](https://www.authorea.com/users/18208) (which offers git-based tracking and LaTeX support) was used by the authors to write the first draft of the narrative portion of the report.  

##### Pain points

There are two major steps that we consider particularly painful.  The first is convincing yourself (and co-authors) to take the time to properly document every action and to take the time to fully annotate the analytical workflow.  This can be especially difficult when deadlines arise or when co-authors do not see the value in reproducibility.  The second is the need to write custom functions that are generalizable.  Writing very specific, single use function can be easy, but are rarely useful in more than a single instance.  Good reproducible research contains flexible functions than can accomodate changes or permutations thereby allowing subsequent users to expand/change your original analysis.  

The current peer-review process also presents a considerable hurdle to reproducibility.  In order to remain anonymous in the review process, we've had to build a set of anonymous code and data repositories and interactive websites for the review process and then switch over to my own repos after the paper is accepted. It means a lot of extra work and remember which Github count we are signed into at all times.  Along this line, judging by usage statistics, reviewers have been uninterested in actually examining the hosted code, data or results. 

##### Key benefits

For me the biggest benefit is efficiency.  The first time we do an analysis it usually take longer than colleagues, but each time after the time savings simply multiply.  One situation where this is particulary helpful is in responding to peer reviewer comments and requests.  Changes to assumptions or sensitiviy tests on parameters can be done in a matter of hours (or minutes), not days or weeks.  This greatly shortens the re-submittal response time.  Related, we constantly find ourselves borrowing old code and re-purposing it, making new analyses easier and faster.  

Better organization is another benefit.  No more folders full of data files with version names and dates. No more mystery fields in a dataset. No more starting all over after forgetting what was previously done. My students and their Excel sheets with dozens of tabs and screen clips from SPSS remind me of this benefit every semester. We are slowly incorporating more and more reproducibility into our classes, with the intent of breaking some of these bad habits.  

##### Key tools

The RStudio IDE and their related Shiny Apps (interactive web applications) have been a huge help in my reproducible research.  If you are an R programmer and want to share your visualizations with non-programmers, we highly recommend these tools from RStudio. 

##### General questions about reproducibility

*Please provide short answers (a few sentences each) to these general questions about reproducibility and scientific research. Rough ideas are appropriate here, as these will not be published with the case study. Please feel free to answer all or only some of these questions.*

1) Why do you think that reproducibility in your domain is important?

A majority of quantitative analysis in real estate (both academic and professional) is usually duplicated by numerous parties, widely disseminated and frequently updated; all characteristics that benefit from reproducible analyses.  Despite these core facts of the discipline, there is very little, if any, discussion on or attempt to create reproducible research in the field.  

2) How or where did you learn the reproducible practices described in your case study? Mentors, classes, workshops, etc.

My pre-academic background is as part of a team of expert witnesses in litigation support.  In this industry, any analysis we produced had to be reproducible by the opposition and therefore, we were constantly striving to produce more efficiency in our reproducible analyses.  

3) What do you see as the major pitfalls to doing reproducible research in your domain, and do you have any suggestions for working around these? Examples could include legal, logistical, human, or technical challenges.

Two challenges exists in the real estate field.  First, most data is proprietary and expensive and therefore it is hard to share data.  Second, it is a small field, that is composed of many senior individuals (both in academia and industry), many of whom are very resistant to change. 

4) What do you view as the major incentives for doing reproducible research?

Time savings, better quality output and the increased opportunity to collaborate/share my ideas.  

5) Are there any broad reproducibility best practices that you'd recommend for researchers in your field?

No more manual data cleaning.  Use code.  

6) Would you recommend any specific websites, training courses, or books for learning more about reproducibility?

For collaboration:  Try [Authorea](https://www.authorea.com/) 

If you are in Australia, the [University of Melbourne's Research Platforms](http://blogs.unimelb.edu.au/researchplatforms/) group offers a number of Research Bazaars, Software Carpentry and Reproducibility-related courses and event.  It is open to researchers world-wide.  
