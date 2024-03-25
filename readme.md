# corp_ling_campaign_analysis
 A comparative corpus analysis of recent U.S. Republican party platforms and presidential campaigns

Documents included in the repository: 

Scripts (named in order of execution):

1_campaign_scraper.py: script to extract campaign-related text data from the website of the American Presidency Project based on customizable parameters and save them it as .txt files

2_unifier.py:	script to open, unify and save individual .txt files into one .txt file

3_corpus_builder.R: script to clean up and pre-process text files, save them as VCorpus objects, count the number of tokens in them, transform and save them into tidy data frames

4_analysis_I_comp_freqlists.R: script to create word frequency lists of tidy objects, then compare two of them and plot the comparison

5_analysis_II_bigrams.R: script to create bigram frequency list of a target word and target corpus, then remove the target word from the bigrams and visualize the result on a word cloud

Corpora:

6 vcorpus objects and 6 tidy objects resulting from the pre-processing and cleaning of the 6 .txt files in the data folder

Data:

All the documents as they were scraped from the TAPP website plus the three unified documents, the output of unified.py

Report:

final report
