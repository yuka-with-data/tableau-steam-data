# tableau-steam-data

This is a repository for Steam Game Dataset and Source Codes for Tableau Visualization Project.

------------------------------------------------------------------------

`tableau_steam_script.R`: This is a source code file to generate Steam Game Review dataset. I performed a basic data stringing and took out common words, but I kept STOPWORDS, because some of the words carry high sentiment values.

`token_words_script.R`: I extracted the review data from original Steam Game Review dataset, and performed data stringing such as removing common words AND STOPWORDS. This data is useful for a word visualization.
