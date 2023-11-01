library(dplyr)
library(ggplot2)
library(tidyr)


#read three datasets
movie_title <- read.delim("/Users/yuxuanhu/Desktop/ai_film_review/title.akas.tsv", 
                          header = TRUE, sep = "\t", nrows = 3)
total_col <- length(names(movie_title))
col_class <- rep("NULL", length(names(movie_title)))
col_class[c(1,3,4,7)] <-"character"
movie_title <- read.delim("/Users/yuxuanhu/Desktop/ai_film_review/title.akas.tsv", 
                          header = TRUE, sep = "\t", colClasses = col_class)


movie_genre <- read.delim("/Users/yuxuanhu/Desktop/ai_film_review/title.basics.tsv", 
                          header = TRUE, sep = "\t", nrows = 3)
total_col <- length(names(movie_genre))
col_class <- rep("NULL", length(names(movie_genre)))
col_class[c(1,2,6,9)] <-"character"
movie_genre <- read.delim("/Users/yuxuanhu/Desktop/ai_film_review/title.basics.tsv", 
                          header = TRUE, sep = "\t", colClasses = col_class)

movie_rating <- read.delim("/Users/yuxuanhu/Desktop/ai_film_review/title.ratings.tsv", 
                          header = TRUE, sep = "\t")

plot_keywords<- read.delim("/Users/yuxuanhu/Desktop/ai_film_review/plotkey.tsv", 
                           header = TRUE)
# Merge movie_title and movie_genre
combined_table <- merge(movie_title, movie_genre, by.x = "titleId", by.y = "tconst", all.x = TRUE)

# Merge the combined_table with movie_rating
final_combined_table <- merge(combined_table, movie_rating, by.x = "titleId", by.y = "tconst", all.x = TRUE)

#Convert start year to integer
final_combined_table$startYear <- as.integer(final_combined_table$startYear)

#After first AI film released in US
US_1956 <-final_combined_table %>%
    filter(startYear > 1956 & region == "US")

#plot keywords of film after 1956
with_keywords <- left_join(plot_keywords, US_1956, by=c('titleId'='titleId'))

#filter the dataset with unique movie id
distinct_movies <- with_keywords %>%
    distinct(titleId, .keep_all = TRUE)
distinct_movies <- distinct_movies %>%
    select(-title.y)

#seperate genres into columns
distinct_movies <- distinct_movies %>%
    separate(genres, sep = ",", into = c("genre1", "genre2", "genre3"), fill='right')

write.csv(distinct_movies, "/Users/yuxuanhu/Desktop/ai_film_review/edit.csv", row.names=FALSE)

#reimport the file with full information
movie_table <- read.delim('/Users/yuxuanhu/Desktop/ai_film_review/new_version.tsv', header = TRUE)

#group by decades
scifi %>% 
    mutate(decade = 10 * (startYear %/% 10)) %>%
    group_by(decade) %>%
    sum(genre1 == 'Sci-Fi') + sum(genre2 == 'Sci-Fi') +sum(genre3=='Sci-Fi')

#converting genres to numbers
#Sci-Fi as 1
#Action as 2
#Drama as 3
#Adventure as 4
#Horror as 5
#Thriller as 6
#Fantasy as 7
#Mystery as 8
#Romance as 9
#Family as 10
#Comedy as 11
#Documentary as 12

genre_lookup <- c("Sci-Fi" = 1, "Action" = 2, "Drama" = 3, "Adventure" = 4, 
                  "Horror" = 5, "Thriller" = 6, "Fantasy" = 7, "Mystery" = 8,
                  "Romance" = 9, "Family" = 10, "Comedy" = 11, "Documentary" = 12,
                  'Animation' = 13, 'Sport' =14, 'Crime' =15, 'Music' =16)

# Convert genres to numbers
genre_number_table <- movie_table %>%
    mutate_at(vars(genre1, genre2, genre3), ~genre_lookup[.])

#Group by decade and count the number of film in each genre
counts <- genre_number_table %>%
    mutate(decade = 10 * (startYear %/% 10)) %>%
    gather(key = "genre_column", value = "genre_value", genre1, genre2, genre3) %>% 
    group_by(decade, genre_value) %>%
    summarize(n = n()) %>%
    arrange(decade, desc(n))

#convert the table column by genre
wide_df <- counts %>%
    spread(key = genre_value, value = n, fill = 0)

# Using the 'gather' function from the tidyr package to convert to long format
long_df <- wide_df %>%
    gather(key = "genre", value = "count", -decade)

#Group the new table by years
long_df <- long_df %>%
    group_by(decade)

ggplot(long_df, aes(x = factor(decade), y = count, fill = genre)) +
    geom_bar(stat="identity", position="dodge") +
    labs(title = "Number of Films per Genre by Decade",
         x = "Decade",
         y = "Number of Films",
         fill = "Genre") +
    theme_minimal()




# Reverse the lookup so it goes from number to genre name
reverse_lookup <- names(genre_lookup)[match(1:16, genre_lookup)]
# Convert the genre-related columns to genre names using reverse_lookup
cols_to_replace <- names(wide_df)[-1]  # Exclude the "decade" column
cols_to_replace <- as.integer(cols_to_replace)
new_names <- sapply(cols_to_replace, function(x) {
    return(reverse_lookup[x])
})

# Rename the columns in wide_df
names(wide_df) <- c("decade", new_names)
wide_df
write.csv(wide_df, "/Users/yuxuanhu/Desktop/ai_film_review/counts.csv", row.names=FALSE)



