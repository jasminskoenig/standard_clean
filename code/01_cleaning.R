library(arrow)
library(tidyverse)
library(lubridate)
library(tidytext)

articles <- arrow::read_feather('standard_project/justiz1.feather') |> 
  rbind(arrow::read_feather('standard_project/justiz2.feather')) |> 
  rbind(arrow::read_feather('standard_project/justiz3.feather')) |> 
  rbind(arrow::read_feather('standard_project/justiz4.feather')) |> 
  distinct(article, .keep_all = TRUE) |> 
  rownames_to_column("id")

articles |> 
  mutate(time = str_replace(time, "Jänner", "Januar"),
         date = dmy_hm(time)) |> 
  mutate(article_id = row_number()) |> 
  mutate(comments = as.integer(parse_number(commentno))) |> 
  select(-time, -commentno) ->
  articles

articles |> 
  mutate(article = str_remove_all(article, "BILD NICHT MEHR VERFÜGBAR."),
         article = str_remove_all(article, "(w*\\-|)(Foto|FOTO)\\: .*\\n")) ->
  articles

write_rds(articles, "data/standard.rds")

articles |> 
  sample_n(5) ->
  test

write_rds(articles, "data/testingpy.rds")

print("Hi from R!")
