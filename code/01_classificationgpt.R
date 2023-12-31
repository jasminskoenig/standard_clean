# LIBRARIES
library(tidyverse)
library(openai)
library(tidytext)
library(purrr)
library(furrr)
library(handcodeR)
library(caret)

# only for nlp part
# library(SnowballC)
# library(spacyr)
# library(yardstick)

# DATA ----

articles <- readRDS("data/standard.rds")

articles |>  
  separate_rows(article, sep = "\n") |> 
  # This deletes empty rows & Image Text
  filter(!str_detect(article, "Galerie|^$|\\*\\*\\*\\*") &
  # This filters out headlines as they usually don't contain punctuation       
           str_detect(article, "\\.|\\:|\\!")) |> 
  mutate(length_par = nchar(article),
         article = str_squish(article)) |> 
  filter(length_par > 50) |> 
  distinct(article, .keep_all = TRUE) |> 
  rowid_to_column("id_paragraph") -> 
  articles_paragraphs

articles_paragraphs |> 
  mutate(article_small = str_to_lower(article)) |> 
  filter(str_detect(article_small, "anwalt|richt|recht|vfgh|gerich|verfassung|justiz|wksta|behörde|institution|bia|ksta")) ->
  articles_paragraphs_judiciary

# HANDCODING PROCESS ----

# set.seed(085)
# articles_paragraphs_judiciary |> 
#   filter(str_detect(article, "Angriff|Attack|angegrif|attack")) |> 
#   sample_n(500) ->
#   articles_paragraphs_judiciary_handcoding
# 
# articles_paragraphs_judiciary |> 
#   filter(!id_paragraph %in% articles_paragraphs_judiciary_handcoding$id_paragraph) |> 
#   sample_n(1500) |> 
#   bind_rows(articles_paragraphs_judiciary_handcoding)->
#   articles_paragraphs_judiciary_handcoding
# 
# annotated <- handcode(articles_paragraphs_judiciary_handcoding$article,
#                       conflict = c("Staatsanwalt", "WKstA", "Gericht", "VfGH", "Justiz", "Richter", "Andere", "Keine"))
# detach(package:handcodeR)

# annotated |>
#   rename("article" = "texts") |> 
#   mutate(conflict_handcode = conflict,
#          conflict = if_else(conflict_handcode == "" | conflict_handcode == "Keine", 0, 1)) |> 
#   left_join(articles_paragraphs_judiciary_handcoding, by = "article") ->
#   annotated
# 
# saveRDS(handcoded, "data/annotated.rds")

# read in handcoded data
handcoded <- readRDS("data/annotated.rds")

handcoded |> 
  select(id_paragraph, conflict_correct = conflict) ->
  handcoded_select

articles_paragraphs_judiciary |> 
  left_join(handcoded_select, by = "id_paragraph") ->
  articles_paragraphs_judiciary_handcoded

saveRDS(articles_paragraphs_judiciary_handcoded, 
        "data/articles_paragraphs_judiciary_annotated.rds")

# GPT ----

## tried requests ----

prompt = "Es werden einzelne Absätze aus Artikeln der Zeitung der Standard gesendet. Bitte folgendes beantworten: 2) Wird in dem Paragraph erwähnt, dass Politiker die Justiz kritisieren oder kritisiert haben? Beispiele für Akteure sind zum Beispiel: Gerichte, der Verfassungsgerichtshof (VfGH), die WKStA, Richter oder Staatsanwälten. Wird ein Gesetz vor dem VfGH angefochten ist dies keine Kritik. Es ist aussließlich Kritik an Akteuren der Justiz von Interesse. Beantworten Sie diese Frage mit der Vorsilbe '2)'.  3) Wird in dem Paragraph erwähnt, dass Politiker die Justiz angreifen oder angegriffen haben? Beantworten Sie diese Frage mit der Vorsilbe '3)'.  4) Wird in dem Paragraph erwähnt, dass Politiker erwägen eine Gerichtsentscheidung nicht anzuerkennen?  Beantworten Sie diese Frage mit der Vorsilbe '4)'. 5) Wird in dem Paragraph ein vergangener Angriff auf die Justiz, Staatsanwaltschaft, WKSta, Gerichte oder den oder den VfGH diskutiert? Beantworten Sie diese Frage mit der Vorsilbe '5)' Beginne jede deiner vier Antworten jeweils mit der angegebenen Nummer und 'ja' oder 'nein'. Erläutere dann deine Antwort in einem Satz."

## api ----

chatGPT_API <- readLines(".secrets/openapikey.txt")

testfunction <- function(article){

  chatGPT_response <- tryCatch(

    # Try to retrieve answer for OpenAI API
    expr = {
      create_chat_completion(
        model = "gpt-3.5-turbo",
        temperature = 0,
        presence_penalty = -2,
        messages = list(
          list(
            "role" = "system",
            "content" = prompt
          ),
          list(
            "role" = "user",
            "content" = article
          )
        )
      )
      },
    # In case an error occurs while using API
    error = function(e){
      message(paste("For article there was an error."))
      return(NA)
    },
    # Notify about a possible warning while using API
    warning = function(w){
      message(paste("For article there was a warning."))
    }
  )

  response_text <- tryCatch(
    expr = {
    response_text <- chatGPT_response |>
      pluck('choices') |>
      pull(message.content)
    },
    error = function(e){
      response_text <- NA
    }
  )

  return(response_text)

}

# no_cores <- availableCores() - 1
# plan(multisession, workers = no_cores)
# 
# returned <- articles_paragraphs_judiciary_handcoded |> 
#   mutate(response_gpt = future_map_chr(article, testfunction))
# 
# plan(sequential)
# 
# saveRDS(returned, "data/paragraphs_gptclassified.rds")

returned <- readRDS("data/paragraphs_gptclassified.rds")

returned |> 
  mutate(conflict = if_else(str_detect(response_gpt, "(2\\)\\s?((y|Y)es|(j|J)a)|3\\)\\s?(y|Y)es|(j|J)a)"), 1, 0))  ->
  articles_paragraphs_judiciary_gpt

articles_paragraphs_judiciary_gpt |> 
  filter(!is.na(conflict) & !is.na(conflict_correct)) ->
  articles_paragraphs_judiciary_gpt_fil

confusionMatrix(as.factor(articles_paragraphs_judiciary_gpt_fil$conflict), 
                as.factor(articles_paragraphs_judiciary_gpt_fil$conflict_correct),
                mode = "everything", 
                positive="1")

articles_paragraphs_judiciary_gpt |> 
  select(id, conflict) |> 
  group_by(id) |> 
  summarize(conflict = sum(conflict)) |> 
  mutate(conflict = if_else(conflict > 0, 1, 0)) ->
  articles_gpt

articles |> 
  left_join(articles_gpt, by = "id") ->
  articles_gpt_final

articles_gpt_final |> 
  filter(date > "2015-01-01" & date < "2022-01-01" & conflict == 1) ->
  articles_control

# control coding of gpt
# annotated_art <- handcode(articles_control$article,
#                           conflict = c("yes"))

# annotated_art |> 
  # if handcoded yes, gpt correct, else false
#   mutate(conflict = if_else(conflict == "yes", 1, 0)) ->
#   annotated_art

# saveRDS(annotated_art, "data/articles_controlled.rds")

annotated_art <- readRDS("data/articles_controlled.rds")

annotated_art |>  
  left_join(articles_control, by = join_by("texts" == "article"))->
  annotated_art

annotated_art |> 
  rename("conflict_improved" = "conflict.x",
         "conflict" = "conflict.y") ->
  annotated_art
  
annotated_art |> 
  select(conflict_improved, id) ->
  annotated_art_sel

articles_gpt_final |> 
  left_join(annotated_art_sel, by = "id") |> 
  # Change conflict to improved handcoding if it is part of the handcoding dataset and something changed
  mutate(conflict = if_else(conflict != conflict_improved & date > (ymd("2015-01-01")) & date < ymd("2022-01-01"), 
                            conflict_improved, 
                            conflict)) ->
  articles_gpt_final_corrected

saveRDS(articles_gpt_final_corrected, "data/articles_final.rds")

# back to paragraph to renew conflation matrix
articles_gpt_final_corrected |> 
  separate_rows(article, sep = "\n") |> 
  # This deletes empty rows & Image Text
  filter(!str_detect(article, "Galerie|^$|\\*\\*\\*\\*") &
           # This filters out headlines as they usually don't contain punctuation       
           str_detect(article, "\\.|\\:|\\!")) |> 
  mutate(length_par = nchar(article),
         article = str_squish(article)) |> 
  filter(length_par > 50) |> 
  distinct(article, .keep_all = TRUE)  -> 
  articles_gpt_final_corrected_para

handcoded |> 
  select(article, conflict_correct = conflict) ->
  handcoded_select

articles_gpt_final_corrected_para |> 
  left_join(handcoded_select, by = "article") ->
  articles_gpt_final_corrected_para_handcoded

confusionMatrix(as.factor(articles_gpt_final_corrected_para_handcoded$conflict_improved), 
                as.factor(articles_gpt_final_corrected_para_handcoded$conflict_correct),
                mode = "everything", 
                positive="1")

articles_gpt_final_corrected_para_handcoded |>  count(conflict)
