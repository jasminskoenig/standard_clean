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

# set.seed(123)
# articles |> 
#   sample_n(40) ->
#   randomsample
# 
# articles |> 
#   filter(id %in% c(592,882,1463,1484,4026,4057)) |> 
#   rbind(randomsample) ->
#   test1
# 
# test1 |> 
#   select(links, id) ->
#   test_tocsv1
# 
# write_delim(test_tocsv, "data/testset.txt")
# lapply(test_tocsv1$links, browseURL)
# 
# set.seed(224)
# 
# articles |> 
#   sample_n(80) ->
#   randomsample
# 
# randomsample |> 
#   select(links) ->
#   test_tocsv2
# lapply(test_tocsv2$links, browseURL)
# 
# write_delim(test_tocsv2, "data/testset2.txt")
# 
# articles |> 
#   filter(str_detect(article, "Angriff|Attack|angegrif|attack")) |> 
#   sample_n(100) |> 
#   select(links) ->
#   test_tocsv3
# lapply(test_tocsv3$links, browseURL)
# 
# write_delim(test_tocsv3, "data/testset3.txt")


testset1_coded <- read.delim("data/testset_coded.txt") 
testset2_coded <- read.delim("data/testset2_coded.txt") 
testset3_coded <- read.delim("data/testset3_coded.txt") 

testset_coded <- rbind(testset1_coded, testset2_coded, testset3_coded)

articles |> 
  left_join(testset_coded, by = "links") ->
  articles_coded

# articles_coded |> 
#   group_by(code) %>% 
#   slice_sample(n=200) -> sample

articles_coded |>  
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
  filter(code == 1) -> 
  articles_paragraphs_coded1

articles_paragraphs |> 
  mutate(article_small = str_to_lower(article)) |> 
  filter(str_detect(article_small, "anwalt|richt|recht|vfgh|gerich|verfassung|justiz|wksta|behörde|institution|bia|ksta")) ->
  articles_paragraphs_judiciary

set.seed(085)
articles_paragraphs_judiciary |> 
  filter(str_detect(article, "Angriff|Attack|angegrif|attack")) |> 
  sample_n(200) ->
  articles_paragraphs_judiciary_handcoding

articles_paragraphs_judiciary |> 
  filter(!id_paragraph %in% articles_paragraphs_judiciary_handcoding$id_paragraph) |> 
  sample_n(400) |> 
  bind_rows(articles_paragraphs_judiciary_handcoding)->
  articles_paragraphs_judiciary_handcoding

# annotated <- handcode(articles_paragraphs_judiciary_handcoding$article,
#                       conflict = c("yes", "no"),
#                       sortout = c("sortout", "no"))
# 
# handcoded <- annotated |> 
#   bind_cols(articles_paragraphs_judiciary_handcoding) 

# saveRDS(handcoded, "data/articles_paragraphs_judiciary_handcoding_annotated.rds")

handcoded <- readRDS("data/articles_paragraphs_judiciary_handcoding_annotated.rds")

articles_paragraphs_judiciary |> 
  anti_join(handcoded, by = "id_paragraph") |> 
  bind_rows(handcoded) |> 
  mutate(coded = case_when(
    conflict == "yes" ~ 1,
    conflict == "no" ~ 0,
    TRUE ~ NA
  )) ->
  articles_paragraphs_judiciary_partlycoded

# We now learned that 100 is a better cut off to filter out headlines etc.
articles_paragraphs_judiciary_partlycoded |> 
  filter(length_par >= 100) ->
  articles_paragraphs_judiciary_partlycoded

saveRDS(articles_paragraphs_judiciary_partlycoded, "data/articles_paragraphs_judiciary_annotated.rds")

articles_coded |> 
  mutate(article = str_squish(article)) |> 
  mutate(art_length = nchar(article)) ->
  articles_clean


# GPT ----

## tried requests ----

systemconflict_ger = "2) Wird in dem Artikel erwähnt, dass Politiker die Justiz kritisieren?  Wir interessieren uns nur für Kritik an Akteuren aus der Justiz. Beispiele für Akteure sind zum Beispiel: Gerichte, der Verfassungsgerichtshof (VfGH), die WKStA, Richter oder Staatsanwälten. Beantworten Sie diese Frage mit der Vorsilbe '2)'.  3) Wird in dem Artikel erwähnt, dass Politiker die Justiz angreifen?  Beantworten Sie diese Frage mit der Vorsilbe '3)'.  4) Wird in dem Artikel erwähnt, dass Politiker erwägen eine Gerichtsentscheidung nicht anzuerkennen?  Beantworten Sie diese Frage mit der Vorsilbe '4)'."

systemconflict_ger = "2) Wird in dem Artikel erwähnt, dass Politiker die Justiz kritisieren?  Wir interessieren uns nur für Kritik an Akteuren aus der Justiz. Beispiele für Akteure sind zum Beispiel: Gerichte, der Verfassungsgerichtshof (VfGH), die WKStA, Richter oder Staatsanwälten. Kritik von der Justiz an der Politik ist nicht von Interesse. Beantworten Sie diese Frage mit der Vorsilbe '2)'.  3) Wird in dem Artikel erwähnt, dass Politiker die Justiz angreifen?  Beantworten Sie diese Frage mit der Vorsilbe '3)'.  4) Wird in dem Artikel erwähnt, dass Politiker erwägen eine Gerichtsentscheidung nicht anzuerkennen?  Beantworten Sie diese Frage mit der Vorsilbe '4)'."

systemconflict_ger = "2) Wird in dem Artikel erwähnt, dass Politiker die Justiz kritisieren?  Wir interessieren uns nur für Kritik an Akteuren aus der Justiz. Beispiele für Akteure sind zum Beispiel: Gerichte, der Verfassungsgerichtshof (VfGH), die WKStA, Richter oder Staatsanwälten. Wird ein Gesetz vor dem VfGH angefochten ist dies keine Kritik. Beantworten Sie diese Frage mit der Vorsilbe '2)'.  3) Wird in dem Artikel erwähnt, dass Politiker die Justiz angreifen?  Beantworten Sie diese Frage mit der Vorsilbe '3)'.  4) Wird in dem Artikel erwähnt, dass Politiker erwägen eine Gerichtsentscheidung nicht anzuerkennen?  Beantworten Sie diese Frage mit der Vorsilbe '4)'."

systemconflict_ger = "You will be provided paragraphs from articles by the Austrian Newspaper Der Standard in German. Please answer the following: 2) Wird in dem Artikel erwähnt, dass Politiker die Justiz kritisieren oder kritisiert haben?  Wir interessieren uns nur für Kritik an Akteuren aus der Justiz. Beispiele für Akteure sind zum Beispiel: Gerichte, der Verfassungsgerichtshof (VfGH), die WKStA, Richter oder Staatsanwälten. Wird ein Gesetz vor dem VfGH angefochten ist dies keine Kritik. Beantworten Sie diese Frage mit der Vorsilbe '2)'.  3) Wird in dem Artikel erwähnt, dass Politiker die Justiz angreifen oder angegriffen haben? Beantworten Sie diese Frage mit der Vorsilbe '3)'.  4) Wird in dem Artikel erwähnt, dass Politiker erwägen eine Gerichtsentscheidung nicht anzuerkennen?  Beantworten Sie diese Frage mit der Vorsilbe '4)'. 5) Wird in dem Artikel ein vergangener Angriff auf die Justiz, Staatsanwaltschaft, WKSta, Gerichte oder den oder den VfGH diskutiert? Beantworten Sie diese Frage mit der Vorsilbe '5)'"

systemconflict_ger = "Es werden einzelne Absätze aus Artikeln der Zeitung der Standard gesendet. Bitte folgendes beantworten: 2) Wird in dem Paragraph erwähnt, dass Politiker die Justiz kritisieren oder kritisiert haben? Beispiele für Akteure sind zum Beispiel: Gerichte, der Verfassungsgerichtshof (VfGH), die WKStA, Richter oder Staatsanwälten. Wird ein Gesetz vor dem VfGH angefochten ist dies keine Kritik. Es ist aussließlich Kritik an Akteuren der Justiz von Interesse. Beantworten Sie diese Frage mit der Vorsilbe '2)'.  3) Wird in dem Paragraph erwähnt, dass Politiker die Justiz angreifen oder angegriffen haben? Beantworten Sie diese Frage mit der Vorsilbe '3)'.  4) Wird in dem Paragraph erwähnt, dass Politiker erwägen eine Gerichtsentscheidung nicht anzuerkennen?  Beantworten Sie diese Frage mit der Vorsilbe '4)'. 5) Wird in dem Paragraph ein vergangener Angriff auf die Justiz, Staatsanwaltschaft, WKSta, Gerichte oder den oder den VfGH diskutiert? Beantworten Sie diese Frage mit der Vorsilbe '5)' Beginne jede deiner vier Antworten jeweils mit der angegebenen Nummer und 'ja' oder 'nein'. Erläutere dann deine Antwort in einem Satz."


systemrequest = "You will be provided paragraphs from articles by the Austrian Newspaper Der Standard in German. All of these mention the justice system or the Austrian Constitutional Court. We want you to take multiple steps for each of these articles:
    1) Summarize the paragraph in one sentences.
    2) Answer whether the article reports about criticism of or by the judiciary? Only answer with yes, no or unsure.
    3) If 2) is answered with yes: Who is being critizised? Is that actor part of the justice system or politics? If 3) is answered with no, answer with 'does not apply'
    4) If 2) is answered with yes: Who is voicing the criticism? Is this actor part of the judicial system or of the legislative/executive? If 3) is answered with no, answer with 'does not apply'"

systemconflict = "You will be provided one article per prompt by the Austrian Newspaper Der Standard. The article is written in German. Each prompt only contains one article. All of these mention the justice system (or actors in the justice system such as Staatsanwalt, WKSta, Richter or Gericht) or the Austrian Constitutional Court (called VfGH or Verfassungsgerichtshof). We want you to take multiple steps for each of these articles:
    1) Summarize the article in 3 sentences.  Answer this question with the prefix '1)'.
    2) Does the article mention that politicians critisize the judiciary or the VfGH? Answer this question with the prefix '2)'.
    3) Does the article mention that politicians attack the judiciary or the VfGH?  Answer this question with the prefix '3)'
    4) If 2) or 3) is answered with yes: Who is criticized or attacked by whom, name the actors. Remember that we are not interested in conflict between politicians, only between politicians and the judiciary. Answer this question with the prefix '4)'"


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
            "content" = systemconflict_ger
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

no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

returned <- articles_paragraphs_judiciary |> 
  mutate(response_gpt = future_map_chr(article, testfunction))

plan(sequential)

saveRDS(returned, "data/paragraphs_gptclassified.rds")

returned |> 
  mutate(conflict = if_else(str_detect(response_gpt, "(2\\)\\s?((y|Y)es|(j|J)a)|3\\)\\s?(y|Y)es|(j|J)a)"), 1, 0)) |> 
  mutate(correct = if_else(code == conflict, 1, 0)) ->
  articles_paragraphs_judiciary_gpt

# check quality of coding
articles_paragraphs_judiciary_gpt |> 
  select(id, conflict) |> 
  group_by(id) |> 
  summarize(conflict = sum(conflict),
            .groups = "drop") |> 
  left_join(articles_clean, by = "id") |> 
  mutate(conflict = as.factor(if_else(conflict > 0, 1, 0)),
         correct = as.factor(if_else(code == conflict, 1, 0)),
         code = as.factor(code)) |> 
  filter(!is.na(correct) & !is.na(conflict)) ->
  articles_paragraphs_judiciary_gpt_forcontrol

confusionMatrix(articles_paragraphs_judiciary_gpt_forcontrol$conflict,
                articles_paragraphs_judiciary_gpt_forcontrol$code)

articles_paragraphs_judiciary_gpt |> 
  filter(conflict == 1) ->
  articles_paragraphs_judiciary_gpt_conflict

controlled3 <- handcode(data = uncoded$texts, 
                       start = "all_empty",
                       conflict = c("Staatsanwalt", "WKstA", "Gericht", "VfGH", "Justiz", "Richter", "Andere", "Keine"))


controlled2 |> 
  filter(conflict == "") ->
  uncoded

annotated |> 
  rbind(controlled) |> 
  rbind(controlled2) |> 
  rbind(controlled3) |> 
  filter(conflict != "") |> 
  rename("conflict_handcode" = "conflict") ->
  annotated_complete

articles_paragraphs_judiciary_gpt_conflict |> 
  left_join(annotated_complete, by = c("article" = "texts")) ->
  articles_paragraphs_judiciary_gpt_conflict_corrected

saveRDS(articles_paragraphs_judiciary_gpt_conflict_corrected, "data/annotated.rds")

articles_paragraphs_judiciary_gpt |> 
  anti_join(articles_paragraphs_judiciary_gpt_conflict, by = "id_paragraph") |> 
  mutate(conflict_handcode = NA) |> 
  rbind(articles_paragraphs_judiciary_gpt_conflict_corrected) |> 
  mutate(conflict_handcode = if_else(conflict_handcode == "Keine" | conflict_handcode == "Andere", 0, 1),
         conflict = if_else(!is.na(conflict_handcode), conflict_handcode, conflict),
         conflict_vfgh = if_else(conflict_handcode == "VfGH", 1, 0)) ->
  articles_paragraphs_judiciary_gpt_corrected

###XXX Conflict VFGH not working yet
articles_paragraphs_judiciary_gpt_corrected |> 
  select(id, conflict) |> 
  group_by(id) |> 
  summarize(conflict = sum(conflict),
            .groups = "drop") |> 
  left_join(articles_clean, by = "id") |> 
  mutate(conflict = if_else(conflict > 0, 1, 0),
         conflict_vfgh = if_else(conflict_vfgh > 0, 1, 0),
         correct = if_else(code == conflict, 1, 0)) ->
  articles_gpt

saveRDS(articles_gpt, "data/articles_gptclassified.rds")

articles_gpt |> 
  left_join(articles_gpt_old, by = "id") |> 
  mutate(diff = if_else(conflict.x == conflict.y, 0, 1)) |>
  relocate(conflict.x, conflict.y, .before = diff) |>  
  View()

articles_gpt |> 
  count(conflict)

articles_gpt_old |> 
  count(conflict)

# systemconflict_ger_bigtrial|> 
#   filter(!is.na(code) & !is.na(conflict)) |> 
#   mutate_at(c("code", "conflict"), ~ as.factor(.x)) |> 
#   conf_mat(conflict, code) 
# 
# articles_paragraphs_judiciary_gpt_art2 |> 
#   arrange(correct, conflict) |>  
#   relocate(article, .before = conflict) |>  
#   group_by(conflict) |>  count() -> articles_paragraphs_judiciary_gpt_art_count2
# 
# articles_paragraphs_judiciary_gpt_art2 |> 
#   filter(correct == 0 & conflict == 1) |> 
#   pull(id) ->
#   wrongs_ids 
# 
# articles_paragraphs_judiciary_gpt_art2 |> 
#   filter(id %in% wrongs_ids) |> 
#   arrange(correct, conflict) |>  
#   relocate(article, .before = conflict) |>
#   View()
# 
# spacy_initialize(model = "de_core_news_sm")
# 
# # FUTURE: From here on function
# 
# articles_paragraphs_judiciary_gpt2 |> 
#   mutate(response_gpt_short = str_extract_all(response_gpt, ".*(j|J)a.*")) |>
#   unnest(response_gpt_short) |> 
#   filter(!is.na(response_gpt_short)) |> 
#   rowid_to_column(var = "doc_id") |> 
#   rename("text" = "response_gpt_short") |> 
#   filter(!str_detect(text, "^5\\)")) ->
#   textsfornlp2
# 
# spacy_parse(textsfornlp2, dependency = TRUE, lemma = FALSE, pos = TRUE) |> 
#   mutate(dep_rel = if_else(dep_rel == "nb", "oa", dep_rel)) |> 
#   filter(dep_rel == "sb" | dep_rel == "oa") |> 
#   group_by(doc_id) |> 
#   mutate(correct_grammar = if_else(any(dep_rel == "sb") & any(dep_rel == "oa"), 1, 0)) |> 
#   filter(correct_grammar == 1) |> 
#   filter(!str_detect(token, "^\\d$") & pos != "PRON") |>
#   mutate(tokens_stemmed =  wordStem(token, language = "german")) |> 
#   mutate(tokens_stemmed = str_to_lower(tokens_stemmed)) |> 
#   mutate(token_type = case_when(
#     str_detect(tokens_stemmed, "minist|generalsekretar|justizsprecher|politik|pilz|haid|pilnacek|polit|landeshauptmann|rauch-kallat") ~ "Politik",
#     str_detect(tokens_stemmed, "fpö|övp|spö|grüne|bzö|neos") ~ "Politik",
#     str_detect(tokens_stemmed, "anwalt|richt|vfgh|gerich|verfassung|^justiz$|wksta|rechtsstaat") ~ "Justiz"
#   )) |>  
#   ungroup() |> 
#   group_by(doc_id, dep_rel) |> 
#   summarise(politicians = as.character(any(token_type == "Politik")),
#             judiciary = as.character(any(token_type == "Justiz")),
#             .groups = "drop") |> 
#   pivot_wider(id_cols = doc_id, names_from = dep_rel, values_from = c(politicians, judiciary)) |> 
#   mutate(politicians_attack = if_else(politicians_sb == TRUE & !is.na(politicians_sb), 1, 0),
#          judiciary_attacked = if_else(politicians_oa != TRUE | is.na(politicians_oa), 1, 0)) |>
#   select(doc_id, politicians_attack, judiciary_attacked) |> 
#   ungroup() ->
#   response_corrected2
# 
# textsfornlp2 |> 
#   mutate(doc_id = as.character(doc_id)) |> 
#   left_join(response_corrected2, 
#             by = "doc_id") |> 
#   mutate(conflict_corrected = if_else(politicians_attack == 0, 
#                                       0, 
#                                       conflict),
#          text_low = str_to_lower(text),
#          text_stemmed = wordStem(text_low, 
#                                  language = "german"), 
#          conflict_corrected2 = if_else(!str_detect(text_low, "anwalt|richt|vfgh|gerich|verfassung|justiz|wksta|rechtsstaat|institution"), 
#                                        0, 
#                                        conflict_corrected)) ->
#   textsfornlp_corrected2
# 
# textsfornlp_corrected2 |> 
#   group_by(id) |> 
#   summarize(conflict_corrected = sum(conflict_corrected2),
#             .groups = "drop") |> 
#   mutate(conflict_corrected = if_else(conflict_corrected > 0, 1, conflict_corrected)) |> 
#   ungroup() ->
#   textsfornlp_corrected2
# 
# articles_paragraphs_judiciary_gpt2 |> 
#   left_join(textsfornlp_corrected2, by = "id") |> 
#   mutate(conflict = if_else(conflict_corrected != conflict & !is.na(conflict_corrected), 
#                             conflict_corrected, 
#                             conflict)) |> 
#   select(id, conflict) |> 
#   group_by(id) |> 
#   summarize(conflict = sum(conflict, na.rm = TRUE),
#             .groups = "drop") |> 
#   mutate(conflict = if_else(conflict > 0, 1, conflict)) ->
#   systemconflict_ger_para_longart_corrected2
# 
# articles_paragraphs_judiciary_gpt_art2 |> 
#   full_join(systemconflict_ger_para_longart_corrected2, 
#             by = "id") |>  
#   arrange(correct, code.x) |> 
#   View()
#   
# spacy_finalize()
