
##loadingrequired packages

if (!require("pacman")) install.packages("pacman")

pacman::p_load(parser, magrittr)


#text
txt <- c(
  "Really, I like chocolate because it is good. It smells great.",
  "Robots are rather evil and most are devoid of decency.",
  "He is my friend.",
  "Clifford the big red dog ate my lunch.",
  "Professor Johns can not teach",
  "",
  NA
)

#create annotator
if(!exists('parse_ann')) {
  parse_ann <- parse_annotator()
}

#parsing text
x <- parser(txt, parse_ann)


#plotting graph of parser
plot(x[[2]])

par(
  mfrow = c(3, 2),
  oma = c(5,4,0,0) + 0.1,
  mar = c(0,0,1,1) + 0.1
)
invisible(lapply(x[1:5], plot))


###get subject, verb and direct object

#subject

get_phrase_type(x, "NP") %>%
  take() %>%
  get_leaves()

#predictive verb
get_phrase_type_regex(x, "VP") %>%
  take() %>%
  get_phrase_type_regex("(VB|MD)") %>%
  take() %>%
  get_leaves()

#direct object
get_phrase_type_regex(x, "VP") %>%
  take() %>%
  get_phrase_type_regex("NP") %>%
  take() %>%
  get_leaves()
