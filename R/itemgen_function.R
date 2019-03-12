#' Automatic Item Generator for Quantitative Multiple-Choice Items
#'
#' @aliases itemgen
#'
#' @description \code{itemgen} function generates group of sibling items from a
#' parent item defined by user.
#'
#' @details User has to develop a short schema that contains formation of stem
#' (question text) along with response choices (formulae) of the parent item as an
#' input. Number-variables and character-variables must be specified in special
#' manner in the stem. \code{itemgen} function delivers the changes in the positions
#' of the variables in stem and calculates the response choices automatically by
#' taking members from the input vectors given by user in the schema. As a result,
#' several permutations of changes in the variables lead to generation of
#' new group of items.
#'
#' @author Shubh Patra and Bao Sheng Loe
#'
#' @param stem_text The stem of the parent item with specified number-variables and character-variables.
#' @param formulae A raw text that contains necessary formulae for the options (response choices) along
#' with necessary values or functions that help to calculate the numeric value for each option.
#' @param N A list of numeric input vector(s) for the number variable(s) in the stem.
#' @param C (Optional) A list of character input vector(s) for the character variable(s) in the stem if there is any.
#' @param options_affix (Optional) A list that consists of vectors with prefixes and suffixes (if there is any) of the
#' numeric values in the options along with any text that can be included as an option with the item.
#' @param ans_key (Optional) A text that indicates the correct response if it is not specified within formulae by '~'.
#' @param save.csv (Optional) A name text given by the user for the output .csv file, if user wants to save the newly
#' generated sibling items in working directory as a data.frame.
#'
#' @return This function returns a data frame that contains stem, options, answer
#' key etc. for all the generated sibling items within its rows to display in console
#' and within its columns if the argument 'save.csv' is not missing in the function.
#'
#' @importFrom stringr str_subset
#' @importFrom stringr str_flatten
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom Formula Formula
#' @importFrom stats model.frame
#' @importFrom stats as.formula
#' @importFrom utils write.csv
#'
#' @references
#' \url{https://www.researchgate.net/publication/239794821_The_Role_of_Item_Models_in_Automatic_Item_Generation}
#' Mark J. Gierl, Hollis Lai
#' The Role of Item Models in Automatic Item Generation (2011)
#' @references
#' \url{https://onlinelibrary.wiley.com/doi/epdf/10.1111/jedm.12166}
#' Susan E. Embretson,  Neal M. Kingston
#' Automatic Item Generation: A More Efficient Process for Developing Mathematics Achievement Items? (2018)
#'
#' @examples
#' stem_text <- "What is the sum of first [n1] [c1] ?"
#' n1 <- c(5, 8, 11, 14, 17)
#' c1 <- c("natural numbers", "non-zero positive integers")
#' N <- list(n1 = n1)
#' C <- list(c1 = c1)
#' formulae <- "Option_A ? 2*n1-1\nOption_B ? 3*n1-2\nOption_C ? n1*(n1+1)/2\nOption_D ? n1*(n1-1)/2\n"
#' options_affix <- list(Option_A = c("Sum value = ", ""), Option_B = c("Sum value = ", ""),
#' Option_C = c("Sum value = ", ""), Option_D = c("Sum value = ", ""))
#'
#' itemgen(stem_text = stem_text, formulae = formulae, N = N, C = C, ans_key = "Option_C",
#' options_affix = options_affix)
#'
#'
#' @note The formula model for each option must be distinct. Otherwise this
#' program will throw an error. If same numeric value needs to be produced in many
#' options, those models can be made different by adding 0 or multiplying 1
#' with the terms in the model.
#'
#' @note The model for the distractor options in formulae must be written using "?".
#' Correct response option can be written using EITHER "~" OR "?". In OR case correct
#' response must be indicated by the function argument "ans_key" to stop this \code{itemgen}
#' function throw an error.
#'
#' @export
itemgen <- function(stem_text = stem_text, formulae = formulae, N = N, C, options_affix, ans_key, save.csv) {
  ## Check for the default inputs
  if(missing(stem_text) || missing(formulae) || missing(N)) {
    stop("Inputs must be provided for all the default arguments: 'stem_text', 'formulae' and 'N'")
  }

  ## Construct a data frame for the available inputs
  (num_inputs <- expand.grid(N))
  if (!missing(C)) {
    char_inputs <- list()
    for (i in 1:length(C)) {
      char_inputs[[i]] <- rep(C[[i]], nrow(num_inputs))[1 : nrow(num_inputs)]
    }
    inputs_frame <- data.frame(char_inputs, num_inputs)
    names(inputs_frame) <- c(names(C), names(N))
  } else {
    inputs_frame <- data.frame(num_inputs)
    names(inputs_frame) <- names(N)
  }

  ## Extract formulae for correct answer and distractors
  (formulae_split <- trimws(unlist(strsplit(formulae, split = '\n'))))
  (response_split <- stringr::str_subset(formulae_split, '[~?]'))
  (support_values <- stringr::str_flatten(formulae_split[stringr::str_detect(formulae_split, '[?~]') == F], '\n'))
  if(length(support_values) != 0){eval(parse(text = support_values))}


  ## Extract correct answer key
  (correct_optn <- response_split[grep('~', response_split)])
  if(missing(ans_key) && length(correct_optn) == 1){
    correct_key <- trimws(gsub("\\~.*", "", correct_optn))
  } else if(!missing(ans_key) && length(correct_optn) == 0){
    correct_key <- ans_key
  } else {
    stop("Write formula for the correct option using single '~' symbol within formulae OR declare ans_key as function argument.")
  }


  ## Extract working function per option from formulae
  response_functions <- c()
  for (i in 1 : length(response_split)){
    response_functions[i] <- stringr::str_split(response_split, '[~?]')[[i]][2]
  }


  ## Extract Option names from the formulae
  option_names <- c()
  for (i in 1 : length(response_split)){
    option_names[i] <- trimws(stringr::str_split(response_split, '[~?]')[[i]][1])
  }


  ## Construct inputs model for the items and calculate the options values per item
  (response_functions <- trimws(response_functions))
  if (missing(C)) {
    ## Introduce a dummy column 'c1'
    (c1 <- sample(LETTERS, nrow(inputs_frame), replace = T))
    (inputs_frame <- data.frame(c1 = c1, inputs_frame))
    (model_formulae <- as.formula(stringr::str_flatten(c('c1 ~ .',  paste('I(' , response_functions , ')')), '|')))
    F1 <- Formula::Formula(model_formulae)
    inputs_model <- model.frame(F1, data = inputs_frame)
    inputs_model <- inputs_model[, -(which(colnames(inputs_model) == 'c1'))]
    inputs_frame <- data.frame(inputs_frame[, -(which(colnames(inputs_frame) == 'c1'))])
    names(inputs_frame) <- names(N)
  } else if (!missing(C)) {
    (model_formulae <- as.formula(stringr::str_flatten(c('c1 ~ .',  paste('I(' , response_functions , ')')), '|')))
    F1 <- Formula::Formula(model_formulae)
    inputs_model <- do.call(rbind,
                            lapply(seq_len(nrow(inputs_frame)),
                                          function(i) stats::model.frame(F1, data = inputs_frame[i,])))
  }

  ## Check whether individual formula model per option are  different to each other
  if (ncol(inputs_model) == ncol(inputs_frame)+length(option_names)) {
    colnames(inputs_model) <- c(colnames(inputs_frame), option_names)
  } else {
    stop('Formulae-models used for the options must be distinct per option and each option-formula must produce single numeric value.')
  }


  ## Attach the affixes to the options
  if (!missing(options_affix)) {
    if (all.equal(option_names, names(options_affix)[1 : length(option_names)]) == TRUE) {
      for (i in option_names) {
        inputs_model[[i]] <- paste0(options_affix[[i]][1], inputs_model[[i]], options_affix[[i]][2])
      }
    } else {
      stop("Affixes are NOT mapped to same option names specified within formulae")
    }
    ## Add options with only text if there is any
    if (length(options_affix) > length(option_names)) {
      for (i in names(options_affix[(length(option_names)+1) : length(options_affix)])) {
        inputs_model[[i]] <- rep(options_affix[[i]], nrow(inputs_model))
      }
    }
  }


  ## Break the stem text in words and extract variables
  (stem_words <- unlist(strsplit(stem_text, split = ' ')))
  (vrble_positions <- intersect(grep('\\[', stem_words), grep('\\]', stem_words)))
  (raw_vrbles <- unique(stem_words[vrble_positions]))


  ## Trace the positions in the stem text for replacements by the inputs
  replace_stem_positions <- list()
  for (j in 1 : ncol(inputs_frame)) {
    replace_stem_positions[[j]] <- grep('\\TRUE', stringr::str_detect(tolower(stem_words), colnames(inputs_frame)[j]))
  }


  ## Generate the stem clones by replacing variables by the inputs
  if (length(inputs_frame) == length(raw_vrbles)) {
    stemclones <- c()
    for (i in 1 : nrow(inputs_frame)) {
      for (j in 1 : length(replace_stem_positions)) {
        stem_words[replace_stem_positions[[j]]] <- as.character(inputs_frame[i, j])
      }
      stemclones[i] <- stringr::str_flatten(stem_words, ' ')
    }
  } else {
    stop("Declared variables in stem should be kept in the format of [.] without space inside
         and declared variables in stem must be similar to the input variables")
  }


  ## Sort out all the new items along with calculated options and correct answer key
  newitems <- data.frame(Stem = stemclones,
                         inputs_model[, -c(1 : length(inputs_frame))],
                         Answer_Key = rep(correct_key, length(stemclones)))
  rownames(newitems) <- paste0('[Q', 1 : nrow(newitems), ']')


  ## Save a .csv file of all the new items in working directory
  time_now <- gsub('\\:', '..', Sys.time())
  if (!missing(save.csv)){
    write.csv(newitems, paste(save.csv, time_now, '.csv'))
    message('Newly generated items have been saved in a .csv file in the working directory')
  }


  ## View the generated items
  return(t(newitems))

}
