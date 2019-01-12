learn_hiragana = function(user_name, fresh_start = F) {
  df = readRDS('../resources/char.rds')
  user_file = paste0('../user_record/', user_name, '.rds')
  if(file.exists(user_file)) {
    user = readRDS(user_file)
    char_correct_count = user$char_correct_count[[nrow(user$record)]]
  } else {
    user = list()
    user$record = data.frame()
    user$char_correct_count = list()
    char_correct_count = rep(10, nrow(df))  # the probability of a char to occur in test is 1 / char_correct_count It will be updated by adding up the number of correctly recognized. So, the more I get it correctly, the less it will be tested (hopefully I won't forget it again)
  }
  if(isTRUE(fresh_start)) {
    char_correct_count = rep(10, nrow(df))
  }
  # images = list()
  # for(r in 1 : nrow(df)) {
  #   i = df$i[r]
  #   j = df$j[r]
  #   consonant = df$consonant[r]
  #   vowel = df$vowel[r]
  #   file = paste('../resources/pos', i, j, paste0(consonant, vowel, '.png'), sep = '_')
  #   img = png::readPNG(file, native = F)
  #   images[[r]] = img
  # }
  ntest = 0
  ncorrect = 0
  while(TRUE) {
    prob = 1 / char_correct_count
    prob = prob / sum(prob)
    r = sample(1 : nrow(df), prob = prob, size = 1)
    # grid::grid.raster(images[[r]])
    answer = readline(paste0('What is this [type `exit` to end] ?   ', df$hiragana[r], '   '))
    correct_answer = paste0(df$consonant[r], df$vowel[r])
    if(answer == 'exit') {
      if(ntest > 0) {
        message('The accuracy is ', ncorrect / ntest)
        user$record$date = as.character(user$record$date)
        user$record$ntest = as.numeric(as.character(user$record$ntest))
        user$record$ncorrect = as.numeric(as.character(user$record$ncorrect))
        user$record = rbind(user$record, c(date(), ntest, ncorrect))
        colnames(user$record) = c('date', 'ntest', 'ncorrect')
        user$char_correct_count[[length(user$char_correct_count) + 1]] = char_correct_count
        saveRDS(user, user_file)
      }
      break
    }
    ntest = ntest + 1
    if(answer == correct_answer) {
      ncorrect = ncorrect + 1
      char_correct_count[r] = char_correct_count[r] + 1
      message('Good')
    }
    else {
      message('Wrong!', ' It should be ', correct_answer)
    }
  }
}