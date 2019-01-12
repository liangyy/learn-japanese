library(stringi)

image = png::readPNG('../resources/gojyuonn.PNG', native = F)

l = dim(image)[1]
w = dim(image)[2]
per_l = (l - 5) / 11
per_w = ((w - 1) / 2 - 14) / 5
get_char = function(i, j) {
  start_l = 1 + per_l * (i - 1)
  end_l = per_l * i
  start_w = 1 + per_w * (j - 1)
  end_w = per_w * j
  return(image[ start_l : end_l, start_w : end_w, ])
}
remove_part = function(mat) {
  l = dim(mat)[1]
  w = dim(mat)[2]
  r_start = w - 10
  r_end = l - round((l - w) / 3)
  mat = mat[-( r_start : r_end), , ]
}
pos_list = data.frame(i = rep(1 : 11, each = 5), j = rep(1 : 5, 11))
white_list = data.frame(i = c(8, 8, 10, 10, 10, 11, 11, 11, 11), j = c(2, 4, 2, 3, 4, 1, 2, 3, 4))
pos_list = pos_list[ ! paste(pos_list$i, pos_list$j) %in% paste(white_list$i, white_list$j), ]

map_consonant = data.frame(i = 1 : 11, consonant = c('', 'k', 's', 't', 'n', 'h', 'm', 'y', 'r', 'w', ''))
map_vowel = data.frame(j = 1 : 5, vowel = c('a', 'i', 'u', 'e', 'o'))
pos_list$consonant = map_consonant$consonant[ match(pos_list$i, map_consonant$i) ]
pos_list$vowel = map_vowel$vowel[ match(pos_list$j, map_vowel$j) ]
pos_list$consonant = as.character(pos_list$consonant)
pos_list$vowel = as.character(pos_list$vowel)
pos_list$consonant[pos_list$i == 3 & pos_list$j == 2] = 'sh'
pos_list$consonant[pos_list$i == 4 & pos_list$j == 2] = 'ch'
pos_list$consonant[pos_list$i == 4 & pos_list$j == 3] = 'ts'
pos_list$consonant[pos_list$i == 6 & pos_list$j == 3] = 'f'
pos_list$consonant[pos_list$i == 10 & pos_list$j == 5] = ''
pos_list$consonant[pos_list$i == 11 & pos_list$j == 5] = 'n'
pos_list$vowel[pos_list$i == 11 & pos_list$j == 5] = ''

for(r in 1 : nrow(pos_list)) {
  i = pos_list$i[r]
  j = pos_list$j[r]
  consonant = pos_list$consonant[r]
  vowel = pos_list$vowel[r]
  file = paste('../resources/pos', i, j, paste0(consonant, vowel, '.png'), sep = '_')
  png(file)
  grid::grid.raster(remove_part(get_char(i, j)))
  dev.off()
}

hiragana_unicode_range = paste0(rep(304 : 309, each = 16), rep(c(0 : 9, 'A', 'B', 'C', 'D', 'E', 'F'), 6))
idx = c(seq(1 : 18) * 2, c(27, 29, 31 : 36, 39, 42, 45, 48, 51 : 56, 58, 60, 62, 63 : 67, 71, 72) + 12)
pos_list$hiragana = stri_unescape_unicode(paste0('\\u', hiragana_unicode_range[ idx ]))
saveRDS(pos_list, '../resources/char.rds')
