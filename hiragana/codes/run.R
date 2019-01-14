library(optparse)

option_list = list(
  make_option(c("-u", "--user"), type="character", default=NULL,
              help="name of user", metavar="character"),
  make_option(c("-f", "--fresh_start"), type="character", default=NULL,
              help="whether start from original char_correct_count, if yes use `Y`", metavar="character")
);

args_parser = OptionParser(option_list=option_list);
opt = parse_args(args_parser);


source('codes/learn_func.R')
learn_hiragana(opt$user, fresh_start = opt$fresh_start)
