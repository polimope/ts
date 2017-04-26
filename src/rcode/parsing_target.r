suppressMessages(library(dplyr))

dirpath = '/Users/esc2359/Desktop/time_series/data/'
filename = 'kospi200_inds.txt'
datafile = paste(dirpath, filename, sep='')

dat_raw = read.table(datafile, header = TRUE, sep="\t", quote = "", comment.char = "", fill = TRUE, stringsAsFactors = FALSE)
str(dat_raw)

rm_comma <- function(numc){
    res <- as.numeric(gsub(",","",numc))
    return(res)
}

make_date <- function(date){
    sp = strsplit(date, "/")[[1]]
    yy = sp[3]
    dd = sprintf("%02.0f", as.numeric(sp[2]))
    mm = sprintf("%02.0f", as.numeric(sp[1]))
    return(as.numeric(paste('20', yy, mm, dd, sep="")))
}

dat <- dat_raw %>%
    rowwise() %>%
    mutate(
        datetime = make_date(datetime)
        , iks200 = rm_comma(IKS200)
        , iks230 = rm_comma(IKS230)
        , iks231 = rm_comma(IKS231)
        , iks232 = rm_comma(IKS232)
        , iks233 = rm_comma(IKS233)
        , iks234 = rm_comma(IKS234)
        , iks235 = rm_comma(IKS235)
        , iks236 = rm_comma(IKS236)
        , iks237 = rm_comma(IKS237)
        , iks242 = rm_comma(IKS242)
    ) %>%
    dplyr::select(
        datetime
        , iks200
        , iks231
        , iks232
        , iks233
        , iks234
        , iks235
        , iks236
        , iks237
        , iks242
    ) %>%
    filter(
        datetime > 20100000
    )

str(dat)
dat %>% head()
