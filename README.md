# zno2016utils

> mkdir archive
> cd archive
> wget stat.testportal.com.ua/uploads/OpenDataZNO2016.7z
> 7z x OpenDataZNO2016.7z
> iconv -f cp1251 -t utf8 OpenData2016.csv > ../zno_2016_utf8.csv
> cd ../

> Rscript preprocess.R