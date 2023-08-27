library('tidyverse')

options(width=256)

accs = c('CZEG','CZQM','CZQX','CZUL',   # Area control centers (not actual aerodromes)
         'CZVR','CZWG','CZYZ')

aerodrome_x1 = 337                      # Right-hand side of aerodrome code in header
aerodrome_y1 = 54                       # Bottom side of aerodrome code in header

split_y = mean(c(54.1,67.5))            # Split point between header and body
split_x = mean(c(70.5,77.79))           # Split point between labels and text

picture_x0 = 201                        # Left-hand edge for all upper-right pictures
picture_x1 = 338                        # Right-hand edge for all upper-right pictures

page_0 = 3                              # First page to consider
page_1 = 492                            # Last page to consider


## Load the pdftotext -tsv output dropping pdftext classifications (has errors) and compute word bounding boxes.
##
##   page_num    y0    y1    x0    x1 width height text
##      <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>
## 1        1  47.1  78.6  43.8  87.8 44.0   31.4  CFS
## 2        1  76.9  88.2  50.3  81.5 31.2   11.3  DIGITAL
## 3        1  86.9  98.2  49.4  82.2 32.7   11.3  EDITION
## 4        1  52.5  62.6 117   154.  37.5   10.1  CAUTION:
## 5        1  52.7  62.6 156.  170.  14.1    9.87 THE

input = read_tsv('ecfs_04_en.txt', quote='') %>%
    mutate(x0=left,x1=left+width,y0=top,y1=top+height) %>%
    filter(between(page_num,page_0,page_1), conf==100) %>%
    select(page_num,y0,y1,x0,x1,width,height,text)


## Find all aerodrome pages by locating C??? this isn't a ACC within 12 pt of where we expect it).
##
##   page_num text
##      <dbl> <chr>
## 1        4 CNS4
## 2        5 CAP2
## 3        6 CPJ2
## 4        7 CPZ2
## 5        8 CNY4

aerodromes = input %>%
    filter(str_detect(text,'^C[0-9A-Z]{3}$') & !(text %in% accs),
           near(x1,aerodrome_x1,12) & near(y1,aerodrome_y1,12)) %>%
    select(page_num, aerodrome=text) %>%
    mutate(aerodrome=fct(aerodrome))


## Select just aerodrome pages and replace page_num with aerodrome and aerodrome relative page.
##
##   aerodrome    y0    y1    x0    x1 width height text        page
##   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>      <dbl>
## 1 CPE2       7.11  15.7  26.8  51.2 24.4    8.58 CANADA         1
## 2 CPE2       7.11  15.7  53.4  74.0 20.7    8.58 FLIGHT         1
## 3 CPE2       7.11  15.7  76.2 116.  39.7    8.58 SUPPLEMENT     1
## 4 CPE2       7.58  16.2 117.  119.   2.08   8.58 /              1
## 5 CPE2       7.58  16.2 121.  133.  12.0    8.58 GPH            1

input = inner_join(aerodromes,input) %>%
    group_by(aerodrome) %>%
    mutate(page=page_num-min(page_num)+1,
           page_num=NULL)


## Drop any really large watermark and very small garbage and classify by page section.
##
## Arrange within locations by y1 and then x0.
##
##   aerodrome    y0    y1    x0    x1 width height text       page location
##   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>     <dbl> <fct>   
## 1 CPE2       9.41  15.6  276.  287. 11.1    6.22 2021          1 header  
## 2 CPE2       9.43  15.6  151.  169. 17.3    6.22 Effective     1 header  
## 3 CPE2       9.43  15.6  170.  184. 13.7    6.22 0901Z         1 header  
## 4 CPE2       9.43  15.6  185.  191.  5.47   6.22 31            1 header  
## 5 CPE2       9.43  15.6  193.  214. 21.3    6.22 December      1 header  

input = input %>%
    filter(between(width*height/str_length(text), 3*3, 72*72/4)) %>%
    mutate(location=fct(case_when(y1 < split_y ~ 'header',
                                  x1 < split_x ~ 'label',
                                  TRUE         ~ 'body'))) %>%
    arrange(aerodrome,page,location,y1,x0)


## Compute lines by looking for > 4 pt difference in y1 location and then chunks by looking for > 4x height
## difference in x space betwwen them.
##
## Arrange within locations by line and then x0.
##
##   aerodrome    y0    y1    x0    x1 width height text        page location  line chunk
##   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>      <dbl> <fct>    <int> <int>
## 1 CPE2       7.11  15.7  26.8  51.2 24.4    8.58 CANADA         1 header       0     0
## 2 CPE2       7.11  15.7  53.4  74.0 20.7    8.58 FLIGHT         1 header       0     0
## 3 CPE2       7.11  15.7  76.2 116.  39.7    8.58 SUPPLEMENT     1 header       0     0
## 4 CPE2       7.58  16.2 117.  119.   2.08   8.58 /              1 header       0     0
## 5 CPE2       7.58  16.2 121.  133.  12.0    8.58 GPH            1 header       0     0

input = input %>%
    group_by(aerodrome,page,location) %>%
    mutate(line = cumsum(replace_na(y1-lag(y1) > 4,TRUE))) %>%
    arrange(aerodrome,page,location,line,x0) %>%
    group_by(aerodrome,page,location,line) %>%
    mutate(chunk = cumsum(replace_na(x0-lag(x1) > 4*height,TRUE)))


## Combine text in chunks. Keep track of first width to aid in determining line wrap later.
##
## Arrange within locations by line and then chunk as not sure if summarize guarnteed to preserves prior arrange.
##
##   aerodrome  page location  line chunk text                                                                                           wrap    x0    x1     y0    y1
##   <fct>     <dbl> <fct>    <int> <int> <chr>                                                                                         <dbl> <dbl> <dbl>  <dbl> <dbl>
## 1 CPE2          1 header       0     0 CANADA FLIGHT SUPPLEMENT / GPH 205 Effective 0901Z 31 December 2020 to 0901Z 25 February 2021  24.4  26.8 287.    7.11  16.2
## 2 CPE2          1 header       1     0 ONTARIO                                                                                        35.3  27.1  62.5  21.3   33.1
## 3 CPE2          1 header       1     1 AERODROME / FACILITY DIRECTORY                                                                 41.4 220.  336.   24.2   32.7
## 4 CPE2          1 header       2     0 ONCPE2                                                                                         16.8  27.4  44.2  35.5   41.2
## 5 CPE2          1 header       3     0 AJAX (PICKERING GEN HOSP) ON (Heli)                                                            20.8  27.4 160.   43.6   54.4

input = input %>%
    group_by(aerodrome,page,location,line,chunk) %>%
    summarize(text=str_flatten(text,' '),
              wrap=first(width),
              x0=min(x0),x1=max(x1),
              y0=min(y0),y1=max(y1)) %>%
    arrange(aerodrome,page,location,line,chunk)


## Classify label depths as 1 = left aligned and 2 = right aligned.
##
##   aerodrome  page location  line chunk text       wrap    x0    x1    y0    y1 level
##   <fct>     <dbl> <fct>    <int> <int> <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <int>
## 1 CPE2          1 label        0     0 REF        14.0  27.4  41.4  59.7  67.5     0
## 2 CPE2          1 label        1     0 OPR        15.2  27.4  42.6  88.9  96.7     0
## 3 CPE2          1 label        2     0 FLT PLN    12.8  27.4  56.2 110.  118.      0
## 4 CPE2          1 label        3     0 FIC        10.4  60.1  70.5 119.  127.      1
## 5 CPE2          1 label        4     0 HELI DATA  15.9  27.4  64.7 148.  156.      0

labels = input %>%
    filter(location=='label') %>%
    mutate(level=case_when(near(x0,27.39,1) ~ 0L,
                           near(x1,70.50,1) ~ 1L))


## Drop explicit label continuation across pages as we handle them implicitly as almost all are implicit. Merge
## labels that end in '-' indicating they have been split across lines.
##
## Arrange within by line as summarize does not preserve prior arrange here (because it differs from the group?).
##
##   aerodrome level  item text       page  line    x0    x1    y0    y1
##   <fct>     <int> <int> <chr>     <dbl> <int> <dbl> <dbl> <dbl> <dbl>
## 1 CPE2          0     1 REF           1     0  27.4  41.4  59.7  67.5
## 2 CPE2          0     2 OPR           1     1  27.4  42.6  88.9  96.7
## 3 CPE2          0     3 FLT PLN       1     2  27.4  56.2 110.  118.
## 4 CPE2          1     4 FIC           1     3  60.1  70.5 119.  127.
## 5 CPE2          0     5 HELI DATA     1     4  27.4  64.7 148.  156.

labels = labels %>%
    filter(!str_detect(text,'\\(Cont’d\\)$')) %>%
    group_by(aerodrome,level) %>%
    mutate(item=cumsum(!str_detect(lag(text,default=''),'-$')),
           text=str_remove(text,'-$')) %>%
    group_by(aerodrome,level,item) %>%
    summarize(text=str_flatten(text),
              page=first(page),
              line=first(line),
              x0=min(x0),x1=x0+sum(x1-x0),
              y0=min(y0),y1=min(y1)) %>%
    arrange(aerodrome,page,line) %>%
    group_by(aerodrome) %>%
    mutate(item=row_number())



## Combine the two label levels
##
## Arrange by item (which is page and line).
##
##   aerodrome  item  page  line    x0    x1    y0    y1 label
##   <fct>     <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl> <fct>
## 1 CPE2          1     1     0  27.4  41.4  59.7  67.5 REF
## 2 CPE2          2     1     1  27.4  42.6  88.9  96.7 OPR
## 3 CPE2          3     1     2  27.4  56.2 110.  118.  FLT PLN
## 4 CPE2          4     1     3  60.1  70.5 119.  127.  FLT PLN:FIC
## 5 CPE2          5     1     4  27.4  64.7 148.  156.  HELI DATA

labels = labels %>%
    pivot_wider(names_prefix='label',names_from=level,values_from=text) %>%
    arrange(aerodrome,item) %>%
    group_by(aerodrome) %>%
    fill(label0) %>%
    unite(label,starts_with('label'),sep=':',na.rm=TRUE) %>%
    mutate(label=fct(label))


## Associate body text with the labels adjacent or above and make line per aerodrome item.
##
## Arrange by item (based on page and line of the label) and line (based on page, line, and chunk of the text).
##
##   location aerodrome  item  page  line    x0    x1    y0    y1 label       chunk text                                  wrap
##   <chr>    <fct>     <int> <dbl> <int> <dbl> <dbl> <dbl> <dbl> <fct>       <int> <chr>                                <dbl>
## 1 body     CPE2          1     1     1  77.8  162.  59.7  67.5 REF             0 N43 50 10 W79 01 02 Adj               12.8
## 2 body     CPE2          1     1     2  77.8  187.  67.7  75.5 REF             0 11°W (2013) UTC-5(4) Elev 300’        17.2
## 3 body     CPE2          1     1     3  77.8  117.  75.7  83.5 REF             0 VTA A5000                             13.6
## 4 body     CPE2          2     1     1  77.8  155.  88.9  96.7 OPR             0 Ajax Pickering Gen Hosp               13.6
## 5 body     CPE2          2     1     2  77.8  160.  96.9 105.  OPR             0 905-683-2320 Cert PPR                 43.5

texts = bind_rows(bind_cols(location='label',labels),
                   filter(input,location=='body')) %>%
    arrange(aerodrome,page,
            case_when(location == 'label' ~ y0-4,
                      TRUE                ~ y0)) %>%
    group_by(aerodrome) %>%
    fill(item,label) %>%
    filter(location!='label') %>%
    arrange(aerodrome,item,page,line,chunk) %>%
    group_by(aerodrome,item) %>%
    mutate(line=row_number(page*max(line)+line))


## Compute the right hand margin as picture_x0 until surpased and then picture_x1.
##
##   aerodrome  page    x0    x1    y0    y1 label       text                                 wrap block margin
##   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <fct>       <chr>                                  <dbl> <int>  <dbl>
## 1 CPE2          1  77.8  162.  59.7  67.5 REF         N43 50 10 W79 01 02 Adj                 12.8     1    201
## 2 CPE2          1  77.8  187.  67.7  75.5 REF         11°W (2013) UTC-5(4) Elev 300’          17.2     1    201
## 3 CPE2          1  77.8  117.  75.7  83.5 REF         VTA A5000                               13.6     1    201
## 4 CPE2          1  77.8  155.  88.9  96.7 OPR         Ajax Pickering Gen Hosp                 13.6     2    201
## 5 CPE2          1  77.8  160.  96.9 105.  OPR         905-683-2320 Cert PPR                   43.5     2    201

texts = texts %>%
    group_by(aerodrome) %>%
    mutate(margin=cummax(if_else(x1 < picture_x0,picture_x0,picture_x1)))


## Combine lines into paragraphs based on whether the margin would have forced a break. Track starting line for
## dealing with chunks.
##
## Arrange by item, line, and chunk.
##
##   aerodrome  item label       chunk paragraph text                                                                                          line    x0    x1    y0    y1
##   <fct>     <int> <fct>       <int>     <int> <chr>                                                                                        <int> <dbl> <dbl> <dbl> <dbl>
## 1 CPE2          1 REF             0         1 N43 50 10 W79 01 02 Adj                                                                          1  77.8  162.  59.7  67.5
## 2 CPE2          1 REF             0         2 11°W (2013) UTC-5(4) Elev 300’ VTA A5000                                                         2  77.8  187.  67.7  83.5
## 3 CPE2          2 OPR             0         1 Ajax Pickering Gen Hosp 905-683-2320 Cert PPR                                                    1  77.8  160.  88.9 105.
## 4 CPE2          3 FLT PLN         0         1 NOTAM FILE CYOO                                                                                  1  77.8  142. 110.  118.
## 5 CPE2          4 FLT PLN:FIC     0         1 London 866-WXBRIEF (Toll free within Canada) or 866-541-4104 (Toll free within Canada & USA)     1  77.8  198. 119.  143.

rule = texts %>%
    group_by(aerodrome,item,label,chunk) %>%
    mutate(paragraph=cumsum(replace_na(lag(margin-x1) > wrap+8,TRUE))) %>%
    group_by(aerodrome,item,label,chunk,paragraph) %>%
    summarize(text=str_flatten(text,' '),
              line=first(line)) %>%
    arrange(aerodrome,item,line,chunk)


## Extract all words.
##
##    line word
##   <int> <chr>
## 1     1 N43
## 2     1 50
## 3     1 10
## 4     1 W79
## 5     1 01

words = rule %>%
    ungroup() %>%
    mutate(word=str_split(text,' +'),
           line=row_number()) %>%
    select(line,word) %>%
    unnest(word)


## Calculate counts for start of paragraph, end of paragraph, and it the middle. Use 1 for start and 0 for end so
## when we check for end followed by start so it goes 0 1 and not 1 0.
##
##   word      count0 count1 count_ prob0 prob1
##   <chr>      <int>  <int>  <int> <dbl> <dbl>
## 1 (1047´)        1      0      0   Inf   NaN
## 2 (1171’)        1      0      0   Inf   NaN
## 3 (1205’)        2      0      0   Inf   NaN
## 4 (1207´)        1      0      0   Inf   NaN
## 5 (122.825)      3      0      0   Inf   NaN

monograms = full_join(full_join(words %>%
                                group_by(line) %>%
                                summarize(word=last(word)) %>%
                                group_by(word) %>%
                                summarize(count0=n())
                               ,
                                words %>%
                                group_by(line) %>%
                                summarize(word=first(word)) %>%
                                group_by(word) %>%
                                summarize(count1=n()))
                     ,
                      words %>%
                      group_by(line) %>%
                      summarize(word=tail(head(word,-1),-1)) %>%
                      group_by(word) %>%
                      summarize(count_=n())) %>%
    mutate(count0=replace_na(count0,0),
           count1=replace_na(count1,0),
           count_=replace_na(count_,0),
           prob0=if_else(count0>0|count_>0,log(count0)-log(count_),0),
           prob1=if_else(count1>0|count_>0,log(count1)-log(count_),0))


## Count word pair occurances within and between rule based paragraphs.
##
##   word0     word1   count01 count_
##   <chr>     <chr>     <int>  <int>
## 1 (1047´)   IKF           1      0
## 2 (1171’)   ISB           1      0
## 3 (1205’)   Adj           1      0
## 4 (1205’)   Arr/dep       1      0
## 5 (1207´)   IYB           1      0

bigrams = full_join(words %>%
                    group_by(line) %>%
                    summarize(word0=last(word),
                              word1=first(word)) %>%
                    ungroup() %>%
                    mutate(word0=lag(word0)) %>%
                    slice(-1) %>%
                    group_by(word0,word1) %>%
                    summarize(count01=n())
                    ,
                    words %>%
                    group_by(line) %>%
                    mutate(word0=lag(word),
                           word1=word,
                           word=NULL) %>%
                    slice(-1) %>%
                    group_by(word0,word1) %>%
                    summarize(count_=n())) %>%
    mutate(count01=replace_na(count01,0),
           count_=replace_na(count_,0),
           prob01=log(count01)-log(count_),
           prob01=if_else(is.nan(prob01),0,prob01))


## Combine lines into paragraphs based on whether the margin would have forced a break and frequency of the
## appearance of the words on both sides in the other combinations.
##
##   aerodrome  item label       chunk paragraph text                                                                                          line
##   <fct>     <int> <fct>       <int>     <int> <chr>                                                                                        <int>
## 1 CPE2          1 REF             0         1 N43 50 10 W79 01 02 Adj 11°W (2013) UTC-5(4) Elev 300’ VTA A5000                                 1
## 2 CPE2          2 OPR             0         1 Ajax Pickering Gen Hosp 905-683-2320 Cert PPR                                                    1
## 3 CPE2          3 FLT PLN         0         1 NOTAM FILE CYOO                                                                                  1
## 4 CPE2          4 FLT PLN:FIC     0         1 London 866-WXBRIEF (Toll free within Canada) or 866-541-4104 (Toll free within Canada & USA)     1
## 5 CPE2          5 HELI DATA       0         1 FATO/TLOF 86’ x 86’ ASPH/GRASS Safety Area 114’ x 114’ GRASS                                     1
           
prob = left_join(left_join(texts %>%
                           group_by(aerodrome,item,label,chunk) %>%
                           mutate(words=str_split(text,' +'),
                                  word0=lag(map_chr(words,tail,1)),
                                  word1=map_chr(words,head,1))
                          ,
                           select(monograms,
                                  word0=word,prob0=prob0))
                ,
                 select(monograms,
                        word1=word,prob1=prob1)) %>%
    mutate(prob_=pmax(log(lag(margin-x1))-log(wrap+8),-2),
           prob=prob0+prob1+prob_) %>%
    group_by(aerodrome,item,label,chunk) %>%
    mutate(paragraph=cumsum(replace_na(prob>0,TRUE))) %>%
    group_by(aerodrome,item,label,chunk,paragraph) %>%
    summarize(text=str_flatten(text,' '),
              line=first(line)) %>%
    arrange(aerodrome,item,line,chunk)
