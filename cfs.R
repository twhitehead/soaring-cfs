library('tidyverse')

options(width = 256)

basename = 'ecfs_04_en'                 # Base name of text and input file

contacts = c('TWR','MF','MF/ATF','ATF', # COMM contacts in order of preference
             'UNICOM','APRT RDO')

accs = c('CZEG','CZQM','CZQX','CZUL',   # Area control centers (not actual aerodromes)
         'CZVR','CZWG','CZYZ')

aerodrome_x0 = 27                       # Left-hand side of aerodrome header line
aerodrome_x1 = 337                      # Right-hand side of aerodrome header line

split_y = 37                            # Split point between header and body
split_x = 74                            # Split point between labels and text

picture_x0 = 24                         # Left-hand edge for full width pictures
picture_x1 = 201                        # Left-hand edge for all upper-right pictures
picture_x2 = 339                        # Right-hand edge for all upper-right pictures
picture_y0 = 58                         # Top edge for all upper-right pictures
picture_y1 = 191                        # Bottom edge for all upper-right pictures

codes_x0 = 49                           # ICAO code spit point in column 1 of cross-reference
codes_x1 = 180                          # Column 1 and 2 spit point in cross-reference
codes_x2 = 213                          # ICAO code spit point in column 2 of cross-reference


## Load the pdftotext -tsv output. Drop pdftext classifications (has errors), any really large text (watermark),
## any really small text (garbage). Compute word bounding boxes.
##
##    page    y0    y1    x0    x1 width height text
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>
## 1     1  47.1  78.6  43.8  87.8 44.0   31.4  CFS
## 2     1  76.9  88.2  50.3  81.5 31.2   11.3  DIGITAL
## 3     1  86.9  98.2  49.4  82.2 32.7   11.3  EDITION
## 4     1  52.5  62.6 117   154.  37.5   10.1  CAUTION:
## 5     1  52.7  62.6 156.  170.  14.1    9.87 THE

input = read_tsv(str_c(basename,'.txt'), quote = '') %>%
    mutate(x0 = left, x1 = left+width,
           y0 = top,  y1 = top+height) %>%
    filter(conf == 100,
           between(width*height/str_length(text), 3*3, 72*72/4)) %>%
    select(page = page_num,
           y0, y1, x0, x1, width, height,
           text)


## ----------------------------------------------------------------------------------------------------------------
## TAGGED AERODROME CONTENT

## Extract headers by being entirely above split_y.
##
##    page    y0    y1    x0    x1 width height text
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>
## 1     3  7.11  15.7  26.8  51.2 24.4    8.58 CANADA
## 2     3  7.11  15.7  53.4  74.0 20.7    8.58 FLIGHT
## 3     3  7.11  15.7  76.2 116.  39.7    8.58 SUPPLEMENT
## 4     3  7.58  16.2 117.  119.   2.08   8.58 /
## 5     3  7.58  16.2 121.  133.  12.0    8.58 GPH

headers = input %>%
    filter(y1 <= split_y)

input = input %>%
    anti_join(headers)


## Convert headers into lines and chunks by looking for > 4 pt difference in y1 location for lines and then > 4x
## height difference in x space betwwen words.
##
##    page  line chunk text                                                                                              x0     x1    y0    y1
##   <dbl> <int> <int> <chr>                                                                                          <dbl>  <dbl> <dbl> <dbl>
## 1     3     1     1 CANADA FLIGHT SUPPLEMENT / GPH 205 Effective 0901Z 31 December 2020 to 0901Z 25 February 2021  26.78 287.2   7.11 16.16
## 2     3     2     1 ONTARIO                                                                                        27.14  62.47 21.27 33.07
## 3     3     2     2 AERODROME / FACILITY DIRECTORY                                                                220.4  335.8  24.21 32.69
## 4     4     1     1 CANADA FLIGHT SUPPLEMENT / GPH 205 Effective 0901Z 31 December 2020 to 0901Z 25 February 2021  26.78 287.2   7.11 16.16
## 5     4     2     1 ONTARIO                                                                                        27.14  62.47 21.27 33.07

headers = headers %>%
    arrange(page, y1) %>%
    group_by(page) %>%
    mutate(line = cumsum(replace_na(y1-lag(y1) > 4, TRUE))) %>%
    arrange(page, line, x0) %>%
    group_by(page, line) %>%
    mutate(chunk = cumsum(replace_na(x0-lag(x1) > 2*height, TRUE))) %>%
    group_by(page, line, chunk) %>%
    summarize(text = str_flatten(text, ' '),
              x0 = min(x0), x1 = max(x1),
              y0 = min(y0), y1 = max(y1))


## Identify aerodrome pages by last line chunk being 'AERODROME / FACILITY DIRECTORY'.
##
##    page
##   <dbl>
## 1     3
## 2     4
## 3     5
## 4     6
## 5     7

pages = headers %>%
    group_by(page) %>%
    filter(last(text) == 'AERODROME / FACILITY DIRECTORY') %>%
    summarize()

headers = headers %>%
    anti_join(pages)


## Get all aerodrome pages.
##
##    page    y0    y1     x0     x1 width height text
##   <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl>  <dbl> <chr>
## 1     3 35.52 41.23  27.36  44.18 16.82   5.71 ONCPE2
## 2     3 43.55 54.41  27.39  48.15 20.76  10.86 AJAX
## 3     3 44.95 54.1   50.31  88.92 38.61   9.15 (PICKERING
## 4     3 44.95 54.1   90.74 104.9  14.2    9.15 GEN
## 5     3 44.95 54.1  106.8  127.5  20.76   9.15 HOSP)

pages = input %>%
    semi_join(pages)

input = input %>%
    anti_join(pages)


## Classify lines and chunks and pages by looking for > 4 pt difference in y1 location for lines and then > 4x
## height difference in x space between words.
##
##    page  line chunk text                                    x0     x1    y0     y1
##   <dbl> <int> <int> <chr>                                <dbl>  <dbl> <dbl>  <dbl>
## 1     3     1     1 ONCPE2                               27.36  44.18 35.52  41.23
## 2     3     2     1 AJAX (PICKERING GEN HOSP) ON (Heli)  27.39 159.9  43.55  54.41
## 3     3     2     2 CPE2                                318.7  336.9  46.09  53.89
## 4     3     3     1 REF                                  27.39  41.38 59.68  67.48
## 5     3     3     2 N43 50 10 W79 01 02 Adj              77.79 161.7  59.68  67.48

pages = pages %>%
    arrange(page, y1) %>%
    group_by(page) %>%
    mutate(line = cumsum(replace_na(y1-lag(y1) > 4, TRUE))) %>%
    arrange(page, line, x0) %>%
    group_by(page, line) %>%
    mutate(chunk = cumsum(replace_na(x0-lag(x1) > 2*height, TRUE)))


## Extract the aerodromes by being a left aligned name chunk and a right aligned ICAO code chunk that
## isn't an area control centre.
##
##    page name                                    aerodrome     y
##   <dbl> <chr>                                   <fct>     <dbl>
## 1     3 AJAX (PICKERING GEN HOSP) ON (Heli)     CPE2      54.41
## 2     4 ALEXANDRIA ON                           CNS4      54.41
## 3     5 ALLAN PARK ON                           CAP2      54.41
## 4     6 ALLISTON ON (Heli)                      CPJ2      54.41
## 5     7 ALLISTON (STEVENSON MEM HOSP) ON (Heli) CPZ2      54.41

aerodromes = pages %>%
    arrange(page, line, chunk, x0) %>%
    group_by(page, line, chunk) %>%
    summarize(text = str_flatten(text, ' '),
              x0 = min(x0), x1 = max(x1),
              y0 = min(y0), y1 = max(y1)) %>%
    pivot_wider(names_prefix = 'chunk', names_from = chunk,
                values_from = c(text, x0, x1, y0, y1)) %>%
    filter(near(x0_chunk1, aerodrome_x0, 4),
           near(x1_chunk2, aerodrome_x1, 4),
           str_detect(text_chunk2, '^C[0-9A-Z]{3}$')) %>%
    mutate(page,name = text_chunk1, aerodrome = text_chunk2,
           y = max(y1_chunk1, y1_chunk2), .keep = 'none') %>%
    filter(!aerodrome %in% accs) %>%
    ungroup() %>%
    mutate(aerodrome = fct(aerodrome)) %>%
    group_by(aerodrome) %>%
    mutate(name = first(name))


## Remove the aerodrome headers lines and the invisible province aerodrome lines above them.
##
##    page    y0    y1    x0    x1 width height text    line chunk
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>  <int> <int>
## 1     3  59.7  67.5  27.4  41.4 14.0     7.8 REF        3     1
## 2     3  59.7  67.5  77.8  90.6 12.8     7.8 N43        3     2
## 3     3  59.7  67.5  92.6 100.   7.77    7.8 50         3     2
## 4     3  59.7  67.5 102.  110.   7.77    7.8 10         3     2
## 5     3  59.7  67.5 112.  126.  14.4     7.8 W79        3     2

pages = pages %>%
    anti_join(aerodromes)

pages = pages %>%
    anti_join(pages %>%
              arrange(page, line, chunk, x0) %>%
              group_by(page, line, chunk) %>%
              summarize(text = str_flatten(text, ' '),
                        x0 = min(x0), x1 = max(x1),
                        y0 = min(y0), y1 = max(y1)) %>%
              mutate(aerodrome = str_extract(text, '^[A-Z]{2} ?(C[0-9A-Z]{3})$', group = 1)) %>%
              inner_join(select(aerodromes, aerodrome, y)) %>%
              filter(near(x0, aerodrome_x0, 4), y1-y < 10) %>%
              select(page, line, chunk))


## Classify items by the aerodrome ICAO code on the page it occurs under.
##
##    page    y0    y1    x0    x1 width height text    line chunk aerodrome
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>  <int> <int> <fct>
## 1     3  59.7  67.5  27.4  41.4 14.0     7.8 REF        3     1 CPE2
## 2     3  59.7  67.5  77.8  90.6 12.8     7.8 N43        3     2 CPE2
## 3     3  59.7  67.5  92.6 100.   7.77    7.8 50         3     2 CPE2
## 4     3  59.7  67.5 102.  110.   7.77    7.8 10         3     2 CPE2
## 5     3  59.7  67.5 112.  126.  14.4     7.8 W79        3     2 CPE2

items = pages %>%
    inner_join(select(aerodromes, page, aerodrome,y),
               by = join_by(page, closest(y0 > y))) %>%
    select(!y)

pages = pages %>%
    anti_join(items)


## Extract labels as being entirely left of split_x.
##
##    page    y0    y1    x0    x1 width height text      line chunk aerodrome
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>    <int> <int> <fct>
## 1     3  59.7  67.5  27.4  41.4  14.0   7.8  REF          3     1 CPE2
## 2     3  88.9  96.7  27.4  42.6  15.2   7.8  OPR          6     1 CPE2
## 3     3 110.  118.   27.4  40.2  12.8   7.8  FLT          8     1 CPE2
## 4     3 110.  118.   42.2  56.2  14     7.8  PLN          8     1 CPE2
## 5     3 119.  127.   60.1  70.5  10.4   7.23 FIC          9     1 CPE2

labels = items %>%
    filter(x1 <= split_x)

items = items %>%
    anti_join(labels)


## Convert labels into lines by looking for > 4 pt difference in y1 location.
##
##   aerodrome  page  line text         x0    x1    y0    y1
##   <fct>     <dbl> <int> <chr>     <dbl> <dbl> <dbl> <dbl>
## 1 CPE2          3     1 REF        27.4  41.4  59.7  67.5
## 2 CPE2          3     2 OPR        27.4  42.6  88.9  96.7
## 3 CPE2          3     3 FLT PLN    27.4  56.2 110.  118.
## 4 CPE2          3     4 FIC        60.1  70.5 119.  127.
## 5 CPE2          3     5 HELI DATA  27.4  64.7 148.  156.

labels = labels %>%
    arrange(page, y1) %>%
    group_by(aerodrome, page) %>%
    mutate(line = cumsum(replace_na(y1-lag(y1) > 4, TRUE))) %>%
    arrange(page, line, x0) %>%
    group_by(aerodrome, page, line) %>%
    summarize(text = str_flatten(text, ' '),
              x0 = min(x0), x1 = max(x1),
              y0 = min(y0), y1 = max(y1))


## Extract label depths according to left marign alignment (1 = left aligned and 2 = not left aligned).
##
##   aerodrome level  item text       page  line    x0    x1    y0    y1
##   <fct>     <int> <int> <chr>     <dbl> <int> <dbl> <dbl> <dbl> <dbl>
## 1 CPE2          0     1 REF           3     1  27.4  41.4  59.7  67.5
## 2 CPE2          0     2 OPR           3     2  27.4  42.6  88.9  96.7
## 3 CPE2          0     3 FLT PLN       3     3  27.4  56.2 110.  118.
## 4 CPE2          1     4 FIC           3     4  60.1  70.5 119.  127.
## 5 CPE2          0     5 HELI DATA     3     5  27.4  64.7 148.  156.

labels = labels %>%
    mutate(level = case_when(near(x0, aerodrome_x0, 4) ~ 1L,
                             TRUE                      ~ 2L))


## Merge labels that end in '-' indicating they have been split across lines, and drop explicit label continuation
## across pages as we handle them implicitly (almost all are implicit).
##
##   aerodrome level  item text       page  line    x0    x1    y0    y1
##   <fct>     <int> <int> <chr>     <dbl> <int> <dbl> <dbl> <dbl> <dbl>
## 1 CPE2          1     1 REF           3     1  27.4  41.4  59.7  67.5
## 2 CPE2          1     2 OPR           3     2  27.4  42.6  88.9  96.7
## 3 CPE2          1     3 FLT PLN       3     3  27.4  56.2 110.  118.
## 4 CPE2          1     4 HELI DATA     3     5  27.4  64.7 148.  156.
## 5 CPE2          1     5 LIGHTING      3     6  27.4  60.8 178.  186.

labels = labels %>%
    arrange(page, line) %>%
    group_by(aerodrome, level) %>%
    mutate(item = cumsum(!str_detect(lag(text, default = ''), '-$')),
           text = str_remove(text, '-$')) %>%
    group_by(aerodrome, level, item) %>%
    summarize(text = str_flatten(text),
              page = first(page),
              line = first(line),
              x0 = min(x0), x1 = max(x1),
              y0 = min(y0), y1 = max(y1)) %>%
    filter(!str_detect(text, '\\(Cont’d\\)$'))


## Pivot the labels into their own columns and compute y break point for association.
##
##   aerodrome  item  page label1    label2     y
##   <fct>     <int> <dbl> <fct>     <fct>  <dbl>
## 1 CPE2          1     3 REF       NA      55.7
## 2 CPE2          2     3 OPR       NA      84.9
## 3 CPE2          3     3 FLT PLN   NA     106.
## 4 CPE2          4     3 FLT PLN   FIC    115.
## 5 CPE2          5     3 HELI DATA NA     144.

labels = labels %>%
    pivot_wider(names_from = level, names_prefix = 'label',
                values_from = text) %>%
    arrange(page, line) %>%
    group_by(aerodrome) %>%
    fill(label1) %>%
    ungroup() %>%
    mutate(aerodrome, page,
           across(starts_with('label'), fct),
           item = row_number(),
           y = y0-4,
           .keep = 'none')


## Add labels to item data by closest y accounting for pages.
##
##    page    y0    y1    x0    x1 width height text      line chunk  item
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>    <int> <int> <int>
## 1     3  59.7  67.5  77.8  90.6 12.8     7.8 N43          3     2     1
## 2     3  59.7  67.5  92.6 100.   7.77    7.8 50           3     2     1
## 3     3  59.7  67.5 102.  110.   7.77    7.8 10           3     2     1
## 4     3  59.7  67.5 112.  126.  14.4     7.8 W79          3     2     1
## 5     3  59.7  67.5 128.  136.   7.77    7.8 01           3     2     1

items = items %>%
    bind_rows(select(labels, aerodrome, page, item, y)) %>%
    arrange(page, coalesce(y0, y)) %>%
    group_by(aerodrome) %>%
    fill(item) %>%
    filter(is.na(y)) %>%
    ungroup() %>%
    select(!aerodrome & !y)


## Convert page data into lines and chunks by looking for > 4 pt difference in y1 location for lines and then > 4x
## height difference in x space betwwen words. Track the length to first and second word for paragraph extraction.
##
##    item  page  line chunk text                                 wrap1 wrap2    x0    x1    y0    y1
##   <int> <dbl> <int> <int> <chr>                                <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1     1     3     1     1 N43 50 10 W79 01 02 Adj               12.8  22.5  77.8  162.  59.7  67.5
## 2     1     3     2     1 11°W (2013) UTC-5(4) Elev 300’        17.2  39.3  77.8  187.  67.7  75.5
## 3     1     3     3     1 VTA A5000                             13.6  39.6  77.8  117.  75.7  83.5
## 4     2     3     1     1 Ajax Pickering Gen Hosp               13.6  44.2  77.8  155.  88.9  96.7
## 5     2     3     2     1 905-683-2320 Cert PPR                 43.5  62.5  77.8  160.  96.9 105.

items = items %>%
    arrange(item, page, y1) %>%
    group_by(item, page) %>%
    mutate(line = cumsum(replace_na(y1-lag(y1) > 4,
                                    TRUE))) %>%
    arrange(item, page, line, x0) %>%
    group_by(item, page, line) %>%
    mutate(chunk = cumsum(replace_na(x0-lag(x1) > 2*height,
                                     TRUE))) %>%
    group_by(item, page, line, chunk) %>%
    summarize(text = str_flatten(text, ' '),
              wrap1 = first(x1-x0),
              wrap2 = first(lead(x1)-x0),
              x0 = min(x0), x1 = max(x1),
              y0 = min(y0), y1 = max(y1))


## Extract picture box based on distance from aerodrome header to first text.
##
##   aerodrome  page   iy0   iy1    x0    x1    y0    y1
##   <fct>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 CPE2          3  54.4  55.7   201   338  54.4  185.
## 2 CNS4          4  54.4  55.7   201   338  54.4  185.
## 3 CAP2          5  54.4  55.7   201   338  54.4  185.
## 4 CPJ2          6  54.4  55.7   201   338  54.4  185.
## 5 CPZ2          7  54.4  55.7   201   338  54.4  185.

images = items %>%
    left_join(select(labels, aerodrome, item, label1, label2, iy1 = y)) %>%
    right_join(select(aerodromes, page, aerodrome, iy0 = y)) %>%
    arrange(item, page, line, chunk) %>%
    group_by(aerodrome) %>%
    summarize(page = first(page),
              iy0 = first(iy0)+1, iy1 = first(iy1)) %>%
    mutate(x0 = if_else(iy1-iy0 < 36, picture_x1, picture_x0),
           x1 = picture_x2,
           y0 = iy0,
           y1 = if_else(iy1-iy0 < 36, y0+picture_y1-picture_y0, iy1))


## Compute the right hand margin as picture_x1 until surpased and then picture_x2.
##
##    item  page  line chunk text                                  wrap1 wrap2    x0    x1    y0    y1 margin
##   <int> <dbl> <int> <int> <chr>                                 <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
## 1     1     3     1     1 N43 50 10 W79 01 02 Adj                12.8  22.5  77.8  162.  59.7  67.5    201
## 2     2     3     1     1 Ajax Pickering Gen Hosp                13.6  44.2  77.8  155.  88.9  96.7    201
## 3     3     3     1     1 NOTAM FILE CYOO                        25.2  41.9  77.8  142. 110.  118.     201
## 4     4     3     1     1 London 866-WXBRIEF (Toll free within   23.3  71    77.8  198. 119.  127.     201
## 5     5     3     1     1 FATO/TLOF 86’ x 86’ ASPH/GRASS         38.5  49.7  77.8  192. 148.  156.     201

items = items %>%
    left_join(select(labels, aerodrome, item)) %>%
    arrange(item, page, line, chunk) %>%
    group_by(aerodrome) %>%
    mutate(margin = cummax(if_else(x1 < picture_x1, picture_x1, picture_x2))) %>%
    ungroup() %>%
    select(!aerodrome)


## ----------------------------------------------------------------------------------------------------------------
## AERODROME CODES

## Identify general section by last line chunk being 'Axx GENERAL' or 'GENERAL Axx'.
##
##    page
##   <dbl>
## 1   495
## 2   497
## 3   499
## 4   501
## 5   503

pages = headers %>%
    group_by(page) %>%
    filter(str_detect(last(text), 'A[0-9]+ GENERAL|GENERAL A[0-9]+')) %>%
    summarize()


## Get all general section pages.
##
# A tibble: 17,075 × 8
##    page    y0    y1    x0    x1 width height text
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>
## 1   495  45.0  53.9 144.  178.  34.6    8.91 SPECIAL
## 2   495  45.0  53.9 181.  216.  35.5    8.91 NOTICES
## 3   495  57.2  65.0  25.2  38.4 13.2    7.8  This
## 4   495  57.2  65.0  40.0  58.7 18.7    7.8  space
## 5   495  57.2  65.0  60.4  70.1  9.72   7.8  will

pages = input %>%
    semi_join(pages)


## Classify lines and chunks and pages by looking for > 4 pt difference in y1 location for lines and then > 4x
## height difference in x space between words.
##
##    page    y0    y1    x0    x1 width height text     line chunk
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>   <int> <int>
## 1   495  45.0  53.9 144.  178.  34.6    8.91 SPECIAL     1     1
## 2   495  45.0  53.9 181.  216.  35.5    8.91 NOTICES     1     1
## 3   495  57.2  65.0  25.2  38.4 13.2    7.8  This        2     1
## 4   495  57.2  65.0  40.0  58.7 18.7    7.8  space       2     1
## 5   495  57.2  65.0  60.4  70.1  9.72   7.8  will        2     1

pages = pages %>%
    arrange(page, y1) %>%
    group_by(page) %>%
    mutate(line = cumsum(replace_na(y1-lag(y1) > 4, TRUE))) %>%
    arrange(page, line, x0) %>%
    group_by(page, line) %>%
    mutate(chunk = cumsum(replace_na(x0-lag(x1) > 2*height, TRUE)))


## Extract the aerodrome tables by their labels.
##
##    page     y line0 line1
##   <dbl> <dbl> <dbl> <int>
## 1   512  84       3     5
## 2   513  65.0     1     3
## 3   514  65.0     1     3
## 4   515  64.9     1     3
## 5   516  64.9     1     3

aerodromes = pages %>%
    arrange(page, line, chunk, x0) %>%
    group_by(page, line, chunk) %>%
    summarize(text = str_flatten(text, ' '),
              x0 = min(x0), x1 = max(x1),
              y0 = min(y0), y1 = max(y1)) %>%
    pivot_wider(names_prefix = 'chunk', names_from = chunk, values_from = c(text, x0, x1, y0, y1)) %>%
    arrange(page, line) %>%
    group_by(page) %>%
    filter(str_detect(lag(text_chunk1, 2L), '^CROSS REFERENCE OF AERODROME$'             ),
           str_detect(lag(text_chunk2, 2L), '^CROSS REFERENCE OF AERODROME$'             ),
           str_detect(lag(text_chunk1, 1L), '^LOCATION INDICATOR & NAME( \\(Cont.d\\))?$'),
           str_detect(lag(text_chunk2, 1L), '^LOCATION INDICATOR & NAME( \\(Cont.d\\))?$'),
           text_chunk1 == 'Indicator', text_chunk2 == 'Name',
           text_chunk3 == 'Indicator', text_chunk4 == 'Name') %>%
    mutate(page,
           y = max(y1_chunk1, y1_chunk2, y1_chunk3, y1_chunk4),
           header0 = line-2, header1 = line,
           .keep = 'none')

headers = headers %>%
    anti_join(aerodromes)

input = input %>%
    anti_join(aerodromes)

pages = pages %>%
    semi_join(aerodromes) %>%
    anti_join(aerodromes,
              join_by(page, between(line, header0, header1)))


## Extract the table columns by being under the labels.
##
##    page    y0    y1    x0    x1 width height text              line chunk column
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>            <int> <int>  <dbl>
## 1   512  91.2  99    25.2  43.5 18.3     7.8 CAA2                 6     1      1
## 2   512  91.2  99    55.2 106.  50.5     7.8 St-André-Avellin     6     1      1
## 3   512  91.2  99   108.  118.  10.5     7.8 QC                   6     1      1
## 4   512  91.2  99.0 189.  207.  18.2     7.8 CAP6                 6     2      2
## 5   512  91.2  99.0 219.  245.  26.4     7.8 Ingenika             6     2      2

aerodromes = pages %>%
    inner_join(select(aerodromes, page, y),
               by = join_by(page, closest(y0 > y))) %>%
    select(!y) %>%
    mutate(column = if_else(x1 < codes_x1, 1, 2))

pages = pages %>%
    anti_join(aerodromes)


## Extract aerodrome ICAO codes as being on the left.
##
##    page    y0    y1    x0    x1 width height text   line chunk column
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr> <int> <int>  <dbl>
## 1   512  91.2  99    25.2  43.5  18.3    7.8 CAA2      6     1      1
## 2   512  91.2  99.0 189.  207.   18.2    7.8 CAP6      6     2      2
## 3   512 101.  109.   25.2  43.4  18.2    7.8 CAA3      7     1      1
## 4   512 101.  109.  189.  207.   18.2    7.8 CAP9      7     2      2
## 5   512 111.  119.  189.  208.   19.0    7.8 CAQ4      8     2      2

codes = aerodromes %>%
    filter(x1<case_match(column,
                         1 ~ codes_x0,
                         2 ~ codes_x2))

aerodromes = aerodromes %>%
    anti_join(codes)


## Compute y break point for association.
##
##    page column aerodrome     y
##   <dbl>  <dbl> <fct>     <dbl>
## 1   512      1 CAA2       87.2
## 2   512      2 CAP6       87.2
## 3   512      1 CAA3       97.2
## 4   512      2 CAP9       97.2
## 5   512      2 CAQ4      107.

codes = codes %>%
    ungroup() %>%
    mutate(page, column,
           aerodrome = fct(text),
           y = y0-4,
           .keep = 'none')

## Add ICAO code to data by closest y accounting for pages and columns.
##
##    page    y0    y1    x0    x1 width height text              line chunk column aerodrome
##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>            <int> <int>  <dbl> <fct>
## 1   512  91.2  99    55.2 106.  50.5     7.8 St-André-Avellin     6     1      1 CAA2
## 2   512  91.2  99   108.  118.  10.5     7.8 QC                   6     1      1 CAA2
## 3   512  91.2  99.0 219.  245.  26.4     7.8 Ingenika             6     2      2 CAP6
## 4   512  91.2  99.0 247.  257    9.71    7.8 BC                   6     2      2 CAP6
## 5   512 101.  109.   55.2  83.5 28.3     7.8 Westlock             7     1      1 CAA3

aerodromes = aerodromes %>%
    left_join(codes,
              by = join_by(page, column, closest(y0 > y))) %>%
    select(!y)


## Combine aerodrome text by breaking into lines by looking for > 4 pt difference in y1 location and combine.
##
##   aerodrome text                                      x0    x1    y0    y1
##   <fct>     <chr>                                  <dbl> <dbl> <dbl> <dbl>
## 1 CAA2      St-André-Avellin QC                     55.2 118.   91.2  99
## 2 CAP6      Ingenika BC                            219.  257    91.2  99.0
## 3 CAA3      Westlock (Healthcare Centre) AB (Heli)  55.2 158.  101.  118.
## 4 CAP9      Strathmore (Appleton Field) AB         219.  316   101.  109.
## 5 CAQ4      Springhouse Airpark BC                 219.  294.  111.  119.

aerodromes = aerodromes %>%
    arrange(aerodrome, page, column, y1) %>%
    group_by(aerodrome, page, column) %>%
    mutate(line = cumsum(replace_na(y1-lag(y1) > 4, TRUE))) %>%
    arrange(aerodrome, page, column, line, x0) %>%
    group_by(aerodrome) %>%
    summarize(text = str_flatten(text, ' '),
              x0 = min(x0), x1 = max(x1),
              y0 = min(y0), y1 = max(y1))


## Breakout province and type fields embedded in the names.
##
##   aerodrome name                         province type
##   <fct>     <chr>                        <fct>    <fct>
## 1 CAA2      St-André-Avellin             QC       NA
## 2 CAP6      Ingenika                     BC       NA
## 3 CAA3      Westlock (Healthcare Centre) AB       Heli
## 4 CAP9      Strathmore (Appleton Field)  AB       NA
## 5 CAQ4      Springhouse Airpark          BC       NA

aerodromes = aerodromes %>%
    ungroup() %>%
    mutate(breakout = str_match(text, '(?<name>.*[^,]),? (?<province>[A-Z]{2})(?: \\((?<type>[^)]+)\\))?') %>%
               as_tibble(.name_repair = 'unique_quiet')) %>%
    unpack(breakout) %>%
    mutate(aerodrome, name,
           province = fct(province),
           type = fct_recode(fct(replace_na(type, 'Land')), Water = 'water aerodrome'),
           .keep = 'none')


## ----------------------------------------------------------------------------------------------------------------
## PARAGRAPH RECONSTRUCTION

## Combine lines into paragraphs based on whether the margin would have forced a break.
##
##    item  page  line chunk text                                  wrap1 wrap2    x0    x1    y0    y1 margin label1    label2 word0 word1          prob paragraph
##   <int> <dbl> <int> <int> <chr>                                 <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <fct>     <fct>  <chr> <chr>         <dbl>     <int>
## 1     1     3     1     1 N43 50 10 W79 01 02 Adj                12.8  22.5  77.8  162.  59.7  67.5    201 REF       NA     NA    N43          NA             1
## 2     2     3     1     1 Ajax Pickering Gen Hosp                13.6  44.2  77.8  155.  88.9  96.7    201 OPR       NA     NA    Ajax         NA             1
## 3     3     3     1     1 NOTAM FILE CYOO                        25.2  41.9  77.8  142. 110.  118.     201 FLT PLN   NA     NA    NOTAM        NA             1
## 4     4     3     1     1 London 866-WXBRIEF (Toll free within   23.3  71    77.8  198. 119.  127.     201 FLT PLN   FIC    NA    London       NA             1
## 5     5     3     1     1 FATO/TLOF 86’ x 86’ ASPH/GRASS         38.5  49.7  77.8  192. 148.  156.     201 HELI DATA NA     NA    FATO/TLOF    NA             1

prob = items %>%
    arrange(item, page, line, chunk) %>%
    group_by(item, chunk) %>%
    mutate(words = str_split(text, ' +'),
           word0 = lag(map_chr(words, tail, 1)),
           word1 = map_chr(words, head, 1),
           words = NULL,
           prob_ = log(case_when(is.na(lag(margin-x1))     ~ NA,
                                lag(margin-x1) <= wrap1+8 ~ 1/2,     # ~ 1:2 split given one+ word would wrap
                                lag(margin-x1) <= wrap2+8 ~ 2/1,     # ~ 2:1 split given two+ words would wrap
                                TRUE                      ~ 10/1)),  # ~ 10:1 split given three+ words would wrap
           change = prob_>0,
           split = prob_>0,
           paragraph = cumsum(replace_na(split, TRUE)))


## Iterate on combining lines into paragraphs based on whether the margin would have forced a break and the
## frequency of the appearance of the words on both sides in the other combinations.
##

tries = 0
changes = ( prob %>%
            ungroup() %>%
            summarize(n = sum(replace_na(change, 0))) )$n

while (changes > 0 & tries < 10) {

    print(str_c('There were ', changes, ' changes'))

    ## Extract all words with a paragraph grouping variable.
    ##
    ##   label1    label2 words
    ##   <fct>     <fct>  <list>
    ## 1 REF       NA     <chr [14]>
    ## 2 OPR       NA     <chr [7]>
    ## 3 FLT PLN   NA     <chr [3]>
    ## 4 FLT PLN   FIC    <chr [14]>
    ## 5 HELI DATA NA     <chr [16]>

    words = prob %>%
        arrange(item, page, line, chunk) %>%
        group_by(item, chunk, paragraph) %>%
        summarize(text = str_flatten(text, ' ')) %>%
        left_join(select(labels, item, label1, label2)) %>%
        group_by(label1, label2) %>%
        mutate(words = str_split(text, ' +')) %>%
        select(label1, label2, words)



    ## Calculate counts for start of paragraph, end of paragraph, and it the middle. Use 1 for start and 0 for end
    ## so when we check for end followed by start it goes 0 1 and not 1 0.
    ##
    ##   label1 label2 word  count0 count1 count_
    ##   <fct>  <fct>  <chr>  <int>  <int>  <int>
    ## 1 REF    NA     1000’      1      0      0
    ## 2 REF    NA     1008´      1      0      1
    ## 3 REF    NA     1010’      2      0      0
    ## 4 REF    NA     1015´      1      0      1
    ## 5 REF    NA     1039'      1      0      0

    monograms = words %>%
        mutate(word = words) %>%
        unnest(word) %>%
        group_by(label1, label2, word) %>%
        summarize(count_ = n()) %>%
        full_join(words %>%
                  mutate(word = map_chr(words, tail, 1)) %>%
                  group_by(label1, label2, word) %>%
                  summarize(count0 = n())) %>%
        full_join(words %>%
                  mutate(word = map_chr(words, head, 1)) %>%
                  group_by(label1, label2, word) %>%
                  summarize(count1 = n())) %>%
        mutate(across(starts_with('count'), ~ replace_na(.x,0)))


    ## Add in the counts for the word locations.
    ##
    ##    item  page  line chunk text                                  wrap1 wrap2    x0    x1    y0    y1 margin word0 word1          prob paragraph label1    label2 count0 count0_ count1 count1_
    ##   <int> <dbl> <int> <int> <chr>                                 <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <chr> <chr>         <dbl>     <int> <fct>     <fct>   <int>   <int>  <int>   <int>
    ## 1     1     3     1     1 N43 50 10 W79 01 02 Adj                12.8  22.5  77.8  162.  59.7  67.5    201 NA    N43          NA             1 REF       NA         NA      NA    108       0
    ## 2     2     3     1     1 Ajax Pickering Gen Hosp                13.6  44.2  77.8  155.  88.9  96.7    201 NA    Ajax         NA             1 OPR       NA         NA      NA      1       0
    ## 3     3     3     1     1 NOTAM FILE CYOO                        25.2  41.9  77.8  142. 110.  118.     201 NA    NOTAM        NA             1 FLT PLN   NA         NA      NA    391      25
    ## 4     4     3     1     1 London 866-WXBRIEF (Toll free within   23.3  71    77.8  198. 119.  127.     201 NA    London       NA             1 FLT PLN   FIC        NA      NA    334       2
    ## 5     5     3     1     1 FATO/TLOF 86’ x 86’ ASPH/GRASS         38.5  49.7  77.8  192. 148.  156.     201 NA    FATO/TLOF    NA             1 HELI DATA NA         NA      NA     55       0

    prob = prob %>%
        select(!starts_with('count')) %>%
        left_join(select(labels, item, label1, label2)) %>%
        left_join(select(monograms, label1, label2, word0 = word, count0, count0_ = count_)) %>%
        left_join(select(monograms, label1, label2, word1 = word, count1, count1_ = count_)) %>%
        mutate(count0 = count0-split, count0_ = count0_-1-count0,
               count1 = count1-split, count1_ = count1_-1-count1,
               prob0 = log(count0+1) - log(count0_+1),
               prob1 = log(count1+1) - log(count1_+1),
               prob = prob_ + prob0 + prob1,
               change = split != (prob > 0),
               split = prob > 0,
               paragraph = cumsum(replace_na(split, TRUE)))

    changes = ( prob %>%
                ungroup() %>%
                summarize(n = sum(replace_na(change, 0))) )$n
    tries = tries+1
}


## Collapse text on combined lines for final results.
##
##    item chunk paragraph text                                                                                          page  line
##   <int> <int>     <int> <chr>                                                                                        <dbl> <int>
## 1     1     1         1 N43 50 10 W79 01 02 Adj 11°W (2013) UTC-5(4) Elev 300’ VTA A5000                                 3     1
## 2     2     1         1 Ajax Pickering Gen Hosp 905-683-2320 Cert PPR                                                    3     1
## 3     3     1         1 NOTAM FILE CYOO                                                                                  3     1
## 4     4     1         1 London 866-WXBRIEF (Toll free within Canada) or 866-541-4104 (Toll free within Canada & USA)     3     1
## 5     5     1         1 FATO/TLOF 86’ x 86’ ASPH/GRASS Safety Area 114’ x 114’ GRASS                                     3     1

final = prob %>%
    arrange(item, page, line, chunk) %>%
    group_by(item, chunk, paragraph) %>%
    summarize(text = str_flatten(text, ' '),
              page = first(page),
              line = first(line))


## ----------------------------------------------------------------------------------------------------------------
## SAMPLE OUTPUT

## An example of extracting the longitude and latitude in decimal degrees and elevation.
##
##   aerodrome latitude longitude
##   <fct>        <dbl>     <dbl>
## 1 CPE2          43.8      79.0
## 2 CNS4          45.3      74.6
## 3 CAP2          44.2      81.0
## 4 CPJ2          44.1      79.8
## 5 CPZ2          44.2      79.9

locations = final %>%
    arrange(item, page, line, chunk) %>%
    group_by(item) %>%
    summarize(text = str_flatten(text, ' '),
              page = first(page),
              line = first(line)) %>%
    left_join(select(labels, item, aerodrome, label1, label2)) %>%
    filter(label1 == 'REF', is.na(label2)) %>%
    mutate(location = str_match(text,
                                str_c('N(?<latD>\\d{2}) (?<latM>\\d{2})(?: (?<latS>\\d{2}))? ',
                                      'W(?<lonD>\\d{2,3}) (?<lonM>\\d{2})(?: (?<lonS>\\d{2}))?')) %>%
               as_tibble(.name_repair = 'unique_quiet'),
           elevation = as.integer(str_extract(text, 'Elev (-?\\d+).?', 1))) %>%
    unpack(location) %>%
    mutate(across(starts_with('lat') | starts_with('lon'),  as.integer),
           latitude  =  latD + latM/60 + replace_na(latS, 0)/3600,
           longitude = -lonD - lonM/60 - replace_na(lonS, 0)/3600) %>%
    select(aerodrome, latitude, longitude, elevation)


## An example of extracting the runways and their directions, lengths, and widths.
##
##   aerodrome  item  page  line direction1 side1 direction2 side2 length width
##   <fct>     <int> <dbl> <int>      <dbl> <fct>      <dbl> <fct>  <int> <int>
## 1 CNS4         17     4     1         70 NA           250 NA      2020   100
## 2 CAP2         27     5     1        176 NA           356 NA      2257    75
## 3 CNY4         60     8     1        180 NA           360 NA      2300    50
## 4 CKB6         79    10     1        120 NA           300 NA      3609   100
## 5 CYYW         91    11     1        123 NA           303 NA      4006   100

runways = final %>%
    arrange(item, page, line, chunk) %>%
    group_by(item) %>%
    summarize(text = str_flatten(text, ' '),
              page = first(page),
              line = first(line)) %>%
    left_join(select(labels, item, aerodrome, label1, label2)) %>%
    filter(label1 == 'RWY DATA', is.na(label2)) %>%
    mutate(runway = str_match_all(text,
                                  str_c('Rwy ',
                                        '(?<dir21>[0-9]{2})(?<side1>[LR])? ?(?:\\((?<dir31>[0-9]{3}).?\\))?/',
                                        '(?<dir22>[0-9]{2})(?<side2>[LR])? ?(?:\\((?<dir32>[0-9]{3}).?\\))? ',
                                        '(?<length>[0-9,]+)[xX](?<width>[0-9,]+)')) %>%
               map(as_tibble,.name_repair = 'unique_quiet')) %>%
    unnest(runway) %>%
    mutate(across(starts_with('dir') | length | width,  \(s) as.integer(str_remove_all(s, ','))),
           side1 = fct(side1), side2 = fct(side2),
           direction1 = coalesce(dir31, dir21*10),
           direction2 = coalesce(dir32, dir22*10)) %>%
    select(aerodrome, item, page, line, direction1, side1, direction2, side2, length, width)


## An example of extracting the contact frequencies.
##
##   aerodrome  page  line type  frequency
##   <fct>     <dbl> <int> <fct> <chr>
## 1 CNS4          4     1 ATF   123.2
## 2 CAP2          5     1 ATF   122.8
## 3 CPJ2          6     1 ATF   123.2
## 4 CNY4          8     1 ATF   123.2
## 5 CKB6         10     1 ATF   123.2

comms = final %>%
    arrange(item, page, line, chunk) %>%
    group_by(item) %>%
    summarize(text = str_flatten(text, ' '),
              page = first(page),
              line = first(line)) %>%
    left_join(select(labels, item, aerodrome, label1, label2)) %>%
    filter(label1 == 'COMM') %>%
    mutate(frequency = str_extract(text, '[0-9]{3}\\.[0-9]{1,3}'),
           contact = label2) %>%
    select(aerodrome, item, page, line, contact, frequency)


## An example of extracting the images (requires imagemagik).
##
##  aerodrome  page command                                                                   result
##  <fct>     <dbl> <chr>                                                                     <chr>
## 1 CPE2          3 magick -density 288 -extract 552x532+804+222 ecfs_04_en.pdf'[2]' CPE2.jpg magick -density 288 -extract 552x532+804+222 ecfs_04_en.pdf'[2]' CPE2.jpg
## 2 CNS4          4 magick -density 288 -extract 552x532+804+222 ecfs_04_en.pdf'[3]' CNS4.jpg magick -density 288 -extract 552x532+804+222 ecfs_04_en.pdf'[3]' CNS4.jpg
## 3 CAP2          5 magick -density 288 -extract 552x532+804+222 ecfs_04_en.pdf'[4]' CAP2.jpg magick -density 288 -extract 552x532+804+222 ecfs_04_en.pdf'[4]' CAP2.jpg
## 4 CPJ2          6 magick -density 288 -extract 552x532+804+222 ecfs_04_en.pdf'[5]' CPJ2.jpg magick -density 288 -extract 552x532+804+222 ecfs_04_en.pdf'[5]' CPJ2.jpg
## 5 CPZ2          7 magick -density 288 -extract 552x532+804+222 ecfs_04_en.pdf'[6]' CPZ2.jpg magick -density 288 -extract 552x532+804+222 ecfs_04_en.pdf'[6]' CPZ2.jpg

imagemagik = images %>%
    mutate(aerodrome, page,
           command = sprintf('magick -density 288 -extract %.0fx%.0f+%.0f+%.0f %s.pdf\'[%.0f]\' %s.jpg',
                           (x1-x0)*4, (y1-y0)*4, x0*4, y0*4, basename, page-1, aerodrome),
           result = map_int(command, system),
           .keep = 'none')


