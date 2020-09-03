Homework4
================

**Questão 1 **

``` r
library(nycflights13)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
flights<-nycflights13::flights

flights_min = flights

nova <- flights_min$dep_time*0.010
nova <- as.integer(nova)
nova <- nova*100
flights_min$dep_time <- flights_min$dep_time-nova
nova <- nova*0.6
flights_min$dep_time <- flights_min$dep_time+nova

nova <- flights_min$arr_time*0.010
nova <- as.integer(nova)
nova <- nova*100
flights_min$arr_time <- flights_min$arr_time-nova
nova <- nova*0.6
flights_min$arr_time <- flights_min$arr_time+nova

nova <- flights_min$sched_dep_time*0.010
nova <- as.integer(nova)
nova <- nova*100
flights_min$sched_dep_time <- flights_min$sched_dep_time-nova
nova <- nova*0.6
flights_min$sched_dep_time <- flights_min$sched_dep_time+nova

nova <- flights_min$sched_arr_time*0.010
nova <- as.integer(nova)
nova <- nova*100
flights_min$sched_arr_time <- flights_min$sched_arr_time-nova
nova <- nova*0.6
flights_min$sched_arr_time <- flights_min$sched_arr_time+nova


print("Atraso na chegada de duas horas ou mais")
```

    ## [1] "Atraso na chegada de duas horas ou mais"

``` r
filter(flights_min,(arr_delay/60)>=2)
```

    ## # A tibble: 10,200 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <dbl>          <dbl>     <dbl>    <dbl>          <dbl>
    ##  1  2013     1     1      491            390       101      647            510
    ##  2  2013     1     1      528           1115       853      601           1190
    ##  3  2013     1     1      597            453       144      656            533
    ##  4  2013     1     1      674            540       134      887            742
    ##  5  2013     1     1      905            790       115      998            871
    ##  6  2013     1     1      925            820       105     1111            986
    ##  7  2013     1     1      949            885        64     1152           1016
    ##  8  2013     1     1      958            839       119     1038            915
    ##  9  2013     1     1     1052            990        62     1228           1105
    ## 10  2013     1     1     1083            980       103     1208           1070
    ## # ... with 10,190 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
print("Voaram com destino a Houston")
```

    ## [1] "Voaram com destino a Houston"

``` r
filter(flights_min,dest == "IAH"|dest == "HOU")
```

    ## # A tibble: 9,313 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <dbl>          <dbl>     <dbl>    <dbl>          <dbl>
    ##  1  2013     1     1      317            315         2      510            499
    ##  2  2013     1     1      333            329         4      530            510
    ##  3  2013     1     1      383            387        -4      573            572
    ##  4  2013     1     1      448            452        -4      641            638
    ##  5  2013     1     1      459            459         0      664            638
    ##  6  2013     1     1      548            548         0      748            739
    ##  7  2013     1     1      628            626         2      830            819
    ##  8  2013     1     1      644            645        -1      832            831
    ##  9  2013     1     1      674            540       134      887            742
    ## 10  2013     1     1      725            720         5      903            905
    ## # ... with 9,303 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
print("Foram realizadas pelas companhias aereas United, American ou Delta Airlines")
```

    ## [1] "Foram realizadas pelas companhias aereas United, American ou Delta Airlines"

``` r
filter(flights_min,carrier == "UA"|dest == "DL" | dest =="AA")
```

    ## # A tibble: 58,665 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <dbl>          <dbl>     <dbl>    <dbl>          <dbl>
    ##  1  2013     1     1      317            315         2      510            499
    ##  2  2013     1     1      333            329         4      530            510
    ##  3  2013     1     1      354            358        -4      460            448
    ##  4  2013     1     1      358            360        -2      564            557
    ##  5  2013     1     1      358            360        -2      563            577
    ##  6  2013     1     1      359            360        -1      534            542
    ##  7  2013     1     1      367            367         0      538            555
    ##  8  2013     1     1      371            360        11      585            571
    ##  9  2013     1     1      383            387        -4      573            572
    ## 10  2013     1     1      388            390        -2      616            587
    ## # ... with 58,655 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
print("Partiram no verão(julho, agosto, setembro)")
```

    ## [1] "Partiram no verão(julho, agosto, setembro)"

``` r
filter(flights_min, month == "7"|month == "8"| month == "9" )
```

    ## # A tibble: 86,326 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <dbl>          <dbl>     <dbl>    <dbl>          <dbl>
    ##  1  2013     7     1        1           1229       212      156           1439
    ##  2  2013     7     1        2           1439         3      224            224
    ##  3  2013     7     1       29           1365       104      111              1
    ##  4  2013     7     1       43           1290       193      202             14
    ##  5  2013     7     1       44           1310       174      180             60
    ##  6  2013     7     1       46           1251       235      184           1438
    ##  7  2013     7     1       48           1201       287      188           1385
    ##  8  2013     7     1       58           1315       183      215             43
    ##  9  2013     7     1       60           1306       194      207             30
    ## 10  2013     7     1       60           1365       135      217             95
    ## # ... with 86,316 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
print("Não partiram atrasados, porem chegaram com mais de duas horas de atraso ao destino")
```

    ## [1] "Não partiram atrasados, porem chegaram com mais de duas horas de atraso ao destino"

``` r
filter(flights_min,dep_delay<=0 & (arr_delay/60)>2)
```

    ## # A tibble: 29 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <dbl>          <dbl>     <dbl>    <dbl>          <dbl>
    ##  1  2013     1    27      859            860        -1     1074            950
    ##  2  2013    10     7      830            830         0     1056            926
    ##  3  2013    10     7      837            839        -2     1138           1014
    ##  4  2013    10    16      417            420        -3      778            656
    ##  5  2013    11     1      418            420        -2      809            615
    ##  6  2013     3    18     1124           1127        -3       39           1339
    ##  7  2013     4    17      995           1000        -5     1249           1125
    ##  8  2013     4    18      358            360        -2      709            530
    ##  9  2013     4    18      415            420        -5      733            590
    ## 10  2013     5    22     1107           1110        -3     1337           1210
    ## # ... with 19 more rows, and 11 more variables: arr_delay <dbl>, carrier <chr>,
    ## #   flight <int>, tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>,
    ## #   distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
print("Partiram entre meia noite e seis da manhã")
```

    ## [1] "Partiram entre meia noite e seis da manhã"

``` r
filter(flights_min, (dep_time/60)>=0 & (dep_time/60)<=6)
```

    ## # A tibble: 9,344 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <dbl>          <dbl>     <dbl>    <dbl>          <dbl>
    ##  1  2013     1     1      317            315         2      510            499
    ##  2  2013     1     1      333            329         4      530            510
    ##  3  2013     1     1      342            340         2      563            530
    ##  4  2013     1     1      344            345        -1      604            622
    ##  5  2013     1     1      354            360        -6      492            517
    ##  6  2013     1     1      354            358        -4      460            448
    ##  7  2013     1     1      355            360        -5      553            534
    ##  8  2013     1     1      357            360        -3      429            443
    ##  9  2013     1     1      357            360        -3      518            526
    ## 10  2013     1     1      358            360        -2      473            465
    ## # ... with 9,334 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

**Questão 2 **

``` r
voos <- aggregate(flights_min$arr_delay,by=list(voos=flights_min$flight),FUN=sum)
head(arrange(voos,desc(x)))
```

    ##   voos    x
    ## 1  141 5701
    ## 2  201 4896
    ## 3 1373 4240
    ## 4  263 3755
    ## 5   43 3504
    ## 6  803 3476

**Questão 3 **

``` r
flight_vel <- aggregate(flights_min$distance/flights_min$air_time*60 ,by=list(flight=flights_min$flight),FUN=mean)
head(arrange(flight_vel,desc(x)))
```

    ##   flight        x
    ## 1   5479 502.0482
    ## 2   2591 499.0067
    ## 3   2275 498.5546
    ## 4     88 496.0465
    ## 5    713 492.7780
    ## 6    803 488.1758

**Questão 4 **

``` r
print("Quando a variavel é inserida varias vezes no select, ele retorna a variavel apenas uma vez")
```

    ## [1] "Quando a variavel é inserida varias vezes no select, ele retorna a variavel apenas uma vez"

``` r
head(select(flights,arr_delay,arr_delay,arr_delay))
```

    ## # A tibble: 6 x 1
    ##   arr_delay
    ##       <dbl>
    ## 1        11
    ## 2        20
    ## 3        33
    ## 4       -18
    ## 5       -25
    ## 6        12

**Questão 5 **

``` r
head(arrange(flights_min,desc(dep_delay)))
```

    ## # A tibble: 6 x 19
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <dbl>          <dbl>     <dbl>    <dbl>          <dbl>
    ## 1  2013     1     9      401            540      1301      762            930
    ## 2  2013     6    15      872           1175      1137      967           1280
    ## 3  2013     1    10      681            995      1126      759           1090
    ## 4  2013     9    20      699           1125      1014      897           1330
    ## 5  2013     7    22      525            960      1005      644           1095
    ## 6  2013     4    10      660           1140       960      822           1331
    ## # ... with 11 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
flights_rank <-mutate(flights_min,rank=min_rank(flights_min$dep_delay))
head(flights_rank)
```

    ## # A tibble: 6 x 20
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <dbl>          <dbl>     <dbl>    <dbl>          <dbl>
    ## 1  2013     1     1      317            315         2      510            499
    ## 2  2013     1     1      333            329         4      530            510
    ## 3  2013     1     1      342            340         2      563            530
    ## 4  2013     1     1      344            345        -1      604            622
    ## 5  2013     1     1      354            360        -6      492            517
    ## 6  2013     1     1      354            358        -4      460            448
    ## # ... with 12 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>, rank <int>

**Questão 6 **

``` r
head(mutate(flights_min,mean_air = mean(flights_min$air_time, na.rm = TRUE)))
```

    ## # A tibble: 6 x 20
    ##    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##   <int> <int> <int>    <dbl>          <dbl>     <dbl>    <dbl>          <dbl>
    ## 1  2013     1     1      317            315         2      510            499
    ## 2  2013     1     1      333            329         4      530            510
    ## 3  2013     1     1      342            340         2      563            530
    ## 4  2013     1     1      344            345        -1      604            622
    ## 5  2013     1     1      354            360        -6      492            517
    ## 6  2013     1     1      354            358        -4      460            448
    ## # ... with 12 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
    ## #   hour <dbl>, minute <dbl>, time_hour <dttm>, mean_air <dbl>
