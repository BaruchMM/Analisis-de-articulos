Análisis estadístico de Age-dependent Gender Differences in COVID-19 in
Mainland China
================
Baruch Mejía Martínez
19/10/2021

Paquetes necesarios:

``` r
library(questionr)
```

## Razón de riesgo de la tasa de ataque por sexo en cada región

Cargamos los datos utilizados en el artículo

``` r
attack = read.csv("Attack.csv",header=TRUE,sep=",")
n=12
attack
```

    ##       Region Fem_Positivo Fem_Negativo Total_fem Mas_Positivo Mas_Negativo
    ## 1      Hubei        34726     27811754  27846480        33402     29357845
    ## 2      Henan          581     46536295  46536876          695     47492368
    ## 3   Zhejiang          608     26460642  26461250          660     27964981
    ## 4  Guangdong          761     49919160  49919921          827     54399711
    ## 5      Hunan          494     31923809  31924303          525     33775934
    ## 6      Anhui          458     29254497  29254955          533     30244980
    ## 7    Jiangxi          445     21563831  21564276          492     23003029
    ## 8   Shandong          255     47345520  47345775          532     48446412
    ## 9    Jiangsu          298     39033936  39034234          355     39626352
    ## 10 Chongqing          275     14237025  14237300          304     14608566
    ## 11   Sichuan          263     39589431  39589694          298     40827536
    ## 12    Others         2416    276764285 276766701         2655    292540112
    ##    Total_Mas
    ## 1   29391247
    ## 2   47493063
    ## 3   27965641
    ## 4   54400538
    ## 5   33776459
    ## 6   30245513
    ## 7   23003521
    ## 8   48446944
    ## 9   39626707
    ## 10  14608870
    ## 11  40827834
    ## 12 292542767

Calculamos el RR para cada grupo de región.

``` r
RiskRatio=c()
Min=c()
Max=c()
p_value=c()
region=c()
for (i in 1:n){
  region=append(region,attack[i,1])
  M <- matrix(c(attack[i,2],attack[i,4],attack[i,5],attack[i,7]), ncol = 2)
  RR = round(odds.ratio(M)[1,1],4)
  RiskRatio = append(RiskRatio,RR)
  Min=append(Min,round(odds.ratio(M)[1,2],4))
  Max=append(Max,round(odds.ratio(M)[1,3],4))
  p_value=append(p_value,round(odds.ratio(M)[1,4],4))
}
Razon_de_riesgo=data.frame(region,RiskRatio,Min,Max,p_value)
Razon_de_riesgo
```

    ##       region RiskRatio    Min    Max p_value
    ## 1      Hubei    1.0973 1.0809 1.1139  0.0000
    ## 2      Henan    0.8531 0.7628 0.9539  0.0047
    ## 3   Zhejiang    0.9736 0.8706 1.0886  0.6531
    ## 4  Guangdong    1.0028 0.9075 1.1079  0.9599
    ## 5      Hunan    0.9955 0.8786 1.1278  0.9500
    ## 6      Anhui    0.8884 0.7824 1.0085  0.0654
    ## 7    Jiangxi    0.9648 0.8468 1.0991  0.6010
    ## 8   Shandong    0.4905 0.4208 0.5705  0.0000
    ## 9    Jiangsu    0.8522 0.7281 0.9969  0.0419
    ## 10 Chongqing    0.9282 0.7856 1.0963  0.3829
    ## 11   Sichuan    0.9102 0.7682 1.0779  0.2724
    ## 12    Others    0.9618 0.9099 1.0167  0.1686

``` r
Razon_de_riesgo=matrix(c(region,RiskRatio,Min,Max,attack[,2],attack[,5]),ncol = 6)
Razon_de_riesgo
```

    ##       [,1]        [,2]     [,3]     [,4]     [,5]    [,6]   
    ##  [1,] "Hubei"     "1.0973" "1.0809" "1.1139" "34726" "33402"
    ##  [2,] "Henan"     "0.8531" "0.7628" "0.9539" "581"   "695"  
    ##  [3,] "Zhejiang"  "0.9736" "0.8706" "1.0886" "608"   "660"  
    ##  [4,] "Guangdong" "1.0028" "0.9075" "1.1079" "761"   "827"  
    ##  [5,] "Hunan"     "0.9955" "0.8786" "1.1278" "494"   "525"  
    ##  [6,] "Anhui"     "0.8884" "0.7824" "1.0085" "458"   "533"  
    ##  [7,] "Jiangxi"   "0.9648" "0.8468" "1.0991" "445"   "492"  
    ##  [8,] "Shandong"  "0.4905" "0.4208" "0.5705" "255"   "532"  
    ##  [9,] "Jiangsu"   "0.8522" "0.7281" "0.9969" "298"   "355"  
    ## [10,] "Chongqing" "0.9282" "0.7856" "1.0963" "275"   "304"  
    ## [11,] "Sichuan"   "0.9102" "0.7682" "1.0779" "263"   "298"  
    ## [12,] "Others"    "0.9618" "0.9099" "1.0167" "2416"  "2655"

``` r
write.csv(Razon_de_riesgo , 'plotear_Riesgo.csv',row.names = F, col.names = F)
```

    ## Warning in write.csv(Razon_de_riesgo, "plotear_Riesgo.csv", row.names = F, :
    ## attempt to set 'col.names' ignored

## Casos severos y críticos por sexo en cada región

Cargamos los datos utilizados en el artículo

``` r
severe = read.csv("Severe_Critical.csv",header=TRUE,sep=",")
n=12
severe
```

    ##       Region Fem_Positivo Fem_Negativo Total_fem Mas_Positivo Mas_Negativo
    ## 1      Hubei         6212        28514     34726         6897        26505
    ## 2      Henan           90          491       581          125          570
    ## 3   Zhejiang           74          534       608          122          538
    ## 4  Guangdong           43          718       761           79          748
    ## 5      Hunan           60          434       494           97          428
    ## 6      Anhui           57          401       458           90          443
    ## 7    Jiangxi           48          397       445           81          411
    ## 8   Shandong           26          229       255           42          490
    ## 9    Jiangsu           19          279       298           37          318
    ## 10 Chongqing           43          232       275           60          244
    ## 11   Sichuan           42          221       263           53          245
    ## 12    Others          303         2113      2416          362         2293
    ##    Total_Mas
    ## 1      33402
    ## 2        695
    ## 3        660
    ## 4        827
    ## 5        525
    ## 6        533
    ## 7        492
    ## 8        532
    ## 9        355
    ## 10       304
    ## 11       298
    ## 12      2655

Calculamos el RR para cada grupo de región.

``` r
OddsRatio=c()
Min=c()
Max=c()
p_value=c()
region=c()
for (i in 1:n){
  region=append(region,severe[i,1])
  M <- matrix(c(severe[i,2],severe[i,4],severe[i,5],severe[i,7]), ncol = 2)
  OR = round(odds.ratio(M)[1,1],4)
  OddsRatio = append(OddsRatio,OR)
  Min=append(Min,round(odds.ratio(M)[1,2],4))
  Max=append(Max,round(odds.ratio(M)[1,3],4))
  p_value=append(p_value,round(odds.ratio(M)[1,4],4))
}
Severe_and_Critical=data.frame(region,OddsRatio,Min,Max,p_value)
Severe_and_Critical
```

    ##       region OddsRatio    Min    Max p_value
    ## 1      Hubei    0.8663 0.8344 0.8995  0.0000
    ## 2      Henan    0.8614 0.6349 1.1651  0.3358
    ## 3   Zhejiang    0.6586 0.4763 0.9060  0.0088
    ## 4  Guangdong    0.5917 0.3930 0.8809  0.0082
    ## 5      Hunan    0.6576 0.4574 0.9396  0.0202
    ## 6      Anhui    0.7372 0.5072 1.0648  0.0925
    ## 7    Jiangxi    0.6554 0.4384 0.9717  0.0303
    ## 8   Shandong    1.2911 0.7422 2.2104  0.3471
    ## 9    Jiangsu    0.6121 0.3252 1.1188  0.0949
    ## 10 Chongqing    0.7925 0.5050 1.2357  0.2864
    ## 11   Sichuan    0.8981 0.5643 1.4223  0.6576
    ## 12    Others    0.9199 0.7792 1.0853  0.3217

``` r
Severe_and_Critical=matrix(c(region,OddsRatio,Min,Max,severe[,2],severe[,5]),ncol = 6)
Severe_and_Critical
```

    ##       [,1]        [,2]     [,3]     [,4]     [,5]   [,6]  
    ##  [1,] "Hubei"     "0.8663" "0.8344" "0.8995" "6212" "6897"
    ##  [2,] "Henan"     "0.8614" "0.6349" "1.1651" "90"   "125" 
    ##  [3,] "Zhejiang"  "0.6586" "0.4763" "0.906"  "74"   "122" 
    ##  [4,] "Guangdong" "0.5917" "0.393"  "0.8809" "43"   "79"  
    ##  [5,] "Hunan"     "0.6576" "0.4574" "0.9396" "60"   "97"  
    ##  [6,] "Anhui"     "0.7372" "0.5072" "1.0648" "57"   "90"  
    ##  [7,] "Jiangxi"   "0.6554" "0.4384" "0.9717" "48"   "81"  
    ##  [8,] "Shandong"  "1.2911" "0.7422" "2.2104" "26"   "42"  
    ##  [9,] "Jiangsu"   "0.6121" "0.3252" "1.1188" "19"   "37"  
    ## [10,] "Chongqing" "0.7925" "0.505"  "1.2357" "43"   "60"  
    ## [11,] "Sichuan"   "0.8981" "0.5643" "1.4223" "42"   "53"  
    ## [12,] "Others"    "0.9199" "0.7792" "1.0853" "303"  "362"

``` r
write.csv(Severe_and_Critical , 'plotear_severe.csv',row.names = F, col.names = F)
```

    ## Warning in write.csv(Severe_and_Critical, "plotear_severe.csv", row.names = F, :
    ## attempt to set 'col.names' ignored

## Casos fatales por sexo en cada región

Cargamos los datos utilizados en el artículo

``` r
fatal = read.csv("Fatal.csv",header=TRUE,sep=",")
n=12
fatal
```

    ##       Region Fem_Positivo Fem_Negativo Total_fem Mas_Positivo Mas_Negativo
    ## 1      Hubei         1635        33091     34726         2877        30525
    ## 2      Henan            6          575       581           16          679
    ## 3   Zhejiang            1          607       608            0          660
    ## 4  Guangdong            0          761       761            8          819
    ## 5      Hunan            2          492       494            2          523
    ## 6      Anhui            1          457       458            5          528
    ## 7    Jiangxi            1          444       445            0          492
    ## 8   Shandong            4          251       255            3          529
    ## 9    Jiangsu            0          298       298            0          355
    ## 10 Chongqing            4          271       275            2          302
    ## 11   Sichuan            2          261       263            1          297
    ## 12    Others           25         2391      2416           38         2617
    ##    Total_Mas
    ## 1      33402
    ## 2        695
    ## 3        660
    ## 4        827
    ## 5        525
    ## 6        533
    ## 7        492
    ## 8        532
    ## 9        355
    ## 10       304
    ## 11       298
    ## 12      2655

Calculamos el RR para cada grupo de región.

``` r
OddsRatio=c()
Min=c()
Max=c()
p_value=c()
region=c()
for (i in 1:n){
  region=append(region,fatal[i,1])
  M <- matrix(c(fatal[i,2],fatal[i,4],fatal[i,5],fatal[i,7]), ncol = 2)
  if (fatal[i,2]==0 ||fatal[i,4]==0 ||fatal[i,5]==0 ||fatal[i,7]==0 ){
    OR = 0
    OddsRatio = append(OddsRatio,OR)
    Min=append(Min,0)
    Max=append(Max,0)
    p_value=append(p_value,0)
  } else {
    OR = round(odds.ratio(M)[1,1],4)
    OddsRatio = append(OddsRatio,OR)
    Min=append(Min,round(odds.ratio(M)[1,2],4))
    Max=append(Max,round(odds.ratio(M)[1,3],4))
    p_value=append(p_value,round(odds.ratio(M)[1,4],4))
  }

}
fatales=data.frame(region,OddsRatio,Min,Max,p_value)
fatales
```

    ##       region OddsRatio    Min      Max p_value
    ## 1      Hubei    0.5466 0.5132   0.5822  0.0000
    ## 2      Henan    0.4488 0.1429   1.2173  0.1288
    ## 3   Zhejiang    0.0000 0.0000   0.0000  0.0000
    ## 4  Guangdong    0.0000 0.0000   0.0000  0.0000
    ## 5      Hunan    1.0627 0.0768  14.7084  1.0000
    ## 6      Anhui    0.2330 0.0049   2.0933  0.2262
    ## 7    Jiangxi    0.0000 0.0000   0.0000  0.0000
    ## 8   Shandong    2.7778 0.4662  19.1027  0.2238
    ## 9    Jiangsu    0.0000 0.0000   0.0000  0.0000
    ## 10 Chongqing    2.2080 0.3137  24.5880  0.4319
    ## 11   Sichuan    2.2630 0.1172 134.0330  0.6031
    ## 12    Others    0.7230 0.4170   1.2335  0.2532

``` r
fatales=matrix(c(region,OddsRatio,Min,Max,fatal[,2],fatal[,5]),ncol = 6)
fatales
```

    ##       [,1]        [,2]     [,3]     [,4]      [,5]   [,6]  
    ##  [1,] "Hubei"     "0.5466" "0.5132" "0.5822"  "1635" "2877"
    ##  [2,] "Henan"     "0.4488" "0.1429" "1.2173"  "6"    "16"  
    ##  [3,] "Zhejiang"  "0"      "0"      "0"       "1"    "0"   
    ##  [4,] "Guangdong" "0"      "0"      "0"       "0"    "8"   
    ##  [5,] "Hunan"     "1.0627" "0.0768" "14.7084" "2"    "2"   
    ##  [6,] "Anhui"     "0.233"  "0.0049" "2.0933"  "1"    "5"   
    ##  [7,] "Jiangxi"   "0"      "0"      "0"       "1"    "0"   
    ##  [8,] "Shandong"  "2.7778" "0.4662" "19.1027" "4"    "3"   
    ##  [9,] "Jiangsu"   "0"      "0"      "0"       "0"    "0"   
    ## [10,] "Chongqing" "2.208"  "0.3137" "24.588"  "4"    "2"   
    ## [11,] "Sichuan"   "2.263"  "0.1172" "134.033" "2"    "1"   
    ## [12,] "Others"    "0.723"  "0.417"  "1.2335"  "25"   "38"

``` r
write.csv(fatales , 'plotear_fatal.csv',row.names = F, col.names = F)
```

    ## Warning in write.csv(fatales, "plotear_fatal.csv", row.names = F, col.names =
    ## F): attempt to set 'col.names' ignored
