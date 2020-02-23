    #all libraries needed
    library(blscrapeR)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)
    library(data.table)

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    library(stats)

    county_employ<-get_bls_county("January 2019")
    county_employ18<-get_bls_county("December 2018")%>% 
      rename(unemployedprev = unemployed, unemployrateprev = unemployed_rate) %>% 
      select(fips, unemployedprev,unemployrateprev)

    ##load previous downloaded txt files to R, with interested columns & bridges
    #copy previous downloaded files to current working directory
    previousfolder<- "/media/hannah/CE59-DABE/madison exchange/19_20 spring/stat479/githw/bridges-SzuHannah/"
    currentfolder<-getwd()
    #txt file list
    mybridge<-list.files(previousfolder,pattern = "*.txt")
    lapply(mybridge, function(x) {
      file.copy(from = paste0(previousfolder,x), to = currentfolder)
      })

    #preferred state
    states = c("55","06","26","36","48")
    #read selected columns and rows from all bridge files
    multibridgelist<-
      lapply(mybridge, function(x){
        x%>%
          fread(select = c("STATE_CODE_001","COUNTY_CODE_003","ADT_029","YEAR_ADT_030","VERT_CLR_OVER_MT_053","MAIN_UNIT_SPANS_045"),
                colClasses = 'character')%>%
          filter(STATE_CODE_001 %in% states)}
      )
    #name elements in list
    names(multibridgelist)<-gsub("\\.txt$","",mybridge)

    summultibridge<-
      lapply(multibridgelist, function(x){
        x%>%
          mutate(ADT_029 = as.numeric(ADT_029),VERT_CLR_OVER_MT_053 = as.numeric(VERT_CLR_OVER_MT_053),
                 MAIN_UNIT_SPANS_045 = as.numeric(MAIN_UNIT_SPANS_045),fips = paste0(STATE_CODE_001,COUNTY_CODE_003))%>%
          group_by(fips)%>%
          summarise(countyADT=mean(ADT_029),verticalclearance = mean(VERT_CLR_OVER_MT_053),spans = mean(MAIN_UNIT_SPANS_045))
      })

    #2018 bridge data(WI, MI, CA, NY, TX) with desired prdictor
    multibridge18<-summultibridge[["2018HwyBridgesDelimitedAllStates"]]
    multibridge18

    ## # A tibble: 527 x 4
    ##    fips  countyADT verticalclearance spans
    ##    <chr>     <dbl>             <dbl> <dbl>
    ##  1 06         642.             100.   3   
    ##  2 06001    55829.              95.9  4.46
    ##  3 06003     1130.             100.   1.44
    ##  4 06005     3644.             100.   2.28
    ##  5 06007     5131.              98.9  3.16
    ##  6 06009     2178.              99.1  2.30
    ##  7 06011     3456.              99.4  2.67
    ##  8 06013    31386.              98.0  2.96
    ##  9 06015     1912.              99.0  2.57
    ## 10 06017     7852.              98.4  1.80
    ## # … with 517 more rows

    #choose state for employ
    multistateemploy<-
    county_employ%>%
      filter(fips_state %in% states)

    #leftjoin 2018bridge data with countyemploy data
    employ_bridgepredictors<-multibridge18%>%left_join(county_employ, by = "fips")%>%left_join(county_employ18,by = "fips")

Build Multiple linear regression model:  
(1)Predictors: Bridge data(2018); outcome: unemployment data(2019.01)

    #lm1: y = number of unemployed; x1 = Average Daily traffic(item29); x2=minimum vertical clearance over bridge(item53); x3=number of spans in main unit(item45)
    lm1<-lm(unemployed~countyADT+verticalclearance+spans, data = employ_bridgepredictors)
    summary(lm1)

    ## 
    ## Call:
    ## lm(formula = unemployed ~ countyADT + verticalclearance + spans, 
    ##     data = employ_bridgepredictors)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -51370  -2081    290   1682 173136 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -1.981e+04  2.023e+04  -0.979    0.328    
    ## countyADT          1.275e+00  6.851e-02  18.611   <2e-16 ***
    ## verticalclearance  1.829e+02  2.019e+02   0.906    0.365    
    ## spans             -3.953e+02  3.345e+02  -1.182    0.238    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10890 on 522 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.451,  Adjusted R-squared:  0.4479 
    ## F-statistic:   143 on 3 and 522 DF,  p-value: < 2.2e-16

    #lm2: y = unemployed rate; x1 = Average Daily traffic(item29); x2=minimum vertical clearance over bridge(item53); x3=number of spans in main unit(item45)
    lm2<-lm(unemployed_rate~countyADT+verticalclearance+spans, data = employ_bridgepredictors)
    summary(lm2)

    ## 
    ## Call:
    ## lm(formula = unemployed_rate ~ countyADT + verticalclearance + 
    ##     spans, data = employ_bridgepredictors)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -2.955 -1.246 -0.451  0.561 15.702 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        1.361e+01  3.824e+00   3.559 0.000407 ***
    ## countyADT         -4.445e-05  1.295e-05  -3.433 0.000645 ***
    ## verticalclearance -7.835e-02  3.816e-02  -2.053 0.040565 *  
    ## spans             -2.256e-01  6.322e-02  -3.568 0.000392 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.058 on 522 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.05107,    Adjusted R-squared:  0.04562 
    ## F-statistic: 9.365 on 3 and 522 DF,  p-value: 4.875e-06

(2)Predictors: Bridge data(2018), unemployment from previous
month(2018.12); outcome: unemployment data(2019.01)

    #lm3: y = number of unemployed; x1 = Average Daily traffic(item29); x2 = minimum vertical clearance over bridge(item53);x3 = number of spans in main unit(item45); x4 =number of unemployed
    lm3<-lm(unemployed~countyADT+verticalclearance+spans+unemployedprev, data = employ_bridgepredictors)
    summary(lm3)

    ## 
    ## Call:
    ## lm(formula = unemployed ~ countyADT + verticalclearance + spans + 
    ##     unemployedprev, data = employ_bridgepredictors)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8547.4  -157.5   -42.4    45.1  5435.5 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        7.667e+03  1.452e+03   5.280 1.90e-07 ***
    ## countyADT          4.713e-02  6.245e-03   7.547 2.00e-13 ***
    ## verticalclearance -7.766e+01  1.449e+01  -5.360 1.25e-07 ***
    ## spans              1.820e+01  2.400e+01   0.758    0.449    
    ## unemployedprev     1.092e+00  3.434e-03 318.054  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 780.3 on 521 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.9972, Adjusted R-squared:  0.9972 
    ## F-statistic: 4.617e+04 on 4 and 521 DF,  p-value: < 2.2e-16

    #lm4: y = unemployed rate; x1 = Average Daily traffic(item29); x2 = minimum vertical clearance over bridge(item53);x3 = number of spans in main unit(item45); x4 = previous unemployed rate
    lm4<-lm(unemployed_rate~countyADT+verticalclearance+spans+unemployrateprev, data = employ_bridgepredictors)
    summary(lm4)

    ## 
    ## Call:
    ## lm(formula = unemployed_rate ~ countyADT + verticalclearance + 
    ##     spans + unemployrateprev, data = employ_bridgepredictors)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.80169 -0.15924 -0.02254  0.15968  1.83363 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        1.891e+00  7.313e-01   2.585   0.0100 *  
    ## countyADT         -5.133e-06  2.476e-06  -2.073   0.0387 *  
    ## verticalclearance -1.569e-02  7.250e-03  -2.164   0.0309 *  
    ## spans             -2.657e-02  1.210e-02  -2.196   0.0285 *  
    ## unemployrateprev   1.131e+00  9.549e-03 118.390   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3901 on 521 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.966,  Adjusted R-squared:  0.9657 
    ## F-statistic:  3700 on 4 and 521 DF,  p-value: < 2.2e-16
