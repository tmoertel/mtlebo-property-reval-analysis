#!/usr/bin/Rscript

### Analysis of 2012/2013 real-estate assessments for Mt. Lebanon, Pennsylvania
### Tom Moertel <tom@moertel.org>
###
### Sources:
###
### [1] Allegheny County 2012 "base-year" assessments
### http://www2.county.allegheny.pa.us/RealEstate/Default.aspx
###
### [2] Allegheny County 2013 "reval" assessments
### http://www2.alleghenycounty.us/reval/


library(ggplot2)


### Load the data set

baseval <- read.csv("data/mtlebo_baseval.csv", sep = "|")    # 2012
reval   <- read.csv("data/mtlebo_reval.csv", sep = "|")      # 2013 reassessment
bothval <- merge(baseval, reval, by = "Parcel_ID", all = T)  # combined



### Compute county and muni/SD tax bases and the corresponding
### "anti-windfall" adjustment factors to keep the pre- and
### post-reassessment taxes about the same ("revenue neutral")

subj_2012 <- subset(baseval, Tax_Code == "Taxable")
subj_2013 <- subset(reval, Tax_Code == "Taxable")


HOMESTEAD_ADJUSTMENT <- -15000

## We don't actually know the county adjustment, so we use Mt. Lebanon
## as a proxy for the county, which isn't right but not wrong enough to
## make our approximations unreasonable
county_base_2012 <- sum(as.numeric(subj_2012$Total_Value_2012_Cty), na.rm = T)
county_base_2013 <- sum(as.numeric(subj_2013$Total_Value_2013_Reval) +
                        HOMESTEAD_ADJUSTMENT * (reval$Homestead == "Yes"),
                        na.rm = T)

## Muni and school district don't use the homestead exemption
muni_sd_base_2012 <- sum(as.numeric(subj_2012$Total_Value_2012_Mkt), na.rm = T)
muni_sd_base_2013 <- sum(as.numeric(subj_2013$Total_Value_2013_Reval), na.rm = T)

## The adjustment factors that keep county and muni/sd taxes "revenue neutral"
anti_windfall_adj_county  <- county_base_2012 / county_base_2013
anti_windfall_adj_muni_sd <- muni_sd_base_2012 / muni_sd_base_2013

## We can compute a composite adjustment for your total tax rate as
## a weighted average of the county, muni, and SD
county_rate <-  5.69
muni_rate   <-  5.43
sd_rate     <- 26.63

anti_windfall_adj_composite <- ((county_rate * anti_windfall_adj_county +
                                 sd_rate     * anti_windfall_adj_muni_sd +
                                 muni_rate   * anti_windfall_adj_muni_sd) /
                                (county_rate + sd_rate + muni_rate))


## Compute assessment and property-tax increases
##
## Notes:
##
## - _Cty values incorporate homestead exemption, used by county
## - _Mkt values represent "Full Market" values, used for muni and SD
## - _Reval values represent "Full Market" values *after* 2013 reassessment

reval_effects <-
  with(bothval,
       mutate(data.frame(PIN = gsub("-", "", Parcel_ID),
                         State_Code = State_Code.y,
                         Homestead = Homestead.y,
                         Land_Value_2012_Mkt,
                         Land_Value_2013_Reval,
                         Building_Value_2012_Mkt,
                         Building_Value_2013_Reval,
                         Total_Value_2012_Cty,  # already includes homestead exmp
                         Total_Value_2012_Mkt,
                         Total_Value_2013_Reval,
                         Total_Value_2013_Reval_Cty =
                           (Total_Value_2013_Reval +
                            (HOMESTEAD_ADJUSTMENT * (Homestead.y == "Yes")))),
              ## Relative assessment
              rel_asm = ((county_rate * (Total_Value_2013_Reval_Cty /
                                         Total_Value_2012_Cty) +
                          (sd_rate + muni_rate) * (Total_Value_2013_Reval /
                                                   Total_Value_2012_Mkt))
                         / (county_rate + sd_rate + muni_rate)),
              ## Increase in assessment
              asm_increase = rel_asm - 1.0,
              ## Increase in property taxes (estimated)
              ptx_increase = anti_windfall_adj_composite * rel_asm - 1.0))

write.csv(reval_effects, file = "data/mtlebo_reval_effects.csv", row.names = F)



### Now we compute the overall distribution of tax increases and
### plot some visualizations of them.

## Given a data frame and ddply-style arguments, partition the frame
## using ddply and summarize the values in each partition with a
## quantized ecdf.  The resulting data frame for each partition has
## two columns: value and value_ecdf.

dd_ecdf <- function(df, ..., .quantizer = identity, .value = value) {
  value_colname <- deparse(substitute(.value))
  ddply(df, ..., .fun = function(rdf) {
    xs <- rdf[[value_colname]]
    qxs <- sort(unique(.quantizer(xs)))
    data.frame(value = qxs, value_ecdf = ecdf(xs)(qxs))
  })
}

rounder <- function(...) function(x) round_any(x, ...)


## Focus on just commercial and residential properties
com_res <- transform(subset(reval_effects,
                            State_Code %in% c("Commercial", "Residential")),
                     State_Code = factor(as.character(State_Code)))

## Compute cumulative distribution
com_res_cdf <- dd_ecdf(com_res, .(State_Code), .value = ptx_increase,
                       .quantizer = rounder(0.0025))


## Plot the cumulative distribution of property-tax increases
p <-
qplot(value, value_ecdf, geom = "step", color = State_Code,
      data = com_res_cdf,
      main = "Reassessment raises taxes for half of Lebo residences",
      xlab = "Effective change in property taxes (% increase)",
      ylab = "Percentage of properties having the given increase or less") +
  scale_x_continuous(formatter = "percent", lim = c(-1, 1)) +
  scale_y_continuous(formatter = "percent") +
  scale_color_discrete(name = "Property type")

ggsave(p, file = "out/mtlebo-reval-property-tax-increases-ecdf.pdf",
       height = 5, width = 7)

## Histogram
p <-
qplot(ptx_increase, binwidth = 0.025,
      data = subset(com_res, ptx_increase > -10 & ptx_increase < 2),
      facets = State_Code ~ .) +
  scale_x_continuous(formatter = "percent", lim = c(-1, 1))



### Summary statistics;
### What percentage of properties will have their property taxes go up/down?

## Method 1:  Use the full data set
ptx_increase_cdf <- ecdf(reval_effects$ptx_increase)
print("percentage of properties (all) that will have lower taxes:")
ptx_increase_cdf(0)        # --> 0.52    (any decrease)
1 - ptx_increase_cdf(0.25) # --> 0.12    (increase by more than 25%)
ptx_increase_cdf(-0.25)    # --> 0.06    (decrase by more than 25%)
ptx_increase_cdf(0.25) - ptx_increase_cdf(-0.25)  # --> 0.82  (|change| <= 25%)

## Method 2:  Drop properties w/ 2013 assessment < $3K
reval_effects_3k <- subset(reval_effects, Total_Value_2013_Reval_Mkt >= 3000)
ptx_3k_increase_cdf <- ecdf(reval_effects_3k$ptx_increase)
print("percentage of properties (>= $3K) that will have lower taxes:")
ptx_3k_increase_cdf(0)  # --> 0.51
