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
                        HOMESTEAD_ADJUSTMENT * (subj_2013$Homestead == "Yes"),
                        na.rm = T)

## Muni and school district don't use the homestead exemption
muni_sd_base_2012 <- sum(as.numeric(subj_2012$Total_Value_2012_Mkt), na.rm = T)
muni_sd_base_2013 <- sum(as.numeric(subj_2013$Total_Value_2013_Reval), na.rm = T)

## The adjustment factors that keep county and muni/sd taxes "revenue neutral"
anti_windfall_adj_county  <- county_base_2012 / county_base_2013
anti_windfall_adj_muni_sd <- muni_sd_base_2012 / muni_sd_base_2013

## Tax rates for local taxing bodies.  We use these to weight the
## contribution of each taxing body to totals.
county_rate <-  5.69
muni_rate   <-  5.43
sd_rate     <- 26.63


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
              rel_asm = ((county_rate * Total_Value_2013_Reval_Cty +
                          (sd_rate + muni_rate) * Total_Value_2013_Reval)
                          /
                          (county_rate * Total_Value_2012_Cty +
                           (sd_rate + muni_rate) * Total_Value_2012_Mkt)),
              ## Increase in assessment
              asm_increase = rel_asm - 1.0,
              ## Relative property taxes (estimated)
              rel_ptx = ((county_rate * anti_windfall_adj_county *
                          Total_Value_2013_Reval_Cty +
                          (sd_rate + muni_rate) * anti_windfall_adj_muni_sd *
                          Total_Value_2013_Reval)
                         /
                         (county_rate * Total_Value_2012_Cty +
                          (sd_rate + muni_rate) * Total_Value_2012_Mkt)),
              ## Increase in property taxes (estimated)
              ptx_increase = rel_ptx - 1))

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


## Neighboorhood analysis:  focus on commercial and residential properties
property_info <- read.csv("data/mtlebo_properties.csv")
property_info <- transform(property_info,
                           Zip_Code = factor(PROPERTYZI),
                           PROPERTYZI = NULL)

neighborhood_info <-
  transform(bothval[, c("Parcel_ID", "Neighborhood_Code.y")],
            PIN = gsub("-", "", Parcel_ID),
            Parcel_ID = NULL,
            Neighborhood = Neighborhood_Code.y,
            Neighborhood_Code.y = NULL)

property_info <- merge(property_info, neighborhood_info)


com_res <- transform(subset(reval_effects,
                            State_Code %in% c("Commercial", "Residential")),
                     State_Code = factor(as.character(State_Code)))
com_res <- merge(com_res, property_info)
com_res <- subset(com_res, ptx_increase < Inf & !is.na(ptx_increase))

res <- transform(subset(com_res, State_Code == "Residential"),
                 Zip_Code = reorder(Zip_Code, ptx_increase, median),
                 Neighborhood = reorder(Neighborhood, ptx_increase, median))



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
qplot(ptx_increase, binwidth = 0.01,
      main = "Some Zip Codes had higher increases than others",
      xlab = "Estimated property-tax increase",
      ylab = "Count of homes having that increase",
      data = subset(res, ptx_increase > -10 & ptx_increase < 2),
      facets = Zip_Code ~ .) +
  scale_x_continuous(formatter = "percent", lim = c(-0.5, 0.5))

p

ggsave(p, file = "out/mtlebo-reval-hist-ptx-incr-by-zip.pdf",
       height = 5, width = 7)


p <-
qplot(ptx_increase, binwidth = 0.025,
      main = "Some Neighborhoods had higher increases than others",
      xlab = "Estimated property-tax increase",
      ylab = "Count of homes having that increase",
      data = subset(res, ptx_increase > -10 & ptx_increase < 2)) +
  scale_x_continuous(formatter = "percent", lim = c(-0.5, 0.5),
                     breaks = c(-0.40, 0, 0.40)) +
  facet_wrap(~ Neighborhood)

p

ggsave(p, file = "out/mtlebo-reval-hist-ptx-incr-by-hood.pdf",
       height = 5, width = 7)


options(digits = 2)

ddply(res, .(Zip_Code), summarize, median_ptx_increase = median(ptx_increase))

ddply(res, .(Neighborhood), summarize,
      median_ptx_increase = median(ptx_increase),
      count = length(ptx_increase))




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
reval_effects_3k <- subset(reval_effects, Total_Value_2013_Reval >= 3000)
ptx_3k_increase_cdf <- ecdf(reval_effects_3k$ptx_increase)
print("percentage of properties (>= $3K) that will have lower taxes:")
ptx_3k_increase_cdf(0)  # --> 0.51



### Now we look into property characteristics to see if we can discern
### what may have caused some properties to be hit harder by the
### reassessment

## First, we load building data for base and reval assessments
baseval_bldg <- read.csv("data/mtlebo_baseval_bldg.csv", sep = "|")  # 2012
reval_bldg   <- read.csv("data/mtlebo_reval_bldg.csv", sep = "|")    # 2013

## Standardize on calling the garage count "Garages"
baseval_bldg <- rename(baseval_bldg, c(Garage = "Garages"))
reval_bldg <- rename(reval_bldg, c(Basement_Garage = "Garages"))

## Merge the two data sets into one and compute differences
bldg <- merge(baseval_bldg, reval_bldg, by = "Parcel_ID",
              suffixes = c("_Base", "_Reval"))

bldg <- transform(bldg,
                  Total_Rooms_Diff = Total_Rooms_Reval - Total_Rooms_Base,
                  Bedrooms_Diff    = Bedrooms_Reval    - Bedrooms_Base,
                  Full_Baths_Diff  = Full_Baths_Reval  - Full_Baths_Base,
                  Half_Baths_Diff  = Half_Baths_Reval  - Half_Baths_Base,
                  Fireplaces_Diff  = Fireplaces_Reval  - Fireplaces_Base,
                  Garages_Diff     = Garages_Reval     - Garages_Base)



## Add a PIN primary key
bldg <- transform(bldg, PIN = gsub("-", "", Parcel_ID))

## And merge it with reval-effects data
bldg_effects <- merge(bldg, reval_effects, by = "PIN")

## Exclude extreme assessment changes
bldg_effects <- subset(bldg_effects, rel_asm >= 0 & rel_asm < 3)

## And now a few simple models . . .

options(digits = 2)


## First, a linear model to predict relative assessment increase
m1 <- lm(rel_asm ~ (Bedrooms_Diff + Full_Baths_Diff +
                    Half_Baths_Diff + Fireplaces_Diff + Garages_Diff +
                    Total_Rooms_Base + Full_Baths_Base + Half_Baths_Base +
                    Style_Base + Heating_Base + Cooling_Base +
                    Grade_Base + Condition_Base +
                    Year_Built_Base +
                    Use_Code_Reval + Homestead + Stories_Base +
                    Total_Value_2012_Mkt),
         data = bldg_effects)

summary(m1)


## And a multiplicative model to predict relative assessment increase
m2 <- lm(log(Total_Value_2013_Reval) ~
         (Bedrooms_Diff + Full_Baths_Diff +
          Half_Baths_Diff + Fireplaces_Diff + Garages_Diff +
          Total_Rooms_Base + Full_Baths_Base + Half_Baths_Base +
          Style_Base + Heating_Base + Cooling_Base +
          Grade_Base + Condition_Base +
          Year_Built_Base +
          Use_Code_Reval + Homestead + Stories_Base +
          I(log(Total_Value_2012_Mkt))),
         data = bldg_effects)
summary(m2)

as.matrix(exp(coef(m2)))



### Compare assessment valuations w/ 2010-2011 residential real-estate
### sales data to see how closely they approximate "fair market" prices

## Load RE sales data set (courtesy of the Pittsburgh Neighborhood and
## Community Information System at the University Center for Social
## and Urban Research:  http://www.ucsur.pitt.edu/thepub.php?pl=370)
re_sales <- read.csv("data/re_sales_2010_2011.csv", comment.char = "#")
re_sales <- rename(re_sales, c(MAPBLOLOT = "PIN"))

re_sales_bldg_effects <- merge(re_sales, bldg_effects, by = "PIN")
re_sales_bldg_effects <- subset(re_sales_bldg_effects,
                                SALEPRICE > 0 &
                                Total_Value_2012_Mkt > 0)

to_dB <- function(x) 10 * log10(x)

re_comp <- with(re_sales_bldg_effects,
                data.frame(PIN = PIN,
                           Sold_For = SALEPRICE,
                           Was_Assessed_At = Total_Value_2012_Mkt,
                           Reassessed_At = Total_Value_2013_Reval,
                           Error_Intensity_dB = to_dB(
                             Total_Value_2013_Reval / SALEPRICE)))

## Narrow comparison to properties in the $50K to $500K range
re_comp <- subset(re_comp,
                  Sold_For      > 50000 & Sold_For      < 5e5 &
                  Reassessed_At > 50000 & Reassessed_At < 5e5)

write.csv(re_comp, file = "data/mtlebo_reval_reality_check.csv", row.names = F)



p <-
qplot(Reassessed_At, Sold_For, data = re_comp,
      geom = c("smooth", "point"),
      method = "lm",
      main = "Lebo homes sold for 4% above reassessed value, on average",
      xlab = "Reassessed value",
      ylab = "Recent sales price (2010 or 2011)"
      ) +
  geom_abline(intercept = 0, slope = 1, color = "gray") +
  scale_x_continuous(formatter = "dollar", lim = c(0, 5e5)) +
  scale_y_continuous(formatter = "dollar", lim = c(0, 5e5))

ggsave(p, file = "out/mtlebo-home-sales-vs-reval.pdf", height = 7, width = 7)

summary(lm(Sold_For ~ Reassessed_At, data = re_comp))



## For homes that were recently sold, did the reassessment move them
## closer to their sales prices?
p <-
ggplot(re_comp) +
  geom_segment(aes(x = Reassessed_At, y = Sold_For,
                   xend = Was_Assessed_At, yend = Sold_For), color = "gray") +
  geom_point(aes(Reassessed_At, Sold_For), color = "blue") +
  geom_point(aes(Was_Assessed_At, Sold_For), color = "red") +
  geom_abline(intercept = 0, slope = 8/8, color = "gray") +
  scale_x_continuous(formatter = "dollar", lim = c(0, 5e5)) +
  scale_y_continuous(formatter = "dollar", lim = c(0, 5e5)) +
  xlab("Assessed property value (red = old assessment, blue = new)") +
  ylab("Recently sold for (2010 or 2011)") +
  opts(title = "Lebo homes moved closer to sales price under new assessment")

ggsave(p, file = "out/mtlebo-home-sales-vs-reval-and-baseval-movement.pdf",
       useDingbats = F,
       height = 7, width = 7)
