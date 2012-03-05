#!/usr/bin/Rscript

### Analysis of 2012/2013 real-estate assessments for Mt. Lebanon, Pennsylvania
### Tom Moertel <tom@moertel.org>
###
### Sources:
###
### [1] Allegheny County 2012 "base-year" assessments
###     http://www2.county.allegheny.pa.us/RealEstate/Default.aspx
###
### [2] Allegheny County 2013 "reval" assessments
###     http://www2.alleghenycounty.us/reval/
###
### [3] Allegheny County residental real-estate sales data set from
###     Pittsburgh Neighborhood and Community Information System at
###     the University Center for Social and Urban Research
###     http://www.ucsur.pitt.edu/thepub.php?pl=370


library(ggplot2)
library(scales)
library(plyr)
library(reshape2)


## Load the data sets

## First: assessments
baseval <- read.csv("data/mtlebo_baseval.csv", sep = "|")    # 2012
reval   <- read.csv("data/mtlebo_reval.csv", sep = "|")      # 2013 reassessment
bothval <- merge(baseval, reval, by = "Parcel_ID", all = T)  # combined
bothval <- mutate(bothval, PIN = gsub("-", "", Parcel_ID),
                  State_Code.y = reorder(State_Code.y,
                    0.0 - Total_Value_2013_Reval, sum, na.rm = T))

## Second: residental real-estate sales
re_sales <- read.csv("data/re_sales_2010_2011.csv", comment.char = "#")
re_sales <- rename(re_sales, c(MAPBLOLOT = "PIN"))

## Merge the data sets
re_sales_bothval <- merge(re_sales, bothval, by = "PIN")

## Narrow to columns useful for comparison of old/new assessments
re_comp <- with(re_sales_bothval,
                data.frame(PIN = PIN,
                           Sold_For = SALEPRICE,
                           Was_Assessed_At = Total_Value_2012_Mkt,
                           Reassessed_At = Total_Value_2013_Reval))


## Establish some cutoffs to exlude properties w/ low predictive value
LOW_CUTOFF  <-   50000  # excludes ~ 1.8% of properties
HIGH_CUTOFF <- 1000000  # excludes ~ 0.2% of properties


## Create a training set for relating assessed values to actual sales
## prices for the same properties; we exclude fringe properties where
## the sales data aren't dense enough to yield reliable predictions.
## The resulting set comprises 863 properties.
training_set <-
  subset(re_comp,
         # drop extreme properties, which have little predictive value
         Reassessed_At   > LOW_CUTOFF &
         Was_Assessed_At > LOW_CUTOFF &
         Sold_For > LOW_CUTOFF & Sold_For < HIGH_CUTOFF &
         Reassessed_At < 2 * Was_Assessed_At  # suggests unusual circumstances
         )


## Now we create some models to "capture" how the assessed values
## related to actual market prices for our training set.  We use
## non-parametric additive models because we have no interest in
## understanding the underlying assessment models and their
## parameters; rather we want to record their predictions, warts and
## all, so that we can "replay" them in reverse, applying them to
## reassessed properties outside of our training set to estimate their
## fair-market values. For example, if the the houses in our training
## set that sold for about $100,000 were reassessed for about $120,000
## on average, our models will predict that, on average, homes
## reassessed at $120,000 will generally sell for about $100,000
## on the market.


require(mgcv)  # load libraries for general additive modeling


## two smooths
(price_gam <- gam(Sold_For ~
                  s(Was_Assessed_At, bs = "ad") +
                  s(Reassessed_At, bs = "ad"),
                  data = training_set,
                  family = Gamma(link = log),
                  method = "ML"))

## one 2-var smooth
(price_gam2 <- gam(Sold_For ~
                  s(Was_Assessed_At, Reassessed_At, bs = "ad"),
                  data = training_set,
                  family = Gamma(link = log),
                  method = "ML"))

## one 2-var smooth
(price_gam3 <- gam(Sold_For ~
                  s(Was_Assessed_At, Reassessed_At, bs = "ad", k = 20),
                  data = training_set,
                  family = Gamma(link = log),
                  method = "ML"))


## All 3 models fit about as well as could be hoped, but the 3rd model
## fits a teensy bit better, so we prefer it, even though the results
## we get later would be about the same with any of the models.
anova(price_gam, price_gam2, price_gam3, test = "F")
AIC(price_gam, price_gam2, price_gam3)

summary(price_gam3)


## Plotting the smoothed surfaces of the first model is instructive.
## It's easy to see that both old and new assessments are regressive,
## (i.e., overvaluing lower-valued properties and undervaluing
## higher-valued properties) and that the new assessments are even
## moreso than the old.
par(mfrow = c(1,2))
plot(price_gam, residuals = T, ylim = c(-1,2))



## Additionally, we fit a nonparametric kernel-smoothing model that is
## less constrained than the GAMs.  This is pretty close to "just
## letting the data speak."  The downside is that we may overfit to
## weirdness in the data set that is not predictive of general market
## pricing.  (That's why we also use our price_gam3 model for
## comparison.)

library(np)

price_np <- npreg(log(Sold_For) ~ Was_Assessed_At + Reassessed_At, bs = "ad",
                  data = training_set)

summary(price_np)




## Now we're going to predict actual market prices for Mt. Lebanon
## residences.  We're going to exclude about 2% of the most- and
## least- expensive residences, for which our model didn't have
## sufficient training data to yield reliable predictions.  This
## exclusion shouldn't affect our conclusions much because the bulk of
## the residential tax base is still accounted for. Further,
## residences account for most of Mt. Lebanon's property value and
## the vast majority of Mt. Lebanon taxpayers, so we're only going to
## consider unfairness among owners of residential property.
##
## > ddply(bothval, .(State_Code.y), summarize,
##         count = length(State_Code.y),
##         assess_val = sum(0.0 + Total_Value_2013_Reval, na.rm = T))
##
##   State_Code.y count assess_val
## 1  Residential 11399 2435232405
## 2   Commercial   359  362086964
## 3        Other    12   19775671
## 4   Industrial     5    2562600
## 5 Agricultural     1     192400
## 6   Government     2      70800
## 7                253          0

## Grab residential properties
mtlebo_props <-
  with(subset(bothval, State_Code.y == "Residential"),
       data.frame(PIN = PIN,
                  Was_Assessed_At = Total_Value_2012_Mkt,
                  Reassessed_At = Total_Value_2013_Reval))
## Exclude fringe properties
mtlebo_props <- subset(mtlebo_props,
                       Was_Assessed_At >  50000 &
                       Was_Assessed_At < 485000 &  # ~ 1%
                       Reassessed_At   >  66000 &  # ~ 2%
                       Reassessed_At   < 686000)   # ~ 0%

## Helper fn to estimate property values given a model and property set
estimate_property_values <-
  function(model = price_gam3, ds = mtlebo_props, yxform = identity) {
    est_mkt_values <- yxform(predict(model, newdata = ds, type = "response"))
    with(ds,
         data.frame(PIN = PIN,
                    Old_Asm = 0.0 + Was_Assessed_At,
                    New_Asm = 0.0 + Reassessed_At,
                    Est_Mkt = 0.0 + est_mkt_values))
  }


## Estimate market prices for all of Lebo homes using both GAM and KS models
mtlebo_fair    <- estimate_property_values(price_gam3, mtlebo_props)
mtlebo_fair_np <- estimate_property_values(price_np, mtlebo_props, exp)

## Also, for comparison, treat the sales data set as if it represented
## its own, isolated community and prepare to calculate the tax fairness
## in that community.
sales_fair <- with(training_set,
                   data.frame(PIN = PIN,
                              Old_Asm = 0.0 + Was_Assessed_At,
                              New_Asm = 0.0 + Reassessed_At,
                              Est_Mkt = 0.0 + Sold_For))


## The following function takes one of the above "_fair" data sets,
## representing a community's properties, slices it into bands, and
## determines how much each band pays in property taxes relative to
## what it ought to pay if all homes were assessed ideally at their
## fair-market prices.
unfairness_table <- function(ds, bands = 20, steps = bands * 10) {
  stopifnot(steps >= bands)
  bandsize = 1 / bands
  stepsize = 1 / steps
  bands <- seq(bandsize, 1, by = stepsize)
  ds <- ds[order(ds$Est_Mkt), ]         # sort properties by market price
  ds_c <- numcolwise(cumsum)(ds)        # compute running totals
  taxbases <- as.vector(tail(ds_c, 1))  # last = community-wide total

  with(ds_c, {
    ## For each band, we compute its fair share of taxes under the
    ## assumption that each band is assessed at fair-market value.
    fair_market_share <- ((quantile(Est_Mkt, bands) -
                           quantile(Est_Mkt, bands - bandsize)) /
                          taxbases$Est_Mkt)

    ## Now we compute each band's share under the old and new
    ## assessments.
    old_asm_share     <- ((quantile(Old_Asm, bands) -
                           quantile(Old_Asm, bands - bandsize)) /
                          taxbases$Old_Asm)

    new_asm_share     <- ((quantile(New_Asm, bands) -
                           quantile(New_Asm, bands - bandsize)) /
                          taxbases$New_Asm)

    ## Finally, we compare the shares under old and new assessments
    ## to the ideal fair-market shares.
      data.frame(Est_Mkt = quantile(ds$Est_Mkt, bands - bandsize/2),
                 Old_Unf = old_asm_share / fair_market_share - 1,
                 New_Unf = new_asm_share / fair_market_share - 1)
  })
}

## Now we use the function above to package tax-fairness estimates for
## all three communities into one composite data table for easy comparison.
unfairness_preds <-
  rbind(data.frame(model = "gam", subject = "Mt. Lebanon",
                   unfairness_table(mtlebo_fair, 10)),
        data.frame(model = "np",  subject = "Mt. Lebanon",
                   unfairness_table(mtlebo_fair_np, 10)),
        data.frame(model = "identity", subject = "Recent sales only",
                   unfairness_table(sales_fair, 10)))

unfairness_preds_m <- melt(unfairness_preds, c("model", "subject", "Est_Mkt"))

levels(unfairness_preds_m$variable) <- c("Old assessment", "New assessment")


## Now we plot the unfairness trend for the imaginary isolated
## community composed solely of recently sold Mt. Lebanon residences
p <-
qplot(Est_Mkt, value,
      data = subset(unfairness_preds_m, subject == "Recent sales only"),
      main = paste(sep = "\n",
        "If all of Lebo were exactly like recently sold homes,",
        "low-value properties would be massively overtaxed"),
      ylab = "Estimated overpaid taxes for properties of similar value",
      xlab = "Fair-market property value",
      geom = "line",
      color = variable) +
  scale_color_discrete(labels = c("Old", "New")) +
  scale_x_continuous(label = dollar_format()) +
  scale_y_continuous(label = percent_format()) +
  labs(colour = "Assessment")

ggsave(p, file = "/tmp/mtlebo-assessments-vs-isolated-sales-overtaxing.pdf",
       useDingbats = F, width = 11, height = 8.5)


## Here's the plot we've been waiting for: Estimated tax unfairness
## for all Mt. Lebanon residences, under both old and new assessments.
## We used two separate models to arrive at these estimates, so we
## plot one model's trends with a solid line (gam), the other's with
## a dashed (np = kernel smoothing).
p <-
qplot(Est_Mkt / 1000, value,
      data = subset(unfairness_preds_m, subject == "Mt. Lebanon"),
      main = "For Mt. Lebanon, New Assessments are More Unfair than Old",
      ylab = "Estimated overpaid taxes for properties of similar value",
      xlab = "Fair-market property value ($thousands)",
      geom = "line",
      color = variable,
      linetype = model) +
  scale_x_continuous(label = dollar_format()) +
  scale_y_continuous(label = percent_format())

ggsave(p, file = "out/mtlebo-reval-unfairness.pdf", width = 11, height = 7)

ggsave(p, file = "out/mtlebo-reval-unfairness.png",
       width = 11 * (2/3), height = 7 * (2/3))
