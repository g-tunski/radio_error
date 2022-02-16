library(dplyr)
library(Bchron)
library(reshape2)
library(ggplot2)


dat = read.csv('neotoma_northamerica_chroncontrols_subset.csv')

idx = which(!is.na(dat$age))
dat = dat[idx,]

idx_depth = which(!is.na(dat$depth))
dat = dat[idx_depth,]

# dat = dat  %>%
# filter(dat$depth > 0)

# dat$ageSds = (dat$geochron.geochronerrorolder + dat$geochron.geochronerroryounger)/2
  
# create the column and fill it with NAs
dat$ageSds = NA

# get ids of rows that have radiocarbon dates
idx_radio = which(dat$type == "Radiocarbon")
dat[idx_radio, "ageSds"] = (dat[idx_radio, "geochron.geochronerrorolder"] + dat[idx_radio, "geochron.geochronerroryounger"])/2

# get ids of rows that have core tops
idx_top = which(dat$type == "Core top")

dat[idx_top, "ageSds"]  = (abs(dat[idx_top, "limitolder"] - dat[idx_top, "age"]) + abs(dat[idx_top, "limityounger"] - dat[idx_top, "age"]))/2


##############################################################################################################
## fit age-depth models for both calibration curves
##############################################################################################################
# dd




# list of all of the dataset ids
ids = unique(dat$datasetid)

# number of datasets
N_datasets = length(ids)

# empty list where bchronolgy output will be stored
# out = list()
out20 = list()
out13 = list()

# for each site, fit age-depth models
# models: intcal13, intcal20

# for now do this for the first three sites
for (i in 1:3){
  # fit the intcal 20 model
  
  
  # get the dataset id for site i
  # this dataset id will correspond with the i-th element in the ids vector
  id_dataset = ids[i]
  
  # subset the data to make it easier to work with
  # not necessary to do this, but I think it makes the rest more intuitive
  dat_sub = dat[which(dat$datasetid == id_dataset), ]
  
  # create a vector 
  cal_curve20 = rep(NA, nrow(dat_sub))
  
  idx_radio = which(dat_sub$type == 'Radiocarbon')
  cal_curve20[idx_radio] = 'intcal20'
  
  idx_top = which(dat_sub$type == 'Core top')
  cal_curve20[idx_top] = 'normal'
  
  out20[[i]] = Bchronology(ages = dat_sub$age,
                           ageSds = dat_sub$ageSds,
                           positions = dat_sub$depth,
                           calCurves = cal_curve20)
  
  plot(out20[[i]])
  
  # predicted depths
  # default size: 100
  depths20 = out20[[i]]$predictPositions
  
  # age predictions at predicted positions (depths)
  # default size: 1000 x 100
  ages_pred20 = t(out20[[i]]$thetaPredict)
  
  # create data frame with depth as first column and age samples in following columns
  # assign this data frame to out list element i
  out20[[i]] = data.frame(depths20 = depths20, ages_pred20)
  
  
  
# intcal13
  
  cal_curve13 = rep(NA, nrow(dat_sub))
  
  idx_radio = which(dat_sub$type == 'Radiocarbon')
  cal_curve13[idx_radio] = 'intcal13'
  
  idx_top = which(dat_sub$type == 'Core top')
  cal_curve13[idx_top] = 'normal'
  
  # fit the intcal 13 model
  out13[[i]] = Bchronology(ages = dat_sub$age,
                           ageSds = dat_sub$ageSds,
                           positions = dat_sub$depth,
                           calCurves = cal_curve13)
  
  plot(out13[[i]])
  
  # predicted depths
  # default size: 100
  depths13 = out13[[i]]$predictPositions
  
  # age predictions at predicted positions (depths)
  # default size: 1000 x 100
  ages_pred13 = t(out13[[i]]$thetaPredict)
  
  # create data frame with depth as first column and age samples in following columns
  # assign this data frame to out list element i
  out13[[i]] = data.frame(depths13 = depths13, ages_pred13)
  
  
  
}

##############################################################################################################
## take means of predicted ages for both age-depth models
## take the differences of these means
##############################################################################################################

# define data frame to store the differences
# for now let's define the columns: datasetid, depths, diff
# this data frame is empty; we need to tell R what type of data will go in each column
age_diff = data.frame(datasetid = numeric(0),
                      depths = numeric(0),
                      diff = numeric(0))

# loop over the sites
# for now do this for the first three sites
for (i in 1:3){
  
  # assigning the output from site i to out_site
  # not necessary but helps think through steps for now
  out_site20 = out20[[i]]
  
  # long format dataset, so we can use dplyr
  out_site_melt20 = melt(out_site20, id.vars = 'depths20')
  
  out_site_mean20 = out_site_melt20 %>% 
    group_by(depths20) %>% 
    summarize(age_mean = mean(value))
  
  # do the same thing for other age-depth model
  out_site13 = out13[[i]]
  
  out_site_melt13 = melt(out_site13, id.vars = 'depths13')
  
  out_site_mean13 = out_site_melt13 %>% 
    group_by(depths13) %>% 
    summarize(age_mean = mean(value))
  
  out_site_mean20 = data.frame(out_site_mean20, curve='20')
  out_site_mean13 = data.frame(out_site_mean13, curve='13')
  
  # can then cbind the results, take the difference, and save result to a new data frame
  age_diff_site = cbind(out_site_mean20[, c('depths20', 'age_mean')], out_site_mean13[, 'age_mean'])
  colnames(age_diff_site) = c('depths', 'age20', 'age13')
  
  # append the differences for site i to the existing data frame age_diff 
  age_diff = rbind(age_diff,
                   age_diff_site)
  
  
}

# Try plotting 
ggplot(data = age_diff) +
  geom_point(aes(x = depths, y = c('age20', 'age13'), colour = curve))




# 
##############################################################################################################
## summarize and visualize results
##############################################################################################################




# 
# id_dataset = ids[1]
# 
# dat_sub = dat[which(dat$datasetid == id_dataset), ]
# 
# out20 = Bchronology(ages = dat_sub$age,
#                     ageSds = dat_sub$ageSds,
#                     positions = dat_sub$depth,
#                     calCurves = rep('intcal20', nrow(dat_sub)))
# plot(out20)
# 
# 
# # predicted depths
# # default size: 100
# 
# out20$predictPositions
# 
# # age predictions at predicted positions (depths)
# # default size: 1000 x 100
# 
# ages20 = t(out20$thetaPredict)
# 
# # create data frame with depth as first column and age samples in following columns
# 
# dat20 = data.frame(depths = out20$predictPositions, ages20)
# 
# dat20_melt = melt(dat20, id.vars = 'depths')
# 
# 
# 
# ggplot(data=dat20_melt) +
#   geom_point(aes(x=value, y=depths))
# 
# age20_mean = dat20_melt %>% 
#   group_by(depths) %>% 
#   summarize(age_mean = mean(value))
# 
# plot(age20_mean$depths, age20_mean$age_mean)
# 
# 
# 
# out13 = Bchronology(ages = dat_sub$age,
#                     ageSds = dat_sub$ageSds,
#                     positions = dat_sub$depth,
#                     calCurves = rep('intcal13', nrow(dat_sub)))
# plot(out13)
# 
# # predicted depths
# out20$predictPositions
# 
# # age predictions at predicted positions (depths)
# out20$thetaPredict
# 
# # age predictions at predicted positions (depths)
# # default size: 1000 x 100
# 
# ages13 = t(out13$thetaPredict)
# 
# # create data frame with depth as first column and age samples in following columns
# 
# dat13 = data.frame(depths = out13$predictPositions, ages13)
# 
# dat13_melt = melt(dat13, id.vars = 'depths')
# 
# age13_mean = dat13_melt %>% 
#   group_by(depths) %>% 
#   summarize(age_mean = mean(value))
# 
# plot(age13_mean$depths, age13_mean$age_mean)
# 
# # have age20_mean and age13_mean
# # columns: depths, age_mean
# 
# age20_mean = data.frame(age20_mean, curve='20')
# age13_mean = data.frame(age13_mean, curve='13')
# 
# 
# #  wide format data
# age_mean = cbind(age20_mean[, c('depths', 'age_mean')], age13_mean[, 'age_mean'])
# colnames(age_mean) = c('depths', 'age20', 'age13')
# 
# age_mean$diff = age_mean$age20 - age_mean$age13
# 
# age_mean$age = (age_mean$age20 + age_mean$age13)/2
# 
# ggplot(data=age_mean) + 
#   geom_point(aes(x=age, y=diff))
# 
# 
# #  dplyr long format data
# age_mean = rbind(age20_mean, 
#                  age13_mean)
# 
# ggplot(data=age_mean) +
#   geom_point(aes(x=depths, y=age_mean, colour=curve))
# 
# 
# foo = age_mean %>% 
#   arrange(depths) %>% 
#   group_by(depths) %>%
#   summarize(diff=age_mean-lag(age_mean))
# 
# foo = foo[which(!is.na(foo$diff)),]
# 
# hist(foo$diff)
# 
# ggplot(data=foo) +
#   geom_point(aes(x=depths, y=diff))
# 
# 
# # fit age depth model for a site with a coretop
# 
# dat_sub = dat[which(dat$datasetid == 1006), ]
# 
# out20 = Bchronology(ages = dat_sub$age,
#                     ageSds = dat_sub$ageSds,
#                     positions = dat_sub$depth,
#                     calCurves = rep('intcal20', nrow(dat_sub)))
