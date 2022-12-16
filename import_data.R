library(tidyverse)
library(haven)

setwd('/Volumes/wiwi/Seminardaten/SOEP_teaching_v34/raw')

hh_raw <- read_dta('bgh.dta')

# cid:       Household ID Number 
# bhpnr:     Person Surveyed ID Number 
# bgh7401: HH has Internet (Yes[1]/No[2])
# bgh7402: Reason (Financial[1]/Other[2])
# bgh7403: HH has Car (Yes[1]/No[2])
# bgh7404: Reason (Financial[1]/Other[2])
# bgh7405: Reserves for Emergencies (Yes[1]/No[2])
# bgh7406: Reason (Financial[1]/Other[2])
# bgh7407: Yearly Holiday Trip (Yes[1]/No[2])
# bgh7408: Reason (Financial[1]/Other[2])
# bgh7409: Invite Friends for dinner 1/month (Yes[1]/No[2])
# bgh7410: Reason (Financial[1]/Other[2])
# bgh7411: Fresh Fish/Meat/Poultry Every 2 Day (Yes[1]/No[2])
# bgh7412: Reason (Financial[1]/Other[2])
# bgh7413: Monthly Leisure Activity (Yes[1]/No[2])
# bgh7414: Reason (Financial[1]/Other[2])
# bgh7415: Replace Old Furniture (Yes[1]/No[2])
# bgh7416: Reason (Financial[1]/Other[2])
# bgh7417: Replace Old Clothes (Yes[1]/No[2])
# bgh7418: Reason (Financial[1]/Other[2])
# bgh7419: Dwelling can be Heated (Yes[1]/No[2])
# bgh7420: Reason (Financial[1]/Other[2])
# bgh7421: Small Amount Available (Yes[1]/No[2])
# bgh7422: Reason (Financial[1]/Other[2])
# bgh7423: Two Pairs Outdoor Shoes (Yes[1]/No[2])
# bgh7424: Reason (Financial[1]/Other[2])

hh <- hh_raw %>% 
  dplyr::select (hid, 
                 bgh7401, bgh7402, bgh7403, bgh7404, bgh7405,
                 bgh7406, bgh7407, bgh7408, bgh7409, bgh7409, 
                 bgh7410, bgh7411, bgh7412, bgh7413, bgh7414, 
                 bgh7415, bgh7416, bgh7417, bgh7418, bgh7419, 
                 bgh7420, bgh7421, bgh7422, bgh7423, bgh7424)

df <- data.frame(matrix(ncol = 13, nrow = dim(hh)[1] ))
colnames(df) <- c('hh_id', 'internet', 'car', 'emergency', 'holiday', 
                  'friends', 'meat', 'leisure', 'furniture', 'clothes', 
                  'heat', 'money', 'shoes')

for (r in 1:dim(hh)[1]) {
  df[r, 1] <- hh[r, 1]
  
  idx = 2
  for (c in 2:13) {
    if (hh[r, idx] == 2 && hh[r, idx+1] == 1) {
      df[r, c] <- 1
    }
    else if (hh[r, idx] == 2 && hh[r, idx+1] == 2) {
      df[r, c] <- 0
    }
    else if (hh[r, idx] == 1) {
      df[r, c] <- 0 
    }
    else {df[r, c] <- NA}
    
    idx = idx + 2
  }
}

setwd('/Users/Miguel/Desktop/Metrics Seminar/Project/')
write.csv(df, file = 'SOEP_depriv_16.csv')
