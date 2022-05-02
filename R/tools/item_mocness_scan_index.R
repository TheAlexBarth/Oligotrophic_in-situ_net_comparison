# an index to refer scans to net bins
# note that the net two is a special case since it is combined with net three
# Thus, if looping through it needs to be treated separately
mocness_scan_index <- list(
  one_lg = list(
    net = 1,
    scans = c('m1_n1_1500_spl2','m1_n1_1500_spl1'),
    dilution = 0.5 # one half dilution
  ),
  one_sm = list(
    net = 1,
    scans = c('m1_n1_153_d03'),
    dilution = .03 # three hundreths
  ),
  two_lg = list(
    net = 2,
    scans = c('m1_n2_1500_spl2','m1_n2_1500_spl1','m1_n3_1500_tot'), #will need special treatement
    dilution = c(.5,1) #dilution 1/2 for net two but whole scan net three
  ),
  two_sm = list(
    net = 2,
    scans = c('m1_n2_153_d02','m1_n3_153_d10'),
    dilution = c(.02,.10) #2/100 for net two but 1/10 for net three to addin
  ),
  four_lg = list(
    net = 4,
    scans = c('m1_n4_1500_spl3','m1_n4_1500_spl2','m1_n4_1500_spl4',
              'm1_n4_1500_spl1'),
    dilution = .5 # one half dilution
  ),
  four_sm = list(
    net = 4,
    scans = c('m1_n4_153_d50'),
    dilution = .02 #one fiftith
  ),
  five_lg = list(
    net = 5,
    scans = c('m1_n5_1500_spl4','m1_n5_1500_spl2','m1_n5_1500_spl3',
              'm1_n5_1500_spl1'),
    dilution = 1 #no dilution
  ),
  five_sm = list(
    net = 5,
    scans = c('m1_n5_153_hndth'),
    dilution = .01 #on hundreth
  ),
  six_lg = list(
    net = 6,
    scans = c('m1_n6_1500_spl4','m1_n6_1500_spl3','m1_n6_1500_spl1',
              'm1_n6_1500_spl2'),
    dilution = 1 #no dilution
  ),
  six_sm = list(
    net = 6,
    scans = c('m1_n6_153_spl1_04'),
    dilution = .04 # four hundreths
  ),
  seven_lg = list(
    net = 7,
    scans = c('m1_n7_1500_spl1_tot','m1_n7_1500_spl2_tot','m1_n7_1500_spl3_tot',
              'm1_n7_1500_spl4_0_tot','m1_n7_1500_spl4_5_tot'),
    dilution = 1 #no dilution
  ),
  seven_sm = list(
    net = 7,
    scans = c('m1_n7_153_hndrd_100'),
    dilution = 0.01 #one hundreth
  ),
  eight_lg = list(
    net = 8,
    scans = c('m1_n8_1500_spl1_tot','m1_n8_1500_spl1_floaters_tot',
              'm1_n8_1500_spl2_tot','m1_n8_1500_spl3_tot','m1_n8_1500_spl5_tot',
              'm1_n8_1500_spl6_tot2','m1_n8_1500_spl8_tot','m1_n8_1500_spl7_tot',
              'm1_n8_1500_spl4_tot'),
    dilution = 1 #no dilution
  ),
  eight_sm = list(
    net = 8,
    scans = c('m1_n8_153_spl2_d100','m1_n8_153_spl1_d100'),
    dilution = .01 #one hundredth
  )
)
