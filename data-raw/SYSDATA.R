kvarven2020 <- list(
  eff_orig = c(0.496, 0.57, 0.459, 0.127, 0.877, 0.31, 0.676, 0.57, 0.31,
    0.236, 0.62, 0.117, 0.2, 0.3541, 0.426),
  eff_rep = c(0.27, 0.6, 0.13, -0.07, 0.785, 0.04, 0.29, 0.03, 0.147, 0.171,
    0.04, -0.02, 0.016, 0.063, -0.04)
  )



hillery1955 <- list(N = 94, n_konz = 16, n_combn = 22, sozint = 91,
  gemverb = 73, geo = 69)



glynn1981 <- list(                                
  descr = matrix(                  
    c(68   , 73   , 30   ,                                             
       4.04,  4.09,  3.98,        
       3.12,  3.67,  3.81),
    ncol = 3,
    dimnames = list(
      c("hy", "gr", "kb"),
      c("n", "psoc_i", "psoc_a"))
    ),                                                                          
  rel = setNames(c(0.924, 0.972), c("psoc_i", "psoc_a")),
  df = c(2, 168),                                                           
  f = setNames(c(1.3, 23.6), c("psoc_i", "psoc_a"))
  )



mannarini2018b <- list(
  acc_rel = setNames(c(0.83, 0.77), c("accorig", "accnew")),
  acc_freq = matrix(
    c(64, 37, 27, 102,
      33, 22, 59,  12),
    ncol = 2,
    dimnames = list(c("int", "ass", "sep", "mar"), c("albania", "srilanka"))),
  albania = matrix(
    c(22.80, 7.35, 0.87,
      22.13, 7.16, 0.87),
    ncol = 2,
    dimnames = list(c("mean", "sd", "rel"), c("tpsoc", "epsoc"))),
  srilanka = matrix(
    c(23.14, 3.32, 0.87,
      27.13, 4.59, 0.87),
    ncol = 2,
    dimnames = list(c("mean", "sd", "rel"), c("tpsoc", "epsoc")))
  )



jason2015_cor <- c(self_mem = 0.669, self_ent = 0.548, mem_ent = 0.634)
jason2015_cormat <- diag(3)
rownames(jason2015_cormat) <- c("self", "mem", "ent")
colnames(jason2015_cormat) <- rownames(jason2015_cormat)
jason2015_cormat[upper.tri(jason2015_cormat)] <- jason2015_cor
jason2015_cormat[lower.tri(jason2015_cormat)] <- t(jason2015_cor)
jason2015 <- list(
  N = 158,
  descr = matrix(
    c(4.49, 1.24, 0.865, 3,
      4.29, 1.18, 0.904, 3,
      4.15, 1.33, 0.833, 3,
      4.31, 1.11, 0.923, 9),
    ncol = 4,
    dimnames = list(
      c("mean", "sd", "alpha", "n_items"),
      c("self", "mem", "ent", "full"))
    ),
  cormat = jason2015_cormat,
  EFA_Fit = c(CFI = 1.000, TLI = 1.007, RMSEA = 0.000),
  AIC = setNames(c(181.703, 162.217, 162.606, 161.899, 164.385),
          c("Nullmodell", "1 Faktor", "3 Faktoren", 
            "3-fach-Interaktion", "3 Faktoren + 3-fach-Interaktion")
          )
  )



halamova2018 <- list(
  excor = matrix(
    c(0.39, 0.64, 0.35,
      0.31, 0.69, 0.29),
    ncol = 2,
    dimnames = list(
      c("ad", "ao", "do"),
      c("RC", "IC"))
    ),
  koncor = matrix(
    c(0.37, 0.87, 0.50,
      0.46, 0.94, 0.45),
    ncol = 2,
    dimnames = list(
      c("ad", "ao", "do"),
      c("RC", "IC"))
    ),
  items_acc = c(1, 6, 8, 9, 13, 14, 16, 19, 21, 22),
  items_dyn = c(2, 4, 5, 7, 12, 15, 17, 20),
  items_opn = c(3, 10, 11, 18),
  exloadings_rc = matrix(
    c(0.844, 0.053, 0.334, 0.177, 0.059, 0.714, 0.109, 0.865, 0.805, 0.028,
      0.228, 0.093, 0.536, 0.852, 0.039, 0.732, 0.011, 0.223, 0.485, 0.229,
      0.693, 0.577, 0.113, 0.273, 0.018, 0.535, 0.667, 0.104, 0.476, 0.064,
      0.030, 0.058, 0.194, 0.730, 0.036, 0.041, 0.839, 0.071, 0.828, 0.174,
      0.036, 0.582, 0.165, 0.166, 0.012, 0.026, 0.407, 0.198, 0.214, 0.135,
      0.112, 0.068, 0.076, 0.459, 0.582, 0.096, 0.250, 0.079, 0.049, 0.119,
      0.043, 0.453, 0.125, 0.171, 0.147, 0.126),
    ncol = 3,
    dimnames = list(
      c("RC01", "RC02", "RC03", "RC04", "RC05", "RC06", "RC07", "RC08",
        "RC09", "RC10", "RC11", "RC12", "RC13", "RC14", "RC15", "RC16",
        "RC17", "RC18", "RC19", "RC20", "RC21", "RC22"),
      c("acc", "dyn", "opn"))
    ),
  exloadings_ic = matrix(
    c(0.959, 0.042, 0.019, 0.041, 0.088, 0.938, 0.110, 0.950, 0.932, 0.086,
      0.092, 0.032, 0.860, 0.955, 0.060, 0.683, 0.025, 0.151, 0.737, 0.181,
      0.861, 0.867, 0.098, 0.362, 0.075, 0.607, 0.543, 0.035, 0.570, 0.013,
      0.014, 0.009, 0.077, 0.547, 0.061, 0.046, 0.792, 0.022, 0.863, 0.095,
      0.026, 0.530, 0.113, 0.102, 0.066, 0.150, 0.756, 0.320, 0.119, 0.092,
      0.318, 0.039, 0.035, 0.390, 0.855, 0.286, 0.078, 0.038, 0.076, 0.037,
      0.072, 0.738, 0.069, 0.149, 0.001, 0.001),
    ncol = 3,
    dimnames = list(
      c("IC01", "IC02", "IC03", "IC04", "IC05", "IC06", "IC07", "IC08",
        "IC09", "IC10", "IC11", "IC12", "IC13", "IC14", "IC15", "IC16",
        "IC17", "IC18", "IC19", "IC20", "IC21", "IC22"),
      c("acc", "dyn", "opn"))
    ),
  konloadings_acc = matrix(
    c(0.851, 0.871, 0.858, 0.899, 0.736, 0.855, 0.714, 0.691, 0.884, 0.721,
      0.957, 0.956, 0.969, 0.963, 0.869, 0.968, 0.783, 0.866, 0.940, 0.942),
    ncol = 2,
    dimnames = list(
      c(1, 6, 8, 9, 13, 14, 16, 19, 21, 22),
      c("RC", "IC"))
    ),
  konloadings_dyn = matrix(
    c(0.386, 0.477, 0.730, 0.614, 0.677, 0.867, 0.817, 0.589,
      0.408, 0.428, 0.638, 0.498, 0.593, 0.807, 0.918, 0.684),
    ncol = 2,
    dimnames = list(
      c(2, 4, 5, 7, 12, 15, 17, 20),
      c("RC", "IC"))
    ),
  konloadings_opn = matrix(
    c(0.642, 0.380, 0.792, 0.680,
      0.832, 0.485, 0.933, 0.844),
    ncol = 2,
    dimnames = list(
      c(3, 10, 11, 18),
      c("RC", "IC"))
    ),
  exloglik = matrix(
    c(-15531, -15140, -15085,
      -10943, -10557, -10535),
    ncol = 2,
    dimnames = list(
      c("1 Faktor", "2 Faktoren", "3 Faktoren"),
      c("RC", "IC"))
    )
  )



fernuni2018 <- list(
  gender = setNames(c(39277, 33697, 0),
    c("Männlich", "Weiblich", "Divers")),
  genderpsych = setNames(c(3673, 9055, 0),
    c("Männlich", "Weiblich", "Divers")),
  agem = setNames(c(60, 1788, 9855, 11153, 7197, 4860, 2633, 1731),
    c("Unter 18", "18--24", "25--31", "32--38", "39--45", "46--52", "53--59",
    "Über 60")),
  agew = setNames(c(29, 2461, 9665, 9277, 5454, 3869, 2168, 774),
    c("Unter 18", "18--24", "25--31", "32--38", "39--45", "46--52", "53--59",
    "Über 60")),
  state = setNames(c(7697, 10621, 5166, 1185, 458, 2272, 5306, 547, 4980,
    20478, 2660, 706, 1990, 683, 1737, 723, 5765),
    c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen",
    "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen",
    "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen",
    "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen",
    "Außerhalb von Deutschland")),
  studstatus = setNames(c(16212, 44931, 170, 22, 431, 3432, 5389, 2387),
    c("Vollzeitstudium", "Teilzeitstudium", "Kooperationsstudium",
    "Jungstudium", "Promotionsstudium", "Zweithörerstudium", "Akademiestudium",
    "Weiterbildungsstudium"))
  )



fernuni2019 <- list(
  gender = setNames(c(40429, 34992, 5),
    names(fernuni2018$gender)),
  genderpsych = setNames(c(4085, 10012, 1),
    names(fernuni2018$gender)),
  agem = setNames(c(82, 2450, 9991, 10972, 7390, 4738, 2895, 1911),
    names(fernuni2018$alterm)),
  agew = setNames(c(33, 3429, 9641, 9199, 5738, 3775, 2283, 894),
    names(fernuni2018$alterm)),
  state = setNames(c(8015, 10906, 5242, 1278, 539, 2333, 5594, 574, 5135,
    21128, 2815, 707, 2081, 703, 1837, 716, 5823),
    names(fernuni2018$bundesland)),
  studstatus = setNames(c(17326, 46363, 139, 8, 436, 2816, 6028, 2310),
    names(fernuni2018$studstatus))
  )



colorscheme_frankmakrdiss <- matrix(
  c("#dcbccc", "#c799b0", "#b97c9b", "#a25079", "#8f275b", "#7c003e",
    "#bcdcdc", "#99c7c7", "#7cb9b9", "#50a2a2", "#278f8f", "#007c7c",
    "#fbf3da", "#f8e8b5", "#f5dc90", "#dbc376", "#aa975c", "#7a6c42",
    "#d1e1ec", "#b3cde0", "#6497b1", "#005b96", "#03396c", "#011f4b",
    "#dcbcbc", "#c79999", "#b97c7c", "#a25050", "#8f2727", "#7c0000"),
  ncol = 5,
  dimnames = list(
    c("light", "light_highlight",
      "mid", "mid_highlight",
      "dark", "dark_highlight"),
    c("pink", "teal", "yellow", "blue", "red"))
  )



theta_comms <- levels(unlist(read.csv("data-raw/03_comms_data.csv",
  colClasses = c(rep("NULL", 6), "factor"))))



usethis::use_data(
  kvarven2020,
  hillery1955, glynn1981, mannarini2018b,
  jason2015, halamova2018,
  fernuni2018, fernuni2019,
  colorscheme_frankmakrdiss,
  theta_comms,
  internal = TRUE, overwrite = TRUE, compress = "xz", version = 3)
