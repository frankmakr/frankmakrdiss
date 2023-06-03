# If not present, download data using Zenodo REST API.
# Get bucket with
# curl https://zenodo.org/api/records/5496167 | \
# json_pp | \
# awk -F\" '/^.*bucket.*https/ {print $(NF-1)}'

## comms_demo
#if (!file.exists("data-raw/02_comms_demo.csv")) {
#  download.file("https://zenodo.org/api/files/.../02_comms_demo.csv")
#}
## comms_data
#if (!file.exists("data-raw/03_comms_data.csv")) {
#  download.file("https://zenodo.org/api/files/.../03_comms_data.csv")
#}
## conval_demo
#if (!file.exists("data-raw/04_conval_data.csv")) {
#  download.file("https://zenodo.org/api/files/.../04_comms_demo.csv")
#}
## conval_data
#if (!file.exists("data-raw/05_conval_data.csv")) {
#  download.file("https://zenodo.org/api/files/.../05_comms_data.csv")
#}
## conval_comms
#if (!file.exists("data-raw/06_conval_comms.csv")) {
#  download.file("https://zenodo.org/api/files/.../06_conval_comms.csv")
#}

datalist <- lapply(Sys.glob("data-raw/*.csv"), function(i) read.csv(i,
  row.names = 1, stringsAsFactors = FALSE))
names(datalist) <- sub(".csv", "", basename(Sys.glob("data-raw/*.csv")))

datalist$comms_data[, 3:6] <- lapply(datalist$comms_data[, 3:6], as.factor)
datalist$comms_demo$gender <- factor(datalist$comms_demo$gender,
  levels = 1:3, labels = c("Männlich", "Weiblich", "Divers"))
datalist$comms_demo$state <- factor(datalist$comms_demo$state,
  levels = 1:17, labels = c(
  "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg",
  "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen",
  "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt",
  "Schleswig-Holstein", "Thüringen", "Außerhalb von Deutschland"))
datalist$comms_demo$studstatus <- factor(datalist$comms_demo$studstatus,
  levels = 1:8, labels = c(
  "Vollzeitstudium", "Teilzeitstudium", "Kooperationsstudium", "Jungstudium",
  "Promotionsstudium", "Zweithörerstudium", "Akademiestudium",
  "Weiterbildungsstudium"))
datalist$conval_demo$gender <- factor(datalist$conval_demo$gender,
  levels = 1:3, labels = levels(datalist$comms_demo$gender))
datalist$conval_demo$state <- factor(datalist$conval_demo$state,
  levels = 1:17, labels = levels(datalist$comms_demo$state))
datalist$conval_demo$studstatus <- factor(datalist$conval_demo$studstatus,
  levels = 1:8, labels = levels(datalist$comms_demo$studstatus))

for (i in seq(datalist)) {
  assign(names(datalist[i]), datalist[[i]])
  do.call(usethis::use_data, list(
    as.name(names(datalist)[i]), overwrite = TRUE, compress = "xz", version = 3)
    )
}
