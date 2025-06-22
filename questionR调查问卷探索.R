
library(questionr)

## Sample table
data(Titanic)
tab <- apply(Titanic, c(4, 1), sum)
## Column percentages
cprop(tab)
## Column percentages with custom display
cprop(tab, digits = 2, percent = TRUE, total = FALSE)
## Could be applied to a table of 3 dimensions or more
cprop(Titanic)

## Not run: 
data(hdv2003)
icut(hdv2003, "age")
## Cutting hdv2003$age into hdv2003$age_rec
hdv2003$age_rec <- cut(hdv2003$age,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = NULL
)
irec(hdv2003, heures.tv)


data(hdv2003)
wtd.table(hdv2003$sexe, weights = hdv2003$poids)
wtd.table(hdv2003$sexe, weights = hdv2003$poids, normwt = TRUE)
table(hdv2003$sexe, hdv2003$hard.rock)
wtd.table(hdv2003$sexe, hdv2003$hard.rock, weights = hdv2003$poids)

data(hdv2003)
describe(hdv2003$sexe)
describe(hdv2003$age)
describe(hdv2003)
describe(hdv2003, "cuisine", "heures.tv")
describe(hdv2003, "trav*")
describe(hdv2003, "trav|lecture")
describe(hdv2003, "trav", "lecture")

data(fertility)
describe(women$residency)
describe(women)
describe(women, "id")


