
require(tidyverse)

col_read <- read_csv("SampleFiles/Flumes/V1V2/HoboU20OutsideWell/S1090418.csv",
                      skip = 1, n_max = 1)
colnames(col_read)



coltypes <- cols("d","c","d","d","d","d","c","c","c","c")


test_read <- read_csv("SampleFiles/Flumes/V1V2/HoboU20OutsideWell/S1090418.csv",
                      skip = 1, col_types = coltypes)
test_read
