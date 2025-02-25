| Data/Instrument                                                  | Extension/Filenames                                                | Scripts to Process      |
|-----------------------------------------------------------------|---------------------------------------------------------|-------------------------|
| Rain/Automatic rain gauges (R1,R7,EM)                           | .hobo/ R1061523.csv, R7061523.csv, EM061523.csv               | TestRainfall.qmd |
| Rain/Automatic rain gauges and Manual Rain gauges (R2,R3,R4,R5,R6,RV1) | .hobo /R1061523.csv, R7061523.csv, EM061523.csv, .xlsx/Precipitaciones La Corona 2023.xls | TestRainfall.qmd |
| Weather/Weather Station                                          | .dat/EM061523.dat                                            | TestWeather.qmd  |
| Flume/Hobou20                                                    |.hobo/  S1061523.csv, S2061523.csv, v3p111219.csv, V4p111219.csv | TestFlumes.qmd      |
| Flume/StevensU12                                                 | .hobo/ V4111219.csv | TestFlumes.qmd      |
| Flume/ISCO                                                       | .csv/ v3110119csv.csv  V1052523.csv | TestFlumes.qmd      |
| Flume/ISCOvel                                                    | .csv/  V3111219VEL.csv, v3110119vel.csv| TestFlumes.qmd      |
|Wells                                                             | Wells/Automatic/  N1111219.csv and further| TestWells.qmd|
Merge processed files and plot at different time steps             | Processed/ for Flumes & Rain |TestMergeFiles.qmd| 



Just the Flume files to be read by TestFlumes.qmd:

| Filename                                            | Logger        | Conversion                                            | Number/Catchment                                                                                                                                                               |   |
|-----------------------------------------------------|---------------|-------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---|
| V1052523.hobo,v2052523.hobo                         | Hobo20        | To convert to .csv using another name than ISCO files.Often V1052523.csv.csv when it does the compensation or is open in.hobo | 2/V1V2                                                                                                                                                                         |   |
| s1052523.hobo, S2052523.hobo                        | Hobo20        | To convert to .csv                                    | 2/V1V2                                                                                                                                                                         |   |
| E1052523.hobo,E2052523.hobo                         | Hobo20        | To convert to .csv                                    | 2/V1V2                                                                                                                                                                         |   |
| V3P052523.hobo,v4P052523.hobo                       | Hobo20        | To convert to .csv                                    | 2/V3V4                                                                                                                                                                         |   |
| bar3052523.hobo, bar4052523.hobo                    | Hobo20        | To convert to .csv                                    | For wells, V1 ,V2 and V3 are compensated with the barometer of   catchment 3 (bar3 or new barometer bar04 files). V4 is compensated with the   barometer of catchment 4 (bar4) |   |
| v3052523.csv,v4052523.csv,v2052523.csv,v1052523.csv | ISCO Nivel    | already are in .csv in the original folder                            | 2/V1V2, 2/V3V4                                                                                                                                                                 |   |
| v4052523vel.csv,v3052523vel.csv                     | ISCO Velocity | already .csv in the folder                            | 2/V3V4                                                                                                                                                                         |   |











