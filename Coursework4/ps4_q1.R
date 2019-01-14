library(tidyverse)
library(dbplyr)
library(Lahman)

# Create a local SQLlite database of the Lahman data
lahman = lahman_sqlite()

A1 = lahman %>% tbl(sql('
-------------------------------------------------------------------------------------
-- SQL can not make aggregate on aggregate, so we need to do sum first and the max --
-------------------------------------------------------------------------------------
SELECT nameFirst, nameLast, debut, birthCountry, max(sh) as Hits
FROM ( 
       ------------------------------
       -- This is the sum aggregate--
       ------------------------------
       SELECT m.playerID, nameFirst, nameLast, debut, birthCountry, sum(H) as sh
       ----------------------------------------------------------------------------------------------------------       
       -- Remember to have all the other variables in SELECT, otherwise you will find error in the SELECT above--
       ----------------------------------------------------------------------------------------------------------
       FROM batting b
       LEFT JOIN (
                   SELECT playerID, nameFirst, nameLast, debut, birthCountry
                   FROM Master
                 ) m
       ON m.playerID = b.playerID
       GROUP BY m.playerID
      ) 
GROUP BY birthCountry
HAVING sh > 200
ORDER BY sh
'
))
