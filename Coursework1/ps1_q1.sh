#!/bin/bash

#Question 1

#Part A
#i
cut -d, -f2 recs2015_public_v3.csv|grep 3|wc -l
#ii
cut -d, -f1,475-571 recs2015_public_v3.csv > recs2015_public_sum.csv && gzip recs2015_public_sum.csv

#Part B
#i
for i in {1..4}
do
    cut -d, -f2 recs2015_public_v3.csv|grep $i|wc -l
done
#ii
cut -d, -f2,3 recs2015_public_v3.csv|sort|uniq > region_division.txt