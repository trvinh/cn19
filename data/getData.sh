#!/bin/bash

# get data from https://github.com/CSSEGISandData/COVID-19
month=$1
if [ -z "$1" ]; then
  month=$(date +'%m')
elif [ "$month" -lt "10" ]; then
  month="0$month"
fi

for i in {1..31}; do
  if [ "$i" -lt "10" ]; then
    day="0$i"
  else
   day=$i
  fi
  # echo "$month-$day"
  if ! [ -f "$month-$day-2020.csv" ]; then
    url="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/$month-$day-2020.csv"
    check=$(curl -Is $url | head -1 | cut -d " " -f 2)
    if [ "$check" -eq "200" ]; then
      echo "$month-$day"
      wget $url
    fi
  fi
done;

echo "DONE!"
