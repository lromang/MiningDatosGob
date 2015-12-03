#!/bin/bash
for i in `cat urls.txt`
do
    urlstatus=$(curl -k -o /dev/null --silent --head --write-out '%{http_code}' "$i" )
    echo "$i $urlstatus"
    echo  "$i,$urlstatus" >> urlstatus.csv
done 
