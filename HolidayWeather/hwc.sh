#!/usr/bin/env bash

# Command Syntax
# hwc -s <start MMDD> -e <end MMDD> -l <country/city> -l <and another> -l <and so on>

API_KEY='PUT YOUR OWN API KEY HERE'
API_REQ="http://api.wunderground.com/api/${API_KEY}/planner_"

cat <<EOF > Output.html
<html>
<head>
<link rel="stylesheet" type="text/css" href="table.css" media="screen" />
<title>Weather Conditions Results For Selected Cities and Dates</title>
</head>
<body>
<div class="datagrid">
<table>
<thead>
<tr><th>Location</th><th>Average High</th><th>Average Low</th><th>Precipitation</th>
<th>Sunny Day</th><th>Partly Cloudy Day</th><th>Cloudy Day</th><th>Rainy Day</th><th>Windy Day</th></tr>
</thead>
<tbody>
EOF

ROWSTYLE='dummy'
ALTROWSTYLE='alt'
while getopts ":s:e:l:" OPTION
do
case ${OPTION} in
s) START=${OPTARG};;
e) END=${OPTARG};;
l) wget -q -O - "${API_REQ}${START}${END}/q/${OPTARG}.xml" | xsltproc -stringparam location "${OPTARG}" -stringparam rowstyle ${ROWSTYLE} WeatherData.xsl - >> Output.html;;
esac
read ROWSTYLE ALTROWSTYLE <<< "${ALTROWSTYLE} ${ROWSTYLE}"
done

echo '</tbody></table></div></body></html>' >> Output.html

x-www-browser Output.html &
