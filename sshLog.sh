#!/bin/bash

CAT_LOG="cat /var/log/auth.log"

echo "<html><title>SSH Proxy Watcher</title><body><h1><center>SSH Proxy Usage</center></h1><br/>" > $1
echo `date` >> $1
IP_ADDRS=( `${CAT_LOG} | grep 'Accepted publickey for rfenced from' | cut -d' ' -f11` )
PARENT_PIDS=( `${CAT_LOG} | grep 'Accepted publickey for rfenced from' | cut -d' ' -f5 | cut -d[ -f2 | cut -d] -f1` )

j=0
for (( i = 0 ; i < ${#PARENT_PIDS[@]} ; i++ ))
do
  CHILD_PID=`${CAT_LOG} | grep 'debug2: User child is on pid' | grep ${PARENT_PIDS[$i]} | cut -d' ' -f12`
  if [ "${CHILD_PID}" != "" ]; then
      #Has spawned a child proxy
      REQUESTED=( `${CAT_LOG} | grep 'debug1: server_request_direct_tcpip: originator' | grep ${CHILD_PID} | cut -d' ' -f1-4,13` )
      REVERSE_CLIENT_IP=`host ${IP_ADDRS[$i]} | cut -d' ' -f2,4 | xargs echo`
      echo "<br/><h3><hr/><br/><table border=2>" >> $1
      echo "<tr><td>CLIENT</td><td>${REVERSE_CLIENT_IP}</td></tr>" >> $1
      echo "<tr><td>PARENT</td><td>${PARENT_PIDS[$i]}</td></tr>" >> $1
      echo "<tr><td>CHILD</td><td>${CHILD_PID}</td></tr>" >> $1
      echo "</table><br/></h3><h6><table border=1><tr><th>Date</th><th>IP</th><th>Reverse DNS</th></tr>" >> $1
      for (( k = 0; k < ${#REQUESTED[@]} ; k=k+5 ))
      do
	echo "<tr><td>${REQUESTED[@]:k:3}</td>" >> $1
	echo "<td>${REQUESTED[@]:k+4:1}</td><td>" >> $1
 	TEST_IP=${REQUESTED[k+4]} 
        SHOULD_BE_SAME_IF_NUMERIC_IP=`echo ${TEST_IP} | awk '/[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+/'`
        if [ "${TEST_IP}" == "${SHOULD_BE_SAME_IF_NUMERIC_IP}" ]; then       
	   host ${TEST_IP} | xargs echo | cut -d' ' -f2 >> $1 
        fi
        echo "</td></tr>" >> $1
      done
      echo "</h6></table>" >> $1
      j=`expr $j + 1`
  fi
done

echo "</body></html>" >> $1
