#!/bin/sh

USERNAME=$USER

echo "Killing all jobs belonging to $USERNAME..."

for j in `qstat|grep $USERNAME|awk '{print $1}'|sed 's|\..*||1'` ; do

  echo "  + killing job $j" ; qdel $j

done

echo "...done"