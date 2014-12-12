#!/bin/bash

mkdir -p ~/MLfinalproject/data/
cd ~/MLfinalproject/data/
mkdir -p testdata
mkdir -p traindata

#Download the training data
cd traindata
s3cmd get -r s3://cmcdf/hive_tables/segment/hispanic/sample/

#Move a random bucket to the testdata
#RANGE=$(($(ls -l | wc -l)-1))
#anumber=$RANDOM
#anumber=$(($RANDOM%$RANGE))
#anumber=$(($anumber+1))
#movefile=$(ls -l | head -n$anumber | tail -n1 | rev | cut -d' ' -f1 | rev)
#mv $movefile ../testdata/

#Copy features of each class to separate files
cd ..
cat weighted_data/* | grep -P '1\t' | awk '{print $2}' > weighted_events.txt
cat weighted_data/* | grep -P '0\t' | awk '{print $2}' | pigz -c > weighted_nonevents.txt.gz
