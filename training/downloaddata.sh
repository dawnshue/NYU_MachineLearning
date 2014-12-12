#!/bin/bash

mkdir -p ~/MLfinalproject/data/
cd ~/MLfinalproject/data/
mkdir -p testdata
mkdir -p traindata

#Download the training data
cd traindata
s3cmd get -r s3://cmcdf/hive_tables/segment/hispanic/sample/

#Move a random bucket to the testdata
RANGE=$(($(ls -l | wc -l)-1))
anumber=$RANDOM
anumber=$(($RANDOM%$RANGE))
anumber=$(($anumber+1))
movefile=$(ls -l | head -n$anumber | tail -n1 | rev | cut -d' ' -f1 | rev)
mv $movefile ../testdata/

#Copy features of each class to separate files
cd ..
cat traindata/* | grep -P '1\t' | awk '{print $2}' > train_events.txt
cat traindata/* | grep -P '0\t' | awk '{print $2}' | pigz -c > train_nonevents.txt.gz

#Copy features for test data
cat testdata/* | grep -P '1\t' | awk '{print $2}' > test_events.txt
cat testdata/* | grep -P '0\t' | awk '{print $2}' | pigz -c > test_nonevents.txt.gz
