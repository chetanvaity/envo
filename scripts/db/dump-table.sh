#!/bin/sh

TBL=$1
BASE_DIR=/home/chetanvaity

SCHEMA_SUFFIX=_schema.sql
DATA_SUFFIX=.sql
FULL_SUFFIX=_f.sql

DBNAME=freecharge
USER=abc
PASSWD=pqr

echo Dumping schema for $TBL
mysqldump --no-data -u $USER -p$PASSWD $DBNAME $TBL > $TBL$SCHEMA_SUFFIX

echo Dumping data for $TBL
mysqldump --quick --no-create-info -u $USER -p$PASSWD $DBNAME $TBL > $TBL$DATA_SUFFIX

echo --- Edit $TBL$SCHEMA_SUFFIX and remove KEY statements. Press any key when done
read

echo Concatenating all into $TBL$FULL_SUFFIX
cat $BASE_DIR/dump-head.sql $TBL$SCHEMA_SUFFIX $TBL$DATA_SUFFIX $BASE_DIR/dump-tail.sql > $TBL$FULL_SUFFIX

echo Done!
