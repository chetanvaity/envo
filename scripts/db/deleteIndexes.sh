#!/bin/sh

USER="root"
PASSWORD="passw0rd"
SCHEMA="freecharge_test"

echo "Removing all indexes (not primary) in Database: '$SCHEMA'"

TABLES=$(mysql -u$USER -p$PASSWORD --force -e "SHOW TABLES FROM $SCHEMA" | awk '{if(NR>1) print $1;}')
    
for t in $TABLES; do
    echo "\tFixing: '$t'"
    echo "SHOW INDEX FROM $t" | mysql -u$USER -p$PASSWORD --force $SCHEMA | awk '{if($3 !~ /Key_name/ && $3 !~ /PRIMARY/) print $1" "$3}' | awk '{print "ALTER TABLE "$1" DROP INDEX "$2";"}' | mysql -u$USER -p$PASSWORD --force $SCHEMA
done
