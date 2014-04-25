#!/bin/sh

USER="root"
PASSWORD="passw0rd"
SCHEMA="freecharge_test"
ROWQUANTUM=10
SLEEPSECONDS=5

for i in `seq 1 10`; do
    echo "`date`: Removing $ROWQUANTUM rows (iteration $i)..."
    mysql -b -u$USER -p$PASSWORD $SCHEMA -e "DELETE FROM freefund_coupon where freefund_class_id=177 order by freefund_coupon_id limit $ROWQUANTUM; SELECT ROW_COUNT()"
    echo "`date`: Sleeping $SLEEPSECONDS seconds..."
    sleep $SLEEPSECONDS
done
