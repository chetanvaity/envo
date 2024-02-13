#!/bin/bash

# Function to delete all contents of the bucket
empty_bucket() {
    echo "Emptying the bucket: $1"
    aws s3 rm s3://$1 --recursive --profile $2
}

# Function to delete the bucket
delete_bucket() {
    echo "Deleting the bucket: $1"
    aws s3 rb s3://$1 --profile $2
}

# Check for correct number of arguments
if [ $# -ne 2 ]; then
    echo "Usage: $0 <bucket_name> <aws_profile>"
    exit 1
fi

BUCKET_NAME=$1
AWS_PROFILE=$2

# Check if the bucket exists
if aws s3 ls "s3://$BUCKET_NAME" --profile $AWS_PROFILE 2>&1 | grep -q 'NoSuchBucket'; then
    echo "Error: Bucket does not exist"
    exit 1
else
    empty_bucket $BUCKET_NAME $AWS_PROFILE
    if [ $? -eq 0 ]; then
        delete_bucket $BUCKET_NAME $AWS_PROFILE
        if [ $? -eq 0 ]; then
            echo "Bucket deleted successfully"
        else
            echo "Error: Failed to delete bucket"
            exit 1
        fi
    else
        echo "Error: Failed to empty bucket"
        exit 1
    fi
fi

