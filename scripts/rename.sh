#!/bin/bash

# Needs GNU find, GNU sed, GNU grep
# brew install findutils
# brew install grep
# brew install gnu-sed

usage() { echo "Usage: $0 -d <directory-for-replace> -p <pattern> -z <replacement>"; exit 1; }

while getopts "d:p:z:" o; do
    case "${o}" in
        d)
            RDIR=${OPTARG}
            ;;
        p)
            PATTERN=${OPTARG}
            ;;
        z)
            REPLACEMENT=${OPTARG}
            ;;
        *)
            usage
            ;;
    esac
done

[ -z "${RDIR}" ] && usage
[ -z "${PATTERN}" ] && usage
[ -z "${REPLACEMENT}" ] && usage

# Generate list of files containing the string you want to change 
# Then use sed to change their contents
gfind ${RDIR} -type f -print | grep -v ".git" | xargs ggrep -l ${PATTERN} | \
while read FN; do
    echo "Replacing ${PATTERN}->${REPLACEMENT} in file ${FN}";
    gsed -i "s/$PATTERN/$REPLACEMENT/g" ${FN}
done

# Now find directory names to change
# (e.g., find mydir -type d -print | egrep <searchstring>)
gfind ${RDIR} -type d -print | ggrep "${PATTERN}$" | \
while read DN; do
    NEWDN=$(echo "$DN" | sed "s/$PATTERN/$REPLACEMENT/")
    echo "Moving directory ${DN} to ${NEWDN}"
    mv ${DN} ${NEWDN}
done

# And now filenames, very similar to the directory change
gfind ${RDIR} -type f -print | ggrep "${PATTERN}" | \
while read FN; do
    NEWFN=$(echo "$FN" | sed "s/$PATTERN/$REPLACEMENT/")
    echo "Moving file ${FN} to ${NEWFN}"
    mv ${FN} ${NEWFN}
done
