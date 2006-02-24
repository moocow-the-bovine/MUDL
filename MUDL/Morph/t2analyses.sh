#!/bin/sh

tfile="$1"
test -z "$tfile" && tfile="-"

tab=`echo -e "\t"`

cat "$tfile" \
 | cut -f 1 \
 | grep -v '^%' \
 | grep . \
 | sort \
 | uniq \
 | recode utf8..latin1 \
 | mootm \
     -s/usr/local/share/moot/mootm-stts.lab \
     -m/usr/local/share/moot/mootm-stts-lemmas-nocase-initial.gfst.gz \
 | grep "${tab}." \
 | recode latin1..utf8
