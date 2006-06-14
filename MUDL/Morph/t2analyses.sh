#!/bin/sh


#fst=/usr/local/share/moot/mootm-stts-lemmas-nocase-initial.gfst.gz
#lab=/usr/local/share/moot/mootm-stts.lab
##--
fst=mootm-stts-lemmas-nocase-initial.gfst
lab=mootm-stts.lab

##-- MAIN
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
     -a \
     -s"$lab" \
     -m"$fst" \
 | grep "${tab}." \
 | recode latin1..utf8
