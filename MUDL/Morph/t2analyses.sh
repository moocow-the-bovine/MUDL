#!/bin/sh

##-- morphology selection
#fstdir=/usr/local/share/moot
fstdir="."
fst="$fstdir/mootm-stts-lemma-segs-nocase-initial.gfst"
lab="$fstdir/mootm-stts.lab"

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
     -e "%" \
     -s"$lab" \
     -m"$fst" \
 | grep "${tab}." \
 | recode latin1..utf8
