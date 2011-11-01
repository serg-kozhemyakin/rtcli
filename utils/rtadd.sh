#!/bin/sh

# main purpose -- scan dir tree, look for torrent files
# and add them to rtorrent

if [ $# = 0 ]; then
    echo "Usage: rtadd <root> <connection url>"
    exit 1
fi

root=$1
url=$2
if [ -z "$root" -o ! -d "$root" ]; then
    echo "Specify correct root directory."
fi
if [ -z "$url" ]; then
    echo "Specify rtorrent connection url"
else
    rtver=`rtcli -c $url -z system.client_version`
    if [ $? != 0 ]; then
        echo "Can't connect to rtorrent at $url"
        exit 1
    fi
fi

# ok. params checked. start scanning.
# find all torrents, add missing, register them as processed
find $root -type f -name *.torrent 2> /dev/null | while read torrent; do
    tdname=`dirname $torrent`
    tbname=`basename $torrent`
    thash=`rtcli -c $url -t $torrent -x`
    add='yes'
    if [ -f "$tdname/processed" ]; then
        # registered line:
        # torrent.file.name torrent.hash
        registered=`grep $tbname $tdname/processed`
        if [ -n "$registered" -a `echo $registered | awk '{print $NF}'` = $thash ]; then
            add='no'
        fi
    else
        touch $tdname/processed
    fi
    if [ $add = "yes" ]; then
        # torrent wasn't registered or its hash was changed
        todir=$tdname
        [ `basename $todir` = ".torrents" ] && todir=`dirname $todir`
        rtcli -c $url -t $torrent -d $todir -a

        # and add it to processed
        grep -v $tbname $tdname/processed > $tdname/processed.temp
        echo $tbname $thash >> $tdname/processed.temp
        mv $tdname/processed.temp $tdname/processed
    fi
done