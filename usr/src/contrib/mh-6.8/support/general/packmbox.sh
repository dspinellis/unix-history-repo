#! /bin/sh
# packmbox - pack an MH folder back into a UUCP-style mbox
# @(#)$Id: packmbox.sh,v 1.16 1992/02/14 17:05:31 jromine Exp $
#
# Defaults:
#    `+folder'	defaults to current folder
#    `msgs' 	defaults to all
#
# Context:
#    Current-Folder
#
# for simplicity (and speed) we don't parse command-line args (much)
case $#/$1 in
   1/-h*) echo "syntax: packmbox [+folder] [msgs] [-help]" 1>&2; exit 0;;
esac

format="%(msg) From \
%<{return-path}%(putstr)%|\
%<(nonnull(mbox{from}))%(putstr)%|nobody%>@\
%<(nonnull(host{from}))%(putstr)%|nowhere%>%> \
%(day{date}) %(month{date}) %2(mday{date}) \
%02(hour{date}):%02(min{date}):%02(sec{date}) \
%(void(year{date}))%<(gt 100)%4(putnum)%|19%02(putnum)%>"

trap 'rm -f /tmp/packm$$; exit 1' 1 2 3 15

scan -noclear -noheader -noreverse -width 256 \
			-format "${format}" $* >/tmp/packm$$
# tricky -- you must do this "cd" after scan has updated the context
cd `mhpath`

exec </tmp/packm$$
rm -f /tmp/packm$$
while read m f
do
    echo "$f"
    sed -e '/^From /s/^/>/' < $m
    echo ""
done
exit
