if $1x = x goto argcnt
echo stripping and installing $1 $2 $3 $4 $5 $6 $7 $8 $9
: loop
if $1x = x goto done
echo $1:
ed $1
1a
%beginstrip%
.
g/%beginstrip%/d
i
.\" This version has had comments stripped; an unstripped version is available.
.
+,$g/[.	]\\".*/s///
g/[ 	][ 	]*$/s///
g/^$/d
g/\\n@/d
w _mac_temp_
q
if $1 = tmac.e goto mainmac
cp _mac_temp_ /usr/lib/me/$1
goto endloop
: mainmac
cp _mac_temp_ /usr/lib/tmac.e
: endloop
rm _mac_temp_
shift
goto	loop
: done
cp revisions /usr/lib/me/revisions
echo	"Done"
exit
: argcnt
echo "Usage: install <-me file list>"
exit
