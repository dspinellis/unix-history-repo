: run this script through /bin/sh

OFS="$IFS" IFS=:

for A in rprompt hostname uname who
do
    for D in $PATH
    do
	if [ ! -f $D/$A ]; then
	    continue
	fi
	case $A in
	    rprompt)    LOCAL=`$A %h`
			;;
	    hostname)   LOCAL=`$A`
			;;
	    uname)	LOCAL=`$A -n`
			;;
	    who)	LOCAL=`$A am i | sed -e 's%^\(.*\)!.*$%\1%'`
			;;
	esac
	break
    done
    if [ "x$LOCAL" != x ]; then
	break
    fi
done

IFS=

if [ ! -r version.major ]; then echo 6 > version.major; fi
if [ ! -r version.minor ]; then echo 2 > version.minor; fi
if [ ! -r version.local ]; then echo 0 > version.local; fi
echo `cat version.major` `cat version.minor` `cat version.local` > version
rm -f version.c version.local

awk '	{ major = $1; minor = $2; local = $3 + 1}\
END	{ printf "char *version = \"MH %d.%d #%d[UCI] ", major, minor, local > "version.c"; \
	  printf ".ds MH %d.%d #%d[UCI]\n", major, minor, local > "../doc/version.rf"; \
	  printf "%d\n", local > "version.local"; }' < version
echo '('$LOCAL') of '`date`'";' >> version.c

rm -f version
