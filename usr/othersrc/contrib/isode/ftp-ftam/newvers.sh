#! /bin/sh
#
#       @(#)newvers.sh	1.1     2/5/86
#
if [ ! -r version ]; then echo 0 > version; fi
touch version
awk '	{	version = $1 + 1; }\
END	{	printf "char version[] = \"Version 4.%d ", version > "vers.c";\
		printf "%d\n", version > "version"; }' < version
echo `date`'";' >> vers.c
