#sccsid	newvers.sh	1.4	85/06/03
if [ ! -r version ]; then echo 0 > version; fi
touch version
echo `cat version` ${USER-root} `pwd` `date` `hostname` | \
awk ' {
	version = $1 + 1; user = $2; host = $10; dir = $3; \
	date = $4 " " $5 " " $6 " " $7 " " $8 " " $9;
}\
END {
	printf "char sccs[] = \"@(#)4.3 BSD #%d: %s (%s@%s:%s)\\n\";\n",\
		version, date, user, host, dir ;\
	printf "char version[] = \"4.3 BSD UNIX #%d: %s\\n", \
		version, date; \
	printf "    %s@%s:%s\\n\";\n", user, host, dir;
	printf "%d\n", version > "version";
}' > vers.c
