#sccsid	newvers.sh	1.3	85/05/08
if [ ! -r version ]; then echo 0 > version; fi
touch version
awk '	{	version = $1 + 1; }\
END	{	printf "static char sccs[4] = '"{'@', '(', '#', ')'}"';\n";\
		printf "char version[] = \"4.3 BSD UNIX #%d: ", version ;\
		printf "%d\n", version > "version"; }' > vers.c < version
echo `date`'\n    '$USER'@'`hostname`':'`pwd`'\n";' >> vers.c
