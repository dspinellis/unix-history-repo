touch version
awk '	{	version = $1 + 1; }\
END	{	printf "char *version = \"Berkeley VAX/UNIX Version 4.%d  ", version > "vers.c";\
		printf "%d\n", version > "version"; }' < version
echo `date`'\n";' >> vers.c
