if [ ! -r version ]; then echo 0 > version; fi
touch version
awk '	{	version = $1 + 1; }\
END	{	printf "char version[] = \"4.1c BSD UNIX #%d: ", version > "vers.c";\
		printf "%d\n", version > "version"; }' < version
echo `date`'\n";' >> vers.c
