#include <stdio.h>
debug(a,b,c,d,e,f,g)
	char *a;
{
	FILE *fd;
	fd = fopen("/tmp/debuglog","a");
	fprintf(fd,a,b,c,d,e,f,g);
	fprintf(fd,"\n");
	fclose(fd);
}

