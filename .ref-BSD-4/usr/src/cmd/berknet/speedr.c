/*
	speedr.c

	with speeds.c used to time the links
*/
# include "defs.h"

/* global */
struct daemonparms netd;

main(argc,argv)
  char **argv; {
	char buf[2000];
	int savd, n;
	register int i;
	debugflg = DBV;
	setupdaemon(argc,argv);
	savd = netd.dp_datasize;
	putchar('!');
	for(;;){
		netd.dp_datasize = savd;
		while(getreset() == BROKENREAD);
		while(nread(&n,2) == BROKENREAD);
		printf("Length = %d\n",n);
		netd.dp_datasize = n;
		for(;;){
			while((i=nread(buf,n)) == BROKENREAD);
			if(i != n)putchar('#');
			if(buf[0] == 'Z')break;
			}
		}
	}
