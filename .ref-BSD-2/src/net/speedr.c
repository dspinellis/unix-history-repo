/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
struct packet *xptr, *gptr;
main(argc,argv)
  char **argv; {
	int savd;
	char buf[2000];
	int i,n;
	debugflg = DBV;
	setupdaemon(argc,argv);
	savd = datasize;
	putchar('!');
	for(;;){
		lastseqno = -1;
		datasize = savd;
		gptr = xptr = NULL;
		while(getreset() == BROKENREAD);
		while(nread(&n,1,2) == BROKENREAD);
		printf("Length = %d\n",n);
		datasize = n;
		xptr = gptr = NULL;
		for(;;){
			while((i=nread(buf,1,n)) == BROKENREAD);
			if(i != n)putchar('#');
			if(buf[0] == 'Z')break;
			}
		}
	}
