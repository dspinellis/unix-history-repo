/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
main(argc,argv)
  char **argv;
	{
	int i,c;
	char buf[2000];
	setupdaemon(argc,argv);
	sendreset();
	for(;;){
		putchar('?');
		i = 0;
		while((c = getchar()) != '\n' && c != EOF)
			buf[i++] = c;
		if(xwrite(buf,1,i) == WRITEFAIL)fprintf(stderr,"writefail\n");
		}
	}
