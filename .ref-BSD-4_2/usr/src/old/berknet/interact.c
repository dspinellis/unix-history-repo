static char sccsid[] = "@(#)interact.c	4.1	(Berkeley)	10/2/82";

/*
	interact.c

	send a packet to the program "listen"
*/
# include "defs.h"
main(argc,argv)
  char **argv;
	{
	int i,c;
	char buf[2000];
	setupdaemon(argc,argv);
	initseqno();
	for(;;){
		putchar('?');
		i = 0;
		while((c = getchar()) != '\n' && c != EOF)
			buf[i++] = c;
		if(xwrite(buf,i) == WRITEFAIL)fprintf(stderr,"writefail\n");
		}
	}
