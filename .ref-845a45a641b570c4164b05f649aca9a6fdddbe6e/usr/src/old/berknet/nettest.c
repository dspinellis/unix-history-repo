static char sccsid[] = "@(#)nettest.c	4.1	(Berkeley)	%G%";

/*

	nettest.c

	fire up two daemons with pipes in between them
	currently, one is the "r" machine, the other the "v" machine
*/
# include "defs.h"

# define NETD "/usr/net/bin/netdaemon"

main(argc,argv)
  char **argv; {
	int pip1[2],pip2[2];
	char b1[20],b2[20],b3[20];
	char *margv[30];
	int i;
	debugflg = 1;
	for(i =0; i < argc; i++)margv[i] = argv[i];
	pipe(pip1);
	pipe(pip2);
	if(fork()){
		/* read pip1[0], write pip2[1] */
		close(pip1[1]); close(pip2[0]);
		sprintf(b1,"-r%d",pip1[0]);
		sprintf(b2,"-w%d",pip2[1]);
		margv[i++] = b1;
		margv[i++] = b2;
		margv[i++] = "-mr";
		margv[i] = 0;
		mexecv(NETD,margv);
		}
	/* read pip2[0], write pip1[1] */
	close(pip2[1]); close(pip1[0]);
	sleep(5);
	sprintf(b1,"-r%d",pip2[0]);
	sprintf(b2,"-w%d",pip1[1]);
	margv[i++] = b1;
	margv[i++] = b2;
	margv[i++] = "-mv";
	margv[i] = 0;
	mexecv(NETD,margv);
	}
