static char sccsid[] = "@(#)listen.c	4.1	(Berkeley)	10/2/82";

/*
	listen.c

	listen for a packet from the program interact.c and print it
*/
# include "defs.h"
main(argc,argv)
	  char **argv;
{
	struct packet *pp;
	char buf[BUFSIZ];
	setupdaemon(argc,argv);
	initseqno();
	putchar('!');
	fflush(stdout);
	for(;;){
		pp = getpacket();
		printpacket(pp,buf);
		printf("got %s\n",buf);
		if(pp == NULL )continue;
		pp->pcode = ACK;
		pp->len = 0;
		sendpacket(pp);
		printpacket(pp,buf);
		printf("sent %s\n",buf);
	}
}
