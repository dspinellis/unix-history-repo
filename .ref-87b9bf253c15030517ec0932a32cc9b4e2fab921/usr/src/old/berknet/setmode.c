static char sccsid[] = "@(#)setmode.c	4.1	(Berkeley)	%G%";

/* 
	setmode.c
	
	used to set the mode to cat files to check the net tty drivers
*/
# include <stdio.h>
# include <sgtty.h>
main(argc,argv)
  char **argv; {
	struct sgttyb stt;
	FILE *readtty;
	if(fork() != 0)exit(0);
	printf("kill %d\n",getpid());
	readtty = fopen(argv[1],"w");
	if(readtty == NULL)goto err1;
	if(gtty(fileno(readtty),&stt) < 0)goto err2;
	stt.sg_ispeed = stt.sg_ospeed = 9;  /* 1200 baud */
	stt.sg_erase = stt.sg_kill = 0;		/* erase and kill off */
	stt.sg_flags = ANYP;	/* even and odd parity, off everything else */
	if(stty(fileno(readtty),&stt) < 0)goto err3;
	sleep(30000);
err1:
	printf("Error1: ");
	perror(argv[1]);
	exit(1);
err2:
	printf("Error2: ");
	perror(argv[1]);
	exit(1);
err3:
	printf("Error3: ");
	perror(argv[1]);
	exit(1);
	}
