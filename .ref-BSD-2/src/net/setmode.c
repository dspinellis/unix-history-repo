/* Copyright (c) 1979 Regents of the University of California */
/* used to set the mode to cat files
   to check the net tty drivers */
# include <stdio.h>
# include <sgtty.h>
main(argc,argv)
  char **argv; {
	struct sgttyb stt;
	FILE *readtty;
	if(fork() != 0)exit(0);
	printf("kill %d\n",getpid());
	if(argc < 2)
# ifdef VAX
		argv[1] = "/dev/net-Cory";
# endif
# ifdef CORY
		argv[1] = "/dev/net-Vax";
# endif
# ifdef LOCALC
		argv[1] = "/dev/net-E";
# endif
# ifdef LOCALE
		argv[1] = "/dev/net-C";
# endif
	readtty = fopen(argv[1],"w");
	if(readtty == NULL)goto err;
	if(gtty(fileno(readtty),&stt) < 0)goto err;
	stt.sg_ispeed = stt.sg_ospeed = 9;  /* 1200 baud */
	stt.sg_erase = stt.sg_kill = 0;		/* erase and kill off */
	stt.sg_flags = ANYP;	/* even and odd parity, off everything else */
	if(stty(fileno(readtty),&stt) < 0)goto err;
	sleep(30000);
err:
	perror(argv[1]);
	exit(1);
	}
