static char sccsid[] = "@(#)netstart.c	4.1	(Berkeley)	%G%";

# include "defs.h"
/* sccs id variable */
static char *netstart_sid = "@(#)netstart.c	1.2";

/*
 * this is a simple program to start up the net daemon,
 * and when it fails, restart it
 * 
 */
static int daemon = 32767;		/* a nice safe process number */
main(argc,argv)
  char **argv; {
	int r,killit(),pid;
	while((pid = fork()) == -1)sleep(2);
	if(pid != 0)exit(EX_OK);
	submit(getpid());
	signal(SIGQUIT,SIG_IGN);
	signal(SIGHUP,SIG_IGN);
	signal(SIGINT,SIG_IGN);
	signal(SIGTERM,killit);
	/* will set remote */
	parseargs(argc,argv);
	addtolog(remote,"Netstart started, process id #%d\n",getpid());
	for(;;){
		while((daemon=fork()) == -1)sleep(2);
		if(daemon == 0){
			argv[0] = "netdaemon";
			argv[argc] = 0;
			execv(NETDAEMON,argv);
			exit(EX_UNAVAILABLE);
			}
		wait(&r);
		addtolog(remote,"Net daemon exit code %d, low byte 0%o\n",
			(r>>8), (r&0377));
		sleep(100);		/* avoid looping too fast */
		}
	}
killit(){
	kill(daemon,SIGTERM);		/* send terminate */
	sleep(2);			/* wait till cleanup */
	kill(daemon,SIGKILL);		/* kill in case too */
	exit(EX_OK);
	}
/* add to allow netstart.c to load */
initseqno(){};
