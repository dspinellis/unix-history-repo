# include <stdio.h>
# include "mach.h"
# include "Paths.h"
/*
 * this is a simple program to start up the net daemon,
 * and when it fails, restart it
 * 
 * 
 */
static int daemon = 32767;		/* a nice safe process number */
main(argc,argv)
  char **argv; {
	char *s,mchTo;
	int r,killit(),pid;
	while((pid = fork()) == -1)sleep(2);
	if(pid != 0)exit(0);
	submit(getpid());
	signal(SIGQUIT,SIG_IGN);
	signal(SIGHUP,SIG_IGN);
	signal(SIGINT,SIG_IGN);
	signal(SIGTRM,killit);
	s = argc == 1 ? 0 : argv[1];
	mchTo = argc == 1 ? 'a' : lookup(s);
	for(;;){
		while((daemon=fork()) == -1)sleep(2);
		if(daemon == 0){
			execl(NETDAEMON,"netdaemon",s,0);
			exit(1);
			}
		wait(&r);
		addtolog(mchTo,"Net daemon exit code %d, low byte %o\n",
			(r>>8), (r&0377));
		sleep(100);		/* avoid looping too fast */
		}
	}
killit(){
	kill(daemon,SIGTRM);		/* send terminate */
	sleep(2);			/* wait till cleanup */
	kill(daemon,SIGKIL);		/* kill in case too */
	exit(0);
	}
