/* Copyright (c) 1979 Regents of the University of California */
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
	char *s;
	int i,r,killit();
	if(fork() != 0)exit(0);
	submit(getpid());
	signal(SIGQUIT,SIG_IGN);
	signal(SIGHUP,SIG_IGN);
	signal(SIGINT,SIG_IGN);
	signal(SIGTERM,killit);
	s = argc == 1 ? 0 : argv[1];
	for(;;){
		while((daemon=fork()) == -1)sleep(2);
		if(daemon == 0){
			execl(NETDAEMON,"netdaemon",s,0);
			exit(1);
			}
		wait(&r);
		sleep(100);		/* avoid looping too fast */
		}
	}
killit(){
	kill(daemon,SIGTERM);		/* send terminate */
	sleep(2);			/* wait till cleanup */
	kill(daemon,SIGKILL);		/* kill in case too */
	exit(0);
	}
