/* Copyright (c) 1979 Regents of the University of California */
# include <stdio.h>
# include "mach.h"
# include "Paths.h"

main(argc,argv)
  char **argv; {
	int pip1[2],pip2[2];
	char b1[20],b2[20];
	char *command1,*command2;
	command1 = argc > 1 ? argv[1] : NETDAEMON;
	command2 = argc > 2 ? argv[2] : NETDAEMON;
	pipe(pip1);
	pipe(pip2);
	if(fork()){
		/* read pip1[0], write pip2[1] */
		close(pip1[1]); close(pip2[0]);
		sprintf(b1,"%d",pip1[0]);
		sprintf(b2,"%d",pip2[1]);
		execl(command1,command1,"y",b1,b2,0);
		}
	/* read pip2[0], write pip1[1] */
	close(pip2[1]); close(pip1[0]);
	sprintf(b1,"%d",pip2[0]);
	sprintf(b2,"%d",pip1[1]);
	execl(command2,command2,"v",b1,b2,0);
	}
