#include "mh.h"
#include <signal.h>
#include <stdio.h>

showfile(file)
char *file;
{
	int pid, wpid, intr, status;
	char *vec[4];

	intr = (int) signal(SIGINT, SIG_IGN);
	m_update();
	fflush(stdout);
	if((pid = fork()) == 0) {
		vec[0] = "mh-type";
		vec[1] = file;
		vec[2] = 0;
		signal(SIGINT, (int (*)()) intr);
		execv(showproc, vec);
		perror("Can't exec showproc");
		goto badleave;
	} else if(pid == -1) {
		fprintf(stderr, "No forks!\n");
		goto badleave;
	} else
		while((wpid = wait(&status)) != -1 && wpid != pid) ;
	signal(SIGINT, (int (*)()) intr);
	if(status & 0377)
		goto badleave;
	return(0);

 badleave:
	fflush(stdout);
	return(1);

}

