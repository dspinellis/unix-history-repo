#include "stdio.h"

makpipe()
{
	int f[2];

	pipe(f);
	if (fork()==0) {
		close(f[1]);
		close(0);
		dup(f[0]);
		close(f[0]);
		execl ("/bin/sh", "sh", "-i", 0);
		execl ("/usr/bin/sh", "sh", "-i", 0);
		write(2,"Exec error\n",11);
	}
	close(f[0]);
	sleep(2);	/* so shell won't eat up too much input */
	return(f[1]);
}
