#include "ex.h"
/*
 * Ex - a text editor
 * Bill Joy UCB October 1977
 */

char	exrecover[]	"/usr/lib/exrecover";
char	expreserve[]	"/usr/lib/expreserve";

recover()
{
	int pvec[2];

	if (pipe(pvec) < 0)
		error("Can't make pipe for recovery");
	pid = fork();
	io = pvec[0];
	if (pid < 0) {
		close(pvec[1]);
		error("Can't fork to execute recovery");
	}
	if (pid == 0) {
		close(2);
		dup(1);
		close(1);
		dup(pvec[1]);
	     /* close(pvec[1]); */
	     /* execl(exrecover+4, exrecover+10, value(DIRECTORY), file, 0); */
		execl(exrecover, exrecover+10, value(DIRECTORY), file, 0);
		die++;
		close(1);
		dup(2);
		error(" No recovery routine");
	}
	close(pvec[1]);
}

waitfor()
{

	do
		rpid = wait(&status);
	while (rpid != pid && rpid != -1);
	status = (status >> 8) & 0377;
}

revocer()
{

	waitfor();
	if (pid == rpid && status != 0)
		value(EDITED) = 0;
	else
		change();
}

preserve()
{

	if (savedfile[0] == 0)
		error("No remembered filename");
	synctmp();
	pid = fork();
	if (pid < 0)
		error("Can't fork to do preserve");
	if (pid == 0) {
		close(0);
		dup(tfile);
	     /* execl(expreserve+4, expreserve+10, 0); */
		execl(expreserve, expreserve+10, 0);
		die++;
		error("No preservation routine");
	}
	waitfor();
	if (rpid == pid && status == 0) {
		sync();
		lprintf("File \"%s\" preserved.  Please seek help immediately.\n", savedfile);
		return (1);
	}
	return (0);
}
