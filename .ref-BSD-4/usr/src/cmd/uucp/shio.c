#include "uucp.h"


/*******
 *	shio(cmd, fi, fo, user)	execute shell of command with
 *	char *cmd, *fi, *fo;	fi and fo as standard input/output
 *	char *user;		user name
 *
 *	return codes:
 *		0  - ok
 *		non zero -  failed  -  status from child
 */

shio(cmd, fi, fo, user)
char *cmd, *fi, *fo, *user;
{
	int status, f;
	int uid, pid, ret;
	char path[MAXFULLNAME];

	if (fi == NULL)
		fi = "/dev/null";
	if (fo == NULL)
		fo = "/dev/null";

	DEBUG(3, "shio - %s\n", cmd);
	if ((pid = fork()) == 0) {
		close(Ifn);
		close(Ofn);
		close(0);
		f = open(fi, 0);
		ASSERT(f == 0, "BAD OPEN fileno %d", f);
		close(1);
		f = creat(fo, 0666);
		ASSERT(f == 1, "BAD OPEN fileno %d", f);
		if (gninfo(user, &uid, path) == 0)
			setuid(uid);
		execl(SHELL, "sh", "-c", cmd, 0);
		exit(100);
	}
	while ((ret = wait(&status)) != pid && ret != -1);
	DEBUG(3, "status %d\n", status);
	return(status);
}
