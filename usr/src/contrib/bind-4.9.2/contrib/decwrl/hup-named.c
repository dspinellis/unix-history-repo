/* hup-named -- cause the name server to reload its data files
 * vix 16sep91 [written]
 */

#include <stdio.h>
#include <signal.h>
#include <errno.h>

#define PIDFILE "/etc/named.pid"
#define NAMED	"/etc/named"

main() {
	int pid;

	if (-1 == (pid = read_pidfile(PIDFILE))) {
		perror(PIDFILE);
		exit(2);
	}

	if (0 > kill(pid, SIGHUP)) {
		int start_new = (errno == ESRCH);
		perror("kill");
		if (start_new) {
			execl(NAMED, NAMED, NULL);
			perror("execl");
		}
		exit(2);
	}

	exit(0);
}

int
read_pidfile(filename)
	char *filename;
{
	FILE *pidfile = fopen(filename, "r");
	char line[10];
	int pid, error;

	if (!pidfile)
		return -1;
	error = (!fgets(line, sizeof line, pidfile));
	fclose(pidfile);
	if (error)
		return -1;
	pid = atoi(line);
	if (!pid)
		return -1;
	return pid;
}
