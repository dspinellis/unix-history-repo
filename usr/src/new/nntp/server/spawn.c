#ifndef lint
static	char	*sccsid = "@(#)spawn.c	1.1	(Berkeley) 10/15/87";
#endif

#include "../common/conf.h"

#include "common.h"

#include <signal.h>

#ifdef XFER_TIMEOUT
static int	xfer_lines;
static int	old_xfer_lines;
#endif

static char	tempfile[256];

/*
 * spawn -- create a child process with the input from the client
 * as stdin.
 *
 *	Parameters:	"path" is the path of the program to invoke.
 *			"name" is the name to call the program.
 *			"flag" is a single flag to be passed to the program.
 *			"cont_code" is the response code to transmit
 *			on successful startup.
 *			"err_code" is the response code to transmit when
 *			something goes wrong.
 *
 *	Returns:	-1 on non-zero return from child,
 *			0 on error before fork/exec,
 *			1 otherwise.
 *
 *	Side effects:	Creates and removes temporary file;
 *			accepts input from client; forks and execs.
 *			Can time out if XFER_TIMEOUT is defined.
 */

spawn(path, name, flag, cont_code, err_code, errbuf)
	char		*path;
	char		*name;
	char		*flag;
	int		cont_code;
	int		err_code;
	char		*errbuf;
{
	char		line[MAX_STRLEN];
	register char	*cp;
	int		i, fd;
	int		fds[2];
	int		pid, npid;
	int		exit_status;
#ifdef XFER_TIMEOUT
	int		xfer_timeout();
	int		(*otimeout)();
#endif
	union wait	status;
	register FILE	*fp;

	(void) strcpy(tempfile, "/tmp/rpostXXXXXX");
	(void) mktemp(tempfile);

	fp = fopen(tempfile, "w");
	if (fp == NULL) {
		printf("%d Cannot create temporary file.\r\n", err_code);
		(void) fflush(stdout);
		return (0);
	} else {
		printf("%d Enter news, period on a line by itself to end.\r\n",
			cont_code);
		(void) fflush(stdout);
	}

#ifdef XFER_TIMEOUT
	xfer_lines = old_xfer_lines = 0;
	otimeout = signal(SIGALRM, xfer_timeout);
	(void) alarm(XFER_TIMEOUT);
#endif

	while (fgets(line, sizeof(line), stdin) != NULL) {
#ifdef XFER_TIMEOUT
		xfer_lines++;
#endif
		if ((cp = index(line, '\r')) != NULL)
			*cp = '\0';
		else if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';

		if (line[0] == '.' && line[1] == '\0')
			break;

		if (line[0] == '.')
			fputs(line+1, fp);
		else
			fputs(line, fp);
		putc('\n', fp);
	}
	(void) fclose(fp);

#ifdef XFER_TIMEOUT
	(void) alarm(0);
	(void) signal(SIGALRM, otimeout);
#endif

	/* See if the connection got closed somehow... */

	if (line[0] != '.' && line[1] != '\0') {
		(void) unlink(tempfile);
#ifdef SYSLOG
# ifdef LOG
		syslog(LOG_ERR, "%s spawn: EOF before period on line by itself",
			hostname);
# else
		syslog(LOG_ERR, "spawn: EOF before period on line by itself");
# endif
#endif
		return (0);
	}
		
#ifdef POSTER
	(void) chown(tempfile, uid_poster, gid_poster);
#endif

	/* Set up a pipe so we can see errors from rnews */

	if (pipe(fds) < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "spawn: pipe: %m");
#endif
		(void) unlink(tempfile);
		return (-1);
	}

	/*
	 * Ok, now we have the article in "tempfile".  We
	 * should be able to fork off, close fd's 0 to 31 (or
	 * whatever), open "tempfile" for input, thus making
	 * it stdin, and then execl the inews.  We think.
	 */

	pid = vfork();
	if (pid == 0) {		/* We're in child */
#ifdef POSTER
		(void) setuid(uid_poster);
		(void) setgid(gid_poster);
#endif

		/* Set up stdout and stderr for child */

		if (fds[1] != 1) {
			(void) dup2(fds[1], 1);
			(void) close(fds[1]);
		}
		(void) dup2(1, 2);

		for (i = 3; i < 10; ++i) /* XXX but getdtablesize is too big */
			(void) close(i);

		fd = open(tempfile, O_RDONLY);
		if (fd != 0) {
			(void) dup2(fd, 0);
			(void) close(fd);
		}

		execl(path, name, flag, (char *) NULL);
		fprintf(stderr, "spawn: execl ");
		perror(path);
		_exit(-1);	/* Error */
	} else {
		(void) close(fds[1]);
		fp = fdopen(fds[0], "r");
		if (fp == NULL) {
			printf("%d Cannot fdopen %s pipe\r\n", err_code, path);
			(void) fflush(stdout);
#ifdef SYSLOG
			syslog(LOG_ERR, "spawn: pipe: %m");
#endif
			(void) unlink(tempfile);
			return (0);
		}

		if (errbuf)
			*errbuf = '\0';

		while (fgets(line, sizeof (line), fp) != NULL) {
			if (line[0] != '\n') {
				if (errbuf) {
					if (cp = index(line, '\n'))
						*cp = '\0';
					(void) strcat(errbuf, line);
					(void) strcat(errbuf, "\\");
				}
#ifdef SYSLOG
				syslog(LOG_ERR, "%s: %s", path, line);
#endif
			}
		}

		while ((npid = wait(&status)) > 0)
			if (npid == pid) {
				exit_status = status.w_T.w_Retcode;
				break;
			}

		(void) fclose(fp);
		(void) unlink(tempfile);
		(void) fflush(stdout);
		if (npid < 0) {
#ifdef SYSLOG
			syslog(LOG_ERR, "spawn: wait pid %d: %m", pid);
#endif
			return (-1);
		}

#ifdef SYSLOG
		if (exit_status != 0)
			syslog(LOG_ERR, "spawn: %s exit status %d",
				path, exit_status);
#endif
			
		return (exit_status ? -1 : 1);
	}
}

#ifdef XFER_TIMEOUT

xfer_timeout()
{
	if (old_xfer_lines < xfer_lines) {
		old_xfer_lines = xfer_lines;
		(void) alarm(XFER_TIMEOUT);
		return;
	}

	/* Timed out. */

	printf("%d timeout after %d seconds, closing connection.\r\n",
		ERR_FAULT, XFER_TIMEOUT);
	fflush(stdout);

#ifdef LOG
	syslog(LOG_ERR, "%s transfer_timeout", hostname);
#endif LOG

	(void) unlink(tempfile);

	exit(1);
}

#endif XFER_TIMEOUT
