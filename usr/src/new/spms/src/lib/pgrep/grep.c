/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <signal.h>
#include <stdio.h>
#include <sys/param.h>
#include "null.h"
#include "slist.h"
#include "system.h"

#define MAXNAMLEN	255
#define READ		0
#define	WRITE		1

static int popen_pid;			/* process identity */

/*
 * closegrep() closes the pipe from the grep command and returns the
 * the exit status of the grep command.
 */ 
closegrep(fp)
	FILE *fp;			/* pipe file pointer */
{
	register (*hstat)();		/* hangup function pointer */
	register (*istat)();		/* interrupt function pointer */
	register (*qstat)();		/* quit function pointer */
	register int w;			/* a child id */
	int status;			/* child return status */

	fclose(fp);

	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);

	while ((w = wait(&status)) != popen_pid && w != -1)
		continue;

	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	signal(SIGHUP, hstat);

	status >>= NBBY;
	status &=  0xff;
	return(status);
}



/*
 * grep() returns a singly-linked list of file names from the grep
 * command. NULL is returned on error.
 */
SLIST *
grep(greppath, grepargv)
	char *greppath;			/* grep command path */
	char **grepargv;		/* grep argv */
{
	char filename[MAXNAMLEN + 1];	/* receiving filename buffer */
	char *readgrep();		/* read input from grep */
	char *slappend();		/* append key */
	FILE *fp;			/* input stream from grep */
	FILE *opengrep();		/* open pipe to grep */
	int closegrep();		/* close pipe to grep */
	SLIST *filelist;		/* list of file names */
	SLIST *slinit();		/* initialize list */

	if ((fp = opengrep(greppath, grepargv)) == NULL)
		return(NULL);

	filelist = slinit();

	while (readgrep(filename, fp) != NULL)
		if (slappend(filename, filelist) == NULL)
			return(NULL);
	
	if (closegrep(fp) == 2)
		return(NULL);
	return(filelist);
}



/*
 * opengrep() opens a pipe to read from the grep command. A file
 * pointer is returned, or NULL on error.
 */
FILE *
opengrep(greppath, grepargv)
	char *greppath;			/* grep command path */
	char **grepargv;		/* grep argv */
{
	FILE *fdopen();			/* associate stream with file descrip */
	int p[2];			/* pipe file descriptors */

	if (pipe(p) < 0)
		return(NULL);
	if ((popen_pid = FORK()) == 0)
		{
		close(p[READ]);
		dup2(p[WRITE], WRITE);
		close(p[WRITE]);
		execv(greppath, grepargv);
		_exit(1);
		}
	if (popen_pid == -1)
		return(NULL);
	close(p[WRITE]);
	return(fdopen(p[READ], "r"));
}



/*
 * readgrep() reads a line from the grep stream. The newline character
 * is replaced by a null character. Returns buf, or NULL at EOF.
 */
char *
readgrep(buf, fp)
	char *buf;			/* receiving buffer */
	register FILE *fp;		/* input stream */
{
	register char *bp;		/* receiving buffer pointer */
	register int c;			/* current character */

	bp = buf;
	while((c = getc(fp)) != '\n' && c != EOF)
		*bp++ = c;
	if (c == EOF && bp == buf)
		return(NULL);
	*bp = '\0';
	return(buf);
}
