/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <signal.h>
#include <stdio.h>
#include <sys/param.h>
#include "bin.h"
#include "path.h"
#include "system.h"
#include "yesno.h"

extern char **environ;			/* global environment cell */
static int Isfg;			/* is process in forground? */
static int Status;			/* exit status */
static int Want_to_quit;		/* do we want to quit? */

/*
 * execcmd() executes the COMMAND string in the current directory by
 * forking the shell in the SHELL project environment variable. The
 * PROJECT environment variable is updated before each fork in case
 * the directory belongs to a subproject. If the forking process is in
 * foreground, then an interrupt or quit signal may cause the shell
 * to terminate. Returns non-zero error status if error.
 */
execcmd(project)
	char *project;			/* project root directory pathname */
{
	extern char *COMMAND;		/* command string to be executed */
	extern char *SHELLNAME;		/* name of command shell */
	extern char *SHELLPATH;		/* pathname of command shell */
	extern int CSHELL;		/* use csh or sh? */
	extern int CSHRC;		/* execute .cshrc if csh shell */
	extern int ERRSTATUS;		/* pexec error status */
	extern int IGNORE_BAD_EXIT;	/* exit if shell doesn't return 0 */
	extern int PVINDEX;		/* environ index for PROJECT variable */
	register int (*istat)();	/* interrupt status */
	register int (*qstat)();	/* quit status */
	static char pv[PATHSIZE+8] = "PROJECT=";
					/* PROJECT env variable buffer */
	char *strcpy();			/* string copy */
	int isfg();			/* is a foreground job? */
	int onintr();			/* function called on interrupt */
	int pid;			/* process id of forked child */
	int quit();			/* do we want to quit? */
	int w;				/* process id of dead child */

	/*
	 * The PROJECT environment variable is updated before being
	 * passed to a child process. This could be done by putting
	 * "setenv PROJECT xxxxxx" or "export PROJECT; PROJECT=xxxxxx"
	 * (depending on the shell) in the COMMAND string.
	 */
	strcpy(pv+8, project);
	environ[PVINDEX] = pv;

	if ((pid = FORK()) == 0)
		{
		if (CSHELL && CSHRC == NO)
			{
			execl(SHELLPATH, SHELLNAME, "-f", "-c", COMMAND, 0);
			}
		else	{
			execl(SHELLPATH, SHELLNAME, "-c", COMMAND, 0);
			}
		_exit(127);
		}
	if ((Isfg = isfg()) == YES)
		{
		qstat = signal(SIGQUIT, SIG_IGN);
		if ((istat = signal(SIGINT, SIG_IGN)) != SIG_IGN)
			{
			signal(SIGINT, onintr);
			signal(SIGQUIT, onintr);
			}
		}
	else	{
		istat = signal(SIGINT, SIG_IGN);
		qstat = signal(SIGQUIT, SIG_IGN);
		}
	while ((w = wait(&Status)) != pid && w != -1)
		continue;
	if (w == -1)
		Status = ERRSTATUS;
	else	{
		Status >>= NBBY;
		Status &=  0xff;
		}
	if (Want_to_quit)
		{
		if (quit() == YES)
			exit(Status);
		Want_to_quit = 0;
		Status = 0;
		}
	if (Isfg == NO)				/* if in foreground, we */
		{				/* always want to catch */
		signal(SIGINT, istat);		/* interrupts after they */
		signal(SIGQUIT, qstat);		/* are first turned on */
		}
	if (Status != 0 && IGNORE_BAD_EXIT == NO)
		exit(Status);
	return(Status);
}



/*
 * onintr() resets signals and indicates that the process may want to exit.
 */
onintr()
{
	signal(SIGINT, onintr);
	signal(SIGQUIT, onintr);

	Want_to_quit = 1;
}



/*
 * quit() returns YES if the process should exit, otherwise NO.
 */
quit()
{
	register int (*istat)();	/* interrupt status */
	register int (*qstat)();	/* quit status */
	extern int NOQUERY;		/* query user about quitting? */
	char *bp;			/* buffer pointer */
	char buf[BUFSIZ];		/* input buffer */
	char *quitmsg;			/* quit message string */
	int fd;				/* I/O file descriptor */
	int strlen();			/* string length */

	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);

	if (NOQUERY)
		return(YES);
	if ((fd = OPEN("/dev/tty", O_RDWR, 0600)) == -1)
		{
		pperror("/dev/tty");
		pxexit();
		}
	quitmsg = "Do you really want to quit? [yn](y): ";
	write(fd, quitmsg, strlen(quitmsg));
	if (read(fd, buf, BUFSIZ) <= 0)
		return(YES);
	for (bp = buf; isspace(*bp); bp++)
		continue;
	if (*bp != 'n')
		return(YES);
	close(fd);

	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);

	return(NO);
}
