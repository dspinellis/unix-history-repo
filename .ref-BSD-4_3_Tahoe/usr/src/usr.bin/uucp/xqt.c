#ifndef lint
static char sccsid[] = "@(#)xqt.c	5.4	(Berkeley) 5/4/88";
#endif

#include <signal.h>
#include "uucp.h"

int LocalOnly = 0;

/*LINTLIBRARY*/

/*
 *	start up uucico for rmtname
 *
 *	return codes:  none
 */

#ifdef	VMS
#define	fork	vfork
#endif VMS

xuucico(rmtname)
char *rmtname;
{
	if (fork() == 0) {
		/*  start uucico for rmtname system  */
		char opt[100];
		close(0);
		close(1);
		close(2);
		open(DEVNULL, 0);
		open(DEVNULL, 1);
		open(DEVNULL, 1);
		signal(SIGINT, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGKILL, SIG_IGN);
		if (rmtname[0] != '\0')
			sprintf(opt, "-s%s", rmtname);
		else
			opt[0] = '\0';
#ifndef	VMS
		if (LocalOnly)
			execl(UUCICO, "uucico", "-r1", "-L", opt, (char *)0);
		else
			execl(UUCICO, "uucico", "-r1", opt, (char *)0);
#else	VMS
		/* Under VMS/EUNICE release the batch job */
		if (LocalOnly)
			execl(STARTUUCP, "startuucp", "uucico", "-r1", "-L", opt, (char *)0);
		else
			execl(STARTUUCP, "startuucp", "uucico", "-r1", opt, (char *)0);
#endif	VMS
		exit(100);
	}
#ifdef	VMS
	while(wait(0) != -1)
		;	/* Wait for it to finish!! */
#endif	VMS
	return;
}

/*
 *	start up uuxqt
 *
 *	return codes:  none
 */
xuuxqt()
{
	if (fork() == 0) {
		/*  start uuxqt  */
		close(0);
		close(1);
		close(2);
		open(DEVNULL, 2);
		open(DEVNULL, 2);
		open(DEVNULL, 2);
		signal(SIGINT, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGKILL, SIG_IGN);
		execl(UUXQT, "uuxqt",  (char *)0);
		exit(100);
	}
}

xuucp(str)
char *str;
{
	char text[300];
	if (fork() == 0) {
		/*  start uucp  */
		close(0);
		close(1);
		close(2);
		open(DEVNULL, 0);
		open(DEVNULL, 1);
		open(DEVNULL, 1);
		signal(SIGINT, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		signal(SIGKILL, SIG_IGN);
		sprintf(text, "%s -r %s", UUCP, str);
		execl(SHELL, "sh", "-c", text, CNULL);
		exit(100);
	}
	sleep(15);	/* Give uucp chance to finish */
}
