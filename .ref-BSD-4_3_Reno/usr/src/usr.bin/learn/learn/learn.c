#ifndef lint
static char sccsid[] = "@(#)learn.c	4.5	(Berkeley)	5/10/89";
#endif not lint

#include <sys/signal.h>
#include <stdio.h>
#include "lrnref.h"
#include "pathnames.h"

char	*direct	= _PATH_LEARN;
int	more;
char	*level;
int	speed;
char	*sname;
char	*todo;
FILE	*incopy	= NULL;
int	didok;
int	sequence	= 1;
int	comfile	= -1;
int	status;
int	wrong;
char	*pwline;
char	*dir;
FILE	*scrin;
int	logging	= 1;	/* set to 0 to turn off logging */
int	ask;
int	again;
int	skip;
int	teed;
int	total;

main(argc,argv)
int argc;
char *argv[];
{
	extern hangup(), intrpt();
	extern char * getlogin(), *malloc();

	speed = 0;
	more = 1;
	pwline = getlogin();
#ifndef BSD4_2
	setbuf(stdout, malloc(BUFSIZ));
	setbuf(stderr, malloc(BUFSIZ));
#endif
	selsub(argc, argv);
	chgenv();
	signal(SIGHUP, hangup);
	signal(SIGINT, intrpt);
	while (more) {
		selunit();
		dounit();
		whatnow();
	}
	wrapup(0);
}

hangup()
{
	wrapup(1);
}

intrpt()
{
	char response[20], *p;

	signal(SIGINT, hangup);
	write(2, "\nInterrupt.\nWant to go on?  ", 28);
	p = response;
	*p = 'n';
	while (read(0, p, 1) == 1 && *p != '\n')
		p++;
	if (response[0] != 'y')
		wrapup(0);
	ungetc('\n', stdin);
	signal(SIGINT, intrpt);
}
