#ifndef lint
static char sccsid[] = "@(#)learn.c	4.1	(Berkeley)	%G%";
#endif not lint

#include "stdio.h"
#include "lrnref.h"
#include "signal.h"

char	*direct	= "/usr/lib/learn";	/* CHANGE THIS ON YOUR SYSTEM */
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

main(argc,argv)
char *argv[];
{
	extern hangup(), intrpt();
	extern char * getlogin();
	char *malloc();

	speed = 0;
	more = 1;
	pwline = getlogin();
	setbuf(stdout, malloc(BUFSIZ));
	selsub(argc, argv);
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
		wrapup(1);
	ungetc('\n', stdin);
	signal(SIGINT, intrpt);
}
