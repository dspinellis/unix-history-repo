static char *sccsid = "@(#)mesg.c	4.3 (Berkeley) 3/13/86";
/*
 * mesg -- set current tty to accept or
 *	forbid write permission.
 *
 *	mesg [y] [n]
 *		y allow messages
 *		n forbid messages
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

struct stat sbuf;

char *tty;
char *ttyname();

main(argc, argv)
char *argv[];
{
	int r=0;
	tty = ttyname(2);
	if (tty == 0)
		exit(13);
	if(stat(tty, &sbuf) < 0) error("cannot stat");
	if(argc < 2) {
		if(sbuf.st_mode & 020)
			fprintf(stderr,"is y\n");
		else {	r=1;
			fprintf(stderr,"is n\n");
		}
	} else	switch(*argv[1]) {
		case 'y':
			newmode(sbuf.st_mode|020); break;

		case 'n':
			newmode(sbuf.st_mode&~020); r=1; break;

		default:
			error("usage: mesg [y] [n]");
		}
	exit(r);
}

error(s)
char *s;
{
	fprintf(stderr,"mesg: %s\n",s);
	exit(-1);
}

newmode(m)
{
	if(chmod(tty,m)<0)
		error("cannot change mode");
}
