/*
char id_system[] = "@(#)system_.c	1.4";
 *
 * execute a unix command
 *
 * calling sequence:
 *	iexit = system(command)
 * where:
 *	iexit will return the exit status of the command
 *	command is a character string containing the command to be executed
 */

#include	"../libI77/fiodefs.h"
#include	"../libI77/f_errno.h"

 
long system_(s, n)
char *s;
long n;
{
	char buf[256];
	long i;

	if (n >= sizeof buf)
		return(-(long)(errno=F_ERARG));
	for (i = 0; i < MXUNIT; i++)
		flush_(&i);
	g_char(s, n, buf);
	return((long)system(buf));
}
