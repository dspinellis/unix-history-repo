/*
char id_system[] = "@(#)system_.c	1.2";
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

 
long system_(s, n)
char *s;
long n;
{
	int i;

	for (i = 0; i < MXUNIT; flush(i++)) ;
	return((long)system(s));
}
