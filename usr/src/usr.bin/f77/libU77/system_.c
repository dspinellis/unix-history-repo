/*
char id_system[] = "@(#)system_.c	1.1";
 *
 * execute a unix command
 *
 * calling sequence:
 *	iexit = system(command)
 * where:
 *	iexit will return the exit status of the command
 *	command is a character string containing the command to be executed
 */

long system_(s, n)
char *s;
long n;
{
	return((long)system(s));
}
