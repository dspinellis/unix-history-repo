/*
char id_perror[] = "@(#)perror_.c	1.1";
 *
 * write a standard error message to the standard error output
 *
 * calling sequence:
 *	call perror(string)
 * where:
 *	string will be written preceeding the standard error message
 */

#include	<stdio.h>
#include	"../libI77/fiodefs.h"
#include	"../libI77/f_errno.h"

extern char *sys_errlist[];
extern int sys_nerr;
extern char *f_errlist[];
extern int f_nerr;
extern unit units[];

perror_(s, len)
char *s; long len;
{
	char buf[40];
	char *mesg = s + len;

	while (len > 0 && *--mesg == ' ')
		len--;
	if (errno >=0 && errno < sys_nerr)
		mesg = sys_errlist[errno];
	else if (errno >= F_ER && errno < (F_ER + f_nerr))
		mesg = f_errlist[errno - F_ER];
	else
	{
		sprintf(buf, "%d: unknown error number", errno);
		mesg = buf;
	}
	while (len-- > 0)
		putc(*s++, units[STDERR].ufd);
	fprintf(units[STDERR].ufd, ": %s\n", mesg);
}
