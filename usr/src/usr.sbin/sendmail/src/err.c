# include <stdio.h>
# include <ctype.h>
# include "sendmail.h"
# ifdef LOG
# include <syslog.h>
# endif LOG

static char	SccsId[] = "@(#)err.c	3.4	%G%";

/*
**  SYSERR -- Print error message.
**
**	Prints an error message via printf to the diagnostic
**	output.  If LOG is defined, it logs it also.
**
**	Parameters:
**		f -- the format string
**		a, b, c, d, e -- parameters
**
**	Returns:
**		-1 always
**
**	Side Effects:
**		increments Errors.
**		sets ExitStat.
*/

/*VARARGS1*/
syserr(fmt, a, b, c, d, e)
	char *fmt;
{
	extern int errno;
	static char errbuf[MAXLINE+1];
	register char *p;
	extern char *sys_errlist[];
	extern int sys_nerr;
	extern char *sprintf();
	register char *eb = errbuf;

	/* add arpanet error number if not present */
	if (!isdigit(*fmt))
	{
		strcpy(eb, "455 ");
		eb += 4;
	}

	/* put error message into buffer */
	sprintf(eb, fmt, a, b, c, d, e);
	if (errno != 0)
	{
		eb += strlen(eb);
		if (errno < sys_nerr && errno > 0)
			sprintf(eb, ": %s", sys_errlist[errno]);
		else
			sprintf(eb, ": error %d", errno);
	}

	if (ArpaFmt)
		printf("%s\n", errbuf);
	else
		printf("sendmail: %s\n", &errbuf[4]);
	fflush(stdout);
	Errors++;

	/* determine exit status if not already set */
	if (ExitStat == EX_OK)
	{
		if (errno == 0)
			ExitStat = EX_SOFTWARE;
		else
			ExitStat = EX_OSERR;
	}

# ifdef LOG
	syslog(LOG_ERR, "%s->%s: %s", From.q_paddr, To, errbuf);
# endif LOG
	errno = 0;
	return (-1);
}
/*
**  USRERR -- Signal user error.
**
**	This is much like syserr except it is for user errors.
**
**	Parameters:
**		fmt, a, b, c, d -- printf strings
**
**	Returns:
**		-1
**
**	Side Effects:
**		increments Errors.
*/

/*VARARGS1*/
usrerr(fmt, a, b, c, d, e)
	char *fmt;
{
	extern char SuprErrs;

	if (SuprErrs)
		return (0);
	Errors++;

	message("450", fmt, a, b, c, d, e);
	return (-1);
}
/*
**  MESSAGE -- print message (not necessarily an error)
**
**	Parameters:
**		num -- the default ARPANET error number (in ascii)
**		msg -- the message (printf fmt) -- if it begins
**			with a digit, this number overrides num.
**		a, b, c, d, e -- printf arguments
**
**	Returns:
**		none
**
**	Side Effects:
**		none.
*/

message(num, msg, a, b, c, d, e)
	register char *num;
	register char *msg;
{
	/* compute error number */
	if (isdigit(*msg))
	{
		num = msg;
		msg += 4;
	}

	/* print arpa format header if needed */
	if (ArpaFmt)
		printf("%.3s ", num);

	if (To != NULL && To[0] != '\0')
		printf("%s... ", To);
	printf(msg, a, b, c, d, e);
	printf("\n");
	fflush(stdout);
}
