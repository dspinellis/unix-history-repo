# include <stdio.h>
# include <ctype.h>
# include "sendmail.h"
# ifdef LOG
# include <syslog.h>
# endif LOG

static char	SccsId[] = "@(#)err.c	3.7	%G%";

extern bool	HasXscrpt;

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
**		none
**
**	Side Effects:
**		increments Errors.
**		sets ExitStat.
*/

# ifdef lint
int	sys_nerr;
char	*sys_errlist[];
# endif lint

/*VARARGS1*/
syserr(fmt, a, b, c, d, e)
	char *fmt;
{
	static char errbuf[MAXLINE+1];
	extern char *sys_errlist[];
	extern int sys_nerr;
	register char *eb = errbuf;

	/* add arpanet error number if not present */
	if (!isdigit(*fmt))
	{
		(void) strcpy(eb, "455 ");
		eb += 4;
	}

	/* put error message into buffer */
	(void) sprintf(eb, fmt, a, b, c, d, e);
	if (errno != 0)
	{
		eb += strlen(eb);
		if (errno < sys_nerr && errno > 0)
			(void) sprintf(eb, ": %s", sys_errlist[errno]);
		else
			(void) sprintf(eb, ": error %d", errno);
	}

	if (ArpaMode != ARPA_NONE && !HasXscrpt)
		printf("%s\r\n", errbuf);
	else
		printf("sendmail: %s\n", &errbuf[4]);
	(void) fflush(stdout);
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
**		none
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
		return;
	Errors++;

	message("450", fmt, a, b, c, d, e);
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

/*VARARGS2*/
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
	if (ArpaMode != ARPA_NONE && !HasXscrpt)
		printf("%.3s ", num);

	if (To != NULL && To[0] != '\0')
		printf("%s... ", To);
	printf(msg, a, b, c, d, e);
	if (ArpaMode != ARPA_NONE && !HasXscrpt)
		printf("\r");
	printf("\n");
	(void) fflush(stdout);
}
