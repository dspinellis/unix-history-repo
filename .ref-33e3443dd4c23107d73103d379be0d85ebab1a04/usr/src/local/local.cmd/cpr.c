# include <stdio.h>
# include <sgtty.h>
# include <signal.h>

static char	SccsId[] =	"@(#)cpr.c	1.8		%G%";

/*
**  CPR -- print on concept 108
**
**	This filter arranges to output onto a printer connected
**	to a Concept 108 terminal.  It probably works on other
**	models in the Concept 100 series also.
**
**	Usage:
**		cpr [-f] [file ...]
**
**	Flags:
**		-f	form feed following to print.
*/

#define LINELEN	132			/* carriage width */

typedef char	bool;
#define TRUE	1
#define FALSE	0

struct sgttyb	tbuf;
int		SysLinePid;		/* pid of sysline process */
bool		FormFeedFollowing;	/* print form feed following print */
bool		EchoDuringPrint;	/* echo on terminal while printing */

main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;
	extern cleanterm();
	extern char *getenv();

	/* arrange to stop the sysline program during printing */
	p = getenv("SYSLINE");
	if (p != NULL)
		SysLinePid = atoi(p);

	/* process arguments */
	while (--argc > 0)
	{
		p = *++argv;
		if (*p != '-')
			break;
		switch (*++p)
		{
		  case 'f':
			FormFeedFollowing = TRUE;
			break;

		  case 'e':
			EchoDuringPrint = TRUE;
			break;
		}
	}

	/* be nice on interrupts, etc. */
	signal(SIGINT, cleanterm);

	/* set the terminal to output to printer */
	setupterm();

	/* print the appropriate files */
	if (argc < 1)
		copyfile();
	else
	{
		while (argc-- > 0)
		{
			p = *argv++;
			if (freopen(p, "r", stdin) == NULL)
				perror(p);
			else
				copyfile();
		}
	}

	/* reset terminal to a nice state */
	cleanterm();
}

copyfile()
{
	int c;
	int col;
	register char *p;
	char bufa[LINELEN + 1];
	char bufb[LINELEN + 2];
	char bufc[LINELEN + 1];
	char bufd[LINELEN + 2];

  clearbuf:
	for (col = 0; col <= LINELEN; col++)
		bufa[col] = bufb[col] = bufc[col] = bufd[col] = ' ';
	col = 0;
	while ((c = getchar()) != EOF)
	{
		switch (c)
		{
		  case '\b':
			if (col > 0)
				col--;
			break;

		  case '\r':
			col = 0;
			break;

		  case ' ':
			col++;
			break;

		  case '\t':
			col = (col + 8) & ~07;
			break;

		  case '\n':
			putout(bufa, FALSE);
			putout(bufb, TRUE);
			putout(bufc, TRUE);
			putout(bufd, TRUE);
			col = 0;
			putchar('\n');
			goto clearbuf;

		  default:
			if (col >= LINELEN)
				col++;
			else if (bufa[col] == ' ')
				bufa[col++] = c;
			else if (bufb[col] == ' ')
				bufb[col++] = c;
			else if (bufc[col] == ' ')
				bufc[col++] = c;
			else if (bufd[col] == ' ')
				bufd[col++] = c;
			else
			{
				int i;

				putout(bufa, FALSE);
				putchar('\r');
				for (i = 0; i < LINELEN; i++)
					bufa[i] = ' ';
				bufa[col++] = c;
			}
		}
	}
}

putout(buf, cr)
	char buf[];
	bool cr;
{
	register char *p;

	/* find the end of the line */
	for (p = &buf[LINELEN-1]; p >= buf && *p == ' '; p--)
		continue;
	*++p = '\0';
	if (buf[0] == '\0')
		return;
	if (cr)
		putchar('\r');
	for (p = buf; *p != '\0'; p++)
		putchar(*p);
}

setupterm()
{
	int oldflags;

	if (gtty(1, &tbuf) < 0)
	{
		perror("cpr: stdout");
		exit(1);
	}
	oldflags = tbuf.sg_flags;
	tbuf.sg_flags &= ~ECHO;
	tbuf.sg_flags |= CBREAK | XTABS;
	stty(1, &tbuf);
	tbuf.sg_flags = oldflags;
	if (SysLinePid > 0)
		kill(SysLinePid, SIGSTOP);
	fputs("\033Y", stdout);
	if (EchoDuringPrint)
		fputs("4", stdout);
	else
		fputs("$", stdout);
	if (getack() >= 0)
		return;
	fprintf(stderr, "Cannot attach printer\n");
	resetmodes();
	exit(1);
}

cleanterm()
{
	/* output trailing formfeed */
	if (FormFeedFollowing)
		fputs("\r\f", stdout);

	/* disconnect printer */
	printf("\033Y0");
	getack();
	fflush(stdout);

	/* back to the real world.... */
	resetmodes();
	exit(0);
}

getack()
{
	char buf[1];

	fflush(stdout);
	if (read(2, buf, 1) <= 0 || buf[0] != '\006')
		return (-1);
	return (0);
}

resetmodes()
{
	stty(1, &tbuf);
	if (SysLinePid > 0)
		kill(SysLinePid, SIGCONT);
}
