# include <stdio.h>
# include <sgtty.h>
# include <signal.h>

static char	SccsId[] =	"@(#)cpr.c	1.2		%G%";

/*
**  CPR -- print on concept 108
**
**	This filter arranges to output onto a printer connected
**	to a Concept 108 terminal.  It probably works on other
**	models in the Concept 100 series also.
**
**	Usage:
**		cpr [file ...]
*/

struct sgttyb	tbuf;

main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;
	extern cleanterm();

	/* be nice on interrupts, etc. */
	signal(SIGINT, cleanterm);

	/* set the terminal to output to printer */
	setupterm();

	/* print the appropriate files */
	if (argc < 2)
		copyfile();
	else
	{
		while (--argc > 0)
		{
			if (freopen(*++argv, "r", stdin) == NULL)
				perror(*argv);
			else
				copyfile();
		}
	}

	/* reset terminal to a nice state */
	cleanterm();
}

copyfile()
{
	char buf[200];
	register char *p;
	extern char *index();

	while (fgets(buf, sizeof buf, stdin) != NULL)
	{
		p = index(buf, '\n');
		if (p == NULL)
			continue;
		*p = '\r';
		printf("\033 5%s\033|", buf);
		if (getack() < 0)
		{
			fprintf(stderr, "Lost printer\n");
			cleanterm();
		}
		fputs("\n", stdout);
	}
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
	tbuf.sg_flags |= CBREAK;
	stty(1, &tbuf);
	tbuf.sg_flags = oldflags;
}

cleanterm()
{
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
}
