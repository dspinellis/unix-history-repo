/*
 * rmail: front end for mail to stack up those stupid >From ... remote from ...
 * lines and make a correct return address.  This works with the -f option
 * to /usr/lib/sendmail so it won't work on systems without sendmail.
 * However, it ought to be easy to modify a standard /bin/mail to do the
 * same thing.
 *
 * NOTE: Rmail is SPECIFICALLY INTENDED for ERNIE COVAX because of its
 * physical position as a gateway between the uucp net and the arpanet.
 * By default, other sites will probably want /bin/rmail to be a link
 * to /bin/mail, as it was intended by BTL.  However, other than the
 * (somewhat annoying) loss of information about when the mail was
 * originally sent, rmail should work OK on other systems running uucp.
 * If you don't run uucp you don't even need any rmail.
 */

static char	SccsId[] =	"@(#)rmail.c	3.4	%G%";

# include <stdio.h>
# include <sysexits.h>
# include "useful.h"
# include "conf.h"

extern FILE *popen();
extern char *index();

bool	Debug;

# define MAILER	"/usr/lib/sendmail"

main(argc, argv)
	char **argv;
{
	FILE *out;	/* output to sendmail */
	char lbuf[512];	/* one line of the message */
	char from[512];	/* accumulated path of sender */
	char ufrom[64];	/* user on remote system */
	char sys[64];	/* a system in path */
	char junk[512];	/* scratchpad */
	char cmd[2000];
	register char *cp;

# ifdef DEBUG
	if (argc > 1 && strcmp(argv[1], "-T") == 0)
	{
		Debug = TRUE;
		argc--;
		argv++;
	}
# endif DEBUG

	if (argc < 2)
	{
		fprintf(stderr, "Usage: rmail user ...\n");
		exit(EX_USAGE);
	}

	for (;;)
	{
		(void) fgets(lbuf, sizeof lbuf, stdin);
		if (strncmp(lbuf, "From ", 5) != 0 && strncmp(lbuf, ">From ", 6) != 0)
			break;
		(void) sscanf(lbuf, "%s %s", junk, ufrom);
		cp = lbuf;
		for (;;)
		{
			cp = index(cp+1, 'r');
			if (cp == NULL)
				cp = "remote from somewhere";
#ifdef DEBUG
			if (Debug)
				printf("cp='%s'\n", cp);
#endif
			if (strncmp(cp, "remote from ", 12)==0)
				break;
		}
		(void) sscanf(cp, "remote from %s", sys);
		strcat(from, sys);
		strcat(from, "!");
#ifdef DEBUG
		if (Debug)
			printf("ufrom='%s', sys='%s', from now '%s'\n", ufrom, sys, from);
#endif
	}
	strcat(from, ufrom);

	sprintf(cmd, "%s -em -f%s", MAILER, from);
	while (*++argv != NULL)
	{
		strcat(cmd, " '");
		strcat(cmd, *argv);
		strcat(cmd, "'");
	}
#ifdef DEBUG
	if (Debug)
		printf("cmd='%s'\n", cmd);
#endif
	out = popen(cmd, "w");
	fputs(lbuf, out);
	while (fgets(lbuf, sizeof lbuf, stdin))
		fputs(lbuf, out);
	pclose(out);

	exit(EX_OK);
}
