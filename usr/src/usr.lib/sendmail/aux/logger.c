# include <stdio.h>
# include <syslog.h>

# ifndef lint
static char	SccsId[] =	"@(#)logger.c	1.2		7/22/82";
# endif lint

/*
**  LOGGER -- read and log utility
**
**	This routine reads from an input and arranges to write the
**	result on the system log, along with a useful tag.
*/

main(argc, argv)
	int argc;
	char **argv;
{
	char buf[200];
	char *tag;
	register char *p;
	int pri = LOG_NOTICE;
	int logflags = 0;
	extern char *getlogin();

	/* initialize */
	tag = getlogin();

	/* crack arguments */
	while (--argc > 0)
	{
		p = *++argv;
		if (*p != '-')
			break;

		switch (*++p)
		{
		  case '\0':		/* dummy */
			/* this can be used to give null parameters */
			break;

		  case 't':		/* tag */
			if (argc > 1 && argv[1][0] != '-')
			{
				argc--;
				tag = *++argv;
			}
			else
				tag = NULL;
			break;

		  case 'p':		/* priority */
			if (argc > 1 && argv[1][0] != '-')
			{
				argc--;
				pri = atoi(*++argv);
			}
			break;

		  case 'i':		/* log process id also */
			logflags |= LOG_PID;
			break;

		  case 'f':		/* file to log */
			if (argc > 1 && argv[1][0] != '-')
			{
				argc--;
				if (freopen(*++argv, "r", stdin) == NULL)
				{
					fprintf("logger: ");
					perror(*argv);
					exit(1);
				}
			}
			break;

		  default:
			fprintf(stderr, "logger: unknown flag -%s\n", p);
			break;
		}
	}

	/* setup for logging */
	openlog(tag, logflags);
	(void) fclose(stdout);

	/* log input line if appropriate */
	if (argc > 0)
	{
		char buf[120];

		buf[0] = '\0';
		while (argc-- > 0)
		{
			strcat(buf, " ");
			strcat(buf, *argv++);
		}
		syslog(pri, buf + 1);
		exit(0);
	}

	/* main loop */
	while (fgets(buf, sizeof buf, stdin) != NULL)
		syslog(pri, buf);

	exit(0);
}
