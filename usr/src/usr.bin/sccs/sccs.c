# include <stdio.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sysexits.h>
# include <whoami.h>

static char SccsId[] = "@(#)sccs.c 1.10 delta %G% 16:34:19 get %H% %T%";

# define bitset(bit, word)	((bit) & (word))

typedef char	bool;
# define TRUE	1
# define FALSE	0

struct sccsprog
{
	char	*sccsname;	/* name of SCCS routine */
	short	sccsoper;	/* opcode, see below */
	short	sccsflags;	/* flags, see below */
	char	*sccspath;	/* pathname of binary implementing */
};

/* values for sccsoper */
# define PROG		0	/* call a program */
# define CMACRO		1	/* command substitution macro */
# define FIX		2	/* fix a delta */

/* bits for sccsflags */
# define NO_SDOT	0001	/* no s. on front of args */
# define REALUSER	0002	/* protected (e.g., admin) */

# ifdef CSVAX
# define PROGPATH(name)	"/usr/local/name"
# endif CSVAX

# ifndef PROGPATH
# define PROGPATH(name)	"/usr/sccs/name"
# endif PROGPATH

struct sccsprog SccsProg[] =
{
	"admin",	PROG,	REALUSER,		PROGPATH(admin),
	"chghist",	PROG,	0,			PROGPATH(rmdel),
	"comb",		PROG,	0,			PROGPATH(comb),
	"delta",	PROG,	0,			PROGPATH(delta),
	"get",		PROG,	0,			PROGPATH(get),
	"help",		PROG,	NO_SDOT,		PROGPATH(help),
	"prt",		PROG,	0,			PROGPATH(prt),
	"rmdel",	PROG,	REALUSER,		PROGPATH(rmdel),
	"what",		PROG,	NO_SDOT,		PROGPATH(what),
	"del",		CMACRO,	0,			"delta/get",
	"delt",		CMACRO,	0,			"delta/get",
	"fix",		FIX,	0,			NULL,
	NULL,		-1,	0,			NULL
};

char	*SccsPath = "SCCS";	/* pathname of SCCS files */
bool	RealUser;		/* if set, running as real user */

main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;

	/*
	**  Detect and decode flags intended for this program.
	*/

	if (argc < 2)
	{
		fprintf(stderr, "Usage: sccs [flags] command [flags]\n");
		exit(EX_USAGE);
	}
	argv[argc] = NULL;

	while ((p = *++argv) != NULL)
	{
		if (*p != '-')
			break;
		switch (*++p)
		{
		  case 'r':		/* run as real user */
			setuid(getuid());
			RealUser++;
			break;

		  case 'p':		/* path of sccs files */
			SccsPath = ++p;
			break;

		  default:
			fprintf(stderr, "Sccs: unknown option -%s\n", p);
			break;
		}
	}
	if (SccsPath[0] == '\0')
		SccsPath = ".";

	command(argv, FALSE);
	exit(EX_OK);
}

command(argv, forkflag)
	char **argv;
	bool forkflag;
{
	register struct sccsprog *cmd;
	register char *p;
	register char *q;
	char buf[40];

	/*
	**  Look up command.
	**	At this point, argv points to the command name.
	*/

	p = *argv;
	for (cmd = SccsProg; cmd->sccsname != NULL; cmd++)
	{
		if (strcmp(cmd->sccsname, p) == 0)
			break;
	}
	if (cmd->sccsname == NULL)
	{
		fprintf(stderr, "Sccs: Unknown command \"%s\"\n", p);
		exit(EX_USAGE);
	}

	/*
	**  Interpret operation associated with this command.
	*/

	switch (cmd->sccsoper)
	{
	  case PROG:		/* call an sccs prog */
		callprog(cmd->sccspath, cmd->sccsflags, argv, forkflag);
		break;

	  case CMACRO:		/* command macro */
		for (p = cmd->sccspath; *p != '\0'; p++)
		{
			for (q = buf; *p != '/' && *p != '\0'; p++, q++)
				*q = *p;
			*q = '\0';
			argv[0] = buf;
			command(argv, *p != '\0');
		}
		fprintf(stderr, "Sccs internal error: CMACRO\n");
		exit(EX_SOFTWARE);

	  case FIX:		/* fix a delta */
		if (strncmp(argv[1], "-r", 2) != 0)
		{
			fprintf(stderr, "Sccs: -r flag needed for fix command\n");
			break;
		}
		xcommand(&argv[1], TRUE, "get", "-k", NULL);
		xcommand(&argv[1], TRUE, "rmdel", NULL);
		xcommand(&argv[2], FALSE, "get", "-e", "-g", NULL);
		fprintf(stderr, "Sccs internal error: FIX\n");
		exit(EX_SOFTWARE);

	  default:
		fprintf(stderr, "Sccs internal error: oper %d\n", cmd->sccsoper);
		exit(EX_SOFTWARE);
	}
}


xcommand(argv, forkflag, arg0)
	char **argv;
	bool forkflag;
	char *arg0;
{
	register char **av;
	char *newargv[1000];
	register char **np;

	np = newargv;
	for (av = &arg0; *av != NULL; av++)
		*np++ = *av;
	for (av = argv; *av != NULL; av++)
		*np++ = *av;
	*np = NULL;
	command(newargv, forkflag);
}

callprog(progpath, flags, argv, forkflag)
	char *progpath;
	short flags;
	char **argv;
	bool forkflag;
{
	register char *p;
	register char **av;
	extern char *makefile();
	register int i;
	auto int st;

	if (*argv == NULL)
		return (-1);

	/*
	**  Fork if appropriate.
	*/

	if (forkflag)
	{
		i = fork();
		if (i < 0)
		{
			fprintf(stderr, "Sccs: cannot fork");
			exit(EX_OSERR);
		}
		else if (i > 0)
		{
			wait(&st);
			return (st);
		}
	}

	/*
	**  Build new argument vector.
	*/

	/* copy program filename arguments and flags */
	av = argv;
	while ((p = *++av) != NULL)
	{
		if (!bitset(NO_SDOT, flags) && *p != '-')
			*av = makefile(p);
	}

	/*
	**  Set protection as appropriate.
	*/

	if (bitset(REALUSER, flags))
		setuid(getuid());
	
	/*
	**  Call real SCCS program.
	*/

	execv(progpath, argv);
	fprintf(stderr, "Sccs: cannot execute ");
	perror(progpath);
	exit(EX_UNAVAILABLE);
}


char *
makefile(name)
	char *name;
{
	register char *p;
	register char c;
	char buf[512];
	struct stat stbuf;
	extern char *malloc();

	/*
	**  See if this filename should be used as-is.
	**	There are three conditions where this can occur.
	**	1. The name already begins with "s.".
	**	2. The name has a "/" in it somewhere.
	**	3. The name references a directory.
	*/

	if (strncmp(name, "s.", 2) == 0)
		return (name);
	for (p = name; (c = *p) != '\0'; p++)
	{
		if (c == '/')
			return (name);
	}
	if (stat(name, &stbuf) >= 0 && (stbuf.st_mode & S_IFMT) == S_IFDIR)
		return (name);

	/*
	**  Prepend the path of the sccs file.
	*/

	strcpy(buf, SccsPath);
	strcat(buf, "/s.");
	strcat(buf, name);
	p = malloc(strlen(buf) + 1);
	if (p == NULL)
	{
		perror("Sccs: no mem");
		exit(EX_OSERR);
	}
	strcpy(p, buf);
	return (p);
}
