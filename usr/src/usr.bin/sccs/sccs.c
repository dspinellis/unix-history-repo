# include <stdio.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sysexits.h>

static char SccsId[] = "@(#)sccs.c 1.7 delta %G% 20:24:38 get %H% %T%";

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

/* bits for sccsflags */
# define NO_SDOT	0001	/* no s. on front of args */
# define REALUSER	0002	/* protected (e.g., admin) */

struct sccsprog SccsProg[] =
{
	"admin",	PROG,	REALUSER,		"/usr/sccs/admin",
	"chghist",	PROG,	0,			"/usr/sccs/rmdel",
	"comb",		PROG,	0,			"/usr/sccs/comb",
	"delta",	PROG,	0,			"/usr/sccs/delta",
	"get",		PROG,	0,			"/usr/sccs/get",
	"help",		PROG,	NO_SDOT,		"/usr/sccs/help",
	"prt",		PROG,	0,			"/usr/sccs/prt",
	"rmdel",	PROG,	REALUSER,		"/usr/sccs/rmdel",
	"what",		PROG,	NO_SDOT,		"/usr/sccs/what",
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

	command(argv);
	exit(EX_OK);
}

command(argv)
	char **argv;
{
	register struct sccsprog *cmd;
	register char *p;

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
		callprog(cmd->sccspath, cmd->sccsflags, argv, FALSE);
		fprintf(stderr, "Sccs internal error: callprog\n");
		exit(EX_SOFTWARE);

	  default:
		fprintf(stderr, "Sccs internal error: oper %d\n", cmd->sccsoper);
		exit(EX_SOFTWARE);
	}
}

callprog(progpath, flags, argv, forkflag)
	char *progpath;
	short flags;
	char **argv;
	bool forkflag;
{
	register char *p;
	register char **av;
	char *newargv[1000];
	extern char *makefile();
	register int i;

	if (*argv == NULL)
		return (-1);

	/*
	**  Build new argument vector.
	*/

	av = newargv;
	*av++ = *argv;

	/* copy program filename arguments and flags */
	while ((p = *++argv) != NULL)
	{
		if (!bitset(NO_SDOT, flags) && *p != '-')
			*av++ = makefile(p);
		else
			*av++ = p;
	}
	
	/* terminate argument vector */
	*av = NULL;

	/*
	**  Call real SCCS program.
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
			return (i);
	}

	/*
	**  Set protection as appropriate.
	*/

	if (bitset(REALUSER, flags))
		setuid(getuid());

	/*
	**  Call the program.
	*/

	execv(progpath, newargv);
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
