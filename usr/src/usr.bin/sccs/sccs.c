# include <stdio.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sysexits.h>

static char SccsId[] = "@(#)sccs.c 1.6 delta %G% 19:12:04 get %H% %T%";

# define bitset(bit, word)	((bit) & (word))

typedef char	bool;

struct sccsprog
{
	char	*sccsname;	/* name of SCCS routine */
	short	sccsflags;	/* see below */
	char	*sccspath;	/* pathname of binary implementing */
};

/* bits for sccsflags */
# define F_NOSDOT	0001	/* no s. on front of args */
# define F_PROT		0002	/* protected (e.g., admin) */

struct sccsprog SccsProg[] =
{
	"admin",	F_PROT,			"/usr/sccs/admin",
	"chghist",	0,			"/usr/sccs/rmdel",
	"comb",		0,			"/usr/sccs/comb",
	"delta",	0,			"/usr/sccs/delta",
	"get",		0,			"/usr/sccs/get",
	"help",		F_NOSDOT,		"/usr/sccs/help",
	"prt",		0,			"/usr/sccs/prt",
	"rmdel",	F_PROT,			"/usr/sccs/rmdel",
	"what",		F_NOSDOT,		"/usr/sccs/what",
	NULL,		0,			NULL
};

char	*SccsPath = "SCCS";	/* pathname of SCCS files */
bool	RealUser;		/* if set, running as real user */

main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;
	register char **av;
	char *newargv[1000];
	extern char *makefile();
	register struct sccsprog *cmd;
	char buf[200];
	register FILE *fp;

	/*
	**  Detect and decode flags intended for this program.
	*/

	while (--argc > 0)
	{
		p = *++argv;
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

	/*
	**  See if this user is an administrator.
	*/

	/*
	**  Look up command.
	**	At this point, p and argv point to the command name.
	*/

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
	**  Set protection as appropriate.
	*/

	if (bitset(F_PROT, cmd->sccsflags))
		setuid(getuid());

	/*
	**  Build new argument vector.
	*/

	av = newargv;
	*av++ = p;

	/* copy program filename arguments and flags */
	while (--argc > 0)
	{
		p = *++argv;
		if (!bitset(F_NOSDOT, cmd->sccsflags) && *p != '-')
			*av++ = makefile(p);
		else
			*av++ = p;
	}
	
	/* terminate argument vector */
	*av = NULL;

	/*
	**  Call real SCCS program.
	*/

	execv(cmd->sccspath, newargv);
	fprintf(stderr, "Sccs: cannot execute ");
	perror(cmd->sccspath);
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

