# include <stdio.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sysexits.h>

struct sccsprog
{
	char	*sccsname;	/* name of SCCS routine */
	short	sccsflags;	/* see below */
	char	*sccspath;	/* pathname of binary implementing */
};

/* bits for sccspath */
# define F_NOSDOT	0001	/* no s. on front of args */

struct sccsprog SccsProg[] =
{
	"admin",	0,			"/usr/sccs/admin",
	"comb",		0,			"/usr/sccs/comb",
	"delta",	0,			"/usr/sccs/delta",
	"get",		0,			"/usr/sccs/get",
	"prt",		0,			"/usr/sccs/prt",
	"rmdel",	0,			"/usr/sccs/rmdel",
	"what",		F_NOSDOT,		"/usr/sccs/what",
	NULL,		0,			NULL
};

char	*SccsPath = "sccs/s.";

main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;
	register char **av;
	char *newargv[1000];
	extern char *makefile();
	register struct sccsprog *cmd;

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
			break;

		  case 'p':		/* path of sccs files */
			SccsPath = ++p;
			break;

		  default:
			fprintf(stderr, "Sccs: unknown option -%s\n", p);
			break;
		}
	}

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
	**  Build new argument vector.
	**	This is in three phases.
	**	1.  Copy program name.
	**	2.  Copy program flags with no translation.
	**	3.  Copy program files, with possible translation.
	*/

	/*  1: copy program name  */
	av = newargv;
	*av++ = p;

	/*  2: copy program flags, no translation  */
	while (--argc > 0 && *(p = *++argv) == '-')
		*av++ = p;

	/*  3: copy program filename arguments  */
	while (argc-- > 0)
	{
		if ((cmd->sccsflags & F_NOSDOT) == 0)
			*av++ = makefile(*argv++);
		else
			*av++ = *argv++;
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

