# include <stdio.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sys/dir.h>
# include <sysexits.h>
# include <whoami.h>

static char SccsId[] = "@(#)sccs.c	1.22 %G%";

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
# define CLEAN		3	/* clean out recreatable files */
# define UNEDIT		4	/* unedit a file */

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
	"edit",		CMACRO,	0,			"get -e",
	"delget",	CMACRO,	0,			"delta/get",
	"deledit",	CMACRO,	0,			"delta/get -e",
	"del",		CMACRO,	0,			"delta/get",
	"delt",		CMACRO,	0,			"delta/get",
	"fix",		FIX,	0,			NULL,
	"clean",	CLEAN,	REALUSER,		(char *) TRUE,
	"info",		CLEAN,	REALUSER,		(char *) FALSE,
	"unedit",	UNEDIT,	0,			NULL,
	NULL,		-1,	0,			NULL
};

struct pfile
{
	char	*p_osid;	/* old SID */
	char	*p_nsid;	/* new SID */
	char	*p_user;	/* user who did edit */
	char	*p_date;	/* date of get */
	char	*p_time;	/* time of get */
};

char	*SccsPath = "SCCS";	/* pathname of SCCS files */
bool	RealUser;		/* if set, running as real user */
# ifdef DEBUG
bool	Debug;			/* turn on tracing */
# endif

main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;
	extern struct sccsprog *lookup();

	/*
	**  Detect and decode flags intended for this program.
	*/

	if (argc < 2)
	{
		fprintf(stderr, "Usage: sccs [flags] command [flags]\n");
		exit(EX_USAGE);
	}
	argv[argc] = NULL;

	if (lookup(argv[0]) == NULL)
	{
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

# ifdef DEBUG
			  case 'T':		/* trace */
				Debug++;
				break;
# endif

			  default:
				fprintf(stderr, "Sccs: unknown option -%s\n", p);
				break;
			}
		}
		if (SccsPath[0] == '\0')
			SccsPath = ".";
	}

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
	extern struct sccsprog *lookup();
	char *nav[200];
	char **avp;
	register int i;
	extern bool unedit();

# ifdef DEBUG
	if (Debug)
	{
		printf("command:\n");
		for (avp = argv; *avp != NULL; avp++)
			printf("    \"%s\"\n", *avp);
	}
# endif

	/*
	**  Look up command.
	**	At this point, argv points to the command name.
	*/

	cmd = lookup(argv[0]);
	if (cmd == NULL)
	{
		fprintf(stderr, "Sccs: Unknown command \"%s\"\n", argv[0]);
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
			avp = nav;
			*avp++ = buf;
			for (q = buf; *p != '/' && *p != '\0'; p++, q++)
			{
				if (*p == ' ')
				{
					*q = '\0';
					*avp++ = &q[1];
				}
				else
					*q = *p;
			}
			*q = '\0';
			*avp = NULL;
			xcommand(&argv[1], *p != '\0', nav[0], nav[1], nav[2],
				 nav[3], nav[4], nav[5], nav[6]);
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

	  case CLEAN:
		clean((bool) cmd->sccspath);
		break;

	  case UNEDIT:
		i = 0;
		for (avp = &argv[1]; *avp != NULL; avp++)
		{
			if (unedit(*avp))
				nav[i++] = *avp;
		}
		nav[i] = NULL;
		if (i > 0)
			xcommand(nav, FALSE, "get", NULL);
		break;

	  default:
		fprintf(stderr, "Sccs internal error: oper %d\n", cmd->sccsoper);
		exit(EX_SOFTWARE);
	}
}
/*
**  LOOKUP -- look up an SCCS command name.
**
**	Parameters:
**		name -- the name of the command to look up.
**
**	Returns:
**		ptr to command descriptor for this command.
**		NULL if no such entry.
**
**	Side Effects:
**		none.
*/

struct sccsprog *
lookup(name)
	char *name;
{
	register struct sccsprog *cmd;

	for (cmd = SccsProg; cmd->sccsname != NULL; cmd++)
	{
		if (strcmp(cmd->sccsname, name) == 0)
			return (cmd);
	}
	return (NULL);
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
# ifdef DEBUG
		if (Debug)
			printf("Forking\n");
# endif
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
/*
**  CLEAN -- clean out recreatable files
**
**	Any file for which an "s." file exists but no "p." file
**	exists in the current directory is purged.
**
**	Parameters:
**		really -- if TRUE, remove everything.
**			else, just report status.
**
**	Returns:
**		none.
**
**	Side Effects:
**		removes files in the current directory.
*/

clean(really)
	bool really;
{
	struct direct dir;
	struct stat stbuf;
	char buf[100];
	char pline[120];
	register FILE *dirfd;
	register char *basefile;
	bool gotedit;
	FILE *pfp;

	dirfd = fopen(SccsPath, "r");
	if (dirfd == NULL)
	{
		fprintf(stderr, "Sccs: cannot open %s\n", SccsPath);
		return;
	}

	/*
	**  Scan the SCCS directory looking for s. files.
	*/

	gotedit = FALSE;
	while (fread(&dir, sizeof dir, 1, dirfd) != NULL)
	{
		if (dir.d_ino == 0 || strncmp(dir.d_name, "s.", 2) != 0)
			continue;
		
		/* got an s. file -- see if the p. file exists */
		strcpy(buf, SccsPath);
		strcat(buf, "/p.");
		basefile = &buf[strlen(buf)];
		strncpy(basefile, &dir.d_name[2], sizeof dir.d_name - 2);
		basefile[sizeof dir.d_name - 2] = '\0';
		pfp = fopen(buf, "r");
		if (pfp != NULL)
		{
			while (fgets(pline, sizeof pline, pfp) != NULL)
				printf("%12s: being edited: %s", basefile, pline);
			fclose(pfp);
			gotedit = TRUE;
			continue;
		}
		
		/* the s. file exists and no p. file exists -- unlink the g-file */
		if (really)
		{
			strncpy(buf, &dir.d_name[2], sizeof dir.d_name - 2);
			buf[sizeof dir.d_name - 2] = '\0';
			unlink(buf);
		}
	}

	fclose(dirfd);
	if (!gotedit && !really)
		printf("Nothing being edited\n");
}
/*
**  UNEDIT -- unedit a file
**
**	Checks to see that the current user is actually editting
**	the file and arranges that s/he is not editting it.
**
**	Parameters:
**		fn -- the name of the file to be unedited.
**
**	Returns:
**		TRUE -- if the file was successfully unedited.
**		FALSE -- if the file was not unedited for some
**			reason.
**
**	Side Effects:
**		fn is removed
**		entries are removed from pfile.
*/

bool
unedit(fn)
	char *fn;
{
	register FILE *pfp;
	char *pfn;
	static char tfn[] = "/tmp/sccsXXXXX";
	FILE *tfp;
	register char *p;
	register char *q;
	bool delete = FALSE;
	bool others = FALSE;
	char *myname;
	extern char *getlogin();
	struct pfile *pent;
	extern struct pfile *getpfile();
	char buf[120];

	/* make "s." filename & find the trailing component */
	pfn = makefile(fn);
	q = &pfn[strlen(pfn) - 1];
	while (q > pfn && *q != '/')
		q--;
	if (q <= pfn && (q[0] != 's' || q[1] != '.'))
	{
		fprintf(stderr, "Sccs: bad file name \"%s\"\n", fn);
		return (FALSE);
	}

	/* turn "s." into "p." */
	*++q = 'p';

	pfp = fopen(pfn, "r");
	if (pfp == NULL)
	{
		printf("%12s: not being edited\n", fn);
		return (FALSE);
	}

	/*
	**  Copy p-file to temp file, doing deletions as needed.
	*/

	mktemp(tfn);
	tfp = fopen(tfn, "w");
	if (tfp == NULL)
	{
		fprintf(stderr, "Sccs: cannot create \"%s\"\n", tfn);
		exit(EX_OSERR);
	}

	myname = getlogin();
	while ((pent = getpfile(pfp)) != NULL)
	{
		if (strcmp(pent->p_user, myname) == 0)
		{
			/* a match */
			delete++;
		}
		else
		{
			fprintf(tfp, "%s %s %s %s %s\n", pent->p_osid,
			    pent->p_nsid, pent->p_user, pent->p_date,
			    pent->p_time);
			others++;
		}
	}

	/* do final cleanup */
	if (others)
	{
		if (freopen(tfn, "r", tfp) == NULL)
		{
			fprintf(stderr, "Sccs: cannot reopen \"%s\"\n", tfn);
			exit(EX_OSERR);
		}
		if (freopen(pfn, "w", pfp) == NULL)
		{
			fprintf(stderr, "Sccs: cannot create \"%s\"\n", pfn);
			return (FALSE);
		}
		while (fgets(buf, sizeof buf, tfp) != NULL)
			fputs(buf, pfp);
	}
	else
	{
		unlink(pfn);
	}
	fclose(tfp);
	fclose(pfp);
	unlink(tfn);

	if (delete)
	{
		unlink(fn);
		printf("%12s: removed\n", fn);
		return (TRUE);
	}
	else
	{
		printf("%12s: not being edited by you\n", fn);
		return (FALSE);
	}
}
/*
**  GETPFILE -- get an entry from the p-file
**
**	Parameters:
**		pfp -- p-file file pointer
**
**	Returns:
**		pointer to p-file struct for next entry
**		NULL on EOF or error
**
**	Side Effects:
**		Each call wipes out results of previous call.
*/

struct pfile *
getpfile(pfp)
	FILE *pfp;
{
	static struct pfile ent;
	static char buf[120];
	register char *p;
	extern char *nextfield();

	if (fgets(buf, sizeof buf, pfp) == NULL)
		return (NULL);

	ent.p_osid = p = buf;
	ent.p_nsid = p = nextfield(p);
	ent.p_user = p = nextfield(p);
	ent.p_date = p = nextfield(p);
	ent.p_time = p = nextfield(p);
	if (p == NULL || nextfield(p) != NULL)
		return (NULL);

	return (&ent);
}


char *
nextfield(p)
	register char *p;
{
	if (p == NULL || *p == '\0')
		return (NULL);
	while (*p != ' ' && *p != '\n' && *p != '\0')
		p++;
	if (*p == '\n' || *p == '\0')
	{
		*p = '\0';
		return (NULL);
	}
	*p++ = '\0';
	return (p);
}
