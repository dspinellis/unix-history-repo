# include <stdio.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <sys/dir.h>
# include <sysexits.h>
# include <whoami.h>

/*
**  SCCS.C -- human-oriented front end to the SCCS system.
**
**	Without trying to add any functionality to speak of, this
**	program tries to make SCCS a little more accessible to human
**	types.  The main thing it does is automatically put the
**	string "SCCS/s." on the front of names.  Also, it has a
**	couple of things that are designed to shorten frequent
**	combinations, e.g., "delget" which expands to a "delta"
**	and a "get".
**
**	This program can also function as a setuid front end.
**	To do this, you should copy the source, renaming it to
**	whatever you want, e.g., "syssccs".  Change any defaults
**	in the program (e.g., syssccs might default -d to
**	"/usr/src/sys").  Then recompile and put the result
**	as setuid to whomever you want.  In this mode, sccs
**	knows to not run setuid for certain programs in order
**	to preserve security, and so forth.
**
**	Usage:
**		sccs [flags] command [args]
**
**	Flags:
**		-d<dir>		<dir> represents a directory to search
**				out of.  It should be a full pathname
**				for general usage.  E.g., if <dir> is
**				"/usr/src/sys", then a reference to the
**				file "dev/bio.c" becomes a reference to
**				"/usr/src/sys/dev/bio.c".
**		-p<path>	prepends <path> to the final component
**				of the pathname.  By default, this is
**				"SCCS".  For example, in the -d example
**				above, the path then gets modified to
**				"/usr/src/sys/dev/SCCS/s.bio.c".  In
**				more common usage (without the -d flag),
**				"prog.c" would get modified to
**				"SCCS/s.prog.c".  In both cases, the
**				"s." gets automatically prepended.
**		-r		run as the real user.
**
**	Commands:
**		admin,
**		get,
**		delta,
**		rmdel,
**		chghist,
**		etc.		Straight out of SCCS; only difference
**				is that pathnames get modified as
**				described above.
**		edit		Macro for "get -e".
**		unedit		Removes a file being edited, knowing
**				about p-files, etc.
**		delget		Macro for "delta" followed by "get".
**		deledit		Macro for "delta" followed by "get -e".
**		info		Tell what files being edited.
**		clean		Remove all files that can be
**				regenerated from SCCS files.
**		check		Like info, but return exit status, for
**				use in makefiles.
**		fix		Remove a top delta & reedit, but save
**				the previous changes in that delta.
**
**	Compilation Flags:
**		UIDUSER -- determine who the user is by looking at the
**			uid rather than the login name -- for machines
**			where SCCS gets the user in this way.
**		SRCDIR -- if defined, forces the -d flag to take on
**			this value.  This is so that the setuid
**			aspects of this program cannot be abused.
**
**	Compilation Instructions:
**		cc -O -n -s sccs.c
**
**	Author:
**		Eric Allman, UCB/INGRES
*/

# ifdef CSVAX
# define UIDUSER
# endif

static char SccsId[] = "@(#)sccs.c	1.28 %G%";

# define bitset(bit, word)	((bit) & (word))

typedef char	bool;
# define TRUE	1
# define FALSE	0

# ifdef UIDUSER
# include <pwd.h>
# endif UIDUSER

char	MyName[] = "sccs";

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

/* modes for the "clean", "info", "check" ops */
# define CLEANC		0	/* clean command */
# define INFOC		1	/* info command */
# define CHECKC		2	/* check command */

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
	"fix",		FIX,	0,			NULL,
	"clean",	CLEAN,	REALUSER,		(char *) CLEANC,
	"info",		CLEAN,	REALUSER,		(char *) INFOC,
	"check",	CLEAN,	REALUSER,		(char *) CHECKC,
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
# ifdef SRCDIR
char	*SccsDir = SRCDIR;	/* directory to begin search from */
# else
char	*SccsDir = "";		/* directory to begin search from */
# endif
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
		fprintf(stderr, "Usage: %s [flags] command [flags]\n", MyName);
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

# ifndef SRCDIR
			  case 'p':		/* path of sccs files */
				SccsPath = ++p;
				break;

			  case 'd':		/* directory to search from */
				SccsDir = ++p;
				break;
# endif

# ifdef DEBUG
			  case 'T':		/* trace */
				Debug++;
				break;
# endif

			  default:
				usrerr("unknown option -%s", p);
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
		usrerr("Unknown command \"%s\"", argv[0]);
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
		syserr("internal error: CMACRO");
		exit(EX_SOFTWARE);

	  case FIX:		/* fix a delta */
		if (strncmp(argv[1], "-r", 2) != 0)
		{
			usrerr("-r flag needed for fix command");
			break;
		}
		xcommand(&argv[1], TRUE, "get", "-k", NULL);
		xcommand(&argv[1], TRUE, "rmdel", NULL);
		xcommand(&argv[2], FALSE, "get", "-e", "-g", NULL);
		syserr("FIX");
		exit(EX_SOFTWARE);

	  case CLEAN:
		clean((int) cmd->sccspath);
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
		syserr("oper %d", cmd->sccsoper);
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
	register char **nav;

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
			syserr("cannot fork");
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
	nav = &argv[1];
	av = argv;
	while ((p = *++av) != NULL)
	{
		if (!bitset(NO_SDOT, flags) && *p != '-')
			*nav = makefile(p);
		else
			*nav = p;
		if (*nav != NULL)
			nav++;
	}
	*nav = NULL;

	/*
	**  Set protection as appropriate.
	*/

	if (bitset(REALUSER, flags))
		setuid(getuid());
	
	/*
	**  Call real SCCS program.
	*/

	execv(progpath, argv);
	syserr("cannot execute %s", progpath);
	exit(EX_UNAVAILABLE);
}
/*
**  MAKEFILE -- make filename of SCCS file
**
**	If the name passed is already the name of an SCCS file,
**	just return it.  Otherwise, munge the name into the name
**	of the actual SCCS file.
**
**	There are cases when it is not clear what you want to
**	do.  For example, if SccsPath is an absolute pathname
**	and the name given is also an absolute pathname, we go
**	for SccsPath (& only use the last component of the name
**	passed) -- this is important for security reasons (if
**	sccs is being used as a setuid front end), but not
**	particularly intuitive.
**
**	Parameters:
**		name -- the file name to be munged.
**
**	Returns:
**		The pathname of the sccs file.
**		NULL on error.
**
**	Side Effects:
**		none.
*/

char *
makefile(name)
	char *name;
{
	register char *p;
	register char c;
	char buf[512];
	extern char *malloc();
	extern char *rindex();
	extern bool isdir();
	register char *q;

	p = rindex(name, '/');
	if (p == NULL)
		p = name;
	else
		p++;

	/*
	**  See if the name can be used as-is.
	*/

	if (SccsPath[0] != '/' || name[0] == '/' || strncmp(name, "./", 2) == 0)
	{
		if (strncmp(p, "s.", 2) == 0)
			return (name);
		if (isdir(name))
			return (name);
	}

	/*
	**  Create the actual pathname.
	*/

	if (name[0] != '/')
	{
		strcpy(buf, SccsDir);
		strcat(buf, "/");
	}
	else
		strcpy(buf, "");
	strncat(buf, name, p - name);
	q = &buf[strlen(buf)];
	strcpy(q, p);
	if (strncmp(p, "s.", 2) != 0 && !isdir(buf))
	{
		strcpy(q, SccsPath);
		strcat(buf, "/s.");
		strcat(buf, p);
	}

	if (strcmp(buf, name) == 0)
		p = name;

	return (stat(name, &stbuf) >= 0 && (stbuf.st_mode & S_IFMT) == S_IFDIR);
}
/*
**  ISDIR -- return true if the argument is a directory.
**
**	Parameters:
**		name -- the pathname of the file to check.
**
**	Returns:
**		TRUE if 'name' is a directory, FALSE otherwise.
**
**	Side Effects:
**		none.
*/

bool
isdir(name)
	char *name;
{
	struct stat stbuf;

	return (stat(name, &stbuf) >= 0 && (stbuf.st_mode & S_IFMT) == S_IFDIR);
}
/*
**  SAFEPATH -- determine whether a pathname is "safe"
**
**	"Safe" pathnames only allow you to get deeper into the
**	directory structure, i.e., full pathnames and ".." are
**	not allowed.
**
**	Parameters:
**		p -- the name to check.
**
**	Returns:
**		TRUE -- if the path is safe.
**		FALSE -- if the path is not safe.
**
**	Side Effects:
**		Prints a message if the path is not safe.
*/

bool
safepath(p)
	register char *p;
{
	extern char *index();

	if (*p != '/')
	{
		while (strncmp(p, "../", 3) != 0 && strcmp(p, "..") != 0)
		{
			p = index(p, '/');
			if (p == NULL)
				return (TRUE);
			p++;
		}
	}

	printf("You may not use full pathnames or \"..\"\n");
	return (FALSE);
}
/*
**  CLEAN -- clean out recreatable files
**
**	Any file for which an "s." file exists but no "p." file
**	exists in the current directory is purged.
**
**	Parameters:
**		tells whether this came from a "clean", "info", or
**		"check" command.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Removes files in the current directory.
**		Prints information regarding files being edited.
**		Exits if a "check" command.
*/

clean(mode)
	int mode;
{
	struct direct dir;
	struct stat stbuf;
	char buf[100];
	char pline[120];
	register FILE *dirfd;
	register char *basefile;
	bool gotedit;
	FILE *pfp;

	strcpy(buf, SccsDir);
	if (buf[0] != '\0')
		strcat(buf, "/");
	strcat(buf, SccsPath);
	dirfd = fopen(buf, "r");
	if (dirfd == NULL)
	{
		usrerr("cannot open %s", buf);
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
		strcpy(buf, SccsDir);
		if (buf[0] != '\0')
			strcat(buf, "/");
		strcat(buf, SccsPath);
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
		if (mode == CLEANC)
		{
			strncpy(buf, &dir.d_name[2], sizeof dir.d_name - 2);
			buf[sizeof dir.d_name - 2] = '\0';
			unlink(buf);
		}
	}

	fclose(dirfd);
	if (!gotedit && mode == INFOC)
		printf("Nothing being edited\n");
	if (mode == CHECKC)
		exit(gotedit);
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
# ifdef UIDUSER
	struct passwd *pw;
	extern struct passwd *getpwuid();
# endif UIDUSER

	/* make "s." filename & find the trailing component */
	pfn = makefile(fn);
	if (pfn == NULL)
		return (FALSE);
	q = rindex(pfn, '/');
	if (q == NULL)
		q = &pfn[-1];
	if (q[1] != 's' || q[2] != '.')
	{
		usrerr("bad file name \"%s\"", fn);
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
		usrerr("cannot create \"%s\"", tfn);
		exit(EX_OSERR);
	}

# ifdef UIDUSER
	pw = getpwuid(getuid());
	if (pw == NULL)
	{
		syserr("who are you? (uid=%d)", getuid());
		exit(EX_OSERR);
	}
	myname = pw->pw_name;
# else
	myname = getlogin();
# endif UIDUSER
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
			syserr("cannot reopen \"%s\"", tfn);
			exit(EX_OSERR);
		}
		if (freopen(pfn, "w", pfp) == NULL)
		{
			usrerr("cannot create \"%s\"", pfn);
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
/*
**  USRERR -- issue user-level error
**
**	Parameters:
**		f -- format string.
**		p1-p3 -- parameters to a printf.
**
**	Returns:
**		-1
**
**	Side Effects:
**		none.
*/

usrerr(f, p1, p2, p3)
	char *f;
{
	fprintf(stderr, "\n%s: ", MyName);
	fprintf(stderr, f, p1, p2, p3);
	fprintf(stderr, "\n");

	return (-1);
}
/*
**  SYSERR -- print system-generated error.
**
**	Parameters:
**		f -- format string to a printf.
**		p1, p2, p3 -- parameters to f.
**
**	Returns:
**		never.
**
**	Side Effects:
**		none.
*/

syserr(f, p1, p2, p3)
	char *f;
{
	extern int errno;

	fprintf(stderr, "\n%s SYSERR: ", MyName);
	fprintf(stderr, f, p1, p2, p3);
	fprintf(stderr, "\n");
	if (errno == 0)
		exit(EX_SOFTWARE);
	else
	{
		perror(0);
		exit(EX_OSERR);
	}
}
