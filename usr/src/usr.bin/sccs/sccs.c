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
**		SCCSDIR -- if defined, forces the -d flag to take on
**			this value.  This is so that the setuid
**			aspects of this program cannot be abused.
**			This flag also disables the -p flag.
**		SCCSPATH -- the default for the -p flag.
**
**	Compilation Instructions:
**		cc -O -n -s sccs.c
**
**	Author:
**		Eric Allman, UCB/INGRES
**		Copyright 1980 Regents of the University of California
*/

static char SccsId[] = "@(#)sccs.c	1.35 %G%";

/*******************  Configuration Information  ********************/

# ifdef CSVAX
# define UIDUSER
# define PROGPATH(name)	"/usr/local/name"
# endif CSVAX

# define SCCSPATH	"SCCS"	/* pathname in which to find s-files */
/* put #define SCCSDIR here */

char	MyName[] = "sccs";	/* name used in messages */

# ifndef PROGPATH
# define PROGPATH(name)	"/usr/sccs/name"	/* place to find binaries */
# endif PROGPATH

/****************  End of Configuration Information  ****************/

# define bitset(bit, word)	((bit) & (word))

typedef char	bool;
# define TRUE	1
# define FALSE	0

# ifdef UIDUSER
# include <pwd.h>
# endif UIDUSER

struct sccsprog
{
	char	*sccsname;	/* name of SCCS routine */
	short	sccsoper;	/* opcode, see below */
	short	sccsflags;	/* flags, see below */
	char	*sccsklets;	/* valid key-letters on macros */
	char	*sccspath;	/* pathname of binary implementing */
};

/* values for sccsoper */
# define PROG		0	/* call a program */
# define CMACRO		1	/* command substitution macro */
# define FIX		2	/* fix a delta */
# define CLEAN		3	/* clean out recreatable files */
# define UNEDIT		4	/* unedit a file */
# define SHELL		5	/* call a shell file (like PROG) */

/* bits for sccsflags */
# define NO_SDOT	0001	/* no s. on front of args */
# define REALUSER	0002	/* protected (e.g., admin) */

/* modes for the "clean", "info", "check" ops */
# define CLEANC		0	/* clean command */
# define INFOC		1	/* info command */
# define CHECKC		2	/* check command */

/*
**  Description of commands known to this program.
**	First argument puts the command into a class.  Second arg is
**	info regarding treatment of this command.  Third arg is a
**	list of flags this command accepts from macros, etc.  Fourth
**	arg is the pathname of the implementing program, or the
**	macro definition, or the arg to a sub-algorithm.
*/

struct sccsprog SccsProg[] =
{
	"admin",	PROG,	REALUSER,	"",		PROGPATH(admin),
	"chghist",	PROG,	0,		"",		PROGPATH(rmdel),
	"comb",		PROG,	0,		"",		PROGPATH(comb),
	"delta",	PROG,	0,		"mysrp",	PROGPATH(delta),
	"get",		PROG,	0,		"ixbeskcl",	PROGPATH(get),
	"help",		PROG,	NO_SDOT,	"",		PROGPATH(help),
	"prt",		PROG,	0,		"",		PROGPATH(prt),
	"rmdel",	PROG,	REALUSER,	"",		PROGPATH(rmdel),
	"what",		PROG,	NO_SDOT,	"",		PROGPATH(what),
	"sccsdiff",	SHELL,	REALUSER,	"",		PROGPATH(sccsdiff),
	"edit",		CMACRO,	NO_SDOT,	"ixbscl",	"get -e",
	"delget",	CMACRO,	NO_SDOT,	"",		"delta/get -t",
	"deledit",	CMACRO,	NO_SDOT,	"",		"delta/get -e -t",
	"fix",		FIX,	NO_SDOT,	"",		NULL,
	"clean",	CLEAN,	REALUSER,	"",		(char *) CLEANC,
	"info",		CLEAN,	REALUSER,	"",		(char *) INFOC,
	"check",	CLEAN,	REALUSER,	"",		(char *) CHECKC,
	"unedit",	UNEDIT,	NO_SDOT,	"",		NULL,
	NULL,		-1,	0,		"",		NULL
};

/* one line from a p-file */
struct pfile
{
	char	*p_osid;	/* old SID */
	char	*p_nsid;	/* new SID */
	char	*p_user;	/* user who did edit */
	char	*p_date;	/* date of get */
	char	*p_time;	/* time of get */
};

char	*SccsPath = SCCSPATH;	/* pathname of SCCS files */
# ifdef SCCSDIR
char	*SccsDir = SCCSDIR;	/* directory to begin search from */
# else
char	*SccsDir = "";
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
	register int i;

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

# ifndef SCCSDIR
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

	i = command(argv, FALSE, FALSE, "");
	exit(i);
}

/*
**  COMMAND -- look up and perform a command
**
**	This routine is the guts of this program.  Given an
**	argument vector, it looks up the "command" (argv[0])
**	in the configuration table and does the necessary stuff.
**
**	Parameters:
**		argv -- an argument vector to process.
**		forkflag -- if set, fork before executing the command.
**		editflag -- if set, only include flags listed in the
**			sccsklets field of the command descriptor.
**		arg0 -- a space-seperated list of arguments to insert
**			before argv.
**
**	Returns:
**		zero -- command executed ok.
**		else -- error status.
**
**	Side Effects:
**		none.
*/

command(argv, forkflag, editflag, arg0)
	char **argv;
	bool forkflag;
	bool editflag;
	char *arg0;
{
	register struct sccsprog *cmd;
	register char *p;
	char buf[40];
	extern struct sccsprog *lookup();
	char *nav[1000];
	char **np;
	register char **ap;
	register int i;
	register char *q;
	extern bool unedit();
	int rval = 0;
	extern char *index();
	extern char *makefile();

# ifdef DEBUG
	if (Debug)
	{
		printf("command:\n\t\"%s\"\n", arg0);
		for (np = argv; *np != NULL; np++)
			printf("\t\"%s\"\n", *np);
	}
# endif

	/*
	**  Copy arguments.
	**	Phase one -- from arg0 & if necessary argv[0].
	*/

	np = ap = &nav[1];
	for (p = arg0, q = buf; *p != '\0' && *p != '/'; )
	{
		*np++ = q;
		while (*p == ' ')
			p++;
		while (*p != ' ' && *p != '\0' && *p != '/')
			*q++ = *p++;
		*q++ = '\0';
	}
	*np = NULL;
	if (*ap == NULL)
		*np++ = *argv++;

	/*
	**  Look up command.
	**	At this point, *ap is the command name.
	*/

	cmd = lookup(*ap);
	if (cmd == NULL)
	{
		usrerr("Unknown command \"%s\"", *ap);
		return (EX_USAGE);
	}

	/*
	**  Copy remaining arguments doing editing as appropriate.
	*/

	for (; *argv != NULL; argv++)
	{
		p = *argv;
		if (*p == '-')
		{
			if (p[1] == '\0' || !editflag || cmd->sccsklets == NULL ||
			    index(cmd->sccsklets, p[1]) != NULL)
				*np++ = p;
		}
		else
		{
			if (!bitset(NO_SDOT, cmd->sccsflags))
				p = makefile(p);
			if (p != NULL)
				*np++ = p;
		}
	}
	*np = NULL;

	/*
	**  Interpret operation associated with this command.
	*/

	switch (cmd->sccsoper)
	{
	  case SHELL:		/* call a shell file */
		*ap = cmd->sccspath;
		*--ap = "sh";
		rval = callprog("/bin/sh", cmd->sccsflags, ap, forkflag);
		break;

	  case PROG:		/* call an sccs prog */
		rval = callprog(cmd->sccspath, cmd->sccsflags, ap, forkflag);
		break;

	  case CMACRO:		/* command macro */
		for (p = cmd->sccspath; *p != '\0'; p++)
		{
			q = p;
			while (*p != '\0' && *p != '/')
				p++;
			rval = command(&ap[1], *p != '\0', TRUE, q);
			if (rval != 0)
				break;
		}
		break;

	  case FIX:		/* fix a delta */
		if (strncmp(ap[1], "-r", 2) != 0)
		{
			usrerr("-r flag needed for fix command");
			rval = EX_USAGE;
			break;
		}
		rval = command(&ap[1], TRUE, TRUE, "get -k");
		if (rval == 0)
			rval = command(&ap[1], TRUE, TRUE, "rmdel");
		if (rval == 0)
			rval = command(&ap[2], FALSE, TRUE, "get -e -g");
		break;

	  case CLEAN:
		rval = clean((int) cmd->sccspath);
		break;

	  case UNEDIT:
		for (argv = np = &ap[1]; *argv != NULL; argv++)
		{
			if (unedit(*argv))
				*np++ = *argv;
		}
		*np = NULL;
		if (i > 0)
			rval = command(&ap[1], FALSE, FALSE, "get");
		break;

	  default:
		syserr("oper %d", cmd->sccsoper);
		exit(EX_SOFTWARE);
	}
# ifdef DEBUG
	if (Debug)
		printf("command: rval=%d\n", rval);
# endif
	return (rval);
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

/*
**  CALLPROG -- call a program
**
**	Used to call the SCCS programs.
**
**	Parameters:
**		progpath -- pathname of the program to call.
**		flags -- status flags from the command descriptors.
**		argv -- an argument vector to pass to the program.
**		forkflag -- if true, fork before calling, else just
**			exec.
**
**	Returns:
**		The exit status of the program.
**		Nothing if forkflag == FALSE.
**
**	Side Effects:
**		Can exit if forkflag == FALSE.
*/

callprog(progpath, flags, argv, forkflag)
	char *progpath;
	short flags;
	char **argv;
	bool forkflag;
{
	register int i;
	auto int st;

# ifdef DEBUG
	if (Debug)
	{
		printf("callprog:\n");
		for (i = 0; argv[i] != NULL; i++)
			printf("\t\"%s\"\n", argv[i]);
	}
# endif

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
			if ((st & 0377) == 0)
				st = (st >> 8) & 0377;
			return (st);
		}
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
		return (EX_NOINPUT);
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
	return (EX_OK);
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
	extern char *makefile();
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
