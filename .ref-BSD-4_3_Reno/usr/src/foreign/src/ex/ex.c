/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char *copyright =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char *sccsid = "@(#)ex.c	7.7.1.1 (Berkeley) 10/21/90";
#endif not lint

#include "ex.h"
#include "ex_argv.h"
#include "ex_temp.h"
#include "ex_tty.h"
#include "pathnames.h"

#ifdef TRACE
#ifdef	vms
char	tttrace[]	= { 't','r','a','c','e','.','l','i','s' };
#else
char	tttrace[]	= { '/','d','e','v','/','t','t','y','x','x',0 };
#endif
#endif

/*
 * The code for ex is divided as follows:
 *
 * ex.c			Entry point and routines handling interrupt, hangup
 *			signals; initialization code.
 *
 * ex_addr.c		Address parsing routines for command mode decoding.
 *			Routines to set and check address ranges on commands.
 *
 * ex_cmds.c		Command mode command decoding.
 *
 * ex_cmds2.c		Subroutines for command decoding and processing of
 *			file names in the argument list.  Routines to print
 *			messages and reset state when errors occur.
 *
 * ex_cmdsub.c		Subroutines which implement command mode functions
 *			such as append, delete, join.
 *
 * ex_data.c		Initialization of options.
 *
 * ex_get.c		Command mode input routines.
 *
 * ex_io.c		General input/output processing: file i/o, unix
 *			escapes, filtering, source commands, preserving
 *			and recovering.
 *
 * ex_put.c		Terminal driving and optimizing routines for low-level
 *			output (cursor-positioning); output line formatting
 *			routines.
 *
 * ex_re.c		Global commands, substitute, regular expression
 *			compilation and execution.
 *
 * ex_set.c		The set command.
 *
 * ex_subr.c		Loads of miscellaneous subroutines.
 *
 * ex_temp.c		Editor buffer routines for main buffer and also
 *			for named buffers (Q registers if you will.)
 *
 * ex_tty.c		Terminal dependent initializations from termcap
 *			data base, grabbing of tty modes (at beginning
 *			and after escapes).
 *
 * ex_unix.c		Routines for the ! command and its variations.
 *
 * ex_v*.c		Visual/open mode routines... see ex_v.c for a
 *			guide to the overall organization.
 */

/*
 * Main procedure.  Process arguments and then
 * transfer control to the main command processing loop
 * in the routine commands.  We are entered as either "ex", "edit", "vi"
 * or "view" and the distinction is made here.  Actually, we are "vi" if
 * there is a 'v' in our name, "view" is there is a 'w', and "edit" if
 * there is a 'd' in our name.  For edit we just diddle options;
 * for vi we actually force an early visual command.
 */
main(ac, av)
	register int ac;
	register char *av[];
{
#ifdef EXSTRINGS
	char *erpath = EXSTRINGS;
#endif
	register char *cp;
	register int c;
	bool recov = 0;
	bool ivis;
	bool itag = 0;
	bool fast = 0;
	extern int onemt();
#ifdef UNIX_SBRK
	extern char *sbrk();
#else
	extern char *malloc();
#endif
#ifdef TRACE
	register char *tracef;
#endif
#ifdef	vms
	char termtype[20];
#endif

	/*
	 * Immediately grab the tty modes so that we wont
	 * get messed up if an interrupt comes in quickly.
	 */
	ex_gTTY(1);
#ifndef USG3TTY
	normf = tty.sg_flags;
#else
	normf = tty;
#endif
	ppid = getpid();
	/*
	 * Defend against d's, v's, w's, and a's in directories of
	 * path leading to our true name.
	 */
#ifndef	vms
	av[0] = tailpath(av[0]);
#else
	/*
	 * This program has to be invoked by using the following
	 * string definitions:
	 *
	 * vi == "$dir:ex.exe vi"
	 * view == "$dir:ex.exe view"
	 * ex == "$dir:ex.exe ex"
	 * edit == "$dir:ex.exe edit"
	 */
	ac--;
	av++;
#endif

	/*
	 * Figure out how we were invoked: ex, edit, vi, view.
	 */
	ivis = any('v', av[0]);	/* "vi" */
	if (any('w', av[0]))	/* "view" */
		value(READONLY) = 1;
	if (any('d', av[0])) {	/* "edit" */
		value(OPEN) = 0;
		value(REPORT) = 1;
		value(MAGIC) = 0;
	}

#ifdef EXSTRINGS
	/*
	 * For debugging take files out of . if name is a.out.
	 */
	if (av[0][0] == 'a')
		erpath = tailpath(erpath);
#endif
	/*
	 * Open the error message file.
	 */
	draino();
#ifdef EXSTRINGS
	erfile = open(erpath+4, 0);
	if (erfile < 0) {
		erfile = open(erpath, 0);
	}
#endif
	pstop();

	/*
	 * Initialize interrupt handling.
	 */
	oldhup = signal(SIGHUP, SIG_IGN);
	if (oldhup == SIG_DFL)
		signal(SIGHUP, onhup);
	oldquit = signal(SIGQUIT, SIG_IGN);
	ruptible = signal(SIGINT, SIG_IGN) == SIG_DFL;
	if (signal(SIGTERM, SIG_IGN) == SIG_DFL)
		signal(SIGTERM, onhup);
	if (signal(SIGEMT, SIG_IGN) == SIG_DFL)
		signal(SIGEMT, onemt);

	/*
	 * Process flag arguments.
	 */
	ac--, av++;
	while (ac && av[0][0] == '-') {
		c = av[0][1];
		if (c == 0) {
			hush = 1;
			value(AUTOPRINT) = 0;
			fast++;
		} else switch (c) {

		case 'R':
			value(READONLY) = 1;
			break;

#ifdef TRACE
		case 'T':
			if (av[0][2] == 0)
				tracef = "trace";
			else {
				tracef = tttrace;
				tracef[8] = av[0][2];
				if (tracef[8])
					tracef[9] = av[0][3];
				else
					tracef[9] = 0;
			}
			trace = fopen(tracef, "w");
#define tracbuf NULL
			if (trace == NULL)
				ex_printf("Trace create error\n");
			else
				setbuf(trace, tracbuf);
			break;

#endif

#ifdef LISPCODE
		case 'l':
			value(LISP) = 1;
			value(SHOWMATCH) = 1;
			break;
#endif

		case 'r':
			recov++;
			break;

		case 't':
			if (ac > 1 && av[1][0] != '-') {
				ac--, av++;
				itag = 1;
				/* BUG: should check for too long tag. */
				CP(lasttag, av[0]);
			}
			break;

		case 'v':
			ivis = 1;
			break;

		case 'w':
			defwind = 0;
			if (av[0][2] == 0) defwind = 3;
			else for (cp = &av[0][2]; isdigit(*cp); cp++)
				defwind = 10*defwind + *cp - '0';
			break;


		default:
			smerror("Unknown option %s\n", av[0]);
			break;
		}
		ac--, av++;
	}

#ifdef SIGTSTP
	if (!hush && signal(SIGTSTP, SIG_IGN) == SIG_DFL)
		signal(SIGTSTP, onsusp), dosusp++;
#endif

	if (ac && av[0][0] == '+') {
		firstpat = &av[0][1];
		ac--, av++;
	}


	/*
	 * If we are doing a recover and no filename
	 * was given, then execute an exrecover command with
	 * the -r option to type out the list of saved file names.
	 * Otherwise set the remembered file name to the first argument
	 * file name so the "recover" initial command will find it.
	 */
	if (recov) {
		if (ac == 0) {
			ppid = 0;
			setrupt();
			execl(_PATH_EXRECOVER, "exrecover", "-r", 0);
			filioerr(_PATH_EXRECOVER);
			ex_exit(1);
		}
		CP(savedfile, *av++), ac--;
	}

	/*
	 * Initialize the argument list.
	 */
	argv0 = av;
	argc0 = ac;
	args0 = av[0];
	erewind();

	/*
	 * Initialize a temporary file (buffer) and
	 * set up terminal environment.  Read user startup commands.
	 */
	if (setexit() == 0) {
		setrupt();
		intty = isatty(0);
		value(PROMPT) = intty;
#ifndef	vms
		if (cp = getenv("SHELL"))
#else
		if (cp = getlog("SHELL"))
#endif
			CP(shell, cp);
		if (fast || !intty)
			setterm("dumb");
		else {
			gettmode();
#ifndef	vms
			if ((cp = getenv("TERM")) != 0 && *cp)
				setterm(cp);
#else
			if ((cp = getlog("TERM")) != 0 && *cp) {
				/*
				 * Can't just use it directly since getlog
				 * returns a pointer to a static buffer that
				 * tgetent() will eventually use
				 */
				CP(termtype, cp);
				setterm(termtype);
			}
#endif
		}
	}
	if (setexit() == 0 && !fast && intty) {
#ifndef	vms
		if ((globp = getenv("EXINIT")) && *globp)
#else
		if ((globp = getlog("EXINIT")) && *globp)
#endif
			commands(1,1);
		else {
			globp = 0;
			if ((cp = getenv("HOME")) != 0 && *cp) {
				(void) strcat(strcpy(genbuf, cp), "/.exrc");
				if (iownit(genbuf))
					source(genbuf, 1);
			}
		}
		/*
		 * Allow local .exrc too.  This loses if . is $HOME,
		 * but nobody should notice unless they do stupid things
		 * like putting a version command in .exrc.  Besides,
		 * they should be using EXINIT, not .exrc, right?
		 */
		 if (iownit(".exrc"))
			source(".exrc", 1);
	}
#ifdef	UNIX_SBRK
	/*
	 * Initialize end of core pointers.
	 * Normally we avoid breaking back to fendcore after each
	 * file since this can be expensive (much core-core copying).
	 * If your system can scatter load processes you could do
	 * this as ed does, saving a little core, but it will probably
	 * not often make much difference.
	 */
	fendcore = (line *) sbrk(0);
	endcore = fendcore - 2;
#else
	/*
	 * Allocate all the memory we will ever use in one chunk.
	 * This is for system such as VMS where sbrk() does not
	 * guarantee that the memory allocated beyond the end is
	 * consecutive.  VMS's RMS does all sorts of memory allocation
	 * and screwed up ex royally because ex assumes that all
	 * memory up to "endcore" belongs to it and RMS has different
	 * ideas.
	 */
	fendcore = (line *) malloc((unsigned)
		value(LINELIMIT) * sizeof (line *));
	if (fendcore == NULL) {
		lprintf("ex: cannot handle %d lines\n", value(LINELIMIT));
		lprintf("ex: set \"linelimit\" lower\n");
		flush();
		ex_exit(1);
	}
	endcore = fendcore + (value(LINELIMIT) - 1);
#endif
	init();	/* moved after prev 2 chunks to fix directory option */

	/*
	 * Initial processing.  Handle tag, recover, and file argument
	 * implied next commands.  If going in as 'vi', then don't do
	 * anything, just set initev so we will do it later (from within
	 * visual).
	 */
	if (setexit() == 0) {
		if (recov)
			globp = "recover";
		else if (itag)
			globp = ivis ? "tag" : "tag|p";
		else if (argc)
			globp = "next";
		if (ivis)
			initev = globp;
		else if (globp) {
			inglobal = 1;
			commands(1, 1);
			inglobal = 0;
		}
	}

	/*
	 * Vi command... go into visual.
	 * Strange... everything in vi usually happens
	 * before we ever "start".
	 */
	if (ivis) {
		/*
		 * Don't have to be upward compatible with stupidity
		 * of starting editing at line $.
		 */
		if (dol > zero)
			dot = one;
		globp = "visual";
		if (setexit() == 0)
			commands(1, 1);
	}

	/*
	 * Clear out trash in state accumulated by startup,
	 * and then do the main command loop for a normal edit.
	 * If you quit out of a 'vi' command by doing Q or ^\,
	 * you also fall through to here.
	 */
	seenprompt = 1;
	ungetchar(0);
	globp = 0;
	initev = 0;
	setlastchar('\n');
	setexit();
	commands(0, 0);
	cleanup(1);
	ex_exit(0);
}

/*
 * Initialization, before editing a new file.
 * Main thing here is to get a new buffer (in fileinit),
 * rest is peripheral state resetting.
 */
init()
{
	register int i;

	fileinit();
	dot = zero = truedol = unddol = dol = fendcore;
	one = zero+1;
	undkind = UNDNONE;
	chng = 0;
	edited = 0;
	for (i = 0; i <= 'z'-'a'+1; i++)
		names[i] = 1;
	anymarks = 0;
}

/*
 * Return last component of unix path name p.
 */
char *
tailpath(p)
register char *p;
{
	register char *r;

	for (r=p; *p; p++)
		if (*p == '/')
			r = p+1;
	return(r);
}

/*
 * Check ownership of file.  Return nonzero if it exists and is owned by the
 * user or the option sourceany is used
 */
iownit(file)
char *file;
{
	struct stat sb;

	if (stat(file, &sb) == 0 && (value(SOURCEANY) || sb.st_uid == getuid()))
		return(1);
	else
		return(0);
}
