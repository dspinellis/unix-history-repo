#ifndef lint
static char sccsid[] = "@(#)main.c	3.12	(Berkeley)	83/08/11";
#endif

/* Copyright (c) 1983 Regents of the University of California */

/*
 *	Modified to recursively extract all files within a subtree
 *	(supressed by the h option) and recreate the heirarchical
 *	structure of that subtree and move extracted files to their
 *	proper homes (supressed by the m option).
 *	Includes the s (skip files) option for use with multiple
 *	dumps on a single tape.
 *	8/29/80		by Mike Litzkow
 *
 *	Modified to work on the new file system and to recover from
 *	tape read errors.
 *	1/19/82		by Kirk McKusick
 *
 *	Full incremental restore running entirely in user code and
 *	interactive tape browser.
 *	1/19/83		by Kirk McKusick
 */

#include "restore.h"
#include <signal.h>

int	cvtflag = 0, dflag = 0, vflag = 0, yflag = 0;
int	hflag = 1, mflag = 1;
char	command = '\0';
long	dumpnum = 1;
long	volno = 0;
char	*dumpmap;
char	*clrimap;
ino_t	maxino;
time_t	dumptime;
time_t	dumpdate;
FILE 	*terminal;

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *cp;
	ino_t ino;
	char *inputdev = "/dev/rmt8";
	char *symtbl = "./restoresymtable";
	char name[MAXPATHLEN];
	int (*signal())();
	extern int onintr();

	if (signal(SIGINT, onintr) == SIG_IGN)
		(void) signal(SIGINT, SIG_IGN);
	if (signal(SIGTERM, onintr) == SIG_IGN)
		(void) signal(SIGTERM, SIG_IGN);
	setlinebuf(stderr);
	if (argc < 2) {
usage:
		fprintf(stderr, "Usage:\n%s%s%s%s%s",
			"\trestore tfhsvy [file file ...]\n",
			"\trestore xfhmsvy [file file ...]\n",
			"\trestore ifhmsvy\n",
			"\trestore rfsvy\n",
			"\trestore Rfsvy\n");
		done(1);
	}
	argv++;
	argc -= 2;
	command = '\0';
	for (cp = *argv++; *cp; cp++) {
		switch (*cp) {
		case '-':
			break;
		case 'c':
			cvtflag++;
			break;
		case 'd':
			dflag++;
			break;
		case 'h':
			hflag = 0;
			break;
		case 'm':
			mflag = 0;
			break;
		case 'v':
			vflag++;
			break;
		case 'y':
			yflag++;
			break;
		case 'f':
			if (argc < 1) {
				fprintf(stderr, "missing device specifier\n");
				done(1);
			}
			inputdev = *argv++;
			argc--;
			break;
		case 's':
			/*
			 * dumpnum (skip to) for multifile dump tapes
			 */
			if (argc < 1) {
				fprintf(stderr, "missing dump number\n");
				done(1);
			}
			dumpnum = atoi(*argv++);
			if (dumpnum <= 0) {
				fprintf(stderr, "Dump number must be a positive integer\n");
				done(1);
			}
			argc--;
			break;
		case 't':
		case 'R':
		case 'r':
		case 'x':
		case 'i':
			if (command != '\0') {
				fprintf(stderr,
					"%c and %c are mutually exclusive\n",
					*cp, command);
				goto usage;
			}
			command = *cp;
			break;
		default:
			fprintf(stderr, "Bad key character %c\n", *cp);
			goto usage;
		}
	}
	if (command == '\0') {
		fprintf(stderr, "must specify i, t, r, R, or x\n");
		goto usage;
	}
	setinput(inputdev);
	if (argc == 0) {
		argc = 1;
		*--argv = ".";
	}
	switch (command) {
	/*
	 * Interactive mode.
	 */
	case 'i':
		setup();
		extractdirs(1);
		initsymtable((char *)0);
		runcmdshell();
		done(0);
	/*
	 * Incremental restoration of a file system.
	 */
	case 'r':
		setup();
		if (dumptime > 0) {
			/*
			 * This is an incremental dump tape.
			 */
			vprintf(stdout, "Begin incremental restore\n");
			initsymtable(symtbl);
			extractdirs(1);
			removeoldleaves();
			vprintf(stdout, "Calculate node updates.\n");
			treescan(".", ROOTINO, nodeupdates);
			findunreflinks();
			removeoldnodes();
		} else {
			/*
			 * This is a level zero dump tape.
			 */
			vprintf(stdout, "Begin level 0 restore\n");
			initsymtable((char *)0);
			extractdirs(1);
			vprintf(stdout, "Calculate extraction list.\n");
			treescan(".", ROOTINO, nodeupdates);
		}
		createleaves(symtbl);
		createlinks();
		setdirmodes();
		checkrestore();
		if (dflag) {
			vprintf(stdout, "Verify the directory structure\n");
			treescan(".", ROOTINO, verifyfile);
		}
		dumpsymtable(symtbl, (long)1);
		done(0);
	/*
	 * Resume an incremental file system restoration.
	 */
	case 'R':
		initsymtable(symtbl);
		skipmaps();
		skipdirs();
		createleaves(symtbl);
		createlinks();
		setdirmodes();
		checkrestore();
		dumpsymtable(symtbl, (long)1);
		done(0);
	/*
	 * List contents of tape.
	 */
	case 't':
		setup();
		extractdirs(0);
		while (argc--) {
			canon(*argv++, name);
			ino = dirlookup(name);
			if (ino == 0)
				continue;
			treescan(name, ino, listfile);
		}
		done(0);
	/*
	 * Batch extraction of tape contents.
	 */
	case 'x':
		setup();
		extractdirs(1);
		initsymtable((char *)0);
		while (argc--) {
			canon(*argv++, name);
			ino = dirlookup(name);
			if (ino == 0)
				continue;
			if (mflag)
				pathcheck(name);
			treescan(name, ino, addfile);
		}
		createfiles();
		createlinks();
		setdirmodes();
		if (dflag)
			checkrestore();
		done(0);
	}
}

/*
 * Read and execute commands from the terminal.
 */
runcmdshell()
{
	register struct entry *np;
	ino_t ino;
	char curdir[MAXPATHLEN];
	char name[MAXPATHLEN];
	char cmd[BUFSIZ];

	canon("/", curdir);
loop:
	getcmd(curdir, cmd, name);
	switch (cmd[0]) {
	/*
	 * Add elements to the extraction list.
	 */
	case 'a':
		ino = dirlookup(name);
		if (ino == 0)
			break;
		if (mflag)
			pathcheck(name);
		treescan(name, ino, addfile);
		break;
	/*
	 * Change working directory.
	 */
	case 'c':
		ino = dirlookup(name);
		if (ino == 0)
			break;
		if (inodetype(ino) == LEAF) {
			fprintf(stderr, "%s: not a directory\n", name);
			break;
		}
		(void) strcpy(curdir, name);
		break;
	/*
	 * Delete elements from the extraction list.
	 */
	case 'd':
		np = lookupname(name);
		if (np == NIL || (np->e_flags & NEW) == 0) {
			fprintf(stderr, "%s: not on extraction list\n", name);
			break;
		}
		treescan(name, np->e_ino, deletefile);
		break;
	/*
	 * Extract the requested list.
	 */
	case 'e':
		createfiles();
		createlinks();
		setdirmodes();
		if (dflag)
			checkrestore();
		volno = 0;
		break;
	/*
	 * List available commands.
	 */
	case 'h':
	case '?':
		fprintf(stderr, "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
			"Available commands are:\n",
			"\tls [arg] - list directory\n",
			"\tcd arg - change directory\n",
			"\tpwd - print current directory\n",
			"\tadd [arg] - add `arg' to list of",
			" files to be extracted\n",
			"\tdelete [arg] - delete `arg' from",
			" list of files to be extracted\n",
			"\textract - extract requested files\n",
			"\tquit - immediately exit program\n",
			"\tverbose - toggle verbose flag",
			" (useful with ``ls'')\n",
			"\thelp or `?' - print this list\n",
			"If no `arg' is supplied, the current",
			" directory is used\n");
		break;
	/*
	 * List a directory.
	 */
	case 'l':
		ino = dirlookup(name);
		if (ino == 0)
			break;
		printlist(name, ino);
		break;
	/*
	 * Print current directory.
	 */
	case 'p':
		if (curdir[1] == '\0')
			fprintf(stderr, "/\n");
		else
			fprintf(stderr, "%s\n", &curdir[1]);
		break;
	/*
	 * Quit.
	 */
	case 'q':
	case 'x':
		return;
	/*
	 * Toggle verbose mode.
	 */
	case 'v':
		if (vflag) {
			fprintf(stderr, "verbose mode off\n");
			vflag = 0;
			break;
		}
		fprintf(stderr, "verbose mode on\n");
		vflag++;
		break;
	/*
	 * Turn on debugging.
	 */
	case 'D':
		if (dflag) {
			fprintf(stderr, "debugging mode off\n");
			dflag = 0;
			break;
		}
		fprintf(stderr, "debugging mode on\n");
		dflag++;
		break;
	/*
	 * Unknown command.
	 */
	default:
		fprintf(stderr, "%s: unknown command; type ? for help\n", cmd);
		break;
	}
	goto loop;
}
