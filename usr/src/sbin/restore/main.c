/* Copyright (c) 1983 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)main.c	3.1	(Berkeley)	83/02/18";
#endif

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
 *	Full incremental restore running entirely in user code.
 *	1/19/83		by Kirk McKusick
 */

#include "restore.h"
#include <signal.h>

int	cvtflag = 0, dflag = 0, vflag = 0, yflag = 0;
int	hflag = 1, mflag = 1;
char	command = '\0';
long	dumpnum = 1;
long	volno = 1;
char	*dumpmap;
char	*clrimap;
ino_t	maxino;
time_t	dumptime;
struct	entry **entry;

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *cp;
	char *inputdev = "/dev/rmt8";
	char *symtbl = "./lost+found/restoresymtable";
	char *dirmodefile = "./lost+found/dirmodes";
	int (*signal())();
	extern int onintr();
	ino_t ino;

	if (signal(SIGINT, onintr) == SIG_IGN)
		signal(SIGINT, SIG_IGN);
	if (signal(SIGTERM, onintr) == SIG_IGN)
		signal(SIGTERM, SIG_IGN);
	if (argc < 2) {
usage:
		fprintf(stderr, "Usage: restor xtfhmsvy file file... or restor rRfsvy\n");
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
			inputdev = *argv++;
			argc--;
			break;
		case 's':
			/*
			 * dumpnum (skip to) for multifile dump tapes
			 */
			dumpnum = atoi(*argv++);
			if (dumpnum <= 0) {
				fprintf(stderr, "Dump number must be a positive integer\n");
				done(1);
			}
			argc--;
			break;
		case 't':
			if (command != '\0') {
				fprintf(stderr,
					"t and %c are mutually exclusive\n",
					command);
				goto usage;
			}
			command = 't';
		case 'R':
			if (command != '\0') {
				fprintf(stderr,
					"R and %c are mutually exclusive\n",
					command);
				goto usage;
			}
			command = 'R';
		case 'r':
			if (command != '\0') {
				fprintf(stderr,
					"r and %c are mutually exclusive\n",
					command);
				goto usage;
			}
			command = 'r';
		case 'x':
			if (command != '\0') {
				fprintf(stderr,
					"x and %c are mutually exclusive\n",
					command);
				goto usage;
			}
			command = 'x';
			break;
		default:
			fprintf(stderr, "Bad key character %c\n", *cp);
			goto usage;
		}
	}
	if (command == '\0') {
		fprintf(stderr, "must specify t, r, R, or x\n");
		goto usage;
	}
	setinput(inputdev);
	if (argc == 0) {
		argc = 1;
		*--argv = ".";
	}
	switch (command) {

	case 't':
		setup();
		extractdirs((char *)0);
		while (argc--) {
			if ((ino = psearch(*argv)) == 0 ||
			    BIT(ino, dumpmap) == 0) {
				fprintf(stderr, "%s: not on tape\n", *argv++);
				continue;
			}
			treescan(*argv++, ino, listfile);
		}
		done(0);

	case 'x':
		setup();
		extractdirs(dirmodefile);
		entry = (struct entry **)
			calloc((int)maxino, sizeof(struct entry *));
		if (entry == (struct entry **)NIL)
			panic("no memory for entry table\n");
		(void)addentry(".", ROOTINO, NODE);
		while (argc--) {
			if ((ino = psearch(*argv)) == 0 ||
			    BIT(ino, dumpmap) == 0) {
				fprintf(stderr, "%s: not on tape\n", *argv++);
				continue;
			}
			if (mflag)
				pathcheck(*argv, NEW);
			if (hflag)
				treescan(*argv++, ino, addfile);
			else
				addfile(*argv++, ino, LEAF);
		}
		createfiles();
		setdirmodes(dirmodefile);
		if (dflag)
			checkrestore();
		done(0);

	case 'r':
		setup();
		extractdirs(dirmodefile);
		if (dumptime > 0) {
			initsymtable(symtbl);
		} else {
			entry = (struct entry **)
				calloc((int)maxino, sizeof(struct entry *));
			if (entry == (struct entry **)NIL)
				panic("no memory for entry table\n");
			(void)addentry(".", ROOTINO, NODE);
		}
		markremove();
		if ((ino = psearch(".")) == 0 || BIT(ino, dumpmap) == 0)
			panic("Root directory is not on tape\n");
		vprintf(stdout, "Calculate extraction list.\n");
		treescan(".", ino, markfile);
		findunref();
		removeleaves();
		renamenodes();
		createnodes();
		renameleaves();
		removenodes();
		createleaves(symtbl);
		createlinks();
		setdirmodes(dirmodefile);
		checkrestore();
		if (dflag) {
			vprintf(stdout, "Verify the directory structure\n");
			treescan(".", ROOTINO, verifyfile);
		}
		dumpsymtable(symtbl, (long)1);
		done(0);

	case 'R':
		initsymtable(symtbl);
		createleaves(symtbl);
		createlinks();
		setdirmodes(dirmodefile);
		checkrestore();
		dumpsymtable(symtbl, (long)1);
		done(0);
	}
}
