/* Copyright (c) 1985 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)interactive.c	3.1	(Berkeley)	%G%";
#endif not lint

#include "restore.h"
#include <dumprestor.h>
#include <setjmp.h>

/*
 * Things to handle interruptions.
 */
static jmp_buf reset;
static char *nextarg = NULL;

/*
 * Structure and routines associated with listing directories.
 */
struct afile {
	ino_t	fnum;		/* inode number of file */
	char	*fname;		/* file name */
	short	fflags;		/* extraction flags, if any */
	char	ftype;		/* file type, e.g. LEAF or NODE */
};
extern int fcmp();
extern char *fmtentry();
char *copynext();

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
	if (setjmp(reset) != 0) {
		nextarg = NULL;
		volno = 0;
	}
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
		printlist(name, ino, curdir);
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
	 * Just restore requested directory modes.
	 */
	case 'R':
		setdirmodes();
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

/*
 * Read and parse an interactive command.
 * The first word on the line is assigned to "cmd". If
 * there are no arguments on the command line, then "curdir"
 * is returned as the argument. If there are arguments
 * on the line they are returned one at a time on each
 * successive call to getcmd. Each argument is first assigned
 * to "name". If it does not start with "/" the pathname in
 * "curdir" is prepended to it. Finally "canon" is called to
 * eliminate any embedded ".." components.
 */
getcmd(curdir, cmd, name)
	char *curdir, *cmd, *name;
{
	register char *cp;
	static char input[BUFSIZ];
	char output[BUFSIZ];
#	define rawname input	/* save space by reusing input buffer */

	/*
	 * Check to see if still processing arguments.
	 */
	if (nextarg != NULL)
		goto getnext;
	/*
	 * Read a command line and trim off trailing white space.
	 */
	do	{
		fprintf(stderr, "restore > ");
		(void) fflush(stderr);
		(void) fgets(input, BUFSIZ, terminal);
	} while (!feof(terminal) && input[0] == '\n');
	if (feof(terminal)) {
		(void) strcpy(cmd, "quit");
		return;
	}
	for (cp = &input[strlen(input) - 2]; *cp == ' ' || *cp == '\t'; cp--)
		/* trim off trailing white space and newline */;
	*++cp = '\0';
	/*
	 * Copy the command into "cmd".
	 */
	cp = copynext(input, cmd);
	/*
	 * If no argument, use curdir as the default.
	 */
	if (*cp == '\0') {
		(void) strcpy(name, curdir);
		return;
	}
	nextarg = cp;
	/*
	 * Find the next argument.
	 */
getnext:
	cp = copynext(nextarg, rawname);
	if (*cp == '\0')
		nextarg = NULL;
	else
		nextarg = cp;
	/*
	 * If it an absolute pathname, canonicalize it and return it.
	 */
	if (rawname[0] == '/') {
		canon(rawname, name);
	} else {
		/*
		 * For relative pathnames, prepend the current directory to
		 * it then canonicalize and return it.
		 */
		(void) strcpy(output, curdir);
		(void) strcat(output, "/");
		(void) strcat(output, rawname);
		canon(output, name);
	}
#	undef rawname
}

/*
 * Strip off the next token of the input.
 */
char *
copynext(input, output)
	char *input, *output;
{
	register char *cp, *bp;
	char quote;

	for (cp = input; *cp == ' ' || *cp == '\t'; cp++)
		/* skip to argument */;
	bp = output;
	while (*cp != ' ' && *cp != '\t' && *cp != '\0') {
		/*
		 * Handle back slashes.
		 */
		if (*cp == '\\') {
			if (*++cp == '\0') {
				fprintf(stderr,
					"command lines cannot be continued\n");
				continue;
			}
			*bp++ = *cp++;
			continue;
		}
		/*
		 * The usual unquoted case.
		 */
		if (*cp != '\'' && *cp != '"') {
			*bp++ = *cp++;
			continue;
		}
		/*
		 * Handle single and double quotes.
		 */
		quote = *cp++;
		while (*cp != quote && *cp != '\0')
			*bp++ = *cp++ | 0200;
		if (*cp++ == '\0') {
			fprintf(stderr, "missing %c\n", quote);
			cp--;
			continue;
		}
	}
	*bp = '\0';
	return (cp);
}

/*
 * Canonicalize file names to always start with ``./'' and
 * remove any imbedded ".." components.
 */
canon(rawname, canonname)
	char *rawname, *canonname;
{
	register char *cp, *np;
	int len;

	if (strcmp(rawname, ".") == 0 || strncmp(rawname, "./", 2) == 0)
		(void) strcpy(canonname, "");
	else if (rawname[0] == '/')
		(void) strcpy(canonname, ".");
	else
		(void) strcpy(canonname, "./");
	(void) strcat(canonname, rawname);
	len = strlen(canonname) - 1;
	if (canonname[len] == '/')
		canonname[len] = '\0';
	/*
	 * Eliminate extraneous ".." from pathnames.
	 */
	for (np = canonname; *np != '\0'; ) {
		np++;
		cp = np;
		while (*np != '/' && *np != '\0')
			np++;
		if (np - cp == 2 && strncmp(cp, "..", 2) == 0) {
			cp--;
			while (cp > &canonname[1] && *--cp != '/')
				/* find beginning of name */;
			(void) strcpy(cp, np);
			np = cp;
		}
	}
}

/*
 * Do an "ls" style listing of a directory
 */
printlist(name, ino, basename)
	char *name;
	ino_t ino;
	char *basename;
{
	register struct afile *fp;
	struct afile *dfp0, *dfplast;
	struct afile single;
	DIR *dirp;

	if ((dirp = rst_opendir(name)) == NULL) {
		single.fnum = ino;
		single.fname = savename(name + strlen(basename));
		dfp0 = &single;
		dfplast = dfp0 + 1;
	} else {
		if (getdir(dirp, &dfp0, &dfplast) == FAIL)
			return;
	}
	qsort((char *)dfp0, dfplast - dfp0, sizeof (struct afile), fcmp);
	formatf(dfp0, dfplast);
	for (fp = dfp0; fp < dfplast; fp++)
		freename(fp->fname);
}

/*
 * Read the contents of a directory.
 */
getdir(dirp, pfp0, pfplast)
	DIR *dirp;
	struct afile **pfp0, **pfplast;
{
	register struct afile *fp;
	register struct direct *dp;
	static struct afile *basefp = NULL;
	static long nent = 20;

	if (basefp == NULL) {
		basefp = (struct afile *)calloc((unsigned)nent,
			sizeof (struct afile));
		if (basefp == NULL) {
			fprintf(stderr, "ls: out of memory\n");
			return (FAIL);
		}
	}
	fp = *pfp0 = basefp;
	*pfplast = *pfp0 + nent;
	while (dp = rst_readdir(dirp)) {
		if (dp == NULL || dp->d_ino == 0)
			break;
		if (!dflag && BIT(dp->d_ino, dumpmap) == 0)
			continue;
		if (vflag == 0 &&
		    (strcmp(dp->d_name, ".") == 0 ||
		     strcmp(dp->d_name, "..") == 0))
			continue;
		fp->fnum = dp->d_ino;
		fp->fname = savename(dp->d_name);
		fp++;
		if (fp == *pfplast) {
			basefp = (struct afile *)realloc((char *)basefp,
			    (unsigned)(2 * nent * sizeof (struct afile)));
			if (basefp == 0) {
				fprintf(stderr, "ls: out of memory\n");
				return (FAIL);
			}
			*pfp0 = basefp;
			fp = *pfp0 + nent;
			*pfplast = fp + nent;
			nent *= 2;
		}
	}
	*pfplast = fp;
	return (GOOD);
}

/*
 * Print out a pretty listing of a directory
 */
formatf(fp0, fplast)
	struct afile *fp0, *fplast;
{
	register struct afile *fp;
	struct entry *np;
	int width = 0, w, nentry = fplast - fp0;
	int i, j, len, columns, lines;
	char *cp;

	if (fp0 == fplast)
		return;
	for (fp = fp0; fp < fplast; fp++) {
		fp->ftype = inodetype(fp->fnum);
		np = lookupino(fp->fnum);
		if (np != NIL)
			fp->fflags = np->e_flags;
		else
			fp->fflags = 0;
		len = strlen(fmtentry(fp));
		if (len > width)
			width = len;
	}
	width += 2;
	columns = 80 / width;
	if (columns == 0)
		columns = 1;
	lines = (nentry + columns - 1) / columns;
	for (i = 0; i < lines; i++) {
		for (j = 0; j < columns; j++) {
			fp = fp0 + j * lines + i;
			cp = fmtentry(fp);
			fprintf(stderr, "%s", cp);
			if (fp + lines >= fplast) {
				fprintf(stderr, "\n");
				break;
			}
			w = strlen(cp);
			while (w < width) {
				w++;
				fprintf(stderr, " ");
			}
		}
	}
}

/*
 * Comparison routine for qsort.
 */
fcmp(f1, f2)
	register struct afile *f1, *f2;
{

	return (strcmp(f1->fname, f2->fname));
}

/*
 * Format a directory entry.
 */
char *
fmtentry(fp)
	register struct afile *fp;
{
	static char fmtres[BUFSIZ];
	register char *cp, *dp;

	if (vflag)
		(void) sprintf(fmtres, "%5d ", fp->fnum);
	else
		fmtres[0] = '\0';
	dp = &fmtres[strlen(fmtres)];
	if (dflag && BIT(fp->fnum, dumpmap) == 0)
		*dp++ = '^';
	else if ((fp->fflags & NEW) != 0)
		*dp++ = '*';
	else
		*dp++ = ' ';
	for (cp = fp->fname; *cp; cp++)
		if (!vflag && (*cp < ' ' || *cp >= 0177))
			*dp++ = '?';
		else
			*dp++ = *cp;
	if (fp->ftype == NODE)
		*dp++ = '/';
	*dp++ = 0;
	return (fmtres);
}

/*
 * respond to interrupts
 */
onintr()
{
	if (command == 'i')
		longjmp(reset, 1);
	if (reply("restore interrupted, continue") == FAIL)
		done(1);
}
