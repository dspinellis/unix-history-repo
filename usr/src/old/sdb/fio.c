static	char sccsid[] = "@(#)fio.c 4.2 %G%";
/*
 * sdb - a symbolic debugger for unix - source file access routines.
 */
#include "head.h"
#include <stdio.h>

/*
 * These procedures manage the source files examined by sdb,
 * providing access to lines by number and utilities for printing
 * and scrolling.  One file is kept open by these routines, and
 * line index tables are maintained for all files which have been
 * ``current'' at any time so far.  This makes line access trivial,
 * since the location of each line in the files is known,
 * although we get ``burned'' if the file is changed.
 * SHOULD WATCH THE MODTIME OF FILES AND REINDEX IF IT CHANGES.
 */

/*
 * Structure for files which have been ``indexed''.
 * Contains a pointer to the file name, a pointer to an
 * array of seek pointers for the lines in the file,
 * and a next link in a chain of these for all files we have indexed,
 * The currently open file is cinfo->; the chain of active files is finfo.
 */
struct	finfo {
	char	*name;			/* name of this file w/o common pfx */
	off_t	*lines;			/* array of seek pointers */
/* line i stretches from lines[i-1] to lines[i] - 1, if first line is 1 */
	int	nlines;			/* number of lines in file */
/* lines array actually has nlines+1 elements, so last line is bracketed */
	struct	finfo *next;		/* link in chain of known files */
} *finfo, *cfile;
FILE	*FIO;				/* current open file (only 1 now) */
char	fibuf[BUFSIZ];
/*
 * We use stdio when first reading the file, but thereafter
 * use our own routines, because we want to be able
 * to read backwards efficiently and avoid a tell() system
 * call on each line.  Fseekpt remebers where we are in the current
 * file.
 */
off_t	fseekpt;

/*
 * Make ``name'' the current source file, if it isn't already.
 * If we have never seen this file before, then we create a finfo
 * structure for it indexing the lines (this requires reading the
 * entire file and building an index, but is well worth it since
 * we otherwise have to brute force search the files all the time.)
 */
finit(name)
	char *name;
{
	char buf[BUFSIZ];
	register off_t *lp;
	
	if (cfile && !strcmp(cfile->name, name))
		return;			/* its already current, do nothing */
	/* IT WOULD BE BETTER TO HAVE A COUPLE OF FILE DESCRIPTORS, LRU */
	if (FIO) {
		fclose(FIO);
		FIO = NULL;
	}
	/*
	 * Paste the given name onto the common prefix (directory path)
	 * to form the full name of the file to be opened.
	 */
	strcpy(fp, name);
	if ((FIO = fopen(filework, "r")) == NULL) {
		nolines = 1;
		perror(filework);
		return;
	}
	setbuf(FIO, fibuf);
	fseekpt = -BUFSIZ;		/* putatively illegal */
	strcpy(curfile, name);
	/*
	 * See if we have alread indexed this file.
	 * If so, nothing much to do.
	 */
	for (cfile = finfo; cfile; cfile = cfile->next)
		if (!strcmp(cfile->name, name))
			return;
	/*
	 * Create a structure for this (new) file.
	 * Lines array grows 100 lines at a time.
	 * 1 extra so last line is bracketed.
	 */
	cfile = (struct finfo *)sbrk(sizeof (struct finfo));
	lp = cfile->lines = (off_t *)sbrk(101 * sizeof (off_t));
	*lp++ = 0;		/* line 1 starts at 0 ... */
	cfile->nlines = 0;
	/* IT WOULD PROBABLY BE FASTER TO JUST USE GETC AND LOOK FOR \n */
	while (fgets(buf, sizeof buf, FIO)) {
		if ((++cfile->nlines % 100) == 0)
			sbrk(100 * sizeof (off_t));
		/*
		 * Mark end of the cfile->nlines'th line
		 */
		lp[0] = lp[-1] + strlen(buf);
		lp++;
	}
	if (cfile->nlines == 0) {
		printf("%s: no lines in file\n", filework);
		cfile = 0;
		return;
	}
	/*
	 * Allocate space for the name, making sure to leave the
	 * break on a word boundary.
	 * IT WOULD BE MUCH BETTER TO USE MALLOC AND REALLOC IN SDB.
	 */
	sbrk(lp + ((strlen(name)+sizeof(off_t)-1)&~(sizeof(off_t)-1)));
	strcpy(cfile->name = (char *)lp, name);
	cfile->next = finfo;
	finfo = cfile;
}

/*
 * Get the current line (fline) into fbuf
 */
fgetline()
{
	register off_t *op = &cfile->lines[fline-1];
	int o, n;

	n = op[1] - op[0];
	fbuf[n] = 0;
	/*
	 * Case 1.  Line begins in current buffer.
	 *
	 * Compute the number of characters into the buffer where
	 * the line starts.  If this offset plus its length is greater
	 * than BUFSIZ, then this line splits across a buffer boundary
	 * so take the rest of this buffer and the first part of the next.
	 * Otherwise just take a chunk of this buffer.
	 */
	if (*op >= fseekpt && *op < fseekpt + BUFSIZ) {
case1:
		o = op[0] - fseekpt;
		if (o + n > BUFSIZ) {
			strncpy(fbuf, fibuf+o, BUFSIZ-o);
			fseekpt += BUFSIZ;
			read(fileno(FIO), fibuf, BUFSIZ);
			strncpy(fbuf+BUFSIZ-o, fibuf, n-(BUFSIZ-o));
		} else
			strncpy(fbuf, fibuf+o, n);
		return;
	}
	/*
	 * Case 2.  Line ends in current buffer.
	 *
	 * If the line ends in this buffer (but doesn't begin in
	 * it or else we would have had case 1) take the beginning
	 * part of the buffer (end of the line) and then back up and
	 * get the rest of the line from the end of the previous block.
	 */
	if (op[1]-1 >= fseekpt && op[1] <= fseekpt+BUFSIZ) {
		o = op[1] - fseekpt;
		strncpy(fbuf+n-o, fibuf, o);
		fseekpt -= BUFSIZ;
		lseek(fileno(FIO), fseekpt, 0);
		read(fileno(FIO), fibuf, BUFSIZ);
		strncpy(fbuf, fibuf+op[0]-fseekpt, n-o);
		return;
	}
	/*
	 * Case 3.  Line not in current buffer at all.
	 *
	 * Read in the buffer where the line starts and then go
	 * back and handle as case 1.
	 */
	fseekpt = (op[0] / BUFSIZ) * BUFSIZ;
	lseek(fileno(FIO), fseekpt, 0);
	read(fileno(FIO), fibuf, BUFSIZ);
	goto case1;
}

/*
 * Advance current line, end-around (like for / search).
 */
fnext()
{
	
	if (cfile == 0)
		return;
	if (fline == cfile->nlines) {
		fline = 1;
	} else
		fline++;
	fgetline();
}

/*
 * Retreat the current line, end around.
 */
fprev()
{

	if (cfile == 0)
		return;
	if (fline == 1)
		fline = cfile->nlines;
	else
		fline--;
	fgetline();
}

/*
 * Print the current line.
 */
fprint()
{
	register char *p;
	
	if (cfile == 0) {
		error("No lines in file");
		return;
	}
	printf("%d: %s", fline, fbuf);
}

/*
 * Make line `num' current.
 */
ffind(num)
	register int num;
{
	
	if (cfile == 0)
		return;
	if (num > cfile->nlines)
		error("Not that many lines in file");
	else if (num <= 0)
		error("Zero or negative line?");
	else {
		fline = num;
		fgetline();
	}
}

/*
 * Go back n lines.
 */
fback(n)
{
	int i;
	
	if (cfile == 0)
		return (0);
	if (n > fline - 1)
		n = fline - 1;
	fline -= n;
	fgetline();
	return (n);
}

/*
 * Go forwards n lines.
 */
fforward(n)
	int n;
{
	register int fnext;
	
	if (cfile == 0)
		return(0);
	if (fline + n > cfile->nlines)
		n = cfile->nlines - fline;
	fline += n;
	fgetline();
	return (n);
}

/*
 * Print (upto) n lines, returning number printed.
 */
fprintn(n)
	int n;
{
	register int i;
	
	if (cfile == 0) {
		error("No lines in file");
		return (0);
	}
	for (i = 1; i <= n; i++) {
		fprint();
		if (fline == cfile->nlines || i == n)
			return(i);
		fnext();
	}
	return (n);
}
