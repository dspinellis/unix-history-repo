/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * File I/O routines callable by users.
 */

#include "stdarg.h"
#include "calc.h"


#define	READSIZE	1024	/* buffer size for reading */

/*
 * Definition of opened files.
 */
typedef struct {
	FILEID id;		/* id to identify this file */
	FILE *fp;		/* real file structure for I/O */
	char *name;		/* file name */
	BOOL reading;		/* TRUE if opened for reading */
	BOOL writing;		/* TRUE if opened for writing */
	char *mode;		/* open mode */
} FILEIO;


/*
 * Table of opened files.
 * The first three entries always correspond to stdin, stdout, and stderr,
 * and cannot be closed.  Their file ids are always 0, 1, and 2.
 */
static FILEIO files[MAXFILES] = {
	FILEID_STDIN,  NULL,  "(stdin)",  TRUE, FALSE, "reading",
	FILEID_STDOUT, NULL, "(stdout)", FALSE, TRUE, "writing",
	FILEID_STDERR, NULL, "(stderr)", FALSE, TRUE, "writing"
};

static FILEID lastid = FILEID_STDERR;		/* last allocated file id */


/*
 * file_init - perform needed initilization work
 *
 * On some systems, one cannot initialize a pointer to a FILE *.
 * This routine, called once at startup is a work-a-round for
 * systems with such bogons.
 */
void
file_init()
{
    static int done = 0;	/* 1 => routine already called */

    if (!done) {
	files[0].fp = stdin;
	files[1].fp = stdout;
	files[2].fp = stderr;
	done = 1;
    }
}


/*
 * Open the specified file name for reading or writing as determined by
 * the specified mode ("r", "w", or "a").  Returns a file id which can be
 * used to do I/O to the file, or else FILEID_NONE if the open failed.
 * Aborts with an error if too many files are opened or the mode is illegal.
 */
FILEID
openid(name, mode)
	char *name;		/* file name */
	char *mode;		/* open mode */
{
	FILEIO *fiop;		/* file structure */
	FILEID id;		/* new file id */
	int count;

	if (((*mode != 'r') && (*mode != 'w') && (*mode != 'a')) || mode[1])
		math_error("Illegal mode for fopen");

	count = MAXFILES;
	do {
		if (--count < 0)
			math_error("Too many open files");
		id = ++lastid;
		fiop = &files[id % MAXFILES];

	} while (fiop->reading || fiop->writing);

	fiop->name = (char *)malloc(strlen(name) + 1);
	if (fiop->name == NULL) {
		lastid--;
		math_error("No memory for filename");
	}
	strcpy(fiop->name, name);

	fiop->fp = f_open(name, mode);
	if (fiop->fp == NULL) {
		free(fiop->name);
		fiop->name = NULL;
		lastid--;
		return FILEID_NONE;
	}

	switch (*mode) {
		case 'r':
			fiop->mode = "reading";
			fiop->reading = TRUE;
			break;
		case 'w':
			fiop->mode = "writing";
			fiop->writing = TRUE;
			break;
		case 'a':
			fiop->mode = "appending";
			fiop->writing = TRUE;
			break;
	}

	fiop->id = id;

	return id;
}


/*
 * Find the file I/O structure for the specified file id, and verify that
 * it is opened in the required manner ('r' for reading or 'w' for writing).
 * If mode is 0, then no open checks are made at all, and NULL is then
 * returned if the id represents a closed file.
 */
static FILEIO *
findid(id, mode)
	int mode;
	FILEID id;
{
	FILEIO *fiop;		/* file structure */
	static char *msg;
	BOOL flag = 0;

	if ((id < 0) || (id > lastid))
		math_error("Illegal file id");

	fiop = &files[id % MAXFILES];

	switch (mode) {
		case 'r':
			msg = "Reading from";
			flag = fiop->reading;
			break;
		case 'w':
			msg = "Writing to";
			flag = fiop->writing;
			break;
		case 0:
			msg = NULL;
			break;
		default:
			math_error("Unknown findid mode");
	}

	if (fiop->id != id) {
		if (msg)
			math_error("%s closed file", msg);
		return NULL;
	}

	if (msg && !flag)
		math_error("%s file not opened that way", msg);
	
	return fiop;
}


/*
 * Return whether or not a file id is valid.  This is used for if tests.
 */
BOOL
validid(id)
	FILEID id;
{
	return (findid(id, 0) != NULL);
}


/*
 * Return the file id for the entry in the file table at the specified index.
 * Returns FILEID_NONE if the index is illegal or the file is closed.
 */
FILEID
indexid(index)
	long index;
{
	FILEIO *fiop;		/* file structure */

	if ((index < 0) || (index >= MAXFILES))
		return FILEID_NONE;

	fiop = &files[index];
	if (fiop->reading || fiop->writing)
		return fiop->id;

	return FILEID_NONE;
}


/*
 * Close the specified file id.  Returns TRUE if there was an error.
 * Closing of stdin, stdout, or stderr is illegal, but closing of already
 * closed files is allowed.
 */
BOOL
closeid(id)
	FILEID id;
{
	FILEIO *fiop;		/* file structure */
	int err;

	if ((id == FILEID_STDIN) || (id == FILEID_STDOUT) ||
		(id == FILEID_STDERR))
			math_error("Cannot close stdin, stdout, or stderr");

	fiop = findid(id, 0);
	if (fiop == NULL)
		return FALSE;

	fiop->id = FILEID_NONE;
	if (!fiop->reading && !fiop->writing)
		math_error("Closing non-opened file");
	fiop->reading = FALSE;
	fiop->writing = FALSE;

	if (fiop->name)
		free(fiop->name);
	fiop->name = NULL;

	err = ferror(fiop->fp);
	err |= fclose(fiop->fp);
	fiop->fp = NULL;

	return (err != 0);
}


/*
 * Return whether or not an error occurred to a file.
 */
BOOL
errorid(id)
	FILEID id;
{
	FILEIO *fiop;		/* file structure */

	fiop = findid(id, 0);
	if (fiop == NULL)
		math_error("Closed file for ferror");
	return (ferror(fiop->fp) != 0);
}


/*
 * Return whether or not end of file occurred to a file.
 */
BOOL
eofid(id)
	FILEID id;
{
	FILEIO *fiop;		/* file structure */

	fiop = findid(id, 0);
	if (fiop == NULL)
		math_error("Closed file for feof");
	return (feof(fiop->fp) != 0);
}


/*
 * Flush output to an opened file.
 */
void
flushid(id)
	FILEID id;
{
	FILEIO *fiop;		/* file structure */

	fiop = findid(id, 'w');
	fflush(fiop->fp);
}


/*
 * Read the next line from an opened file.
 * Returns a pointer to an allocated string holding the null-terminated
 * line (without any terminating newline), or else a NULL pointer on an
 * end of file or error.
 */
void
readid(id, retptr)
	FILEID	id;		/* file to read from */
	char **retptr;		/* returned pointer to string */
{
	FILEIO *fiop;		/* file structure */
	char *str;		/* current string */
	int len;		/* current length of string */
	int totlen;		/* total length of string */
	char buf[READSIZE];	/* temporary buffer */

	totlen = 0;
	str = NULL;

	fiop = findid(id, 'r');

	while (fgets(buf, READSIZE, fiop->fp) && buf[0]) {
		len = strlen(buf);
		if (totlen)
			str = (char *)realloc(str, totlen + len + 1);
		else
			str = (char *)malloc(len + 1);
		if (str == NULL)
			math_error("No memory in freadline");
		strcpy(&str[totlen], buf);
		totlen += len;
		if (buf[len - 1] == '\n') {
			str[totlen - 1] = '\0';
			*retptr = str;
			return;
		}
	}
	if (totlen && ferror(fiop->fp)) {
		free(str);
		str = NULL;
	}
	*retptr = str;
}


/*
 * Return the next character from an opened file.
 * Returns EOF if there was an error or end of file.
 */
int
getcharid(id)
	FILEID id;
{
	return fgetc(findid(id, 'r')->fp);
}


/*
 * Print out the name of an opened file.
 * If the file has been closed, a null name is printed.
 * If flags contain PRINT_UNAMBIG then extra information is printed
 * identifying the output as a file and some data about it.
 */
void
printid(id, flags)
	int flags;
	FILEID id;
{
	FILEIO *fiop;		/* file structure */
	FILE *fp;

	fiop = findid(id, 0);
	if (fiop == NULL) {
		math_str((flags & PRINT_UNAMBIG) ? "FILE (closed)" : "\"\"");
		return;
	}
	if ((flags & PRINT_UNAMBIG) == 0) {
		math_chr('"');
		math_str(fiop->name);
		math_chr('"');
		return;
	}

	fp = fiop->fp;
	math_fmt("FILE \"%s\" (%s, pos %ld", fiop->name,  fiop->mode,
		ftell(fp));
	if (ferror(fp))
		math_str(", error");
	if (feof(fp))
		math_str(", eof");
	math_chr(')');
}


/*
 * Print a formatted string similar to printf.  Various formats of output
 * are possible, depending on the format string AND the actual types of the
 * values.  Mismatches do not cause errors, instead something reasonable is
 * printed instead.  The output goes to the file with the specified id.
 */
void
idprintf(id, fmt, count, vals)
	int count;
	FILEID id;			/* file id to print to */
	char *fmt;			/* standard format string */
	VALUE **vals;			/* table of values to print */
{
	FILEIO *fiop;
	VALUE *vp;
	char *str;
	int ch, len;
	int oldmode, newmode;
	long olddigits, newdigits;
	long width, precision;
	BOOL didneg, didprecision;

	fiop = findid(id, 'w');

	math_setfp(fiop->fp);

	while ((ch = *fmt++) != '\0') {
		if (ch == '\\') {
			ch = *fmt++;
			switch (ch) {
				case 'n': ch = '\n'; break;
				case 'r': ch = '\r'; break;
				case 't': ch = '\t'; break;
				case 'f': ch = '\f'; break;
				case 'v': ch = '\v'; break;
				case 'b': ch = '\b'; break;
				case 0:
					math_setfp(stdout);
					return;
			}
			math_chr(ch);
			continue;
		}

		if (ch != '%') {
			math_chr(ch);
			continue;
		}

		/*
		 * Here to handle formats.
		 */
		didneg = FALSE;
		didprecision = FALSE;
		width = 0;
		precision = 0;

		ch = *fmt++;
		if (ch == '-') {
			didneg = TRUE;
			ch = *fmt++;
		}
		while ((ch >= '0') && (ch <= '9')) {
			width = width * 10 + (ch - '0');
			ch = *fmt++;
		}
		if (ch == '.') {
			didprecision = TRUE;
			ch = *fmt++;
			while ((ch >= '0') && (ch <= '9')) {
				precision = precision * 10 + (ch - '0');
				ch = *fmt++;
			}
		}
		if (ch == 'l')
			ch = *fmt++;

		oldmode = _outmode_;
		newmode = oldmode;
		olddigits = _outdigits_;
		newdigits = olddigits;
		if (didprecision)
			newdigits = precision;

		switch (ch) {
			case 'd':
			case 's':
			case 'c':
				break;
			case 'f':
				newmode = MODE_REAL;
				break;
			case 'e':
				newmode = MODE_EXP;
				break;
			case 'r':
				newmode = MODE_FRAC;
				break;
			case 'o':
				newmode = MODE_OCTAL;
				break;
			case 'x':
				newmode = MODE_HEX;
				break;
			case 'b':
				newmode = MODE_BINARY;
				break;
			case 0:
				math_setfp(stdout);
				return;
			default:
				math_chr(ch);
				continue;
		}

		if (--count < 0)
			math_error("Not enough arguments for fprintf");
		vp = *vals++;

		math_setdigits(newdigits);
		math_setmode(newmode);

		/*
		 * If there is no width specification, or if the type of
		 * value requires multiple lines, then just output the
		 * value directly.
		 */
		if ((width == 0) ||
			(vp->v_type == V_MAT) || (vp->v_type == V_LIST))
		{
			printvalue(vp, PRINT_NORMAL);
			math_setmode(oldmode);
			math_setdigits(olddigits);
			continue;
		}

		/*
		 * There is a field width.  Collect the output in a string,
		 * print it padded appropriately with spaces, and free it.
		 * However, if the output contains a newline, then ignore
		 * the field width.
		 */
		math_divertio();
		printvalue(vp, PRINT_NORMAL);
		str = math_getdivertedio();
		if (strchr(str, '\n'))
			width = 0;
		len = strlen(str);
		while (!didneg && (width > len)) {
			width--;
			math_chr(' ');
		}
		math_str(str);
		free(str);
		while (didneg && (width > len)) {
			width--;
			math_chr(' ');
		}
		math_setmode(oldmode);
		math_setdigits(olddigits);
	}
	math_setfp(stdout);
}

/* END CODE */
