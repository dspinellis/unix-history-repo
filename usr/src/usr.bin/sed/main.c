/*-
 * Copyright (c) 1992 Diomidis Spinellis.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Diomidis Spinellis of Imperial College, University of London.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <regex.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "defs.h"
#include "extern.h"

/*
 * Linked list of units (strings and files) to be compiled
 */
struct s_compunit {
	struct s_compunit *next;
	enum e_cut {CU_FILE, CU_STRING} type;
	char *s;			/* Pointer to string or fname */
};

/*
 * Linked list pointer to compilation units and pointer to current
 * next pointer.
 */
static struct s_compunit *script, **cu_nextp = &script;

/*
 * Linked list of files to be processed
 */
struct s_flist {
	char *fname;
	struct s_flist *next;
};

/*
 * Linked list pointer to files and pointer to current
 * next pointer.
 */
static struct s_flist *files, **fl_nextp = &files;

int compile_errors;		/* Compile error count. */
int aflag, eflag, nflag;

/*
 * Current file and line number; line numbers restart across compilation
 * units, but span across input files.
 */
char *fname;			/* File name. */
u_long linenum;
int lastline;			/* TRUE on the last line of the last file */

static void add_compunit __P((enum e_cut, char *));
static void add_file __P((char *));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int c, fflag;

	fflag = 0;
	while ((c = getopt(argc, argv, "ae:f:n")) != EOF)
		switch (c) {
		case 'a':
			aflag = 1;
			break;
		case 'e':
			eflag = 1;
			add_compunit(CU_STRING, optarg);
			break;
		case 'f':
			fflag = 1;
			add_compunit(CU_FILE, optarg);
			break;
		case 'n':
			nflag = 1;
			break;
		default:
		case '?':
			(void)fprintf(stderr,
"usage:\tsed script [-an] [file ...]\n\tsed [-an] [-e script] ... [-f scipt_file] ... [file ...]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	/* First usage case; script is the first arg */
	if (!eflag && !fflag && *argv) {
		add_compunit(CU_STRING, *argv);
		argv++;
	}

	compile();
	if (compile_errors)
		exit(1);

	/* Continue with first and start second usage */
	if (*argv)
		for (; *argv; argv++)
			add_file(*argv);
	else
		add_file(NULL);
	process();
	cfclose(prog);
	if (fclose(stdout))
		err(FATAL, "stdout: %s", strerror(errno));
	exit (0);
}

/*
 * Like fgets, but go through the chain of compilation units chaining them
 * together.  Empty strings and files are ignored.
 */
char *
cu_fgets(buf, n)
	char *buf;
	int n;
{
	static enum {ST_EOF, ST_FILE, ST_STRING} state = ST_EOF;
	static FILE *f;		/* Current open file */
	static char *s;		/* Current pointer inside string */
	static char string_ident[20];
	char *p;

again:
	switch (state) {
	case ST_EOF:
		if (script == NULL)
			return (NULL);
		linenum = 0;
		switch (script->type) {
		case CU_FILE:
			if ((f = fopen(script->s, "r")) == NULL)
				err(FATAL,
				    "%s: %s", script->s, strerror(errno));
			fname = script->s;
			state = ST_FILE;
			goto again;
		case CU_STRING:
			/* Have better handling here */
			(void)strncpy(string_ident,
			    script->s, sizeof(string_ident));
			(void)strcpy(string_ident + sizeof(string_ident) - 5,
			    "...");
			fname = string_ident;
			s = script->s;
			state = ST_STRING;
			goto again;
		}
	case ST_FILE:
		if ((p = fgets(buf, n, f)) != NULL) {
			linenum++;
			if (linenum == 1 && buf[0] == '#' && buf[1] == 'n')
				nflag = 1;
			return (p);
		}
		script = script->next;
		(void)fclose(f);
		state = ST_EOF;
		goto again;
	case ST_STRING:
		if (linenum == 0 && s[0] == '#' && s[1] == 'n')
			nflag = 1;
		p = buf;
		for (;;) {
			if (n-- <= 1) {
				*p = '\0';
				linenum++;
				return (buf);
			}
			switch (*s) {
			case '\0':
				state = ST_EOF;
				if (s == script->s) {
					script = script->next;
					goto again;
				} else {
					script = script->next;
					*p = '\0';
					linenum++;
					return (buf);
				}
			case '\n':
				*p++ = '\n';
				*p = '\0';
				s++;
				linenum++;
				return (buf);
			default:
				*p++ = *s++;
			}
		}
	}
	/* NOTREACHED */
}

/*
 * Like fgets, but go through the list of files chaining them together.
 * Set len to the length of the line.
 */
char *
mf_fgets(lenp)
	size_t *lenp;
{
	static FILE *f;		/* Current open file */
	char c, *p;

	if (f == NULL)
		/* Advance to first non-empty file */
		for (;;) {
			if (files == NULL) {
				lastline = 1;
				return (NULL);
			}
			if (files->fname == NULL) {
				f = stdin;
				fname = "stdin";
			} else {
				fname = files->fname;
				if ((f = fopen(fname, "r")) == NULL)
					err(FATAL, "%s: %s",
					    fname, strerror(errno));
			}
			if ((c = getc(f)) != EOF) {
				(void)ungetc(c, f);
				break;
			}
			(void)fclose(f);
			files = files->next;
		}

	if (lastline) {
		*lenp = 0;
		return (NULL);
	}

	p = fgetline(f, lenp);
	if (ferror(f))
		err(FATAL, "%s: %s", fname, strerror(errno ? errno : EIO));

	linenum++;
	/* Advance to next non-empty file */
	while ((c = getc(f)) == EOF) {
		(void)fclose(f);
		files = files->next;
		if (files == NULL) {
			lastline = 1;
			return (p);
		}
		if (files->fname == NULL) {
			f = stdin;
			fname = "stdin";
		} else {
			fname = files->fname;
			if ((f = fopen(fname, "r")) == NULL)
				err(FATAL, "%s: %s", fname, strerror(errno));
		}
	}
	(void)ungetc(c, f);
	return (p);
}

/*
 * Add a compilation unit to the linked list
 */
static void
add_compunit(type, s)
	enum e_cut type;
	char *s;
{
	struct s_compunit *cu;

	cu = xmalloc(sizeof(struct s_compunit));
	cu->type = type;
	cu->s = s;
	cu->next = NULL;
	*cu_nextp = cu;
	cu_nextp = &cu->next;
}

/*
 * Add a file to the linked list
 */
static void
add_file(s)
	char *s;
{
	struct s_flist *fp;

	fp = xmalloc(sizeof(struct s_flist));
	fp->next = NULL;
	*fl_nextp = fp;
	fp->fname = s;
	fl_nextp = &fp->next;
}
