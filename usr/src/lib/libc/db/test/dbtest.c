/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)dbtest.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>

#include <ctype.h>
#include <db.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

enum S { COMMAND, GET, PUT, REMOVE, SEQ, SEQFLAG, KEY, DATA };

DBTYPE	 dbtype __P((char *));
void	 err __P((const char *, ...));
void	 get __P((DB *, DBT *));
void	 put __P((DB *, DBT *, DBT *));
void	 rem __P((DB *, DBT *));
void	*rfile __P((char *, size_t *));
void	 seq __P((DB *));
u_int	 setflags __P((char *));
void	*setinfo __P((DBTYPE, char *));
void	 usage __P((void));
void	*xmalloc __P((char *, size_t));

DBTYPE type;
void *infop;
u_long lineno;
u_int flags;
int ofd = STDOUT_FILENO;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	enum S command, state;
	DB *dbp;
	DBT data, key;
	size_t len;
	int ch;
	char *infoarg, *p, buf[8 * 1024];

	infoarg = NULL;
	while ((ch = getopt(argc, argv, "i:o:")) != EOF)
		switch(ch) {
		case 'i':
			infoarg = optarg;
			break;
		case 'o':
			if ((ofd = open(optarg,
			    O_WRONLY|O_CREAT|O_TRUNC, 0666)) < 0)
				err("%s: %s", optarg, strerror(errno));
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 2)
		usage();

	/* Set the type. */
	type = dbtype(*argv++);

	/* Open the descriptor file. */
	if (freopen(*argv, "r", stdin) == NULL)
		err("%s: %s", *argv, strerror(errno));

	/* Set up the db structure as necessary. */
	if (infoarg == NULL)
		infop = NULL;
	else
		while ((p = strsep(&infoarg, ",\t ")) != NULL)
			if (*p != '\0')
				infop = setinfo(type, p);

#define	BACKINGFILE	"/tmp/__dbtest"
	/* Open the DB. */
	(void)unlink(BACKINGFILE);
	if ((dbp = dbopen(BACKINGFILE,
	    O_CREAT | O_RDWR, S_IRUSR | S_IWUSR, type, infop)) == NULL)
		err("dbopen: %s", strerror(errno));

	state = COMMAND;
	for (lineno = 1;
	    (p = fgets(buf, sizeof(buf), stdin)) != NULL; ++lineno) {
		len = strlen(buf);
		switch(*p) {
		case 'g':			/* get */
			if (state != COMMAND)
				err("line %lu: not expecting command", lineno);
			state = KEY;
			command = GET;
			break;
		case 'p':			/* put */
			if (state != COMMAND)
				err("line %lu: not expecting command", lineno);
			state = KEY;
			command = PUT;
			break;
		case 'r':			/* remove */
			if (state != COMMAND)
				err("line %lu: not expecting command", lineno);
			state = KEY;
			command = REMOVE;
			break;
		case 's':			/* seq */
			if (state != COMMAND)
				err("line %lu: not expecting command", lineno);
			seq(dbp);
			break;
		case 'f':
			flags |= setflags(p + 1);
			break;
		case 'D':			/* data file */
			if (state != DATA)
				err("line %lu: not expecting data", lineno);
			if (command != PUT)
				err("line %lu: command doesn't take data",
				    lineno);
			data.data = rfile(p + 1, &data.size);
			put(dbp, &key, &data);
			free(key.data);
			free(data.data);
			state = COMMAND;
			break;
		case 'd':			/* data */
			if (state != DATA)
				err("line %lu: not expecting data", lineno);
			if (command != PUT)
				err("line %lu: command doesn't take data",
				    lineno);
			data.data = xmalloc(p + 1, len - 1);
			data.size = len - 1;
			put(dbp, &key, &data);
			free(key.data);
			free(data.data);
			state = COMMAND;
			break;
		case 'K':			/* key file */
			if (state != KEY)
				err("line %lu: not expecting a key", lineno);
			if (type == DB_RECNO)
				err("line %lu: 'K' not available for recno",
				    lineno);
			key.data = rfile(p + 1, &key.size);
			switch(command) {
			case GET:
				get(dbp, &key);
				free(key.data);
				state = COMMAND;
				break;
			case PUT:
				state = DATA;
				break;
			case REMOVE:
				rem(dbp, &key);
				free(key.data);
				state = COMMAND;
				break;
			default:
				err("line %lu: command doesn't take a key",
				    lineno);
			}
			break;
		case 'k':			/* key */
			if (state != KEY)
				err("line %lu: not expecting a key", lineno);
			if (type == DB_RECNO) {
				static recno_t recno;
				recno = strtol(p + 1, NULL, 0);
				key.data = &recno;
				key.size = sizeof(recno);
			} else {
				key.data = xmalloc(p + 1, len - 1);
				key.size = len - 1;
			}
			switch(command) {
			case GET:
				get(dbp, &key);
				if (type != DB_RECNO)
					free(key.data);
				state = COMMAND;
				break;
			case PUT:
				state = DATA;
				break;
			case REMOVE:
				rem(dbp, &key);
				if (type != DB_RECNO)
					free(key.data);
				state = COMMAND;
				break;
			default:
				err("line %lu: command doesn't take a key",
				    lineno);
			}
			break;
		default:
			err("line %lu: %s: unknown command character",
			    *p, lineno);
		}
	}
	(void)close(ofd);
	exit(0);
}

void
get(dbp, kp)
	DB *dbp;
	DBT *kp;
{
	DBT data;

	if (dbp->get(dbp, kp, &data, flags) == -1)
		err("line %lu: get: %s", lineno, strerror(errno));
	(void)write(ofd, data.data, data.size);
}

void
put(dbp, kp, dp)
	DB *dbp;
	DBT *kp, *dp;
{
	if (dbp->put(dbp, kp, dp, flags) == -1)
		err("line %lu: put: %s", lineno, strerror(errno));
}

void
rem(dbp, kp)
	DB *dbp;
	DBT *kp;
{
	if (dbp->del(dbp, kp, flags) == -1)
		err("line %lu: get: %s", lineno, strerror(errno));
}

void
seq(dbp)
	DB *dbp;
{
	DBT key, data;
	size_t len;
	char nbuf[20];

	if (dbp->seq(dbp, &key, &data, flags) == -1)
		err("line %lu: seq: %s", lineno, strerror(errno));
	if (type == DB_RECNO) {
		len = sprintf(nbuf, "%lu\n", *(u_long *)key.data);
		(void)write(ofd, nbuf, len);
	} else
		(void)write(ofd, key.data, key.size);
	(void)write(ofd, data.data, data.size);
}
	
u_int
setflags(s)
	char *s;
{
	char *p;

	for (; isspace(*s); ++s);
	if (*s == '\n')
		return (0);
	if ((p = index(s, '\n')) != NULL)
		*p = '\0';
	if (!strcmp(s, "R_APPEND"))
		return (R_APPEND);
	if (!strcmp(s, "R_CURSOR"))
		return (R_CURSOR);
	if (!strcmp(s, "R_IAFTER"))
		return (R_IAFTER);
	if (!strcmp(s, "R_IBEFORE"))
		return (R_IBEFORE);
	if (!strcmp(s, "R_NOOVERWRITE"))
		return (R_NOOVERWRITE);
	if (!strcmp(s, "R_FIRST"))
		return (R_FIRST);
	if (!strcmp(s, "R_LAST"))
		return (R_LAST);
	if (!strcmp(s, "R_NEXT"))
		return (R_NEXT);
	if (!strcmp(s, "R_PREV"))
		return (R_PREV);
	err("line %lu: %s: unknown flag", lineno, s);
	/* NOTREACHED */
}
	
DBTYPE
dbtype(s)
	char *s;
{
	if (!strcmp(s, "btree"))
		return (DB_BTREE);
	if (!strcmp(s, "hash"))
		return (DB_HASH);
	if (!strcmp(s, "recno"))
		return (DB_RECNO);
	err("%s: unknown type (use btree, hash or recno)", s);
	/* NOTREACHED */
}

void *
setinfo(type, s)
	DBTYPE type;
	char *s;
{
	static BTREEINFO ib;
	static HASHINFO ih;
	static RECNOINFO rh;
	char *eq;

	if ((eq = index(s, '=')) == NULL)
		err("%s: illegal structure set statement", s);
	*eq++ = '\0';
	if (!isdigit(*eq))
		err("%s: structure set statement must be a number", s);
		
	switch(type) {
	case DB_BTREE:
		if (!strcmp("flags", s)) {
			ib.flags = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("cachesize", s)) {
			ib.cachesize = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("maxkeypage", s)) {
			ib.maxkeypage = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("minkeypage", s)) {
			ib.minkeypage = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("lorder", s)) {
			ib.lorder = strtoul(eq, NULL, 0);
			return (&ib);
		}
		break;
	case DB_HASH:
		if (!strcmp("bsize", s)) {
			ih.bsize = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("ffactor", s)) {
			ih.ffactor = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("nelem", s)) {
			ih.nelem = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("cachesize", s)) {
			ih.cachesize = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("lorder", s)) {
			ih.lorder = strtoul(eq, NULL, 0);
			return (&ib);
		}
		break;
	case DB_RECNO:
		if (!strcmp("flags", s)) {
			rh.flags = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("cachesize", s)) {
			rh.cachesize = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("lorder", s)) {
			rh.lorder = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("reclen", s)) {
			rh.reclen = strtoul(eq, NULL, 0);
			return (&ib);
		}
		if (!strcmp("bval", s)) {
			rh.bval = strtoul(eq, NULL, 0);
			return (&ib);
		}
		break;
	}
	err("%s: unknown structure value", s);
	/* NOTREACHED */
}

void *
rfile(name, lenp)
	char *name;
	size_t *lenp;
{
	struct stat sb;
	void *p;
	int fd;
	char *np;

	for (; isspace(*name); ++name);
	if ((np = index(name, '\n')) != NULL)
		*np = '\0';
	if ((fd = open(name, O_RDONLY, 0)) < 0 ||
	    fstat(fd, &sb))
		err("%s: %s\n", name, strerror(errno));
	if (sb.st_size > SIZE_T_MAX)
		err("%s: %s\n", name, strerror(E2BIG));
	if ((p = malloc((u_int)sb.st_size)) == NULL)
		err("%s", strerror(errno));
	(void)read(fd, p, (int)sb.st_size);
	*lenp = sb.st_size;
	return (p);
}

void *
xmalloc(text, len)
	char *text;
	size_t len;
{
	void *p;

	if ((p = malloc(len)) == NULL)
		err("%s", strerror(errno));
	bcopy(text, p, len);
	return (p);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: dbtest [-i info] [-o file] type script\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
        va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "dbtest: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}
