/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)strip.c	5.12 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include <limits.h>
#include <fcntl.h>
#include <errno.h>
#include <a.out.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct exec EXEC;
typedef struct nlist NLIST;

#define	strx	n_un.n_strx

void err __P((int, const char *fmt, ...));
void s_stab __P((const char *, int, EXEC *));
void s_sym __P((const char *, int, EXEC *));
void usage __P((void));

int eval;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register int fd, nb;
	EXEC head;
	void (*sfcn)__P((const char *, int, EXEC *));
	int ch;
	char *fn;

	sfcn = s_sym;
	while ((ch = getopt(argc, argv, "d")) != EOF)
		switch(ch) {
		case 'd':
			sfcn = s_stab;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	while (fn = *argv++) {
		if ((fd = open(fn, O_RDWR)) < 0 ||
		    (nb = read(fd, &head, sizeof(EXEC))) == -1) {
			err(0, "%s: %s", fn, strerror(errno));
			continue;
		}
		if (nb != sizeof(EXEC) || N_BADMAG(head)) {
			err(0, "%s: %s", fn, strerror(EFTYPE));
			continue;
		}
		sfcn(fn, fd, &head);
		if (close(fd))
			err(0, "%s: %s", fn, strerror(errno));
	}
	exit(eval);
}

void
s_sym(fn, fd, ep)
	const char *fn;
	int fd;
	register EXEC *ep;
{
	static int pagesize = -1;
	register off_t fsize;

	/* If no symbols or data/text relocation info, quit. */
	if (!ep->a_syms && !ep->a_trsize && !ep->a_drsize)
		return;

	/*
	 * New file size is the header plus text and data segments; OMAGIC
	 * and NMAGIC formats have the text/data immediately following the
	 * header.  ZMAGIC format wastes the rest of of header page.
	 */
	if (ep->a_magic == ZMAGIC)
		fsize = pagesize == -1 ? (pagesize = getpagesize()) : pagesize;
	else
		fsize = sizeof(EXEC);
	fsize += ep->a_text + ep->a_data;

	/* Set symbol size and relocation info values to 0. */
	ep->a_syms = ep->a_trsize = ep->a_drsize = 0;

	/* Rewrite the header and truncate the file. */
	if (lseek(fd, (off_t)0, SEEK_SET) == -1 ||
	    write(fd, ep, sizeof(EXEC)) != sizeof(EXEC) ||
	    ftruncate(fd, fsize))
		err(0, "%s: %s", fn, strerror(errno)); 
}

void
s_stab(fn, fd, ep)
	const char *fn;
	int fd;
	EXEC *ep;
{
	register int cnt, len;
	register char *nstr, *nstrbase, *p, *strbase;
	register NLIST *sym, *nsym;
	struct stat sb;
	NLIST *symbase;

	/* Quit if no symbols. */
	if (ep->a_syms == 0)
		return;

	/* Stat the file. */
	if (fstat(fd, &sb) < 0) {
		err(0, "%s: %s", fn, strerror(errno));
		return;
	}

	/* Check size. */
	if (sb.st_size > SIZE_T_MAX) {
		err(0, "%s: %s", fn, strerror(EFBIG));
		return;
	}

	/* Map the file. */
	if ((ep = (EXEC *)mmap(NULL, (size_t)sb.st_size,
	    PROT_READ | PROT_WRITE, MAP_SHARED, fd, (off_t)0)) == (EXEC *)-1) {
		err(0, "%s: %s", fn, strerror(errno));
		return;
	}

	/*
	 * Initialize old and new symbol pointers.  They both point to the
	 * beginning of the symbol table in memory, since we're deleting
	 * entries.
	 */
	sym = nsym = symbase = (NLIST *)((char *)ep + N_SYMOFF(*ep));

	/*
	 * Allocate space for the new string table, initialize old and
	 * new string pointers.  Handle the extra long at the beginning
	 * of the string table.
	 */
	strbase = (char *)ep + N_STROFF(*ep);
	if ((nstrbase = malloc((u_int)*(u_long *)strbase)) == NULL)
		err(1, "%s", strerror(errno));
	nstr = nstrbase + sizeof(u_long);

	/*
	 * Read through the symbol table.  For each non-debugging symbol,
	 * copy it and save its string in the new string table.  Keep
	 * track of the number of symbols.
	 */
	for (cnt = ep->a_syms / sizeof(NLIST); cnt--; ++sym)
		if (!(sym->n_type & N_STAB) && sym->strx) {
			*nsym = *sym;
			nsym->strx = nstr - nstrbase;
			p = strbase + sym->strx;
			len = strlen(p) + 1;
			bcopy(p, nstr, len);
			nstr += len;
			++nsym;
		}

	/* Fill in new symbol table size. */
	ep->a_syms = (nsym - symbase) * sizeof(NLIST);

	/* Fill in the new size of the string table. */
	*(u_long *)nstrbase = len = nstr - nstrbase;

	/*
	 * Copy the new string table into place.  Nsym should be pointing
	 * at the address past the last symbol entry.
	 */
	bcopy(nstrbase, (void *)nsym, len);

	/* Truncate to the current length. */
	if (ftruncate(fd, (char *)nsym + len - (char *)ep))
		err(0, "%s: %s", fn, strerror(errno));
	munmap((caddr_t)ep, (size_t)sb.st_size);
}

void
usage()
{
	(void)fprintf(stderr, "usage: strip [-d] file ...\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(int fatal, const char *fmt, ...)
#else
err(fatal, fmt, va_alist)
	int fatal;
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
	(void)fprintf(stderr, "strip: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	if (fatal)
		exit(1);
	eval = 1;
}
