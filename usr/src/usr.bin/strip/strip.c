/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)strip.c	5.7 (Berkeley) 5/26/91";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <a.out.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct exec EXEC;
typedef struct nlist NLIST;

void err __P((const char *fmt, ...));
void s_stab __P((const char *, int, EXEC *));
void s_sym __P((const char *, int, EXEC *));
void usage __P((void));

int eval;

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
			err("%s: %s", fn, strerror(errno));
			continue;
		}
		if (nb != sizeof(EXEC) || N_BADMAG(head)) {
			err("%s: %s", fn, strerror(EFTYPE));
			continue;
		}
		sfcn(fn, fd, &head);
		if (close(fd))
			err("%s: %s", fn, strerror(errno));
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
	if (lseek(fd, 0L, SEEK_SET) == -1 ||
	    write(fd, ep, sizeof(EXEC)) != sizeof(EXEC) ||
	    ftruncate(fd, fsize))
		err("%s: %s", fn, strerror(errno)); 
}

void
s_stab(fn, fd, ep)
	const char *fn;
	int fd;
	EXEC *ep;
{
	struct stat sb;
	register NLIST *bsym2, *sym1, *sym2;
	register u_long nsym1, nsym2;
	register char *p, *bstr2, *str1, *str2;
	register int len, symlen;
	off_t fsize;
	int nb;
	char *bp;

	/* Quit if no symbols. */
	if (ep->a_syms == 0)
		return;

	bsym2 = NULL;
	bp = bstr2 = NULL;

	/* Read the file into memory.			XXX mmap */
	if (fstat(fd, &sb))
		goto syserr;
	if ((bp = malloc(sb.st_size)) == NULL)
		goto syserr;
	if (lseek(fd, 0L, SEEK_SET) == -1)
		goto syserr;
	if ((nb = read(fd, bp, (int)sb.st_size)) == -1)
		goto syserr;
	if (nb != sb.st_size) {
		errno = EIO;
		goto syserr;
	}

	/*
	 * Allocate space for new symbol and string tables.  Allocate before
	 * reading the symbol tables so we can do it all in a single pass.
	 * This loses if there weren't any symbols to strip, but that's life.
	 */
	sym1 = (NLIST *)(bp + N_SYMOFF(*ep));
	if ((bsym2 = sym2 = malloc((u_int)ep->a_syms)) == NULL) {
		err("%s", strerror(errno));
		goto mem;
	}
	str1 = bp + N_STROFF(*ep);
	if ((bstr2 = malloc((u_int)*(u_long *)str1)) == NULL) {
		err("%s", strerror(errno));
		goto mem;
	}
	str2 = bstr2 + sizeof(u_long);
	symlen = sizeof(u_long);

	/*
	 * Read through the symbol table.  For each non-debugging symbol,
	 * copy it into the new symbol and string tables.  Keep track of
	 * how many symbols are copied and the length of the new string
	 * table.
	 */
#define	strx	n_un.n_strx
	nsym2 = 0;
	for (nsym1 = ep->a_syms / sizeof(NLIST); nsym1--; ++sym1)
		if (!(sym1->n_type & N_STAB) && sym1->strx) {
			*sym2 = *sym1;
			sym2->strx = str2 - bstr2;
			p = str1 + sym1->strx;
			len = strlen(p) + 1;
			bcopy(p, str2, len);
			symlen += len;
			str2 += len;
			++sym2;
			++nsym2;
		}

	/* If no debugging symbols, quit. */
	if (!nsym2)
		goto mem;

	/* Fill in new symbol table size. */
	ep->a_syms = nsym2 * sizeof(NLIST);

	/* Write out the header. */
	if (lseek(fd, 0L, SEEK_SET) == -1 ||
	    write(fd, ep, sizeof(EXEC)) != sizeof(EXEC))
		goto syserr;

	/* Write out the symbol table. */
	if (lseek(fd, N_SYMOFF(*ep), SEEK_SET) == -1 ||
	    write(fd, bsym2, ep->a_syms) != ep->a_syms)
		goto syserr;

	/* Fill in the new size of the string table. */
	*(u_long *)bstr2 = symlen;

	/* Write out the string table. */
	if (write(fd, bstr2, symlen) != symlen)
		goto syserr;

	/* Truncate to the current length. */
	if ((fsize = lseek(fd, 0L, SEEK_CUR)) == -1)
		goto syserr;
	if (ftruncate(fd, fsize))
syserr:		err("%s: %s", fn, strerror(errno));

mem:	if (bp)
		free(bp);
	if (bstr2)
		free(bstr2);
	if (bsym2)
		free(bsym2);
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
	(void)fprintf(stderr, "strip: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	eval = 1;
}
