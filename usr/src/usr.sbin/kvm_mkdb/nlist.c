/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)nlist.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <fcntl.h>
#include <limits.h>
#include <ndbm.h>
#include <a.out.h>
#include <errno.h>
#include <unistd.h>
#include <kvm.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct nlist NLIST;
#define	_strx	n_un.n_strx
#define	_name	n_un.n_name

static char *kfile;

create_knlist(name, db)
	char *name;
	DBM *db;
{
	register int nsyms;
	struct exec ebuf;
	FILE *fp;
	NLIST nbuf;
	datum key, data;
	int fd, nr, strsize;
	char *strtab, buf[1024];

	kfile = name;
	if ((fd = open(name, O_RDONLY, 0)) < 0)
		error(name);

	/* Read in exec structure. */
	nr = read(fd, (char *)&ebuf, sizeof(struct exec));
	if (nr != sizeof(struct exec))
		badfmt(nr, "no exec header");

	/* Check magic number and symbol count. */
	if (N_BADMAG(ebuf))
		badfmt("bad magic number");
	if (!ebuf.a_syms)
		badfmt("stripped");

	/* Seek to string table. */
	if (lseek(fd, N_STROFF(ebuf), SEEK_SET) == -1)
		badfmt("corrupted string table");

	/* Read in the size of the symbol table. */
	nr = read(fd, (char *)&strsize, sizeof(strsize));
	if (nr != sizeof(strsize))
		badread(nr, "no symbol table");

	/* Read in the string table. */
	strsize -= sizeof(strsize);
	if (!(strtab = (char *)malloc(strsize)))
		error(name);
	if ((nr = read(fd, strtab, strsize)) != strsize)
		badread(nr, "corrupted symbol table");

	/* Seek to symbol table. */
	if (!(fp = fdopen(fd, "r")))
		error(name);
	if (fseek(fp, N_SYMOFF(ebuf), SEEK_SET) == -1)
		error(name);
	
	data.dptr = (char *)&nbuf;
	data.dsize = sizeof(NLIST);

	/* Read each symbol and enter it into the database. */
	nsyms = ebuf.a_syms / sizeof(struct nlist);
	while (nsyms--) {
		if (fread((char *)&nbuf, sizeof (NLIST), 1, fp) != 1) {
			if (feof(fp))
				badfmt("corrupted symbol table");
			error(name);
		}
		if (!nbuf._strx || nbuf.n_type&N_STAB)
			continue;

		key.dptr = strtab + nbuf._strx - sizeof(long);
		key.dsize = strlen(key.dptr);
		if (dbm_store(db, key, data, DBM_INSERT) < 0)
			error("dbm_store");

		if (!strncmp(key.dptr, VRS_SYM, sizeof(VRS_SYM) - 1)) {
			off_t cur_off, rel_off, vers_off;

			/* Offset relative to start of text image in VM. */
#ifdef hp300
			rel_off = nbuf.n_value;
#endif
#ifdef tahoe
			/*
			 * On tahoe, first 0x800 is reserved for communication
			 * with the console processor.
			 */
			rel_off = ((nbuf.n_value & ~KERNBASE) - 0x800);
#endif
#ifdef vax
			rel_off = nbuf.n_value & ~KERNBASE;
#endif
			/*
			 * When loaded, data is rounded to next page cluster
			 * after text, but not in file.
			 */
			rel_off -= CLBYTES - (ebuf.a_text % CLBYTES);
			vers_off = N_TXTOFF(ebuf) + rel_off;

			cur_off = ftell(fp);
			if (fseek(fp, vers_off, SEEK_SET) == -1)
				badfmt("corrupted string table");

			/*
			 * Read version string up to, and including newline.
			 * This code assumes that a newline terminates the
			 * version line.
			 */
			if (fgets(buf, sizeof(buf), fp) == NULL)
				badfmt("corrupted string table");

			key.dptr = VRS_KEY;
			key.dsize = sizeof(VRS_KEY) - 1;
			data.dptr = buf;
			data.dsize = strlen(buf);
			if (dbm_store(db, key, data, DBM_INSERT) < 0)
				error("dbm_store");

			/* Restore to original values. */
			data.dptr = (char *)&nbuf;
			data.dsize = sizeof(NLIST);
			if (fseek(fp, cur_off, SEEK_SET) == -1)
				badfmt("corrupted string table");
		}
	}
	(void)fclose(fp);
}

badread(nr, p)
	int nr;
	char *p;
{
	if (nr < 0)
		error(kfile);
	badfmt(p);
}

badfmt(p)
	char *p;
{
	(void)fprintf(stderr,
	    "symorder: %s: %s: %s\n", kfile, p, strerror(EFTYPE));
	exit(1);
}
