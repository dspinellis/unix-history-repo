/*
 * Copyright (c) 1980, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fstab.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <fstab.h>
#include <stdio.h>

static FILE *_fs_fp;
static struct fstab _fs_fstab;

static
fstabscan()
{
	register char *cp;
#define	MAXLINELENGTH	100
	static char line[MAXLINELENGTH];
	char *fgets(), *strsep();

	for (;;) {
		if (!(cp = fgets(line, sizeof(line), _fs_fp)))
			return(0);
		_fs_fstab.fs_spec = strsep(cp, ":\n");
		_fs_fstab.fs_file = strsep((char *)NULL, ":\n");
		_fs_fstab.fs_type = strsep((char *)NULL, ":\n");
		if (_fs_fstab.fs_type && strcmp(_fs_fstab.fs_type, FSTAB_XX)) {
			if (!(cp = strsep((char *)NULL, ":\n")))
				continue;
			_fs_fstab.fs_freq = atoi(cp);
			if (!(cp = strsep((char *)NULL, ":\n")))
				continue;
			_fs_fstab.fs_passno = atoi(cp);
			return(1);
		}
	}
	/* NOTREACHED */
}

struct fstab *
getfsent()
{
	if (!_fs_fp && !setfsent() || !fstabscan())
		return((struct fstab *)NULL);
	return(&_fs_fstab);
}

struct fstab *
getfsspec(name)
	register char *name;
{
	if (setfsent())
		while (fstabscan())
			if (!strcmp(_fs_fstab.fs_spec, name))
				return(&_fs_fstab);
	return((struct fstab *)NULL);
}

struct fstab *
getfsfile(name)
	register char *name;
{
	if (setfsent())
		while (fstabscan())
			if (!strcmp(_fs_fstab.fs_file, name))
				return(&_fs_fstab);
	return((struct fstab *)NULL);
}

setfsent()
{
	if (_fs_fp) {
		rewind(_fs_fp);
		return(1);
	}
	return((_fs_fp = fopen(FSTAB, "r")) != NULL);
}

void
endfsent()
{
	if (_fs_fp) {
		(void)fclose(_fs_fp);
		_fs_fp = NULL;
	}
}
