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
static char sccsid[] = "@(#)fstab.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <fstab.h>
#include <stdio.h>

static	struct fstab fs;
static	FILE *fs_file = NULL;
static	char line[BUFSIZ+1];

static
fstabscan(fsp)
	register struct fstab *fsp;
{
	register char *cp;
	char *fgets(), *strtok();

	for (;;) {
		if (!(cp = fgets(line, sizeof(line), fs_file)))
			return(1);
		fsp->fs_spec = strtok(cp, ":\n");
		fsp->fs_file = strtok((char *)NULL, ":\n");
		fsp->fs_type = strtok((char *)NULL, ":\n");
		if (fsp->fs_type && strcmp(fsp->fs_type, FSTAB_XX)) {
			if (!(cp = strtok((char *)NULL, ":\n")))
				continue;
			fsp->fs_freq = atoi(cp);
			if (!(cp = strtok((char *)NULL, ":\n")))
				continue;
			fsp->fs_passno = atoi(cp);
			return(0);
		}
	}
	/* NOTREACHED */
}

setfsent()
{
	if (fs_file)
		(void)endfsent();
	if ((fs_file = fopen(FSTAB, "r")) == NULL) {
		fs_file = NULL;
		return(0);
	}
	return(1);
}

endfsent()
{
	if (fs_file) {
		(void)fclose(fs_file);
		fs_file = NULL;
	}
	return(1);
}

struct fstab *
getfsent()
{
	if (fs_file == NULL && !setfsent() || fstabscan(&fs))
		return((struct fstab *)NULL);
	return(&fs);
}

struct fstab *
getfsspec(name)
	register char *name;
{
	register struct fstab *fsp;

	if (!setfsent())		/* start from the beginning */
		return((struct fstab *)NULL);
	while (fsp = getfsent())
		if (!strcmp(fsp->fs_spec, name))
			return(fsp);
	return((struct fstab *)NULL);
}

struct fstab *
getfsfile(name)
	register char *name;
{
	register struct fstab *fsp;

	if (!setfsent())		/* start from the beginning */
		return((struct fstab *)NULL);
	while (fsp = getfsent())
		if (!strcmp(fsp->fs_file, name))
			return(fsp);
	return((struct fstab *)NULL);
}
