/*
 * Copyright (c) 1980, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fstab.c	5.11 (Berkeley) 6/1/90";
#endif /* LIBC_SCCS and not lint */

#include <fstab.h>
#include <unistd.h>
#include <stdio.h>

static FILE *_fs_fp;
static struct fstab _fs_fstab;

static
fstabscan()
{
	register char *cp;
#define	MAXLINELENGTH	1024
	static char line[MAXLINELENGTH];
	char subline[MAXLINELENGTH];
	char *fgets(), *strtok();
	int typexx;

	for (;;) {
		if (!(cp = fgets(line, sizeof(line), _fs_fp)))
			return(0);
		_fs_fstab.fs_spec = strtok(cp, " \t\n");
		if (!_fs_fstab.fs_spec || *_fs_fstab.fs_spec == '#')
			continue;
		_fs_fstab.fs_file = strtok((char *)NULL, " \t\n");
		_fs_fstab.fs_vfstype = strtok((char *)NULL, " \t\n");
		_fs_fstab.fs_mntops = strtok((char *)NULL, " \t\n");
		if (_fs_fstab.fs_mntops == NULL)
			goto bad;
		_fs_fstab.fs_freq = 0;
		_fs_fstab.fs_passno = 0;
		if ((cp = strtok((char *)NULL, " \t\n")) != NULL) {
			_fs_fstab.fs_freq = atoi(cp);
			if ((cp = strtok((char *)NULL, " \t\n")) != NULL)
				_fs_fstab.fs_passno = atoi(cp);
		}
		strcpy(subline, _fs_fstab.fs_mntops);
		for (typexx = 0, cp = strtok(subline, ","); cp;
		     cp = strtok((char *)NULL, ",")) {
			if (strlen(cp) != 2)
				continue;
			if (!strcmp(cp, FSTAB_RW)) {
				_fs_fstab.fs_type = FSTAB_RW;
				break;
			}
			if (!strcmp(cp, FSTAB_RQ)) {
				_fs_fstab.fs_type = FSTAB_RQ;
				break;
			}
			if (!strcmp(cp, FSTAB_RO)) {
				_fs_fstab.fs_type = FSTAB_RO;
				break;
			}
			if (!strcmp(cp, FSTAB_SW)) {
				_fs_fstab.fs_type = FSTAB_SW;
				break;
			}
			if (!strcmp(cp, FSTAB_XX)) {
				_fs_fstab.fs_type = FSTAB_XX;
				typexx++;
				break;
			}
		}
		if (typexx)
			continue;
		if (cp != NULL)
			return(1);
	bad:
		/* no way to distinguish between EOF and syntax error */
		(void)write(STDERR_FILENO, "fstab: ", 7);
		(void)write(STDERR_FILENO, _PATH_FSTAB,
		    sizeof(_PATH_FSTAB) - 1);
		(void)write(STDERR_FILENO, ": syntax error.\n", 16);
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
	return((_fs_fp = fopen(_PATH_FSTAB, "r")) != NULL);
}

void
endfsent()
{
	if (_fs_fp) {
		(void)fclose(_fs_fp);
		_fs_fp = NULL;
	}
}
