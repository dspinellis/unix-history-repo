/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs.h	8.1 (Berkeley) %G%
 */

int	ufs_open __P((char *path, struct open_file *f));
int	ufs_close __P((struct open_file *f));
int	ufs_read __P((struct open_file *f, char *buf,
		u_int size, u_int *resid));
int	ufs_write __P((struct open_file *f, char *buf,
		u_int size, u_int *resid));
off_t	ufs_seek __P((struct open_file *f, off_t offset, int where));
int	ufs_stat __P((struct open_file *f, struct stat *sb));
