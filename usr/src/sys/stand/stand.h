/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stand.h	7.1 (Berkeley) %G%
 */

#include <sys/types.h>
#include <sys/cdefs.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <stand/saioctl.h>

#define	UNIX	"/vmunix"

#ifndef NULL
#define	NULL	0
#endif

extern int errno;

struct open_file;

/*
 * This structure is used to define file system operations in a file system
 * independent way.
 */
struct fs_ops {
	int	(*open) __P((char *path, struct open_file *f));
	int	(*close) __P((struct open_file *f));
	int	(*read) __P((struct open_file *f, char *buf,
			u_int size, u_int *resid));
	int	(*write) __P((struct open_file *f, char *buf,
			u_int size, u_int *resid));
	off_t	(*seek) __P((struct open_file *f, off_t offset, int where));
	int	(*stat) __P((struct open_file *f, struct stat *sb));
};

extern struct fs_ops file_system[];

/* where values for lseek(2) */
#define	SEEK_SET	0	/* set file offset to offset */
#define	SEEK_CUR	1	/* set file offset to current plus offset */
#define	SEEK_END	2	/* set file offset to EOF plus offset */

/* Device switch */
struct devsw {
	char	*dv_name;
	int	(*dv_strategy) __P((void *devdata, int rw,
			daddr_t blk, u_int size, char *buf, u_int *rsize));
	int	(*dv_open)();	/* (struct open_file *f, ...) */
	int	(*dv_close) __P((struct open_file *f));
	int	(*dv_ioctl) __P((struct open_file *f, int cmd, void *data));
};

extern struct devsw devsw[];	/* device array */
extern int ndevs;		/* number of elements in devsw[] */

struct open_file {
	int		f_flags;	/* see F_* below */
	struct devsw	*f_dev;		/* pointer to device operations */
	void		*f_devdata;	/* device specific data */
	struct fs_ops	*f_ops;		/* pointer to file system operations */
	void		*f_fsdata;	/* file system specific data */
};

#define	SOPEN_MAX	4
extern struct open_file files[SOPEN_MAX];

/* f_flags values */
#define	F_READ		0x0001	/* file opened for reading */
#define	F_WRITE		0x0002	/* file opened for writing */
#define	F_RAW		0x0004	/* raw device open - no file system */

int	devopen __P((struct open_file *f, char *fname, char **file));
void	*alloc __P((unsigned size));
void	free __P((void *ptr, unsigned size));
struct	disklabel;
char	*getdisklabel __P((const char *buf, struct disklabel *lp));
