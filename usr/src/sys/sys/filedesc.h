/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)filedesc.h	7.2 (Berkeley) %G%
 */

#ifndef _FILEDESC_H_
#define _FILEDESC_H_

/*
 * This structure is used for the management of descriptors.
 * It may be shared by multiple threads.
 *
 * A process is initially started out with NDFILE worth of
 * descriptors, selected to be enough for typical applications
 * based on the historic limit of 20 open files. Additional
 * descriptors may be allocated up to a system defined limit
 * defined by the global variable nofile; the initial value
 * of nofile is set to NOFILE. The initial expansion is set to
 * NDEXTENT; each time it runs out it is doubled until nofile
 * is reached. NDEXTENT should be selected to be the biggest
 * multiple of OFILESIZE (see below) that will fit in a
 * power-of-two sized piece of memory.
 */
#define NDFILE		20
#define NDEXTENT	25

struct filedesc {
	struct	vnode *fd_cdir;		/* current directory */
	struct	vnode *fd_rdir;		/* root directory */
	u_short	fd_cmask;		/* mask for file creation */
	u_short	fd_refcnt;		/* reference count */
	short	fd_lastfile;		/* high-water mark of fd_ofile */
	short	fd_maxfiles;		/* maximum number of open files */
	struct	file *fd_ofile[NDFILE];	/* file structures for open files */
	struct	file **fd_moreofiles;	/* the rest of the open files */
	char	fd_ofileflags[NDFILE];	/* per-process open file flags */
	char	*fd_moreofileflags;	/* the rest of the open file flags */
	long	fd_spare;		/* unused to round up to power of two */
};

/*
 * Per-process open flags.
 */
#define	UF_EXCLOSE 	0x01		/* auto-close on exec */
#define	UF_MAPPED 	0x02		/* mapped from device */

/*
 * Data structure access macros.
 */
#if !defined(vax) && !defined(tahoe)
#define OFILE(fd, indx) ((indx) < NDFILE ? \
	(fd)->fd_ofile[indx] : \
	(fd)->fd_moreofiles[(indx) - NDFILE])
#define OFILEFLAGS(fd, indx) ((indx) < NDFILE ? \
	(fd)->fd_ofileflags[indx] : \
	(fd)->fd_moreofileflags[(indx) - NDFILE])
#define OFILESIZE (sizeof(struct file *) + sizeof(char))
#else
/* PCC cannot handle above as lvalues */
struct file **ofilefunc();
char *ofileflagsfunc();
#define OFILE(fd, indx) *ofilefunc(fd, indx)
#define OFILEFLAGS(fd, indx) *ofileflagsfunc(fd, indx)
#define OFILESIZE (sizeof(struct file *) + sizeof(char))
#endif

/*
 * Kernel global variables and routines.
 */
extern struct filedesc *fddup();
extern int nofile;

#endif /* !_FILEDESC_H_ */
