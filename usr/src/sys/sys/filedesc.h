/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)filedesc.h	7.3 (Berkeley) %G%
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
 * descriptors may be allocated up to a process' resource limit.
 * The initial expansion is set to NOEXTENT; each time it runs out,
 * it is doubled until the resource limit is reached. NOEXTENT should
 * be selected to be the biggest multiple of OFILESIZE (see below)
 * that will fit in a power-of-two sized piece of memory.
 */
#define NOEXTENT	25		/* 125 bytes in 128-byte alloc. */ 

struct filedesc {
	struct	file **fd_ofiles;	/* file structures for open files */
	char	*fd_ofileflags;		/* per-process open file flags */
	struct	vnode *fd_cdir;		/* current directory */
	struct	vnode *fd_rdir;		/* root directory */
	int	fd_nfiles;		/* number of open files allocated */
	int	fd_lastfile;		/* high-water mark of fd_ofiles */
	int	fd_freefile;		/* approx. next free file */
	u_short	fd_cmask;		/* mask for file creation */
	u_short	fd_refcnt;		/* reference count */
};

/*
 * Per-process open flags.
 */
#define	UF_EXCLOSE 	0x01		/* auto-close on exec */
#define	UF_MAPPED 	0x02		/* mapped from device */

/*
 * Data structure access macros.
 */
#define OFILE(fd, indx)	((fd)->fd_ofiles[indx])
#define OFILEFLAGS(fd, indx) ((fd)->fd_ofileflags[indx])
#define OFILESIZE (sizeof(struct file *) + sizeof(char))

/*
 * Kernel global variables and routines.
 */
extern struct filedesc *fdcopy();

#endif /* !_FILEDESC_H_ */
