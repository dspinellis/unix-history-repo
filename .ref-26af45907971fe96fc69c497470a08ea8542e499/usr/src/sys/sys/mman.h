/*-
 * Copyright (c) 1982, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mman.h	8.1 (Berkeley) %G%
 */

/*
 * Protections are chosen from these bits, or-ed together
 */
#define	PROT_READ	0x01	/* pages can be read */
#define	PROT_WRITE	0x02	/* pages can be written */
#define	PROT_EXEC	0x04	/* pages can be executed */

/*
 * Flags contain sharing type and options.
 * Sharing types; choose one.
 */
#define	MAP_SHARED	0x0001	/* share changes */
#define	MAP_PRIVATE	0x0002	/* changes are private */
#define	MAP_COPY	0x0004	/* "copy" region at mmap time */

/*
 * Other flags
 */
#define	MAP_FIXED	 0x0010	/* map addr must be exactly as requested */
#define	MAP_RENAME	 0x0020	/* Sun: rename private pages to file */
#define	MAP_NORESERVE	 0x0040	/* Sun: don't reserve needed swap area */
#define	MAP_INHERIT	 0x0080	/* region is retained after exec */
#define	MAP_NOEXTEND	 0x0100	/* for MAP_FILE, don't change file size */
#define	MAP_HASSEMAPHORE 0x0200	/* region may contain semaphores */

/*
 * Mapping type; default is map from file.
 */
#define	MAP_ANON	0x1000	/* allocated from memory, swap space */

/*
 * Advice to madvise
 */
#define	MADV_NORMAL	0	/* no further special treatment */
#define	MADV_RANDOM	1	/* expect random page references */
#define	MADV_SEQUENTIAL	2	/* expect sequential page references */
#define	MADV_WILLNEED	3	/* will need these pages */
#define	MADV_DONTNEED	4	/* dont need these pages */

#ifndef KERNEL

#include <sys/cdefs.h>

__BEGIN_DECLS
/* Some of these int's should probably be size_t's */
caddr_t	mmap __P((caddr_t, size_t, int, int, int, off_t));
int	mprotect __P((caddr_t, size_t, int));
int	munmap __P((caddr_t, size_t));
int	msync __P((caddr_t, size_t));
int	mlock __P((caddr_t, size_t));
int	munlock __P((caddr_t, size_t));
__END_DECLS

#endif /* !KERNEL */
