/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego and Lance
 * Visser of Convex Computer Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dd.h	8.2 (Berkeley) %G%
 */

/* Input/output stream state. */
typedef struct {
	u_char	*db;			/* buffer address */
	u_char	*dbp;			/* current buffer I/O address */
	u_long	dbcnt;			/* current buffer byte count */
	int	dbrcnt;			/* last read byte count */
	u_long	dbsz;			/* buffer size */

#define	ISCHR		0x01		/* character device (warn on short) */
#define	ISPIPE		0x02		/* pipe (not truncatable) */
#define	ISTAPE		0x04		/* tape (not seekable) */
#define	NOREAD		0x08		/* not readable */
	u_int	flags;

	char 	*name;			/* name */
	int	fd;			/* file descriptor */
	u_long	offset;			/* # of blocks to skip */

	u_long	f_stats;		/* # of full blocks processed */
	u_long	p_stats;		/* # of partial blocks processed */
	u_long	s_stats;		/* # of odd swab blocks */
	u_long	t_stats;		/* # of truncations */
} IO;

typedef struct {
	u_long	in_full;		/* # of full input blocks */
	u_long	in_part;		/* # of partial input blocks */
	u_long	out_full;		/* # of full output blocks */
	u_long	out_part;		/* # of partial output blocks */
	u_long	trunc;			/* # of truncated records */
	u_long	swab;			/* # of odd-length swab blocks */
	u_long	bytes;			/* # of bytes written */
	time_t	start;			/* start time of dd */
} STAT;

/* Flags (in ddflags). */
#define	C_ASCII		0x00001
#define	C_BLOCK		0x00002
#define	C_BS		0x00004
#define	C_CBS		0x00008
#define	C_COUNT		0x00010
#define	C_EBCDIC	0x00020
#define	C_FILES		0x00040
#define	C_IBS		0x00080
#define	C_IF		0x00100
#define	C_LCASE		0x00200
#define	C_NOERROR	0x00400
#define	C_NOTRUNC	0x00800
#define	C_OBS		0x01000
#define	C_OF		0x02000
#define	C_SEEK		0x04000
#define	C_SKIP		0x08000
#define	C_SWAB		0x10000
#define	C_SYNC		0x20000
#define	C_UCASE		0x40000
#define	C_UNBLOCK	0x80000
#define	C_OSYNC		0x100000
