/*-
 * Copyright (c) 1992 Keith Muller.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tar.h	8.1 (Berkeley) %G%
 */

/*
 * defines and data structures common to all tar formats
 */
#define CHK_LEN		8		/* length of checksum field */
#define TNMSZ		100		/* size of name field */
#ifdef _PAX_
#define NULLCNT		2		/* number of null blocks in trailer */
#define CHK_OFFSET	148		/* start of chksum field */
#define BLNKSUM		256L		/* sum of checksum field using ' ' */
#endif /* _PAX_ */

/*
 * Values used in typeflag field in all tar formats
 * (only REGTYPE, LNKTYPE and SYMTYPE are used in old bsd tar headers)
 */
#define	REGTYPE		'0'		/* Regular File */
#define	AREGTYPE	'\0'		/* Regular File */
#define	LNKTYPE		'1'		/* Link */
#define	SYMTYPE		'2'		/* Symlink */
#define	CHRTYPE		'3'		/* Character Special File */
#define	BLKTYPE		'4'		/* Block Special File */
#define	DIRTYPE		'5'		/* Directory */
#define	FIFOTYPE	'6'		/* FIFO */
#define	CONTTYPE	'7'		/* high perf file */

/*
 * Mode field encoding of the different file types - values in octal
 */
#define TSUID		04000		/* Set UID on execution */
#define TSGID		02000		/* Set GID on execution */
#define TSVTX		01000		/* Reserved */
#define TUREAD		00400		/* Read by owner */
#define TUWRITE		00200		/* Write by owner */
#define TUEXEC		00100		/* Execute/Search by owner */
#define TGREAD		00040		/* Read by group */
#define TGWRITE		00020		/* Write by group */
#define TGEXEC		00010		/* Execute/Search by group */
#define TOREAD		00004		/* Read by other */
#define TOWRITE		00002		/* Write by other */
#define TOEXEC		00001		/* Execute/Search by other */

#ifdef _PAX_
/*
 * Pad with a bit mask, much faster than doing a mod but only works on powers
 * of 2. Macro below is for block of 512 bytes.
 */
#define TAR_PAD(x)	((512 - ((x) & 511)) & 511)
#endif /* _PAX_ */

/*
 * structure of an old tar header as it appeared in BSD releases
 */
typedef struct {
	char name[TNMSZ];		/* name of entry */
	char mode[8]; 			/* mode */
	char uid[8]; 			/* uid */
	char gid[8];			/* gid */
	char size[12];			/* size */
	char mtime[12];			/* modification time */
	char chksum[CHK_LEN];		/* checksum */
	char linkflag;			/* norm, hard, or sym. */
	char linkname[TNMSZ];		/* linked to name */
} HD_TAR;

#ifdef _PAX_
/*
 * -o options for BSD tar to not write directories to the archive
 */
#define TAR_NODIR	"nodir"
#define TAR_OPTION	"write_opt"
#endif /* _PAX_ */

/*
 * Data Interchange Format - Extended tar header format - POSIX 1003.1-1990
 */
#define TPFSZ		155
#define	TMAGIC		"ustar"		/* ustar and a null */
#define	TMAGLEN		6
#define	TVERSION	"00"		/* 00 and no null */
#define	TVERSLEN	2

typedef struct {
	char name[TNMSZ];		/* name of entry */
	char mode[8]; 			/* mode */
	char uid[8]; 			/* uid */
	char gid[8];			/* gid */
	char size[12];			/* size */
	char mtime[12];			/* modification time */
	char chksum[CHK_LEN];		/* checksum */
	char typeflag;			/* type of file. */
	char linkname[TNMSZ];		/* linked to name */
	char magic[TMAGLEN];		/* magic cookie */
	char version[TVERSLEN];		/* version */
	char uname[32];			/* ascii owner name */
	char gname[32];			/* ascii group name */
	char devmajor[8];		/* major device number */
	char devminor[8];		/* minor device number */
	char prefix[TPFSZ];		/* linked to name */
} HD_USTAR;
