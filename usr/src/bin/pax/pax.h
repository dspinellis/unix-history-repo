/*-
 * Copyright (c) 1992 Keith Muller.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pax.h	1.1 (Berkeley) %G%
 */

/*
 * BSD PAX global data structures and constants.
 */

#define	MAXBLK		32256	/* MAX blocksize supported (posix SPEC) */
				/* WARNING: increasing MAXBLK past 32256 */
				/* will violate posix spec. */
#define BLKMULT		512	/* blocksize must be even mult of 512 bytes */
				/* Don't even think of changing this */
#define DEVBLK		8192	/* default read blksize for devices */
#define FILEBLK		10240	/* default read blksize for files */
#define PAXPATHLEN	3072	/* maximium path length for pax. MUST be */
				/* longer than the system max */

/*
 * Pax modes of operation
 */
#define	LIST		0	/* List the file in an archive */
#define	EXTRACT		1	/* extract the files in an archive */
#define ARCHIVE		2	/* write a new archive */
#define APPND		3	/* append to the end of an archive */
#define	COPY		4	/* copy files to destination dir */
#define DEFOP		LIST	/* if no flags default is to LIST */

/*
 * Device type of the current archive volume 
 */
#define ISREG		0	/* regular file */
#define ISCHR		1	/* character device */
#define ISBLK		2	/* block device */
#define ISTAPE		3	/* tape drive */
#define ISPIPE		4	/* pipe/socket */

/*
 * Format Specific Routine Table
 *
 * The format specific routine table allows new archive formats to be quickly
 * added. Overall pax operation is independent of the actual format used to
 * form the archive. Only those routines which deal directly with the archive 
 * are tailored to the oddities of the specifc format. All other routines are
 * independent of the archive format. Data flow in and out of the format
 * dependnent routines pass pointers to ARCHD structure (described below).
 */
typedef struct {
	char *name;		/* name of format, this is the name the user */
				/* gives to -x to select it. */
	int bsz;		/* default block size. used when the user */
				/* does not specify a blocksize for writing */
				/* Appends continue to with the blocksize */
				/* the archive is currently using.*/
	int hsz;		/* Header size in bytes. this is the size of */
				/* the smallest header this format supports. */
				/* Headers are assumed to fit in a BLKMULT. */
				/* If they are bigger, get_head() and */
				/* get_arc() must be adjusted */
	int udev;		/* does append require unique dev/ino. some */
				/* formats use the device and inode fields */
				/* to specify hard links. when members in */
				/* the archive have the same inode/dev they */
				/* are assumed to be hard links. During */
				/* append we may have to generate unique ids */
				/* to avoid creating incorrect links */
	int hlk;		/* does archive store hard links info? if */
				/* not we do not bother to look for them */
				/* during write operations */
	int blkalgn;		/* writes must be aligned to blkalgn boundry */
	int inhead;		/* is the trailer encoded in a valid header? */
				/* if not, trailers are assumed to be */
				/* invalid headers */
	int (*id)();		/* checks if a buffer is a valid header */
				/* returns 1 if it is, o.w. returns a 0 */
	int (*st_rd)();		/* initialize routine for read. so format */
				/* can set up tables etc before it starts */
				/* reading */
	int (*rd)();		/* read header routine. passed a pointer to */
				/* ARCHD. It must extract the info from the */
				/* format and store it in the ARCHD struct. */
				/* 0 is returned when a valid header is */
				/* found. -1 when not valid. This routine */
				/* set the skip and pad fields so the format */
				/* independent routines know the amount of */
				/* padding and the number of bytes to get to */
				/* the next file header */
	off_t (*end_rd)();	/* read is over. Allows format to clean up */
				/* and MUST return the length of the trailer */
				/* record (so append knows how many bytes */
				/* to move back to rewrite the trailer */
	int (*st_wr)();		/* initialize routine for write operations */
	int (*wr)();		/* write archive header. Passed an ARCHD */
				/* filled with the specs on the next file to */
				/* archived. Returns a 1 if no file data is */
				/* is to be stored; 0 if file data is to be */
				/* added. A -1 is returned if a write */
				/* operation to the archive failed. this */
				/* function sets the skip and pad fields so */
				/* the proper padding can be added after */
				/* file data. This routine must NEVER write */
				/* a flawed archive header. */
	int (*end_wr)();	/* end write. write the trailer and do any */
				/* other format specific functions needed */
				/* at the ecnd of a archive write */
	int (*trail)();		/* returns 0 if a valid trailer, -1 if not */
				/* For formats which encode the trailer */
				/* outside of a valid header, a return value */
				/* of 1 indicates that the block passed to */
				/* it can never contain a valid header (skip */
				/* this block, no point in looking at it)  */
				/* CAUTION: parameters to this function are */
				/* different for trailers inside or outside */
				/* of headers. Se get_head() for details */
	int (*rd_data)();	/* read/process file data on the archive */
	int (*wr_data)();	/* read/process file data on the archive */
	int (*options)();	/* process format options (-x) flags */
} FSUB;

/*
 * Pattern matching structure
 *
 * Used to store command line patterns
 */
typedef struct pattern {
	char		*pstr;		/* pattern to match, user supplied */
	int		plen;		/* length of pstr */
	int		flgs;		/* processing/state flags */
#define MTCH		0x1		/* this pattern has been matched */
#define DIR_MTCH	0x2		/* this pattern matched a directory */
	struct pattern	*fow;		/* next pattern */
} PATTERN;

/*
 * General Archive Structure (used internal to pax)
 *
 * This structure is used to pass information about archive members between
 * the format independent routines and the format specific routines. When
 * new archive formats are added, they must accept requests and supply info
 * encoded in a structure of this type. The name fields are declared statically
 * here. The cost of malloc() and free on every archive member was found to be
 * excessive. Since there is only ONE of these flowting around, size is not a
 * big consideration.
 */
typedef struct {
	int nlen;			/* file name length */
	char name[PAXPATHLEN+1];	/* file name */
	int ln_nlen;			/* link name length */
	char ln_name[PAXPATHLEN+1];	/* name to link to (if any) */
	char *org_name;			/* orig name in file system */
	PATTERN *pat;			/* ptr to pattern match (if any) */
	struct stat sb;			/* stat buffer see stat(2) */
	off_t pad;			/* bytes of padding after file xfer */
	off_t skip;			/* bytes of real data after header */
					/* the st_size field may not apply */
	u_long crc;			/* file crc */
	int type;			/* type of file node */
#define PAX_DIR		1		/* directory */
#define PAX_CHR		2		/* character device */
#define PAX_BLK		3		/* block device */
#define PAX_REG		4		/* regular file */
#define PAX_SLK		5		/* symbolic link */
#define PAX_SCK		6		/* socket */
#define PAX_FIF		7		/* fifo */
#define PAX_HLK		8		/* hard link */
#define PAX_HRG		9		/* hard link (to a file if known) */
#define PAX_CTG		10		/* high performance file */ 
} ARCHD;

/*
 * Format Specific Options List
 *
 * Used to pass format options to the format options handler
 */
typedef struct oplist {
	char		*name;		/* option variable name e.g. name= */
	char		*value;		/* value for option variable */
	struct oplist	*fow;		/* next option */
} OPLIST;

/*
 * General Macros
 */
#ifndef MIN
#define        MIN(a,b) (((a)<(b))?(a):(b))
#endif
#define MAJOR(x)        (((unsigned)(x) >> 8) & 0xff)
#define MINOR(x)        ((x) & 0xff)
#define TODEV(x, y)	(((unsigned)(x) << 8) | ((unsigned)(y)))

/*
 * General Defines
 */
#define HEX	16
#define OCT	8
