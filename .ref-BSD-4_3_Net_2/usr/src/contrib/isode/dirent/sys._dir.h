/*
	<sys/_dir.h> -- definitions for 4.2,4.3BSD directories

	last edit:	25-Apr-1987	D A Gwyn

	A directory consists of some number of blocks of DIRBLKSIZ bytes each,
	where DIRBLKSIZ is chosen such that it can be transferred to disk in a
	single atomic operation (e.g., 512 bytes on most machines).

	Each DIRBLKSIZ-byte block contains some number of directory entry
	structures, which are of variable length.  Each directory entry has the
	beginning of a (struct direct) at the front of it, containing its
	filesystem-unique ident number, the length of the entry, and the length
	of the name contained in the entry.  These are followed by the NUL-
	terminated name padded to a (long) boundary with 0 bytes.  The maximum
	length of a name in a directory is MAXNAMELEN.

	The macro DIRSIZ(dp) gives the amount of space required to represent a
	directory entry.  Free space in a directory is represented by entries
	that have dp->d_reclen > DIRSIZ(dp).  All DIRBLKSIZ bytes in a
	directory block are claimed by the directory entries; this usually
	results in the last entry in a directory having a large dp->d_reclen.
	When entries are deleted from a directory, the space is returned to the
	previous entry in the same directory block by increasing its
	dp->d_reclen.  If the first entry of a directory block is free, then
	its dp->d_fileno is set to 0; entries other than the first in a
	directory do not normally have 	dp->d_fileno set to 0.

	prerequisite:	<sys/types.h>
*/

#if defined(accel) || defined(sun) || defined(vax)
#define	DIRBLKSIZ	512		/* size of directory block */
#else
#ifdef alliant
#define	DIRBLKSIZ	4096		/* size of directory block */
#else
#ifdef gould
#define	DIRBLKSIZ	1024		/* size of directory block */
#else
#ifdef ns32000	/* Dynix System V */
#define	DIRBLKSIZ	2600		/* size of directory block */
#else	/* be conservative; multiple blocks are okay but fractions are not */
#define	DIRBLKSIZ	4096		/* size of directory block */
#endif
#endif
#endif
#endif

#define	MAXNAMELEN	255		/* maximum filename length */
/* NOTE:  not MAXNAMLEN, which has been preempted by SVR3 <dirent.h> */

struct direct				/* data from read()/_getdirentries() */
	{
	unsigned long	d_fileno;	/* unique ident of entry */
	unsigned short	d_reclen;	/* length of this record */
	unsigned short	d_namlen;	/* length of string in d_name */
	char		d_name[MAXNAMELEN+1];	/* NUL-terminated filename */
	/* typically shorter */
	};

/*
	The DIRSIZ macro gives the minimum record length which will hold the
	directory entry.  This requires the amount of space in a (struct
	direct) without the d_name field, plus enough space for the name with a
	terminating NUL character, rounded up to a (long) boundary.

	(Note that Berkeley didn't properly compensate for struct padding,
	but we nevertheless have to use the same size as the actual system.)
*/

#define	DIRSIZ( dp )	((sizeof(struct direct) - (MAXNAMELEN+1) \
			+ sizeof(long) + (dp)->d_namlen) \
			/ sizeof(long) * sizeof(long))
