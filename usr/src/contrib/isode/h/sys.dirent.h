/*
	<sys/dirent.h> -- file system independent directory entry (SVR3)

	last edit:	25-Apr-1987	D A Gwyn

	prerequisite:	<sys/types.h>
*/


/* The following nonportable ugliness could have been avoided by defining
   DIRENTSIZ and DIRENTBASESIZ to also have (struct dirent *) arguments. */
#define	DIRENTBASESIZ		(((struct dirent *)0)->d_name \
				- (char *)&((struct dirent *)0)->d_ino)
#define	DIRENTSIZ( namlen )	((DIRENTBASESIZ + sizeof(long) + (namlen)) \
				/ sizeof(long) * sizeof(long))

/* DAG -- the following was moved from <dirent.h>, which was the wrong place */
#define	MAXNAMLEN	512		/* maximum filename length */

#ifndef NAME_MAX
#define	NAME_MAX	(MAXNAMLEN - 1)	/* DAG -- added for POSIX */
#endif


#ifdef	dirent
#undef	dirent
#endif
struct dirent				/* data from getdents()/readdir() */
	{
#ifdef apollo
	long	d_ino;			/* inode number of entry */
	unsigned short	d_reclen;	/* length of this record */
	unsigned short	d_namlen;	/* length of string in d_name */
	off_t	d_off;			/* offset of disk directory entry */
	char	d_name[MAXNAMLEN + 1];	/* name must be no longer than this */
#else
	long		d_ino;		/* inode number of entry */
	off_t		d_off;		/* offset of disk directory entry */
	unsigned short	d_reclen;	/* length of this record */
	char		d_name[1];	/* name of file */	/* non-POSIX */
#endif
	};

#ifdef XOS_2				
#define	S_ISDIR( mode )		(((mode) & S_IFMT) == S_IFDIR)
#endif
