/*
** sys/stat.h
**
** Emulation of the Unix sys/stat.h header file for PRIMOS
**
** Author: Peter Eriksson <pen@lysator.liu.se>
*/

#ifndef __SYS_STAT_H__
#define __SYS_STAT_H__


#include <sys/types.h>

struct	stat {
        /* First some PRIMOS standard entries */
	off_t	st_size;
	time_t	st_mtime;
	short	st_type;        /* Primos file type */
	short	st_rwlock;      /* Primos read/write lock */

	/* Begin Unix compatibility - don't believe these entries! */
	dev_t	st_dev;       
	ino_t	st_ino;
	mode_t	st_mode;
	short	st_nlink;
	uid_t	st_uid;
	gid_t	st_gid;
	dev_t	st_rdev;
	time_t	st_atime;
	time_t	st_ctime;
	long	st_blksize;
	long	st_blocks;
};

#define	_IFMT		0170000	/* type of file */
#define	_IFREG		0100000	/* regular */
#define	_IFDIR		0040000	/* directory */

/* Some stupid programs check if these are defined and then
   believe these are supported in the OS - not so in PRIMOS ... */
#ifndef __50SERIES
#  define _IFCHR	0020000
#  define _IFBLK	0060000
#  define _IFLNK	0120000
#  define _IFSOCK	0140000
#  define _IFIFO	0010000
#endif

#define	S_ISUID		0004000
#define	S_ISGID		0002000
#define	S_ISVTX		0001000
#define	S_IREAD		0000400
#define	S_IWRITE 	0000200
#define	S_IEXEC		0000100

#define	S_ENFMT 	0002000

#define	S_IFMT		_IFMT
#define	S_IFREG		_IFREG
#define	S_IFDIR		_IFDIR
#ifndef __50SERIES
#  define S_IFCHR	_IFCHR
#  define S_IFBLK	_IFBLK
#  define S_IFLNK	_IFLNK
#  define S_IFSOCK	_IFSOCK
#  define S_IFIFO	_IFIFO
#endif

#define	S_IRWXU 	0000700
#define	S_IRUSR		0000400
#define	S_IWUSR		0000200
#define	S_IXUSR		0000100
#define	S_IRWXG		0000070
#define	S_IRGRP		0000040
#define	S_IWGRP		0000020
#define	S_IXGRP		0000010
#define	S_IRWXO		0000007
#define	S_IROTH		0000004
#define	S_IWOTH		0000002
#define	S_IXOTH		0000001

#define	S_ISREG(m)	(((m) & _IFMT) == _IFREG)
#define	S_ISDIR(m)	(((m) & _IFMT) == _IFDIR)
#ifndef __50SERIES
#  define S_ISBLK(m)	(((m) & _IFMT) == _IFBLK)
#  define S_ISCHR(m)	(((m) & _IFMT) == _IFCHR)
#  define S_ISFIFO(m)	(((m) & _IFMT) == _IFIFO)
#endif


#endif
