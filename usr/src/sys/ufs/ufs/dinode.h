/*	dinode.h	4.11	82/04/19	*/

/*	inode.h	2.1	3/25/82	*/

/*
 * The I node is the focus of all file activity in UNIX.
 * There is a unique inode allocated for each active file,
 * each current directory, each mounted-on file, text file, and the root.
 * An inode is 'named' by its dev/inumber pair. (iget/iget.c)
 * Data in icommon is read in from permanent inode on volume.
 */

#define	NDADDR	8		/* direct addresses in inode */
#define	NIADDR	2		/* indirect addresses in inode */

struct inode {
	char	i_flag;
	char	i_count;	/* reference count */
	dev_t	i_dev;		/* device where inode resides */
	ino_t	i_number;	/* i number, 1-to-1 with device address */
	long	i_hlink;	/* link in hash chain (iget/iput/ifind) */
	struct	fs *i_fs;	/* file sys associated with this inode */
	union {
		daddr_t	if_lastr;	/* last read (read-ahead) */
		struct	socket *is_socket;
	} i_un;
	struct 	icommon
	{
		u_short	ic_mode;	/*  0: mode and type of file */
		short	ic_nlink;	/*  2: number of links to file */
		short	ic_uid;		/*  4: owner's user id */
		short	ic_gid;		/*  6: owner's group id */
		off_t	ic_size;	/*  8: number of bytes in file */
		daddr_t	ic_db[NDADDR];	/* 12: disk block addresses */
		daddr_t	ic_ib[NIADDR];	/* 44: indirect blocks */
		time_t	ic_atime;	/* 52: time last accessed */
		time_t	ic_mtime;	/* 56: time last modified */
		time_t	ic_ctime;	/* 60: time created */
	} i_ic;
};

struct dinode {
	union {
		struct	icommon di_icom;
		char	di_size[64];
	} di_un;
};

#define	i_mode		i_ic.ic_mode
#define	i_nlink		i_ic.ic_nlink
#define	i_uid		i_ic.ic_uid
#define	i_gid		i_ic.ic_gid
#define	i_size		i_ic.ic_size
#define	i_db		i_ic.ic_db
#define	i_ib		i_ic.ic_ib
#define	i_atime		i_ic.ic_atime
#define	i_mtime		i_ic.ic_mtime
#define	i_ctime		i_ic.ic_ctime
#define	i_rdev		i_ic.ic_db[0]
#define	i_lastr		i_un.if_lastr
#define	i_socket	is_socket

#define di_ic		di_un.di_icom
#define	di_mode		di_ic.ic_mode
#define	di_nlink	di_ic.ic_nlink
#define	di_uid		di_ic.ic_uid
#define	di_gid		di_ic.ic_gid
#define	di_size		di_ic.ic_size
#define	di_db		di_ic.ic_db
#define	di_ib		di_ic.ic_ib
#define	di_atime	di_ic.ic_atime
#define	di_mtime	di_ic.ic_mtime
#define	di_ctime	di_ic.ic_ctime
#define	di_rdev		di_ic.ic_db[0]

#ifdef KERNEL
extern	struct inode *inode;		/* The inode table itself */
extern	struct inode *inodeNINODE;	/* The end of the inode table */
extern	int ninode;			/* number of slots in the table */

struct	inode *rootdir;			/* pointer to inode of root directory */

struct	inode *ialloc();
struct	inode *ifind();
struct	inode *iget();
struct	inode *owner();
struct	inode *maknode();
struct	inode *namei();
#endif

/* flags */
#define	ILOCK	01		/* inode is locked */
#define	IUPD	02		/* file has been modified */
#define	IACC	04		/* inode access time to be updated */
#define	IMOUNT	010		/* inode is mounted on */
#define	IWANT	020		/* some process waiting on lock */
#define	ITEXT	040		/* inode is pure text prototype */
#define	ICHG	0100		/* inode has been changed */

/* modes */
#define	IFMT		0170000		/* type of file */
#define	IFCHR		0020000		/* character special */
#define	IFDIR		0040000		/* directory */
#define	IFBLK		0060000		/* block special */
#define	IFREG		0100000		/* regular */
#define	IFLNK		0120000		/* symbolic link */
#define	IFPORTAL	0140000		/* portal */
#define	ISUID		04000		/* set user id on execution */
#define	ISGID		02000		/* set group id on execution */
#define	ISVTX		01000		/* save swapped text even after use */
#define	IREAD		0400		/* read, write, execute permissions */
#define	IWRITE		0200
#define	IEXEC		0100
