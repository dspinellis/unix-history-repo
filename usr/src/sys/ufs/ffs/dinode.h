/*	dinode.h	4.9	81/11/14	*/

/*
 * The I node is the focus of all file activity in UNIX.
 * There is a unique inode allocated for each active file,
 * each current directory, each mounted-on file, text file, and the root.
 * An inode is 'named' by its dev/inumber pair. (iget/iget.c)
 * Data, from mode on, is read in from permanent inode on volume.
 */
#define	NADDR	13

struct inode {
	char	i_flag;
	char	i_count;	/* reference count */
	dev_t	i_dev;		/* device where inode resides */
	ino_t	i_number;	/* i number, 1-to-1 with device address */
/* begin read from disk */
	u_short	i_mode;
	short	i_nlink;	/* directory entries */
	short	i_uid;		/* owner */
	short	i_gid;		/* group of owner */
	off_t	i_size;		/* size of file */
	union {
		struct i_f {
			daddr_t	if_addr[NADDR];	/* if normal file/directory */
			daddr_t	if_lastr;	/* last read (read-ahead) */
		} i_f;
		struct i_d {
			daddr_t	id_rdev;	/* i_addr[0] */
		} i_d;
		struct i_s {
			struct	socket *is_socket;
		} i_s;
#define	i_addr		i_f.if_addr
#define	i_lastr		i_f.if_lastr
#define	i_rdev		i_d.id_rdev
#define	i_socket	i_s.is_socket
	} i_un;
/* end read from disk */
	short	i_XXXXXX;	/* ### */
/* SHOULD USE POINTERS, NOT INDICES, FOR HAS CHAIN */
	short	i_hlink;	/* link in hash chain (iget/iput/ifind) */
};

#ifdef KERNEL
struct	inode *inode, *inodeNINODE;
int	ninode;

struct	inode *rootdir;		/* pointer to inode of root directory */

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
#define	IFMT	0170000		/* type of file */
#define		IFCHR		0020000		/* character special */
#define		IFDIR		0040000		/* directory */
#define		IFBLK		0060000		/* block special */
#define		IFREG		0100000		/* regular */
#define		IFSYMREG	0110000		/* regular symbolic link */
#define		IFSYMDIR	0130000		/* directory symbolic link */
#define		IFPORTAL	0140000		/* portal */
#define	ISUID	04000		/* set user id on execution */
#define	ISGID	02000		/* set group id on execution */
#define	ISVTX	01000		/* save swapped text even after use */
#define	IREAD	0400		/* read, write, execute permissions */
#define	IWRITE	0200
#define	IEXEC	0100
