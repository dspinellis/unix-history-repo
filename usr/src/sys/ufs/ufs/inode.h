/*	inode.h	4.6	81/06/15	*/

/*
 * The I node is the focus of all
 * file activity in unix. There is a unique
 * inode allocated for each active file,
 * each current directory, each mounted-on
 * file, text file, and the root. An inode is 'named'
 * by its dev/inumber pair. (iget/iget.c)
 * Data, from mode on, is read in
 * from permanent inode on volume.
 */

#define	NADDR	13

#define	NINDEX		6
struct group
{
	short	g_state;
	char	g_index;
	char	g_rot;
	struct	group	*g_group;
	struct	inode	*g_inode;
	struct	file	*g_file;
	short	g_rotmask;
	short	g_datq;
	struct	chan *g_chans[NINDEX];
};
struct	inode
{
	char	i_flag;
	char	i_count;	/* reference count */
	dev_t	i_dev;		/* device where inode resides */
	ino_t	i_number;	/* i number, 1-to-1 with device address */
	unsigned short i_mode;
	short	i_nlink;	/* directory entries */
	short	i_uid;		/* owner */
	short	i_gid;		/* group of owner */
	off_t	i_size;		/* size of file */
	union {
		struct {
			daddr_t	I_addr[NADDR];	/* if normal file/directory */
			daddr_t	I_lastr;	/* last read (for read-ahead) */
		} i_f;
#define	i_addr	i_f.I_addr
#define	i_lastr	i_f.I_lastr
		struct {
			daddr_t	I_rdev;		/* i_addr[0] */
			struct	group I_group;	/* multiplexor group file */
		} i_d;
#define	i_rdev	i_d.I_rdev
#define	i_group	i_d.I_group
		struct {
			daddr_t	I_port0;	/* low 16 bits of portid */
			daddr_t	I_port1;	/* high 16 bits of portid */
		} i_p;
#define	i_port0	i_p.I_port0
#define	i_port1	i_p.I_port1
	} i_un;
	short	i_vfdcnt;	/* number of fd's vreading this inode */
	short	i_hlink;	/* link in hash chain (iget/iput/ifind) */
};

#ifdef KERNEL
struct	inode *inode, *inodeNINODE;
int	ninode;

struct	inode *rootdir;		/* pointer to inode of root directory */
struct	inode *mpxip;		/* mpx virtual inode */

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
#define	IPIPE	0200		/* inode is a pipe */

/* modes */
#define	IFMT	0170000		/* type of file */
#define		IFDIR	0040000	/* directory */
#define		IFCHR	0020000	/* character special */
#define		IFBLK	0060000	/* block special */
#define		IFREG	0100000	/* regular */
#define		IFPORT	0010000	/* named port */
#define		IFMPC	0030000	/* multiplexed char special */
#define		IFMPB	0070000	/* multiplexed block special */
#define	ISUID	04000		/* set user id on execution */
#define	ISGID	02000		/* set group id on execution */
#define	ISVTX	01000		/* save swapped text even after use */
#define	IREAD	0400		/* read, write, execute permissions */
#define	IWRITE	0200
#define	IEXEC	0100
