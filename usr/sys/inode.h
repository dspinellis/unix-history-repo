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
struct	inode
{
	char	i_flag;
	char	i_count;	/* reference count */
	int	i_dev;		/* device where inode resides */
	int	i_number;	/* i number, 1-to-1 with device address */
	int	i_mode;
	char	i_nlink;	/* directory entries */
	char	i_uid;		/* owner */
	char	i_gid;		/* group of owner */
	char	i_size0;	/* most significant of size */
	char	*i_size1;	/* least sig */
	int	i_addr[8];	/* device addresses constituting file */
	int	i_lastr;	/* last logical block read (for read-ahead) */
} inode[NINODE];

/* flags */
#define	ILOCK	01		/* inode is locked */
#define	IUPD	02		/* inode has been modified */
#define	IACC	04		/* inode access time to be updated */
#define	IMOUNT	010		/* inode is mounted on */
#define	IWANT	020		/* some process waiting on lock */
#define	ITEXT	040		/* inode is pure text prototype */

/* modes */
#define	IALLOC	0100000		/* file is used */
#define	IFMT	060000		/* type of file */
#define		IFDIR	040000	/* directory */
#define		IFCHR	020000	/* character special */
#define		IFBLK	060000	/* block special, 0 is regular */
#define	ILARG	010000		/* large addressing algorithm */
#define	ISUID	04000		/* set user id on execution */
#define	ISGID	02000		/* set group id on execution */
#define ISVTX	01000		/* save swapped text even after use */
#define	IREAD	0400		/* read, write, execute permissions */
#define	IWRITE	0200
#define	IEXEC	0100
