struct inode {
	char	i_flag;
	char	i_count;
	int	i_dev;
	int	i_number;
	int	i_mode;
	char	i_nlink;
	char	i_uid;
	char	i_gid;
	char	i_size0;
	char	*i_size1;
	int	i_addr[8];
	int	i_lastr;
} inode[NINODE];

/* flags */
#define	ILOCK	01
#define	IUPD	02
#define	IACC	04
#define	IMOUNT	010
#define	IWANT	020
#define	ITEXT	040

/* modes */
#define	IALLOC	0100000
#define	IFMT	060000
#define		IFDIR	040000
#define		IFCHR	020000
#define		IFBLK	060000
#define	ILARG	010000
#define	ISUID	04000
#define	ISGID	02000
#define ISVTX	01000
#define	IREAD	0400
#define	IWRITE	0200
#define	IEXEC	0100
