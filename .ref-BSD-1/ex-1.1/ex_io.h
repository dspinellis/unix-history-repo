/*
 * Ex - a text editor
 * Bill Joy UCB June 1977
 */

#define	FILETYP	060000
#define	FPLAIN	000000
#define	FDIRECT	040000
#define	FCHSPEC	020000
#define	FBLSPEC	060000

struct stb {
	char	minor;
	char	major;
	int	inumber;
	int	flags;
	char	nlinks;
	char	uid;
	char	gid;
	char	size0;
	int	size1;
	char	dminor;
	char	dmajor;
	int	addr2[7];
	long	actime;
	long	modtime;
};

#define	ENOENT	2
#define	EIO	5
#define	EACCESS	13
#define	ENOTDIR	20
#define	EISDIR	21
#define	ENFILE	23
#define	ENOSPC	28
#define	EROFS	30
#define	EQUOT	33

int	errno;

#define	DTTYMAJ	3
#define	DTTYMIN	0

#define	DVNLMAJ	8
#define	DVNLMIN	2
