/*	c-version of tp?.s
 *
 *	M. Ferentz
 *	August 1976
 *
 *	revised July 1977 BTL
 */

#define	MDIRENT	496		/* must be zero mod 8 */
#define DIRSZ	sizeof(struct dent)
#define MAPSIZE 4096
#define MAPMASK 07777
#define NAMELEN 32
#define BSIZE   512
#define	TCSIZ	578
#define TCDIRS	192
#define	MTSIZ	32767
#define TPB	(BSIZE/sizeof(struct tent))
#define	OK	0100000
#define	BRKINCR	512

#define	tapeblk	&tpentry[0]
#define tapeb	&tpentry[0]

struct 	tent	{	/* Structure of a tape directory block */
	char	pathnam[NAMELEN];
	short	mode;
	char	uid;
	char	gid;
	char	spare;
	char	size0;
	unsigned short	size1;
	long	time;
	unsigned short	tapea;	/* tape address */
	short	unused[8];
	short	cksum;
}	tpentry[TPB];

struct	dent {	/* in core version of tent with "unused" removed
		 * and pathname replaced by pointer to same in a
		 * packed area (nameblock).
		 */
	char	*d_namep;
	int	d_mode;
	int	d_uid;
	int	d_gid;
	long	d_size;
	long	d_time;
	int	d_tapea;
}  dir[MDIRENT];

char	map[MAPSIZE];
char	name[NAMELEN];
char	name1[NAMELEN];
extern	char mt[];
extern	char tc[];
char	*tname;
extern	char mheader[];
extern	char theader[];

int	narg, rnarg;
char	**parg;
int	wseeka,rseeka;
int	tapsiz;
int	fio;
short	ndirent, ndentb;
struct	dent	*edir;
struct	dent *lastd;		/* for improvement */
char	*sbrk();
char	*strcpy();
long	lseek();
int	(*command)();

char	*nameblk;
char	*top;
char	*nptr;

extern	int	flags;
#define	flc	0001
#define	fli	0004
#define	flm	0010
#define	flu	0020
#define	flv	0040
#define	flw	0100
#define fls	0200
