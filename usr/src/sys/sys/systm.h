/*	systm.h	4.16	81/04/28	*/

/*
 * Random set of variables
 * used by more than one
 * routine.
 */
int	hz;			/* frequency of the clock */
int	timezone;		/* minutes west from greenwich */
int	dstflag;		/* daylight savings time in effect? */
char	canonb[CANBSIZ];	/* buffer for erase and kill (#@) */
char	version[];		/* system version */
int	lbolt;			/* time of day in 60th not in time */
time_t	time;			/* time in sec from 1970 */
time_t	bootime;		/* time at which booted */

int	hand;			/* current index into coremap used by daemon */

/*
 * Nblkdev is the number of entries
 * (rows) in the block switch. It is
 * set in binit/bio.c by making
 * a pass over the switch.
 * Used in bounds checking on major
 * device numbers.
 */
int	nblkdev;

/*
 * Number of character switch entries.
 * Set by cinit/prim.c
 */
int	nchrdev;

int	nswdev;			/* number of swap devices */
int	mpid;			/* generic for unique process id's */
char	runin;			/* scheduling flag */
char	runout;			/* scheduling flag */
int	runrun;			/* scheduling flag */
char	kmapwnt;		/* kernel map want flag */
char	curpri;			/* more scheduling */

int	maxmem;			/* actual max memory per process */
int	physmem;		/* physical memory on this CPU */

int	nswap;			/* size of swap space */
int	updlock;		/* lock for sync */
daddr_t	rablock;		/* block to be read ahead */
extern	int intstack[];		/* stack for interrupts */
dev_t	rootdev;		/* device of the root */
dev_t	dumpdev;		/* device to take dumps on */
long	dumplo;			/* offset into dumpdev */
dev_t	swapdev;		/* swapping device */
dev_t	argdev;			/* device for argument lists */
dev_t	pipedev;		/* pipe device */

extern	int icode[];		/* user init code */
extern	int szicode;		/* its size */

dev_t	getmdev();
daddr_t	bmap();
caddr_t	calloc();
unsigned max();
unsigned min();
int	memall();
int	uchar(), schar();
int	vmemall();
char	*wmemall();
swblk_t	vtod();

/*
 * Structure of the system-entry table
 */
extern struct sysent
{
	int	sy_narg;		/* total number of arguments */
	int	(*sy_call)();		/* handler */
} sysent[];

char	vmmap[];		/* poor name! */
int	umbabeg,umbaend;	/* where sensitive vm begins/ends */
int	noproc;			/* no one is running just now */
extern	int catcher[256];
char	*panicstr;
int	wantin;
