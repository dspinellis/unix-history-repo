#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/tty.h"
#include "../h/conf.h"
#include "../h/proc.h"
#include "../h/text.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/mx.h"
#include "../h/acct.h"
#include "../h/mba.h"

int	nulldev();
int	nodev();
int 	random();

int	hpstrategy(),hpread(),hpwrite(),hpintr();
struct	buf	hptab;
 
int	htopen(),htclose(),htstrategy(),htread(),htwrite();
struct	buf	httab;

struct bdevsw	bdevsw[] =
{
/* 0 */	nulldev,	nulldev,	hpstrategy,	&hptab,
/* 1 */	htopen,		htclose,	htstrategy,	&httab,
	0,
};

int	consopen(),consclose(),consread(),conswrite(),consioctl();
int	dzopen(),dzclose(),dzread(),dzwrite(),dzioctl();
struct	tty	dz_tty[];
int	syopen(),syread(),sywrite(),sysioctl();
int 	mmread(),mmwrite();
int	mxopen(),mxclose(),mxread(),mxwrite(),mxioctl();
int	mcread(),mcwrite();
 
struct cdevsw	cdevsw[] =
{
/* 0 */ consopen, consclose, consread, conswrite, consioctl, 0,
/* 1 */ dzopen, dzclose, dzread, dzwrite, dzioctl, dz_tty,
/* 2 */ syopen,		nulldev,	syread,	sywrite,	sysioctl, 0,
/* 3 */	nulldev,	nulldev,	mmread,	mmwrite,	nodev, 0,
/* 4 */	nulldev,	nulldev,	hpread,	hpwrite,	nodev, 0,
/* 5 */ htopen,		htclose,	htread,	htwrite,	nodev, 0,
/* 6 */	mxopen, mxclose, mxread, mxwrite, mxioctl, 0,
	0,
};

int ttyopen(),ttyclose(),ttread(),ttwrite();
int ttyinput(),ttstart() ;
 
struct linesw linesw[] =
{
/* 0 */ ttyopen, nulldev, ttread, ttwrite, nodev, ttyinput,	ttstart,
/* 1 */	mxopen, mxclose, mcread, mcwrite, mxioctl, nulldev, nulldev,
	0
};
 
int nldisp = 1;
dev_t	rootdev	= makedev(0, 0);
dev_t	swapdev	= makedev(0, 1);
dev_t	pipedev	= makedev(0, 0);
daddr_t swplo = 0;
int nswap = 8778;
 
struct	buf	buf[NBUF];
struct	file	file[NFILE];
struct	inode	inode[NINODE];
struct	text	text[NTEXT];
struct	proc	proc[NPROC];
struct	buf	bfreelist;
struct	acct	acctbuf;
struct	inode	*acctp;
 
int *usrstack = (int *)USRSTACK;
/*tbl*/ int mem_no = 3; 	/* major device number of memory special file */


extern int Sysmap[];
extern struct user u;

int mbanum[] = {	/* mba number of major device */
	0,		/* disk */
	1,		/* tape */
	9999999,	/* unused */
	9999999,	/* unused */
	0,		/* disk, raw */
	1,		/* tape, raw */
	};

int *mbaloc[] = { /* virtual location of mba */
	(int *)MBA0,
	(int *)MBA1,
	};
