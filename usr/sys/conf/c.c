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
#include "../h/acct.h"

int	nulldev();
int	nodev();
int	rkstrategy();
struct	buf	rktab;
int	htopen(), htclose(), htstrategy();
struct	buf	httab;
struct	bdevsw	bdevsw[] =
{
	nulldev, nulldev, rkstrategy, &rktab,	/* rk = 0 */
	nodev, nodev, nodev, 0, /* rp = 1 */
	nodev, nodev, nodev, 0, /* rf = 2 */
	nodev, nodev, nodev, 0, /* tm = 3 */
	nodev, nodev, nodev, 0, /* tc = 4 */
	nodev, nodev, nodev, 0, /* hs = 5 */
	nodev, nodev, nodev, 0, /* hp = 6 */
	htopen, htclose, htstrategy, &httab,	/* ht = 7 */
	nodev, nodev, nodev, 0, /* rl = 8 */
	0
};

int	klopen(), klclose(), klread(), klwrite(), klioctl();
int	mmread(), mmwrite();
int	rkread(), rkwrite();
int	htread(), htwrite();
int	syopen(), syread(), sywrite(), sysioctl();

struct	cdevsw	cdevsw[] =
{
	klopen, klclose, klread, klwrite, klioctl, nulldev, 0,	/* console = 0 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* pc = 1 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* lp = 2 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* dc = 3 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* dh = 4 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* dp = 5 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* dj = 6 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* dn = 7 */
	nulldev, nulldev, mmread, mmwrite, nodev, nulldev, 0, 	/* mem = 8 */
	nulldev, nulldev, rkread, rkwrite, nodev, nulldev, 0,	/* rk = 9 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* rf = 10 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* rp = 11 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* tm = 12 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* hs = 13 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* hp = 14 */
	htopen, htclose, htread, htwrite, nodev, nulldev, 0,	/* ht = 15 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* du = 16 */
	syopen, nulldev, syread, sywrite, sysioctl, nulldev, 0,	/* tty = 17 */
	nodev, nodev, nodev, nodev, nodev, nulldev, 0, /* rl = 18 */
	0
};

int	ttyopen(), ttyclose(), ttread(), ttwrite(), ttyinput(), ttstart();
struct	linesw	linesw[] =
{
	ttyopen, nulldev, ttread, ttwrite, nodev, ttyinput, ttstart, /* 0 */
	0
};
int	rootdev	= makedev(0, 0);
int	swapdev	= makedev(0, 0);
int	pipedev = makedev(0, 0);
int	nldisp = 1;
daddr_t	swplo	= 4000;
int	nswap	= 872;
	
struct	buf	buf[NBUF];
struct	file	file[NFILE];
struct	inode	inode[NINODE];
int	mpxchan();
int	(*ldmpx)() = mpxchan;
struct	proc	proc[NPROC];
struct	text	text[NTEXT];
struct	buf	bfreelist;
struct	acct	acctbuf;
struct	inode	*acctp;
