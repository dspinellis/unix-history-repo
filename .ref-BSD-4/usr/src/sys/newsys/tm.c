
#

/*
 * TM tape driver
 * Modified by Margie Murphy 9/1/80
 */

#include "../ch/param.h"
#include "../ch/buf.h"
#include "../ch/dir.h"
#include "../ch/conf.h"
#include "../ch/user.h"
#include "../ch/file.h"
#include "../ch/map.h"
#include "../ch/pte.h"
#include "../ch/uba.h"

struct device {
	short	tmer;		/* status register*/
	short	tmcs;		/* command register */
	short	tmbc;		/* byte (read/write) record (space) counter */
	unsigned short	tmba;	/* current memory address counter */
	short	tmdb;		/* data buffer register */
	short	tmrd;		/* tu10 read lines */
};

#define	b_repcnt  b_bcount	/* Command repetition counter */
#define	b_command b_resid	/* Command to do */
#define	b_status  b_resid	/* Status after a tcommand */

struct	buf	tmtab;		/* block device list header */
struct	buf	ctmbuf;		/* control semaphore*/
struct	buf	rtmbuf;		/* raw i/o device list header */

#define NUNIT     1		/* number of units on controller*/

#define	U_REWD	  04		/* Rewind on close */
#define	U_DUALD	  08		/* Dual density */

char	t_flags[NUNIT];		/* used for T_WRITTEN flag */
char	t_openf[NUNIT];		/* open flag for device */
daddr_t	t_blkno[NUNIT];		/* pointer to current block on tape */
daddr_t	t_nxrec[NUNIT];		/* pointer to end of file on tape */

#define	TMADDR ((struct device *)(UBA0_DEV + 0172520))	/* unibus address of tm device*/
/* CONTROL FLAGS */

#define	GO	01		/* initiate command and reset CU RDY */
#define	RCOM	02		/* read */
#define	WCOM	04		/* write */
#define	WEOF	06		/* write eof mark */
#define	SFORW	010		/* space forward */
#define	SREV	012		/* space reverse */
#define	WIRG	014		/* write with extended irg -- for write errors */
#define	REW	016		/* rewind */
#define	NOP	0100		/* nop = interrupt enable */
#define	IENABLE	0100		/* enable interrupts */
#define	DCLR	010000		/* Drive clear -- reset any hard error bits */
#define	D800	060000		/* 9-channel, 800 bpi density */
#define D1600   0117777		/* 9-channel, 1600 bpi density*/

/* STATUS FLAGS */
#define	TUR	1		/* tape unit ready */
#define RWS	02		/* rewind in progress--set on first rewind int*/
#define	WL	04		/* write lock bit -- read ring off tape */
#define GAPSD	010		/* gap settle down bit */
#define RLE	0100		/* record length error on read */
#define	CRDY	0200		/* control unit ready */
#define	EOF	040000		/* end of file */
#define HARD    0100000         /* ILC (could be a stronger test if we trusted the UNIBUS) */

/* DRIVER MODES */
#define	SSEEK	1		/* seek mode */
#define	SIO	2		/* sequential i/o mode */
#define	SCOM	3		/* command mode */

#define T_WRITTEN 1		/* device flag for eof writes */
#define T_WANTED  02		/* wait for rewind flag*/

tmopen(dev, flag)
dev_t dev;
int flag;
{
	register unit, ds;

	unit = minor(dev) & 03;
	if (t_openf[unit])
	{
		u.u_error = ENXIO;
		return;
	}
	t_blkno[unit] = 0;
	t_flags[unit] = 0;
	t_nxrec[unit] = 65535;

	tmtab.b_flags |= B_TAPE;

	ds = tcommand(dev, NOP, 1);

	if ((ds & TUR) == 0)
	{
		while (ds = tcommand(dev, NOP, 1) & RWS)
		{
			/* sleep during rewind initiated during prior IO */
			t_flags[NUNIT] |= T_WANTED;
			sleep((caddr_t)&t_flags[NUNIT],PRIBIO);
		}
	}

	while ((ds = tcommand(dev, NOP, 1)) & GAPSD);

	if ((ds&TUR)==0 || (flag && ds&WL))
	{
		/* Offline or needs a write ring */
		TMADDR->tmcs = DCLR|GO;
		u.u_error = EIO;
	}
	/* t_openf may have value -1 if certain hardware conditions came up */
	t_openf[unit] = (u.u_error == 0);
}

tmclose(dev, flag)
register dev_t dev;
register flag;
{
	register unit;

	unit = minor(dev)&03;
	t_openf[unit]++;
	if(
	   flag == FWRITE ||
	   ((flag&FWRITE) && (t_flags[minor(dev)&03] & T_WRITTEN))
	  ) {
		(void) tcommand(dev, WEOF, 2);
		(void) tcommand(dev, SREV, 1);
	    }
	if ((minor(dev)&U_REWD) == 0)
		/* rewind if bit 2 is set in minor */
		(void) tcommand(dev, REW, 1);

	t_openf[minor(dev)&03] = 0;
}


tcommand(dev, com, cnt)
register dev_t dev;
register com, cnt;
{
	register struct buf *bp;

	bp = &ctmbuf;
	(void) spl5();
	while (bp->b_flags&B_BUSY) 
	{
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	bp->b_flags = B_BUSY|B_READ;
	(void) spl0();

	bp->b_dev = dev;
	bp->b_repcnt = -cnt;
	bp->b_command = com;
	bp->b_blkno = 0;

	tmstrategy(bp);

	iowait(bp);
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags = 0;
	return(bp->b_status);
}

tmstrategy(bp)
register struct buf *bp;
{
	register daddr_t *p;

	if (bp != &ctmbuf) 
	{
		p = &t_nxrec[minor(bp->b_dev)&03];
		if (*p <= dbtofsb(bp->b_blkno)) 
		{
			if (*p < dbtofsb(bp->b_blkno)) 
			{
				/* Blkno past EOF */
				bp->b_flags |= B_ERROR;
				iodone(bp);
				return;
			}
			if (bp->b_flags&B_READ) 
			{
				/* Reading at EOF */
				clrbuf(bp);
				bp->b_resid = bp->b_bcount;
				iodone(bp);
				return;
			}
		}
		if ((bp->b_flags&B_READ) == 0) 
		{
			t_flags[minor(bp->b_dev)&03] |= T_WRITTEN;
			/* Change software location of EOF */
			*p = dbtofsb(bp->b_blkno) + 1;
		}
	}

	bp->av_forw = 0;

	(void) spl5();
	if (tmtab.b_actf == NULL)
		tmtab.b_actf = bp;
	else
		tmtab.b_actl->av_forw = bp;
	tmtab.b_actl = bp;
	if (tmtab.b_active == NULL)
		tmstart();
	(void) spl0();
}

tmstart()
{
	register struct buf *bp;
	register com;
	register unit;
	register daddr_t blkno;

loop:
	if ((bp = tmtab.b_actf) == 0)
		return;
	unit = minor(bp->b_dev)&03;
	blkno = t_blkno[unit];
	if (t_openf[unit] < 0 || (TMADDR->tmcs & CRDY) == NULL) 
	{
		bp->b_flags |= B_ERROR;
		goto next;
	}
	com = IENABLE | (unit << 8) | GO;

	if (minor(bp->b_dev) & U_DUALD)
		com &= D1600;
	else
		com |= D800;

	if (bp == &ctmbuf) 
	{
		if (bp->b_command == NOP) 
		{
			/* return device status */
			bp->b_status = TMADDR->tmer;
			goto next;
		}
		com |= bp->b_command;
		tmtab.b_active = SCOM;
		if(bp->b_command == SFORW || bp->b_command == SREV)
			TMADDR->tmbc = bp->b_repcnt;
		TMADDR->tmcs = com;
		return;
	}
	if (blkno != dbtofsb(bp->b_blkno)) 
	{
		/* Block mode, seek to correct block */
		tmtab.b_active = SSEEK;
		if (blkno < dbtofsb(bp->b_blkno)) 
		{
			com |= SFORW;
			TMADDR->tmbc = blkno - dbtofsb(bp->b_blkno);
		}
		else
		{
			com |= SREV;
			TMADDR->tmbc = dbtofsb(bp->b_blkno) - blkno;
		}
		TMADDR->tmcs = com;
		return;
	}
	if (tmtab.b_un.b_addr == 0)
		tmtab.b_un.b_addr = (caddr_t)ubasetup(bp,1);

	/* Set high 2 bits of UNIBUS address */
	com |= (((long)tmtab.b_un.b_addr >> (16-4)) & 060); 

	tmtab.b_active = SIO;
	com |= ((bp->b_flags & B_READ) ? RCOM : ((tmtab.b_errcnt) ? WIRG : WCOM ));
	TMADDR->tmbc = -bp->b_bcount;
	TMADDR->tmba = ((int)tmtab.b_un.b_addr & 0xffff);
	TMADDR->tmcs = com; 
	return;

next:
	if (tmtab.b_un.b_addr != 0)
	{
		ubafree(tmtab.b_un.b_addr);
		tmtab.b_un.b_addr = 0;
	}
	tmtab.b_actf = bp->av_forw;
	iodone(bp);
	goto loop;
}

tmintr()
{
	register struct buf *bp;
	register state, unit;

	if (t_flags[NUNIT] & T_WANTED)  
	{
		/* wake up any rewind sleepers*/
		t_flags[NUNIT] &= ~T_WANTED;
		wakeup((caddr_t)&t_flags[NUNIT]);
	}
	if ((bp = tmtab.b_actf) == NULL)
		return;

	unit = minor(bp->b_dev)&03;
	state = tmtab.b_active;
	tmtab.b_active = NULL;
	if (TMADDR->tmcs < 0)
	{
		/*
		 * On error, first wait for gap shutdown 
		 */
		while(TMADDR->tmer & GAPSD) ;

		if (TMADDR->tmer&EOF)
		{
			/* EOF, set nxrec to sought block */
			t_nxrec[unit] = dbtofsb(bp->b_blkno);
			state = SCOM;
			TMADDR->tmbc = -bp->b_bcount;
			goto out;
		}
		if ((bp->b_flags&B_READ) && (TMADDR->tmer&(HARD|RLE)) == RLE)
			goto out;
		if ((TMADDR->tmer&(HARD|EOF)) == NULL && state==SIO)
		{
			/* if not a hard error or eof and in sequential mode */
			if (++tmtab.b_errcnt < 3)
			{
				if((TMADDR->tmer&~RLE) != 0xC0)
					t_blkno[unit]++;
				else
					printf("TM UBA late error\n");
				if(tmtab.b_un.b_addr)
				{
					ubafree(tmtab.b_un.b_addr);
					tmtab.b_un.b_addr = 0;
				}
				/* retry: skip reverse one and do IO again */
				tmstart();
				return;
			}
		}
		else if(t_openf[unit]>0 && bp!=&rtmbuf && (TMADDR->tmer&EOF)==0)
			t_openf[unit] = -1;
		deverror(bp, TMADDR->tmer&0xffff, TMADDR->tmcs&0xffff);
		bp->b_flags |= B_ERROR;
		state = SIO;
	}
out:
	switch (state)
	{
	  case SIO:
		t_blkno[unit]++;

	  case SCOM:
		if(bp == &ctmbuf)
		  	if(bp->b_command == SFORW || bp->b_command == SREV)
			{
				if(bp->b_command == SREV)
					t_blkno[unit] -= -bp->b_repcnt;
				else
					t_blkno[unit] += -bp->b_repcnt;
			}
			else if(++bp->b_repcnt)
				break;
		tmtab.b_errcnt = 0;
		tmtab.b_actf = bp->av_forw;
		bp->b_resid = -TMADDR->tmbc;
		if(tmtab.b_un.b_addr != 0)
		{
			ubafree(tmtab.b_un.b_addr);
			tmtab.b_un.b_addr = 0;
		}
		iodone(bp);
		break;

	  case SSEEK:
		t_blkno[unit] = dbtofsb(bp->b_blkno);
		break;

	  default:
		return;
	}
	tmstart();
}

tmread(dev)
{
	tmphys(dev);
	physio(tmstrategy, &rtmbuf, dev, B_READ, minphys);
}

tmwrite(dev)
{
	tmphys(dev);
	physio(tmstrategy, &rtmbuf, dev, B_WRITE, minphys);
}

tmphys(dev)
{
	register unit;
	register daddr_t a;

	unit = minor(dev) & 03;
	a = dbtofsb(u.u_offset >> 9);
	t_blkno[unit] = a;
	t_nxrec[unit] = a + 1;
}

# define	NTMIOCTRL	(sizeof tmiolist)

char	tmiolist[]	=
{
	SFORW,		/* 0 = skip forward */
	SREV,		/* 1 = skip reverse */
	WEOF,		/* 2 = Write EOF */
	REW,		/* 3 = rewind */
	0,		/* 4 = skip forward file */
	0		/* 5 = skip reverse file */
};

tmioctrl(dev, cmd, arg, flag)
register dev_t dev;
register int cmd, flag, arg;
{
	register ioctr;

	if((unsigned)cmd >= NTMIOCTRL || arg <= 0)
		u.u_error = ENXIO;
	else
	{
		if(cmd > 3)
		{
			/* Skip forward/reverse file(s) */
			cmd -= 3;
			ioctr = arg;
			arg = -1;
		}
		else
			ioctr = 1;
		do
			(void) tcommand(dev, tmiolist[cmd], arg);
		while(--ioctr);
	}
}
