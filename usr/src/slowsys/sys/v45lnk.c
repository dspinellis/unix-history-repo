# include "../h/param.h"
# include "../h/dir.h"
# include "../h/user.h"
# include "../h/proc.h"
# include "../h/uba.h"
# include "../h/buf.h"
/*		*/
# define LNKADR (UBA0_DEV+0172410) /* DR11-B devive reg's */
/*
*  UBA0_DEV = 0x80018000
*  LNKADR = 0x80027508
*/
/*  DA11-B  DR-11B Status & Command reg */
# define ERR  0x8000  /*  error bit */
# define NEX  0x4000  /*  non-existent memory */
# define IIR  0x800  /*  Input Interrupt Request */
# define ID  0x400  /*  Input direction */
# define IM  0x200  /*  Input mode */
# define CYCLE  0x100  /* Cycle */
# define RDY  0x80  /* Ready */
# define IE  0x40  /*  Interrupt Enable */
# define OIR  8  /*  Output interrupt request */
# define OD  4  /* Output direction */
# define OM  2  /* output mode */
# define GO  1  /* Go bit */
/*	states	*/
# define CLOSED 0  /*  closed */
# define  OPEN  1  /* open  */
# define  S1  2  /* write state 1 */
# define  S2  4  /* write state 2 */
# define  R1  8  /* read state 1 */
# define  R2  16  /* read state 2 */
# define L_NOT 128  /*  send OIR to other end link */
# define IPEND 256
# define PREread  512  /*  'lnkmon' wait */
# define L_MON 1024  /* 'lnkmon' decr V11opn */
# define L_WAKE  2048
# define BUSY (S1|S2|R1|R2)  /* i-o in progress for driver */
# define S_ERR  0x8000
/*		*/
# define  V11PRI  0
# define BUFSIZ  512
# define VSPEC  0100000
/*		*/
struct lnkreg {
	short drwc , drba , drst , drdb ;
	} ;
struct {
	short state ;
	struct buf *bp ;
	} V11tab ;
char V11opn ;
 
/*		*/
 
V11open(dev,flag)
{
struct buf *geteblk() ;
 
if (V11opn) {
	u.u_error = EBUSY;
	return;
	}
V11opn++;
if (V11tab.bp == 0) /* grab system buffer */
	V11tab.bp = geteblk() ;  /*  VAX call */
((struct lnkreg *)LNKADR)->drst |= IE ;
V11tab.state |= OPEN ;
}
 
/*		*/
 
V11close(dev,flag) /* called on last close only ! ! */
{
V11opn = 0;
V11tab.state = CLOSED ;
((struct lnkreg *)LNKADR)->drst = 0 ; /* IE off */
if (V11tab.bp) brelse(V11tab.bp) ; /* release system buffer */
V11tab.bp = 0 ;
}
 
/*		*/
 
V11write(dev)
{
register int i , j  , k ;
 
/*  INIT clears OUTPUT DIR bit = transmitter */
spl5() ;
V11tab.state |= S1 ;
/* setup for transmit */
((struct lnkreg *)LNKADR)->drst &= ~(OD) ; /* 0 -> transmitter */
spl0() ;
 
i = uballoc(V11tab.bp->b_un.b_addr,BUFSIZ,1) ; /* get UBA map entry for
	system buffer and set valid bit & BDP no. & BO bit & load map */
j = i & 0x3ffff ; /* start map no. & byte offset */
 
/* write loop, until done or error */
while ((u.u_count > 0) && (u.u_error == 0)) {
	iomove(V11tab.bp->b_un.b_addr,min(BUFSIZ,u.u_count),B_WRITE) ; /* move
		data from user space to system buffer */
	((struct lnkreg *)LNKADR)->drwc = (-(BUFSIZ>>1)) ; /* word count */
	((struct lnkreg *)LNKADR)->drba = j ; /* map no. (SBI page) & byte offset */
	spl5() ;
	((struct lnkreg *)LNKADR)->drst |= GO ; /* setup for transfer */
	sleep(LNKADR,V11PRI) ; /* wait for transfer to finish(interrupt) */
	spl0() ;
	if (V11tab.state & S_ERR)
		u.u_error = ENXIO ;
	}
ubafree(i) ;
((struct lnkreg *)LNKADR)->drst &= (~OM);
V11tab.state &= (~BUSY) ;
}
 
/*		*/
 
V11read(dev)
{
register i , j  ;
 
spl5() ;
V11tab.state |= R1 ;
((struct lnkreg *)LNKADR)->drst |= OD ; /* receiver */
spl0() ;
i = uballoc(V11tab.bp->b_un.b_addr,BUFSIZ,1) ;
j = i & 0x3ffff ; /* start map no. & byte offset */
 
while ((u.u_count>0) && (u.u_error == 0)) {
	((struct lnkreg *)LNKADR)->drwc = (-(BUFSIZ>>1)) ;
	((struct lnkreg *)LNKADR)->drba = j ;
	spl5() ;
	((struct lnkreg *)LNKADR)->drst |= GO ;
	/* wait for i-o to finish */
	sleep(LNKADR,V11PRI) ;
	spl0() ;
	if (V11tab.state & S_ERR) {
		u.u_error = ENXIO ;
		continue ;
		}
	iomove(V11tab.bp->b_un.b_addr,min(BUFSIZ,u.u_count),B_READ) ;
	}
 
ubafree(i) ;
V11tab.state &= (~BUSY) ;
}
 
/*		*/
 
V11int(dev)
{
register unsigned short state ;
register int i ;
extern int V11ioctl() ;
 
if ((i = ((struct lnkreg *)LNKADR)->drst) & ERR)
	V11tab.state |= S_ERR ;
state = V11tab.state ;
if (state&BUSY) {
	wakeup(LNKADR) ;
	return ;
	}
if (i&IM) {
	if (state&PREread) {
		wakeup(V11ioctl);
		return;
		}
	if (((state&BUSY) == 0) && (state&OPEN))
		V11tab.state |= IPEND ;
	return;
	}
}
 
/*		*/
 
V11ioctl(dev,cmd,addr,flag)
dev_t dev;
caddr_t addr;
{
if (cmd & VSPEC) {
	switch (cmd & 077777) {
		case L_WAKE : { /* debug wakeup */
			wakeup(LNKADR);
			break;
			}
		case L_NOT : { /* send OIR to other end of link */
			spl5();
			((struct lnkreg *)LNKADR)->drst |= (OIR|OM) ;
			((struct lnkreg *)LNKADR)->drst &= (~OIR);
			spl0();
			break;
			}
		case L_MON : { /* 'lnkmon' decr 'V11opn' */
			V11opn--;
			break;
			}
		case PREread : { /* 'lnkmon' wait */
			spl5();
			if (V11tab.state&IPEND) {
				V11tab.state &= (~IPEND);
				}
			else {
				V11tab.state |= PREread;
				sleep(V11ioctl,PZERO+1);
				V11tab.state &= (~PREread);
				}
			spl0();
			break;
			}
		}
	}
else {
	*((short *)addr) = V11tab.state;
	}
}
