/*
*  DZ-11 Driver
*/
# include "../h/param.h"
# include "../h/tty.h"
# include "../h/uba.h"
# include "../h/proc.h"
# include "../h/dir.h"
# include "../h/file.h"
# include "../h/inode.h"
# include "../h/user.h"
# include "../h/conf.h"
 
# define DZADDR  (UBA0_DEV + 0160100)
# define NDZ (1*8)
 
# define  BITS7  020
# define  BITS8  030
# define  TWOSB  040
# define  PENABLE  0100
# define  OPAR  0200
# define  MSE  040  /* Master Scan Enable */
# define  RIE  0100  /* Receiver Interrupt Enable */
# define  TIE  040000  /*  Transmit interrupt enable */
# define  DZ_IEN  (MSE+RIE+TIE)
# define  PERROR  010000
# define  FRERROR  020000
# define SSPEED  7  /* std speed = 300 baud */

 
# define dzlpr  dzrbuf
# define dzmsr  dzbrk
# define  ON  1
# define  OFF  0
 
struct tty dz_tty[NDZ] ;
int dz_cnt = { NDZ }  ;
struct dzregs {
	short dzcsr ;
	short dzrbuf ;
	char  dztcr ;
	char  dzdtr ;
	char  dztbuf ;
	char  dzbrk ;
	} ;
struct dzregs *dz_addr[] = {
	(struct dzregs *)(DZADDR),
	(struct dzregs *)(DZADDR+010),
	(struct dzregs *)(DZADDR+020),
	(struct dzregs *)(DZADDR+030)
	} ;
char dz_timer ;
char dz_speeds[] = {
	0, 020 , 021 , 022 , 023 , 024 , 0, 025,
	026 , 027 , 030 , 032 , 034 , 036 , 0 , 0,
	} ;
 
dzopen(d,flag)
{
	register struct tty *tp ;
	register dev ;
	extern dzstart() , dzscan() ;
 
	dev = minor(d);
	if (dev >= dz_cnt) {
		u.u_error = ENXIO ;
		return ;
		}
	if (dz_timer == 0) {
		dz_timer++ ;
		timeout(dzscan,0,60) ;
		}
	tp = &dz_tty[dev] ;
	tp->t_addr = ((struct dzregs *)DZADDR) + (dev>>3);
	tp->t_oproc = dzstart ;
	tp->t_iproc = NULL;
	tp->t_state |= WOPEN ;
	if ((tp->t_state & ISOPEN) == 0) {
		tp->t_erase = CERASE ;
		tp->t_kill = CKILL ;
		tp->t_ospeed = tp->t_ispeed = SSPEED ;
		tp->t_flags = ODDP|EVENP|ECHO|HUPCLS ;
		dzparam(dev) ;
		}
	else
		if (tp->t_state&XCLUDE && u.u_uid != 0) {
			u.u_error = EBUSY ;
			return ;
			}
	dzmodem(dev,ON) ;
	spl5() ;
	while ((tp->t_state & CARR_ON) == 0) {
		tp->t_state |= WOPEN ;
		sleep(&tp->t_rawq,TTIPRI) ;
		}
	spl0() ;
	(*linesw[tp->t_line].l_open)(d,tp);
}
 
/*		*/
 
dzclose(d)
{
	register struct tty *tp ;
	register dev ;
 
	dev = minor(d);
	tp = &dz_tty[dev] ;
	(*linesw[tp->t_line].l_close)(tp);
	if (tp->t_flags & HUPCLS)
		dzmodem(dev,OFF) ;
	ttyclose(tp);
}
 
/*		*/
 
dzread(d)
{
	register struct tty *tp;
 
	tp = &dz_tty[minor(d)];
	(*linesw[tp->t_line].l_read)(tp);
}
 
/*		*/
 
dzwrite(d)
{
	register struct tty *tp;
 
	tp = &dz_tty[minor(d)];
	(*linesw[tp->t_line].l_write)(tp);
}
 
/*		*/
 
dzrint(dev)
{
	register struct tty *tp ;
	register int c ;
	register struct dzregs *dzaddr ;
 
	dzaddr = dz_addr[dev] ;
	while ((c = dzaddr->dzrbuf) < 0) { /* char present */
		tp = &dz_tty[((c>>8)&07)|(dev<<3)] ;
		if (tp >= &dz_tty[dz_cnt]) continue ;
		if ((tp->t_state & ISOPEN) == 0) {
			wakeup(&tp->t_rawq) ;
			continue ;
			}
		if (c & FRERROR) /* framing error = break */
			if (tp->t_flags & RAW) c = 0 ; /* null for getty */
			else c = 0177 ; /* DEL = interrupt */
		if (c & PERROR) /* parity error */
			if (((tp->t_flags & (EVENP|ODDP)) == EVENP)
			  || ((tp->t_flags & (EVENP|ODDP)) == ODDP))
				continue ;
		(*linesw[tp->t_line].l_rint)(c,tp);
		}
}
 
/*		*/
 
dzioctl(dev,cmd,addr,flag)
caddr_t addr;
dev_t dev;
{
	register struct tty *tp;
 
	tp = &dz_tty[minor(dev)];
	if (ttioccomm(cmd,tp,addr,dev)) {
		if (cmd==TIOCSETP || cmd==TIOCSETN)
			dzparam(minor(dev));
	} else
		u.u_error = ENOTTY;
}
 
/*		*/
 
dzparam(dev)
{
	register struct tty *tp ;
	register struct dzregs *dzaddr ;
	register short lpr ;
 
	tp = &dz_tty[dev] ;
	dzaddr = dz_addr[dev>>3] ;
	dzaddr->dzcsr = DZ_IEN ;
	if (tp->t_ispeed == 0) { /* hang up line */
		dzmodem(dev,OFF) ;
		return ;
		}
	lpr = (dz_speeds[tp->t_ispeed]<<8) | (dev & 07) ;
	if (tp->t_flags & RAW) lpr |= BITS8 ;
	else lpr |= (BITS7|PENABLE) ;
	if ((tp->t_flags & EVENP) == 0) lpr |= OPAR ;
	if (tp->t_ispeed == 3)  /*  110 baud */
		lpr |= TWOSB ; /* 2 stop bits */
	dzaddr->dzlpr = lpr ;
}
 
/*		*/
 
dzxint(dev)
dev_t dev;
{
	register struct tty *tp ;
	register struct dzregs *dzaddr ;
	register unit ;
 
	dzaddr = dz_addr[dev] ;
	while (dzaddr->dzcsr < 0) { /* Transmit Ready is on */
		unit = (dzaddr->dzcsr >> 8) & 07 ;
		tp = &dz_tty[(dev<<3) | unit] ;
		/* the following is an attempt to fix what appears
		   to be a DZ hardware bug which causes the system
		   to loop here.  Transmitting the NUL should not
		   cause too many problems.... */
		if ((dzaddr->dztcr & (1<<unit)) == 0 ) {
			printf("dzxint,line=%d\n", unit);
			dzaddr->dztbuf = 0;
			continue;
		}
		if (tp->t_state & BUSY) {
			dzaddr->dztbuf = tp->t_char ; /* output the char */
			tp->t_state &= ~BUSY ;
			if (tp->t_line)
				(*linesw[tp->t_line].l_start)(tp);
			else
				dzstart(tp);
			continue ;
			}
		unit = (1<<unit) ;
		dzaddr->dztcr &= (~unit) ; /* Transmit enable off */
		}
}
 
/*		*/
 
dzstart(tp)
register struct tty *tp ;
{
	register unit , c ;
	register struct dzregs *dzaddr ;
	extern ttrstrt() ;
	int sps ;
 
	unit = tp - dz_tty ;
	dzaddr = dz_addr[unit>>3] ;
	unit = 1<<(unit&07) ;
	sps = spl5() ;
	if (tp->t_state & (TIMEOUT|BUSY|TTSTOP)) {
		splx(sps) ;
		return ;
		}
	if ((c = getc(&tp->t_outq)) >= 0) {
		if (c >= 0200 && (tp->t_flags&RAW) == 0) {
			dzaddr->dztcr &= ~unit ;
			tp->t_state |= TIMEOUT ;
			timeout(ttrstrt,tp,(c&0177)+6) ;
			}
		else {
			tp->t_char = c ;
			tp->t_state |= BUSY ;
			dzaddr->dztcr |= unit ;
			}
		if (tp->t_outq.c_cc <= TTLOWAT && tp->t_state&ASLEEP) {
			tp->t_state &= ~ASLEEP ;
			if (tp->t_chan)
				mcstart(tp->t_chan, (caddr_t)&tp->t_outq);
			else wakeup(&tp->t_outq) ;
			}
		}
		splx(sps) ;
}
 
/*		*/
 
dzmodem(dev,flag)
register int dev;
{
	register struct dzregs *dzaddr ;
	register char bit ;
 
	dzaddr = dz_addr[dev>>3] ;
	bit = 1<<(dev&07) ;
	if (flag == OFF) dzaddr->dzdtr &= ~bit ;
	else dzaddr->dzdtr |= bit ;
}
 
/*		*/
 
dzscan()
{
	register i ;
	register struct dzregs *dzaddr ;
	register bit ;
	register struct tty *tp ;
	extern dzscan() ;
 
	for (i = 0 ; i < dz_cnt ; i++) {
		dzaddr = dz_addr[i>>3] ;
		tp = &dz_tty[i] ;
		bit = 1<<(i&07) ;
		if (dzaddr->dzmsr & bit) { /* carrier present */
			if ((tp->t_state & CARR_ON) == 0) {
				wakeup(&tp->t_rawq) ;
				tp->t_state |= CARR_ON ;
				}
			}
		else {
			if ((tp->t_state & CARR_ON)) { /* carrier lost */
				signal(tp->t_pgrp,SIGHUP) ;
				dzaddr->dzdtr &= ~bit ;
				flushtty(tp) ;
				}
			tp->t_state &= ~CARR_ON ;
			}
		}
		timeout(dzscan,0,2*60) ;
}
