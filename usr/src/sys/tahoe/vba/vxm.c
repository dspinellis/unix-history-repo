/*	vxm.c	1.3	86/01/12	*/

#include "vx.h"
#if NVX > 0
/*
 *	VIOC-X Modem control
 */

#include "param.h"
#include "file.h"
#include "ioctl.h"
#include "tty.h"
#include "conf.h"

#include "../tahoevba/vioc.h"
#include "vbsc.h"
#if NVBSC > 0
#include "../tahoebsc/bscio.h"
#include "../tahoebsc/bsc.h"
extern char bscport[];
#endif


extern	struct	vcx	vcx[] ;
extern	struct	tty	vx_tty[];
extern	struct	vcmds	v_cmds[] ;

extern	int	vxstart() ;
extern	struct	vxcmd	*vobtain() ;
extern	struct	vxcmd	*nextcmd() ;


vcmodem(dev,flag)
dev_t dev ;
{
	struct tty *tp ;
	register struct vxcmd *cp ;
	register struct vcx *xp ;
	register struct vblok *kp ;
	register port ;

	port = minor(dev) ;
	tp = &vx_tty[port] ;
	port &= 017 ;
	xp = (struct vcx *)tp->t_addr ;
	cp = vobtain(xp) ;
	kp = VBAS(xp->v_nbr) ;

	/*
	 * Issue MODEM command
	 */
	cp->cmd = MDMCTL ;
	cp->par[0] = (flag == VMOD_ON) ? V_ENAB : V_DISAB ;
	cp->par[1] = port;
	vcmd(xp->v_nbr, (caddr_t)&cp->cmd) ;
	port -= xp->v_loport ;
	if((kp->v_dcd >> port) & 1) {
		if(flag == VMOD_ON)
			tp->t_state |= TS_CARR_ON ;
		return(1) ;
	}
	return(0) ;
}


/*
 * VCMINTR called when an unsolicited interrup occurs signaling
 * some change of modem control state.
 */
vcmintr(n)
register n ;	/* viocx number */
{
	register struct vblok *kp ;
	register struct tty *tp ;
	register port ;

	kp = VBAS( n ) ;
	port = kp->v_usdata[0] & 017 ;
	tp = &vx_tty[port+n*16] ;

#if NVBSC > 0
			/*
			 * Check for change in DSR for BISYNC port.
			 */
	if ((kp->v_ustat & DSR_CHG) && (bscport[port+n*16] & BISYNC)) {
		register struct	vcx *xp ;
		register struct bsc *bp ;
		extern	 struct	bsc bsc[] ;

		xp = (struct vcx *)tp->t_addr ;
		bp = &bsc[minor(tp->t_dev)] ;
		bp->b_hlflgs &= ~BSC_DSR ;
		if (kp->v_ustat & DSR_ON)
			bp->b_hlflgs |= BSC_DSR ;
/*debug*/printf("BSC DSR Chg: %x\n", kp->v_ustat & DSR_CHG);
	}
	if (bscport[port+n*16] & BISYNC) return;
#endif
	if((kp->v_ustat & DCD_ON) && ((tp->t_state & TS_CARR_ON) == 0) ) {
		tp->t_state |= TS_CARR_ON ;
		wakeup((caddr_t)&tp->t_canq) ;
		return ;
	}

	if((kp->v_ustat & DCD_OFF) && (tp->t_state & TS_CARR_ON)) {
		tp->t_state &= ~TS_CARR_ON ;
		if(tp->t_state & TS_ISOPEN) {
			register struct vcx *xp ;
			register struct vcmds *cp ;
			register struct vxcmd *cmdp ;

			ttyflush(tp, FREAD|FWRITE);
			/* clear all pending trnansmits */
			xp = &vcx[n];
			if(tp->t_state&(TS_BUSY|TS_FLUSH) && xp->v_vers==V_NEW) {
				int i, cmdfound = 0;
				cp = &v_cmds[n];
				for(i = cp->v_empty; i!=cp->v_fill; ) {
					cmdp = (struct vxcmd *)((long *)cp->cmdbuf[i]-1);
					if((cmdp->cmd==XMITDTA || cmdp->cmd==XMITIMM)
					 && ((struct vxmit *)cmdp->par)->line == port) {
						cmdfound++;
						cmdp->cmd = FDTATOX ;
						cmdp->par[1] = port ;
					}
					if(++i >= VC_CMDBUFL)
						i = 0;
				}
				if(cmdfound)
					tp->t_state &= ~(TS_BUSY|TS_FLUSH);
				/* cmd is already in vioc, have to flush it */
				else {
					cmdp = vobtain(xp);
					cmdp->cmd = FDTATOX ;
					cmdp->par[1] = port ;
					vcmd(n, (caddr_t)&cmdp->cmd);
				}
			}
			if((tp->t_flags&NOHANG)==0) {
				gsignal(tp->t_pgrp, SIGHUP) ;
				gsignal(tp->t_pgrp, SIGCONT);
			}
		}
		return ;
	}

	if((kp->v_ustat & BRK_CHR)  && (tp->t_state & TS_ISOPEN) ) {
		(*linesw[tp->t_line].l_rint)(tp->t_intrc & 0377, tp) ;
		return ;
	}
}
#endif
