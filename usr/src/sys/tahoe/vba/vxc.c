/*	vxc.c	1.5	86/01/12	*/

#include "vx.h"
#if NVX > 0
/*
 * VIOC driver
 */
#ifdef VXPERF
#define	DOSCOPE
#endif

#include "param.h"
#include "file.h"
#include "ioctl.h"
#include "tty.h"
#include "errno.h"
#include "time.h"
#include "kernel.h"
#include "proc.h"

#include "../tahoevba/vioc.h"
#include "../tahoesna/snadebug.h"
#include "../tahoevba/scope.h"

#define CMDquals 0
#define RSPquals 1
#define UNSquals 2

extern	struct	vcx	vcx[] ;
extern	struct	tty	vx_tty[];
struct	vcmds	v_cmds[NVIOCX] ;

extern char vxtype[];
extern char vxbbno;
extern char vxbopno[];
#ifdef SNA_DEBUG
extern vbrall();
#endif SNA_DEBUG
extern struct vxcmd *vobtain();

#ifdef VX_DEBUG
#include "../vba/vxdebug.h"
#endif

/*
 *  Write a command out to the VIOC
 */
vcmd(n, cmdad)
register int	n ;
register caddr_t cmdad ;		/* command address */
{

	register struct	vcmds *cp ;
	register struct vcx *xp;
	int	s ;

	s = spl8() ;
	cp = &v_cmds[n] ;
	xp = &vcx[n];
	if (xp->v_state&V_RESETTING && cmdad != NULL) {
		/*
		 * When the vioc is resetting, don't process
		 * anything other than LIDENT commands.
		 */
		register struct vxcmd *cmdp = (struct vxcmd *)
				((char *)cmdad - sizeof(cmdp->c_fwd));
		if (cmdp->cmd != LIDENT) {
			vrelease(xp, cmdp);
			return(0);
		}
	}
	if (cmdad != (caddr_t) 0) {
		cp->cmdbuf[cp->v_fill] = cmdad ;
		if( ++cp->v_fill >= VC_CMDBUFL )  cp->v_fill = 0 ;
		if(cp->v_fill == cp->v_empty) {
			vpanic("vc: CMD Q OVFLO") ;
			vxstreset(n);
			splx(s);
			return(0);
		}
		cp->v_cmdsem++;
	}
	if(cp->v_cmdsem && cp->v_curcnt < vcx[n].v_maxcmd) {
		cp->v_cmdsem--;
		cp->v_curcnt++;
		vinthandl(n, ((V_BSY | CMDquals) << 8) | V_INTR ) ;
	}
	splx(s) ;
	return(1);
}

/*
 * VIOC acknowledge interrupt.  The VIOC has received the new
 * command.  If no errors, the new command becomes one of 16 (max)
 * current commands being executed.
 */
vackint(n)
register n ;		/* VIOC number */
{

	register struct	vblok	*vp ;
	register struct	vcmds	*cp ;
	register s;

	scope_out(5);
	if (vxtype[n]) {	/* Its a BOP */
#ifdef SNA_DEBUG
		if (snadebug & SVIOC)
		printf("vack: interrupt from BOP at VIOC%d,1st vector.\n",n);
		vbrall(n); 	/* Int. from BOP, port 0 */
#endif
		return;
	}
	s = spl8();
	vp = VBAS(n) ;
	cp = &v_cmds[n] ;
	if( vp->v_vcid & V_ERR ) {
		register char *resp;
		register i;
		printf ("INTR ERR type = %x VIOC = %x, v_dcd: %lx\n", 
			vp->v_vcid & 07, n, vp->v_dcd & 0xff);
		/* resp = (char *)vp + (vp->v_rspoff & 0x7FFF); */
		resp = (char *)(&vcx[n])->v_mricmd;
		for(i=0; i<16; i++)
			printf("%x ", resp[i]&0xff);
		vpanic( "\nvcc: vackint") ;
		splx(s);
		vxstreset(n);
		return ;
	} else
	if((vp->v_hdwre&017) == CMDquals)  {
#ifdef VX_DEBUG
		if (vxintr4 & VXERR4) {	/* causes VIOC INTR ERR 4 */
			register struct vxcmd *cp1;
			register struct vxcmd *cp0 = (struct vxcmd *)
				((long)cp->cmdbuf[cp->v_empty] - 4);
			if ((cp0->cmd == XMITDTA) || (cp0->cmd == XMITIMM)) {
				cp1 = vobtain(&vcx[n]);
				*cp1 = *cp0;
				vxintr4 &= ~VXERR4;
				(void) vcmd(n,&cp1->cmd);
			}
		}
#endif
		cp->v_curcmd[vp->v_vcid & VCMDLEN-1] = cp->cmdbuf[cp->v_empty] ;
		if( ++cp->v_empty >= VC_CMDBUFL )  cp->v_empty = 0 ;
	}
	if( ++cp->v_itrempt >= VC_IQLEN ) cp->v_itrempt = 0 ;
	vintempt(n) ;
	splx(s);
	(void) vcmd(n, (caddr_t)0);	/* queue next cmd, if any */
}

/*
 *  Command Response interrupt.  The Vioc has completed
 *  a command.  The command may now be returned to
 *  the appropriate device driver .
 */
vcmdrsp(n)
register n ;
{

	register struct	vblok	*vp ;
	register struct	vcmds	*cp ;
	register caddr_t cmd ;
	register char *resp ;
	register k ;
	register int s ;

	scope_out(6);
	if (vxtype[n]) {	/* Its a BOP */
		printf("vcmdrsp: stray interrupt from BOP at VIOC%d...\n",n);
		return;
	}
	s = spl8();
	vp = VBAS(n) ;
	cp = &v_cmds[n] ;
	resp = (char *)vp;
	resp += vp->v_rspoff & 0x7FFF;

	if( (k=resp[1]) & V_UNBSY )  {
		k &= VCMDLEN-1;
		cmd = cp->v_curcmd[k];
		cp->v_curcmd[k] = (caddr_t)0;
		cp->v_curcnt--;
		k = *((short *)&resp[4]);	/* cmd operation code */
		if((k & 0xFF00) == LIDENT) {	/* want hiport number */
			for(k=0; k<VRESPLEN; k++)
				cmd[k] = resp[k+4];
		}
		resp[1] = 0;
		vxxint(n, (struct vxcmd *)cmd) ;
		if ((&vcx[n])->v_state == V_RESETTING) return;
	}
	else {
		vpanic( "vc, cmdresp debug") ;
		splx(s);
		vxstreset(n);
		return;
	}

	vinthandl(n, ( (V_BSY | RSPquals) << 8 ) | V_INTR ) ;
	splx(s);

}


/*
 * Unsolicited interrupt.
 */
vunsol(n)
register(n) ;
{

	register struct	vblok	*vp ;
	register s;

	scope_out(1);
	if (vxtype[n]) {	/* Its a BOP */
		printf("vunsol: stray interrupt from BOP at VIOC%d...\n",n);
		return;
	}
	s = spl8();
	vp = VBAS(n) ;
	if(vp->v_uqual & V_UNBSY) {
		vxrint(n) ;
		vinthandl(n, ( (V_BSY | UNSquals) << 8 ) | V_INTR ) ;
#ifdef notdef
	} else {
		vpanic("vc: UNSOL INT ERR") ;
		splx(s);
		vxstreset(n);
#endif
	}
	splx(s);
}

/*
 * Enqueue an interrupt
 */
vinthandl(n, item)
register int n ;
register item ;
{

	register struct  vcmds *cp ;
	register int	empflag = 0 ;

	cp = &v_cmds[n] ;
	if( cp->v_itrfill == cp->v_itrempt ) empflag++ ;
	cp->v_itrqueu[cp->v_itrfill] = item ;
	if( ++cp->v_itrfill >= VC_IQLEN ) cp->v_itrfill = 0 ;
	if(cp->v_itrfill == cp->v_itrempt) {
		vpanic( "vc: INT Q OVFLO" ) ;
		vxstreset(n);
	}
	else if( empflag ) vintempt(n) ;
}

vintempt(n)
register int n ;
{
	register  struct  vcmds *cp ;
	register  struct  vblok *vp ;
	register  short   item ;
	register  short	*intr ;

	vp = VBAS(n) ;
	if(vp->v_vioc & V_BSY) return ;
	cp = &v_cmds[n] ;
	if(cp->v_itrempt == cp->v_itrfill) return ;
	item = cp->v_itrqueu[cp->v_itrempt] ;
	intr = (short *)&vp->v_vioc ;
	switch( (item >> 8) & 03 ) {

	case CMDquals:		/* command */
		{
		int phys;

		if(cp->v_empty == cp->v_fill || vp->v_vcbsy&V_BSY)
			break;
		(&vcx[n])->v_mricmd = (caddr_t)cp->cmdbuf[cp->v_empty];
		phys = vtoph((struct proc *)0, (unsigned)cp->cmdbuf[cp->v_empty]) ; /* should be a sys address */
		vp->v_vcp[0] = ((short *)&phys)[0];
		vp->v_vcp[1] = ((short *)&phys)[1];
		vp->v_vcbsy = V_BSY ;
		*intr = item ;
		}
		scope_out(4);
		break ;

	case RSPquals:		/* command response */
		*intr = item ;
		scope_out(7);
		break ;

	case UNSquals:		/* unsolicited interrupt */
		vp->v_uqual = 0 ;
		*intr = item ;
		scope_out(2);
		break ;
	}
}


/* start a reset on a vioc after error (hopefully) */
vxstreset(n)
	register n;
{
	register struct vcx *xp;
	register struct	vblok *vp ;
	register struct vxcmd *cp;
	register int j;
	extern int vxinreset();
	int	s ;

	s = spl8() ;
	vp = VBAS(n);
	xp = &vcx[n];

	if (xp->v_state&V_RESETTING)
		/*
		 * Avoid infinite recursion.
		 */
		return;

	/*
	 * Zero out the vioc structures, mark the vioc as being
	 * reset, reinitialize the free command list, reset the vioc
	 * and start a timer to check on the progress of the reset.
	 */
	bzero((caddr_t)&v_cmds[n], (unsigned)sizeof (struct vcmds));
	bzero((caddr_t)xp, (unsigned)sizeof (struct vcx));

	/*
	 * Setting V_RESETTING prevents others from issuing
	 * commands while allowing currently queued commands to
	 * be passed to the VIOC.
	 */
	xp->v_state |= V_RESETTING;
	for(j=0; j<NVCXBUFS; j++)	/* init all cmd buffers */
	{
		cp = &xp->vx_lst[j];	/* index a buffer */
		cp->c_fwd = &xp->vx_lst[j+1];	/* point to next buf */
	}
	xp->vx_avail = &xp->vx_lst[0];	/* set idx to 1st free buf */
	cp->c_fwd = (struct vxcmd *)0;	/* mark last buf in free list */

	printf("resetting VIOC %x .. ", n);

	vp->v_fault = 0 ;
	vp->v_vioc = V_BSY ;
	vp->v_hdwre = V_RESET ;		/* reset interrupt */

	timeout(vxinreset, (caddr_t)n, hz*5);
	splx(s);
	return;
}

/* continue processing a reset on a vioc after an error (hopefully) */
vxinreset(vioc)
caddr_t vioc;
{
	register int n = (int)vioc;
	register struct	vblok *vp ;
	int s = spl8();
printf("vxinreset ");

	vp = VBAS(n);

	/*
	 * See if the vioc has reset.
	 */
	if (vp->v_fault != VREADY) {
		printf("failed\n");
		splx(s);
		return;
	}

	/*
	 * Send a LIDENT to the vioc and mess with carrier flags
	 * on parallel printer ports.
	 */
	vxinit(n, (long)0);
	splx(s);
}

/*
 * Restore modem control, parameters and restart output.
 * Since the vioc can handle no more then 24 commands at a time
 * and we could generate as many as 48 commands, we must do this in
 * phases, issuing no more then 16 commands at a time.
 */
/* finish the reset on the vioc after an error (hopefully) */
vxfnreset(n, cp)
register int n;
register struct vxcmd *cp;
{
	register struct vcx *xp;
	register struct	vblok *vp ;
	register struct tty *tp;
	register int i;
#ifdef notdef
	register int on;
#endif
	extern int vxrestart();
	int s = spl8();
printf("vxfnreset ");

	vp = VBAS(n);
	xp = &vcx[n];

	xp->v_loport = cp->par[5];	/* save low port number */
	xp->v_hiport = cp->par[7];/* VIOC knows high port numbr */
	vrelease(xp,cp);	/* done with this control block */
	xp->v_nbr = n;		/* assign VIOC-X board number */

	xp->v_state &= ~V_RESETTING;

	vp->v_vcid = 0;

	/*
	 * Restore modem information and control.
	 */
	for(i=xp->v_loport; i<=xp->v_hiport; i++) {
		tp = &vx_tty[i+n*16];
		if (tp->t_state&(TS_ISOPEN|TS_WOPEN)) {
			tp->t_state &= ~TS_CARR_ON;
			vcmodem(tp->t_dev, VMOD_ON);
			if (tp->t_state&TS_CARR_ON)  {
				wakeup((caddr_t)&tp->t_canq) ;
			}
			else {
				if(tp->t_state & TS_ISOPEN) {
					ttyflush(tp, FREAD|FWRITE);
					if(tp->t_state&TS_FLUSH)
						wakeup((caddr_t)&tp->t_state) ;
					if((tp->t_flags&NOHANG)==0) {
						gsignal(tp->t_pgrp, SIGHUP) ;
						gsignal(tp->t_pgrp, SIGCONT);
					}
				}
			}
		}
		/*
		 * If carrier has changed while we were resetting,
		 * take appropriate action.
		 */
#ifdef notdef
		on = vp->v_dcd & 1<<i;
		if (on && (tp->t_state&TS_CARR_ON) == 0) {
			tp->t_state |= TS_CARR_ON ;
			wakeup((caddr_t)&tp->t_canq) ;
		} else if (!on && tp->t_state&TS_CARR_ON) {
			tp->t_state &= ~TS_CARR_ON ;
			if(tp->t_state & TS_ISOPEN) {
				ttyflush(tp, FREAD|FWRITE);
				if(tp->t_state&TS_FLUSH)
					wakeup((caddr_t)&tp->t_state) ;
				if((tp->t_flags&NOHANG)==0) {
					gsignal(tp->t_pgrp, SIGHUP) ;
					gsignal(tp->t_pgrp, SIGCONT);
				}
			}
		}
#endif
	}

	xp->v_state |= V_RESETTING;

	timeout(vxrestart, (caddr_t)n, hz);
	splx(s);
}

/*
 * Restore a particular aspect of the VIOC.
 */
vxrestart(vioc)
caddr_t vioc;
{
	register struct tty *tp, *tp0;
	register struct vcx *xp;
	register int i, cnt;
	register int n = (int)vioc;
	int s = spl8();

	cnt = n>>8;
printf("vxrestart %d ",cnt);
	n &= 0xff;

	tp0 = &vx_tty[n*16];
	xp = &vcx[n];

	xp->v_state &= ~V_RESETTING;

	for(i=xp->v_loport; i<=xp->v_hiport; i++) {
		tp = tp0 + i;
		if (cnt != 0) {
			tp->t_state &= ~(TS_BUSY|TS_TIMEOUT);
			if(tp->t_state&(TS_ISOPEN|TS_WOPEN))	/* restart pending output */
				vxstart(tp);
		} else {
			if (tp->t_state&(TS_WOPEN|TS_ISOPEN))
				vxcparam(tp->t_dev, 0);
		}
	}

	if (cnt == 0) {
		xp->v_state |= V_RESETTING;
		timeout(vxrestart, (caddr_t)(n + 1*256), hz);
	} else
		printf("done\n");
	splx(s);
}

vxreset(dev)
dev_t dev;
{
	vxstreset(minor(dev)>>4);	/* completes asynchronously */
}

vxfreset(n)
register int n;
{

	if (n < 0 || n > NVX || VBAS(n) == NULL)
		return(ENODEV);
	vcx[n].v_state &= ~V_RESETTING;
	vxstreset(n);
	return(0);		/* completes asynchronously */
}
#endif

