/*	vx.c	1.1	85/07/21	*/

#include "vx.h"
#if NVX > 0
/*
 *	VIOC-X driver
 */

#include "../h/param.h"
#include "../h/ioctl.h"
#include "../h/tty.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../machine/pte.h"
#include "../h/buf.h"
#include "../vba/vbavar.h"
#include "../h/conf.h"
#include "../h/file.h"
#include "../h/uio.h"
#include "../vba/vioc.h"
#ifdef VXPERF
#include "../vba/scope.h"
#endif VXPERF
#include "vbsc.h"
#if NVBSC > 0
#include "../bsc/bscio.h"
#include "../bsc/bsc.h"
char bscport[NVXPORTS];
#endif

#ifdef BSC_DEBUG
#include "../bsc/bscdebug.h"
#endif

#ifdef	VX_DEBUG
long vxintr4 = 0;
long vxdebug = 0;
#include "../vba/vxdebug.h"
#endif

#define RSPquals	1

struct	vcx	vcx[NVIOCX] ;
struct	tty	vx_tty[NVXPORTS];
extern struct vcmds v_cmds[];
extern long reinit;

int	vxstart() ;
int	ttrstrt() ;
caddr_t vtoph();
struct	vxcmd	*vobtain() ;
struct	vxcmd	*nextcmd() ;

/*
 * Driver information for auto-configuration stuff.
 * (not tested and probably should be changed)
 */
int	vxprobe(), vxattach(), vxrint();
struct	vba_device *vxinfo[NVIOCX];
long	vxstd[] = { 0 };
struct	vba_driver vxdriver =
	{ vxprobe, 0, vxattach, 0, vxstd, "vioc ", vxinfo };

char vxtype[NVIOCX];	/* 0: viox-x/vioc-b; 1: vioc-bop */
char vxbbno = -1;
char vxbopno[NVIOCX];	/* BOP board no. if indicated by vxtype[] */
extern vbrall();


vxprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct vblok *vp = (struct vblok *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
#endif

	if(badaddr(vp, 1))
		return(0);
	vp->v_fault = 0 ;
	vp->v_vioc = V_BSY ;
	vp->v_hdwre = V_RESET ;		/* reset interrupt */

	DELAY(4000000);
	return ( vp->v_fault == VREADY);
}

vxattach(ui)
	register struct vba_device *ui;
{
	VIOCBAS[ui->ui_unit] = ui->ui_addr;
	vxinit(ui->ui_unit,1);
}

/*
 * Open a VX line.
 */
vxopen(dev, flag)
{
	register struct tty *tp;	/* pointer to tty struct for port */
	register struct vcx *xp;	/* pointer to VIOC-X info/cmd buffer */
	register d;			/* minor device number */
	register long jj;


	d = minor(dev);			/* get minor device number */
	if (d >= NVXPORTS)		/* validate minor device number */
		return ENXIO;		/* set errno to indicate bad port # */
	tp = &vx_tty[d];		/* index the tty structure for port */

	xp = &vcx[d>>4];			/* index VIOC-X info/cmd area */
	d &= 017;

	/* If we did not find a board with the correct port number on
	   it, or the entry for the VIOC-X had no ports on it, inform the
	   caller that the port does not exist. */
	if(!( xp->v_loport <= d && d <= xp->v_hiport )	/* home? */
	 || (xp->v_hiport - xp->v_loport)==0)
		return ENXIO;	/* bad minor device number */
	tp->t_addr = (caddr_t)xp;	/* store address of VIOC-X info */
	tp->t_oproc = vxstart;		/* store address of startup routine */
	tp->t_dev = dev;		/* store major/minor device numbers */
	d = spl8();
	tp->t_state |= TS_WOPEN;	/* mark device as waiting for open */
	if ((tp->t_state&TS_ISOPEN) == 0)	/* is device already open? */
	{				/*  no, open it */
		ttychars(tp);		/* set default control chars */
		if (tp->t_ispeed == 0)	/* if no default speeds set them */
		{
			tp->t_ispeed = SSPEED;	/* default input baud */
			tp->t_ospeed = SSPEED;	/* default output baud */
			tp->t_flags |= (ODDP|EVENP|ECHO); /* default modes */
		}
		vxparam(dev);		/* set parameters for this port */
	}
	splx(d);
	/* ? if already open for exclusive use open fails unless caller is 
	     root. */
	if (tp->t_state&TS_XCLUDE && u.u_uid!=0)
		return EBUSY;	/* device is busy, sorry */

	/* wait for data carrier detect to go high */
	d = spl8();
	if( !vcmodem(dev,VMOD_ON) )
		while( (tp->t_state&TS_CARR_ON) == 0 )
			sleep(&tp->t_canq,TTIPRI);
	jj= (*linesw[tp->t_line].l_open)(dev,tp); /*let tty.c finish the open */
	splx(d);	/* 1/2/85 : assures open complete */
	return (jj);
}

/*
 * Close a VX line.
 */
vxclose(dev, flag)
dev_t dev;
int  flag;
{
	register struct tty *tp;
	register d;

	d = minor(dev) & 0377;
	tp = &vx_tty[d];
	d = spl8();
	(*linesw[tp->t_line].l_close)(tp);
	if ((tp->t_state&TS_ISOPEN) && (tp->t_state&TS_HUPCLS))
		if( !vcmodem(dev,VMOD_OFF) )
			tp->t_state &= ~TS_CARR_ON;
	/* wait for the last response */
	while(tp->t_state & TS_FLUSH)
		sleep( (caddr_t)&tp->t_state, TTOPRI ) ;
	ttyclose(tp);	/* let tty.c finish the close */
	splx(d);
}

/*
 * Read from a VX line.
 */
vxread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &vx_tty[minor(dev) & 0377];
	return (*linesw[tp->t_line].l_read)(tp, uio);
}

/*
 * write on a VX line
 */
vxwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &vx_tty[minor(dev) & 0377];
	return (*linesw[tp->t_line].l_write)(tp, uio);
}

/*
 * VIOCX unsolicited interrupt.
 */
vxrint(n)
register n;				/* mux number */
{
	register struct tty *tp;
	register struct vcx *xp;
	register short *sp;
	register struct vblok *kp;
	register int i, c;
	short *savsilo;
	struct silo {
		char	data;
		char	port;
	};

	kp = VBAS(n);
	xp = &vcx[n];
	switch(kp->v_uqual&037) {
	case 0:
		break;
	case 2:
		printf(" ERR NBR %x\n",kp->v_ustat);
		vpanic("vc: VC PROC ERR");
		vxstreset(n);
		return(0);
	case 3:
		vcmintr(n);
		return(1);
	case 4:
		return(1);
	default:
		printf(" ERR NBR %x\n",kp->v_uqual);
		vpanic("vc: VC UQUAL ERR");
		vxstreset(n);
		return(0);
	}
	if(xp->v_vers == V_NEW) {
		register short *aa ;
		aa = (short *)kp->v_usdata;
		sp = (short *)(*aa  + (char *)kp) ;
	} else {
		c = kp->v_usdata[0] << 6;
		sp = (short *)((char *)kp + SILOBAS + c);
	}
nextsilo:
	i = *(savsilo = sp);
	if (i == 0) return(1);
	if(xp->v_vers == V_NEW)
		if( i > xp->v_silosiz ) {
			printf("vx: %d exceeds silo size\n",i) ;
			i = xp->v_silosiz;
		}
	for(sp++;i > 0;i--,sp++) {
		c = ((struct silo *)sp)->port & 017;
		tp = &vx_tty[c+n*16];
		if(xp->v_loport > c || c > xp->v_hiport)
			continue;	/* port out of bounds */
		if( (tp->t_state & TS_ISOPEN) == 0) {
			wakeup((caddr_t)&tp->t_rawq);
			continue;
		}
		c = ((struct silo *)sp)->data;
		switch(((struct silo *)sp)->port&(PERROR|FERROR)) {
		case PERROR:
		case PERROR|FERROR:
			if( (tp->t_flags&(EVENP|ODDP)) == EVENP
			|| (tp->t_flags & (EVENP|ODDP)) == ODDP )
				continue;
			if(!(((struct silo *)sp)->port&FERROR))
				break;
		case FERROR:
			if(tp->t_flags & RAW) c = 0;
			else c = tp->t_intrc;
		}
		(*linesw[tp->t_line].l_rint)(c, tp);
	}
	*savsilo = 0;
	return(1);
}

/*
 * stty/gtty for VX
 */
vxioctl(dev, cmd, data, flag)
int	dev;			/* major, minor device numbers */
int	cmd;			/* command */
caddr_t	data;
int	flag;
{
	register struct tty	*tp;
	register error;

	tp = &vx_tty[minor(dev) & 0377];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error == 0)
		return error;
	if((error = ttioctl(tp, cmd, data, flag)) >= 0)
	{
		if (cmd==TIOCSETP||cmd==TIOCSETN)
			vxparam(dev);
		return error;
	} else
		return ENOTTY;
}


vxparam(dev)
dev_t	dev;
{
	vxcparam(dev, 1);
}

/*
 * Set parameters from open or stty into the VX hardware
 * registers.
 */
vxcparam(dev, wait)
dev_t	dev;			/* major, minor device numbers */
int wait;			/* nonzero if we should wait for finish */
{
	register struct tty	*tp;
	register struct vcx	*xp;
	register struct vxcmd	*cp;
	register s;

	tp = &vx_tty[minor(dev)];	/* pointer to tty structure for port */
	xp = (struct vcx *)tp->t_addr;	/* pointer to VIOCX info/cmd buffer */
	cp = vobtain(xp);
	s = spl8();
	cp->cmd = LPARAX;		/* set command to "load parameters" */
	cp->par[1] = minor(dev)&017;	/* port number */

	cp->par[2] = (tp->t_flags&RAW)? 0 : tp->t_startc;	/* XON char */
	cp->par[3] = (tp->t_flags&RAW)? 0 : tp->t_stopc;	/* XOFF char */

	if(tp->t_flags&(RAW|LITOUT) ||
	  (tp->t_flags&(EVENP|ODDP)) == (EVENP|ODDP)) {
		cp->par[4] = 0xc0;	/* 8 bits of data */
		cp->par[7] = 0;		/* no parity */
	} else {
		cp->par[4] = 0x40;	/* 7 bits of data */
		if((tp->t_flags&(EVENP|ODDP)) == ODDP)
			cp->par[7] = 1;		/* odd parity */
		else if((tp->t_flags&(EVENP|ODDP)) == EVENP)
			cp->par[7] = 3;		/* even parity */
		else
			cp->par[7] = 0;		/* no parity */
	}
	cp->par[5] = 0x4;			/* 1 stop bit */
	cp->par[6] = tp->t_ospeed;

	if (vcmd(xp->v_nbr, &cp->cmd) && wait)
		sleep(cp,TTIPRI);
	splx(s);
}

/*
 * VIOCX command response interrupt.
 * For transmission, restart output to any active port.
 * For all other commands, just clean up.
 */
vxxint(n,cp)
register int n;			/* VIOC number */
register struct vxcmd	*cp;	/* command structure */
{
	register struct	vxmit	*vp, *pvp;
	register struct	tty	*tp;
	register struct	vcx	*xp;
	register struct tty	*hp;

	xp = &vcx[n];
	cp = (struct vxcmd *)( (long *)cp - 1);
#if NVBSC > 0
	switch(cp->cmd) {
	case MDMCTL1: case HUNTMD1: case LPARAX1:
		vrelease(xp, cp);
		wakeup(cp);
		return;
	}
#endif
	switch(cp->cmd&0xff00) {
	case LIDENT:	/* initialization complete */
		if (xp->v_state & V_RESETTING) {
			vxfnreset(n,cp);
			vinthandl(n,((V_BSY | RSPquals) << 8) | V_INTR);
		}
		cp->cmd++;
		return;
	case XMITDTA: case XMITIMM:
		break;
	case LPARAX:
		wakeup(cp);
	default:	/* MDMCTL or FDTATOX */
		vrelease(xp, cp);
		if (xp->v_state & V_RESETTING) {
			vinthandl(n,((V_BSY | RSPquals) << 8) | V_INTR);
		}
		return;
	}
	for(vp = (struct vxmit *)(cp->par + (cp->cmd & 07)*sizvxmit);
	    vp >= (struct vxmit *)cp->par;
	    vp = (struct vxmit *) ((char *)vp - sizvxmit) )
	{
		tp = &vx_tty[(vp->line & 017)+n*16];
/* cjk buffer bug */
#if NVBSC > 0
					/* bsc change */
		if (tp->t_line == LDISP) {
			vrelease(xp, cp);
			bsctxd((vp->line & 017));
			return ;
		}
					/* End of bsc change */
#endif
/* cjk */
		pvp = vp;
		tp->t_state &= ~TS_BUSY;
		if(tp->t_state & TS_FLUSH) {
			tp->t_state &= ~TS_FLUSH;
			wakeup( (caddr_t)&tp->t_state ) ;
		}
		else
		 	ndflush(&tp->t_outq, vp->bcount+1);
	}
	xp->v_xmtcnt--;
	vrelease(xp,cp);
	if(xp->v_vers == V_NEW) {
		vp = pvp;
		xp->v_actport[(vp->line & 017) - xp->v_loport] |= 1 ;
		if(vxstart(tp) && (cp = nextcmd(xp)) != NULL)
		{
			xp->v_xmtcnt++;
			vcmd(n, &cp->cmd);
			return ;
		}
		xp->v_actport[(vp->line & 017) - xp->v_loport] = 0 ;
		return ;
	}
	xp->v_actflg = 1;
	hp = &vx_tty[xp->v_hiport+n*16];
	for(tp = &vx_tty[xp->v_loport+n*16];tp <= hp;tp++)
		if(vxstart(tp) && (cp = nextcmd(xp)) != NULL)
		{
			xp->v_xmtcnt++;
			vcmd(n, &cp->cmd);
		}
	if( (cp = nextcmd(xp)) != NULL )		/* command to send ? */
	{
		xp->v_xmtcnt++;
		vcmd(n,&cp->cmd);
	}
	xp->v_actflg = 0;
}

/*
 * Force out partial XMIT command after timeout
 */
vxforce(xp)
register struct vcx	*xp;
{
	register struct vxcmd	*cp;
	register int s;

	s = spl8();
	if((cp = nextcmd(xp)) != NULL) {
		xp->v_xmtcnt++;
		vcmd(xp->v_nbr, &cp->cmd);
	}
	splx(s);
}

/*
 * Start (restart) transmission on the given VX line.
 */
vxstart(tp)
register struct tty *tp;
{
	register short nch;
	register struct	vcx	*xp;
	register char *outb;
	register full = 0;
	int k, s, port;

	s = spl8();
	port = minor(tp->t_dev) & 017;
	xp = (struct vcx *)tp->t_addr;
	if (!(tp->t_state&(TS_TIMEOUT|TS_BUSY|TS_TTSTOP))) {
		if (tp->t_outq.c_cc<=TTLOWAT(tp)) {
			if (tp->t_state&TS_ASLEEP) {
				tp->t_state &= ~TS_ASLEEP;
				wakeup((caddr_t)&tp->t_outq);
			}
			if (tp->t_wsel) {
				selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
				tp->t_wsel = 0;
				tp->t_state &= ~TS_WCOLL;
			}
		}
		if(tp->t_outq.c_cc == 0) {
			splx(s);
			return(0);
		}
#ifdef VXPERF
	scope_out(3);
#endif VXPERF
		if(!(tp->t_flags&(RAW|LITOUT)))  
			full = 0200;
		if((nch = ndqb(&tp->t_outq, full)) == 0)   {
			if(full) {
				nch = getc(&tp->t_outq);
				timeout(ttrstrt, (caddr_t)tp, (nch&0177) +6);
				tp->t_state |= TS_TIMEOUT;
				full = 0;
			}
		} else {
			outb = (char *)tp->t_outq.c_cf;
			tp->t_state |= TS_BUSY;
			if(xp->v_vers == V_NEW)
				k = xp->v_actport[port - xp->v_loport] ;
			else
				k = xp->v_actflg ;

			full = vsetq(xp, port, outb, nch);

			if( (k&1) == 0 ) {	/* not called from vxxint */
				if(full || xp->v_xmtcnt == 0) {
					outb = (char *)(&nextcmd(xp)->cmd);
					xp->v_xmtcnt++;
					vcmd(xp->v_nbr, outb );
				} else
					timeout(vxforce,xp,3);
			}
		}
	}
	splx(s);
	return(full);	/* indicate if max commands or not */
}

/*
 * Stop output on a line.
 */
vxstop(tp)
register struct tty *tp;
{
	register  s;

	s = spl8();
	if (tp->t_state & TS_BUSY) {
		if ((tp->t_state&TS_TTSTOP)==0) {
			tp->t_state |= TS_FLUSH;
		}
	}
	splx(s);
}

/*
 * VIOCX Initialization.  Makes free lists of command buffers.
 * Resets all viocx's.  Issues a LIDENT command to each
 * viocx which establishes interrupt vectors and logical
 * port numbers
 */
vxinit(i,wait) 
register int	i;
long wait;
{
	register struct	vcx	*xp;	/* ptr to VIOC-X info/cmd buffer */
	register struct	vblok	*kp;	/* pointer to VIOC-X control block */
	register struct	vxcmd	*cp;	/* pointer to a command buffer */
	register char	*resp;		/* pointer to response buffer */
	register int	j;
	register struct	vcmds	*cpp;
	char type;
	register struct	bsc	*bp;	/* bsc change */
	extern	 struct	bsc	bsc[];


	kp = VBAS(i);		/* get base adr of cntl blok for VIOC */

	xp = &vcx[i];		/* index info/command buffers */
	cpp = &v_cmds[i];
	type = kp->v_ident;
	vxtype[i] =  0;		/* Type is Viox-x */
	switch(type) {
	case VIOCX:
		{
		xp->v_vers = V_OLD ;
		/* set DCD for printer ports */
		for(j = 0;j < 16;j++)
			if (kp->v_portyp[j] == 4 )
				kp->v_dcd |= 1 << j ;
		}
		break ;
	case NWVIOCX:
		{
		xp->v_vers = V_NEW ;
		xp->v_silosiz = kp->v_maxsilo ;
		/* set DCD for printer ports */
		for(j = 0;j < 16;j++)
			if (kp->v_portyp[j] == 4 )
				kp->v_dcd |= 1 << j ;
		}
		break ;
	case PVIOCX:
		xp->v_vers = V_OLD ;
		break ;
	case NPVIOCX:
		xp->v_vers = V_NEW ;
		xp->v_silosiz = kp->v_maxsilo ;
		break ;
#if NVBSC > 0
	case VIOCB:	/* old f/w, Bisync board */
		printf("%X: %x%x OLD VIOC-B, ",
					(long)kp, (int)kp->v_ident,
					(int)kp->v_fault);
		xp->v_vers = V_OLD ;
		/* save device specific info */
		for(bp = &bsc[0]; bp <= &bsc[NBSC]; bp++)
			bp->b_devregs = (caddr_t)xp ;
		printf("%d BSC Ports initialized.\n",NBSC);
		break ;

	case NWVIOCB:	/* new f/w, Bisync board */
		printf("%X: %x%x 16K VIOC-B, ",
					(long)kp, (int)kp->v_ident,
					(int)kp->v_fault);
		xp->v_vers = V_NEW ;
		xp->v_silosiz = kp->v_maxsilo ;
		/* save device specific info */
		for(bp = &bsc[0]; bp <= &bsc[NBSC]; bp++)
			bp->b_devregs = (caddr_t)xp ;
		printf("%d BSC Ports initialized.\n",NBSC);
		if(CBSIZE > kp->v_maxxmt)
			printf("vxinit: Warning CBSIZE > maxxmt\n") ;
		break ;
#endif
	case VBOPID:		/* VIOC-BOP */
		vxbbno++;
		vxtype[i] = 1;
		vxbopno[i] = vxbbno;
		printf("VIOC-BOP no. %d at %lx\n",vxbopno[i],VIOCBAS[i]);
	default:
		return ;	/* Not a viocx type */
	}
	xp->v_nbr = -1;		/* no number for it yet */
	xp->v_maxcmd = xp->v_vers == V_NEW ? 24 : 4;

	for(j=0; j<NVCXBUFS; j++)	/* init all cmd buffers */
	{
		cp = &xp->vx_lst[j];	/* index a buffer */
		cp->c_fwd = &xp->vx_lst[j+1];	/* point to next buf */
	}
	xp->vx_avail = &xp->vx_lst[0];	/* set idx to 1st free buf */
	cp->c_fwd = (struct vxcmd *)0;	/* mark last buf in free list */

	cp = vobtain(xp);	/* grap the control block */
	cp->cmd = LIDENT;	/* set command type */
	cp->par[0] = i * 4 + VCVECT; 	/* ack vector */
	cp->par[1] = cp->par[0] + 1;	/* cmd resp vector */
	cp->par[3] = cp->par[0] + 2;	/* unsol intr vector */
	cp->par[4] = 15;	/* max ports, no longer used */
	cp->par[5] = 0;		/* set 1st port number */
	vcmd(i, &cp->cmd);	/* initialize the VIOC-X */

	if (!wait) return;
	while(cp->cmd == LIDENT);    /* wait for command completion */

 	/* calculate address of response buffer */
 	resp = (char *)kp;
 	resp += kp->v_rspoff & 0x3FFF;
 
	if(resp[0] != 0 && (resp[0]&0177) != 3)	/* did init work? */
	{
		vrelease(xp,cp);	/* init failed */
		return;			/* try next VIOC-X */
	}

	xp->v_loport = cp->par[5];	/* save low port number */
	xp->v_hiport = cp->par[7];/* VIOC knows high port numbr */
	vrelease(xp,cp);	/* done with this control block */
	xp->v_nbr = i;		/* assign VIOC-X board number */
}

/*
 * Obtain a command buffer
 */
struct	vxcmd *
vobtain(xp)
register struct	vcx	*xp;
{

	register struct	vxcmd	*p;
	register s;

	s = spl8();
	p = xp->vx_avail;
	if(p == (struct vxcmd *)0) {
#ifdef VX_DEBUG
		if (vxintr4 & VXNOBUF) vxintr4 &= ~VXNOBUF;
#endif
		vpanic("vx: no buffs");
		vxstreset(xp - vcx);
		splx(s);
		return(vobtain(xp));
	}
	xp->vx_avail = (xp->vx_avail)->c_fwd;
	splx(s);
	return( (struct vxcmd *)p);
}

/*
 * Release a command buffer
 */
vrelease(xp,cp)
register struct	vcx	*xp;
register struct	vxcmd	*cp;
{

	register s;

#ifdef VX_DEBUG
	if (vxintr4 & VXNOBUF) return;
#endif
	s = spl8();
	cp->c_fwd = xp->vx_avail;
	xp->vx_avail = cp;
	splx(s);
}

/*
 * vxcmd - 
 *
 */
struct vxcmd 	*
nextcmd(xp)
register struct	vcx	*xp;
{
	register struct	vxcmd	*cp;
	register int	s;

	s = spl8();
	cp = xp->vx_build;
	xp->vx_build = (struct vxcmd *)0;
	splx(s);
	return(cp);
}

/*
 * assemble transmits into a multiple command.
 * up to 8 transmits to 8 lines can be assembled together
 */
vsetq(xp ,d ,addr, cnt)
register struct	vcx	*xp;
caddr_t	addr;
{

	register struct	vxcmd	*cp;
	register struct	vxmit	*mp;
	register char	*p;
	register i;

	cp = xp->vx_build;
	if(cp == (struct vxcmd *)0) {
		cp = vobtain(xp);
		xp->vx_build = cp;
		cp->cmd = XMITDTA;
	} else {
		if((cp->cmd & 07) == 07) {
			vpanic("vx: vsetq overflow");
			vxstreset(xp->v_nbr);
			return(0);
		}
		cp->cmd++;
	}

	mp = (struct vxmit *)(cp->par + (cp->cmd & 07)*sizvxmit);
	mp->bcount = cnt-1;

	mp->line = d;
	if((xp->v_vers == V_NEW) && (cnt <= 6)) {
		cp->cmd = XMITIMM ;
		p = addr;
		/* bcopy(addr, &(char *)mp->ostream, cnt) ; */
	} else {
		addr = vtoph(0, (caddr_t)addr) ; /* should be a sys address */
		p = (char *)&addr;
		cnt = sizeof addr;
		/* mp->ostream = addr ; */
	}
	for(i=0; i<cnt; i++)
		mp->ostream[i] = *p++;
	if(xp->v_vers == V_NEW)
		return(1) ;
	else
		return((cp->cmd&07) == 7) ;	/* Indicate if full */
}
#endif
