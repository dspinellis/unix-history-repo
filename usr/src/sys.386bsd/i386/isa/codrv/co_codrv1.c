/*      Copyright 1992 by Holger Veit
 *	May be freely used with Bill Jolitz's port of 
 *	386bsd and may be included in a 386bsd collection
 *	as long as binary and source are available and reproduce the above
 *	copyright. This includes special software collections which
 *	are derived from 386bsd, such as the so-called NetBSD.
 *
 *	You may freely modify this code and contribute improvements based
 *	on this code as long as you don't claim to be the original author.
 *	Commercial use of this source requires permittance of the copyright 
 *	holder. A general license for 386bsd will override this restriction.
 *
 *	Use at your own risk. The copyright holder or any person who makes
 *	this code available for the public (administrators of public archives
 *	for instance) are not responsible for any harm to hardware or software
 *	that might happen due to wrong application or program faults.
 *
 *	@(#) $RCSfile: co_codrv1.c,v $	$Revision: 1.6 $ (Contributed to 386bsd) $Date: 93/01/23 23:14:27 $
 *
 * This file processes the vga/kbdioctls
 *
 * History: see CO_HISTORY
 *
 */
static char *rcsid = "$Header: /usr/src/sys.386bsd/i386/isa/codrv/RCS/co_codrv1.c,v 1.6 93/01/23 23:14:27 root Exp Locker: root $";

#define COMPAT_CO011
#include "co_hdr.h"

/**************************************************************************
 *
 * This file serves as a plug-in interface for IOCTL processing.
 *
 * This file has been modified to understand most of the CODRV1 and
 * CODRV2 ioctls which have been tested long in codrv-0.1.2-ALPHA.
 * For some reasons, the CODRV2 font interface will be revised
 * again. It is not integrated here. The other ioctls are available.
 *
 * This file has four public entry points:
 *
 * coioctl_init()			(called by coattach, initialization)
 * consioctl(dev,cmd,addr,flag)		(serves /dev/console specific things)
 * kbdioctl(dev,cmd,addr,flags)		(serves /dev/kbd specific things)
 * vgaioctl(dev,cmd,addr,flags)		(serves /dev/vga specific things)
 *
 * In CODRV1 these functions were available with any console device,
 * be it /dev/console, /dev/kbd, /dev/vga
 *
 * This ioctl package contains its own set of service functions, that
 * were in co_kbd.c and co_vga.c before, with the exception of the 
 * keyboard overload subsystem, which is still in co_kbd.c.
 * 
 * Programmers are invited to provide different interfaces, such as
 * SVR3/SVR4/SUN-OS. There will be hopefully fully runtime-replacable
 * kernel modules, which can make use of this.
 *
 ***********************************************************************/

#ifdef CODRV1
#ifndef MINITERM

/*
 *	initialize ioctl system
 */
void coioctl_init()
{
	cons_capabilities.info1 |= (CONS_CODRV1|CONS_CODRV2);
}	

/*
 *	process /dev/console ioctls
 */
int consioctl(dev_t dev, int cmd, caddr_t data, int flag)
{
	register error;
	register struct tty *tp = dev2tty(dev);

	if (!tp) return ENXIO;

	/* mandatory */
	if (cmd==CONSGINFO) {
		*((struct consinfo*)data) = cons_capabilities;
		return 0;
 	} else if (cmd==OLDCONSGINFO) {
		((struct oldconsinfo*)data)->info1 = cons_capabilities.info1;
		return 0;
	}

	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	/* must be executed for backward compatibility
	 * must be executed *after* the tty ioctls,
	 * because it shares an essential ioctl with them.
	 */
	error = kbdioctl(dev, cmd, data, flag);
	return error==0 ? 0 : ENOTTY;
}

/**********************************************************************
 *  utility functions for kbdioctl
 *********************************************************************/

static struct vty *n2vty(int n) 
{
	if (n == KBD_ACTVTY) return actvty;
	if (n >= 0 && n < nvty) return &vtys[n];
	return 0;
}

static void kioc_setlockkeys(dev_t dev,int snc)
{
	register struct vty *vp = dev2vty(dev);
	if (!vp) {
		register int i;
		for (i=0; i<nvty; i++) {
			vp = &vtys[i];
			vp->scroll = snc & 1;
			vp->num = (snc & 2) ? 1 : 0;
			vp->caps = (snc & 4) ? 1 : 0;
		}
		vp = actvty;
	}
 	vp->scroll = snc & 1;
	vp->num = (snc & 2) ? 1 : 0;
	vp->caps = (snc & 4) ? 1 : 0;
	kbd_setleds (leds (vp));
}

static int kioc_assignleds(dev_t dev,int param)
{
	u_char ostate;
	struct vty *vp;
	register int i;

	if (param&4) {
		/* used for initialization */
		for (i=0; i<nvty; i++) {
			vp = &vtys[i];
			vp->altgrled = (param&3)==2;
			vp->shiftled = (param&3)==1;
		}
	} else
	{
		vp = dev2vty(dev);
		if (!vp) vp=actvty;
		vp->altgrled = param==2;
		vp->shiftled = param==1;
	}

	vp = actvty;
	kbd_setleds (leds (vp));
	ostate = vp->altgrled ? 2 : vp->shiftled ? 1 : 0;
	return (ostate);
}

static int kioc_dobeep(struct kbd_bell *data)
{
	if (data) {
		int p, d;

		kbd_cvtsound(data->pitch, &p, data->duration, &d);
		sysbeep (p, d);
	} else
		sysbeep (kbs.pitch, kbs.duration);
	return 0;
}

static int kioc_getbeep2(struct kbd_bell *data)
{
	struct vty *vp;
	
	if (data->nr == KBD_DEFLT) {
		data->pitch = 1193180 / kbs.pitch;
		data->duration = kbs.duration / hz * 1000;
		return 0;
	}
	
	vp = n2vty(data->nr);
	if (!vp) return EINVAL;

	data->pitch = 1193180 / vp->pitch;
	data->duration = vp->duration / hz * 1000;
	return 0;
}

static int kioc_setbeep2(struct kbd_bell *data)
{
	struct vty *vp;

	if (data->nr == KBD_DEFLT) {
		kbd_cvtsound(data->pitch, &kbs.pitch,
			data->duration, &kbs.duration);
		return 0;
	}

	vp = n2vty(data->nr);
	if (!vp) return EINVAL;

	kbd_cvtsound(data->pitch, &vp->pitch, data->duration, &vp->duration);
	return 0;
}

/*
 *	process keyboard ioctls
 */

static int ofl = 0;

int kbdioctl(dev_t dev, int cmd, caddr_t data, int flag)
{
	int s,n,error;
	struct vty *vp = dev2vty(dev);

	switch (cmd) {
	/* CONSGINFO is mandatory ! */
	case CONSGINFO:
		*((struct consinfo*)data) = cons_capabilities;
		return 0;
	case OLDCONSGINFO:
		((struct oldconsinfo*)data)->info1 = cons_capabilities.info1;
		return 0;
	case KBDFORCEASCII:
		/* this is a relic from my youth mistakes */
		return 0;
	case KBDCOLDRESET:
		kbd_coldreset();
		kbd_ovlinit();
		kbd_settpmrate(KBD_TPD500|KBD_TPM100);
		kioc_setlockkeys(dev,0);
		return 0;
	case KBDWARMRESET:
		kbd_warmreset();
		return 0;
	case KBDGTPMAT:
		*(int *)data = kbs.tpmrate;
		return 0;
	case KBDSTPMAT:
		kbd_settpmrate(*(int *)data);
		return 0;
	case KBDGREPSW:
		*(int *)data = kbs.repeat;
		return 0;
	case KBDSREPSW:
		kbs.repeat = (*(int *)data) & 1;
		return 0;
	case KBDGLEDS:
		*(int *)data = kbs.ledstate;
		return 0;
	case KBDSLEDS:
		kbd_setleds(*(int *)data);
		return 0;
	case KBDGLOCK:
		if (!vp) vp=actvty;
		*(int *)data =	vp->scroll | (vp->num<<1) |
				(vp->caps<<2) | (vp->altgrlock<<3) |
				(vp->shiftlock<<4);
		return 0;
	case KBDSLOCK:
		kioc_setlockkeys (dev,*(int *) data);
		return 0;
	case KBDSCAPSLED: /* assign CapsLock LED to AltgrLock or ShiftLock */
		*(int *) data = kioc_assignleds (dev, *(int *)data);
		return 0;
	case KBDGCAPSLED:
		if (!vp) vp = actvty;
		*(int *) data = vp->altgrled ? 2 : 
				vp->shiftled ? 1 : 0;
		return 0;
	case OLDKBDSETBEEP:
		return kbd_cvtsound(((struct kbd_sound*)data)->pitch, &kbs.pitch, 
			((struct kbd_sound*)data)->duration, &kbs.duration);
	case KBDGETBEEP:
		return kioc_getbeep2((struct kbd_bell*)data);
	case KBDSETBEEP:
		return kioc_setbeep2((struct kbd_bell*)data);
	case KBDBELL:
	case KBDSETBELL: /* compatibility */
		return kioc_dobeep((struct kbd_bell*)data);
	case KBDGCKEY:
		return kbd_getckeydef (((Ovl_tbl *)data)->keynum, (Ovl_tbl *) data);
	case OLDKBDGCKEY:
		{
			int stat;
			Ovl_tbl ok;
			stat = kbd_getckeydef (ok.keynum, &ok);
#if XCHAR != u_char
ERROR! FIX ME!
#else
			if (!stat)
				bcopy(&ok,data,sizeof(struct oldkbd_ovlkey));
#endif
			return stat;
		}
	case KBDSCKEY:
		return kbd_setkeydef((Ovl_tbl *)data);
	case KBDGOKEY:
		return kbd_getokeydef (((Ovl_tbl *)data)->keynum, (Ovl_tbl *) data);
	case KBDRMKEY:
		return kbd_rmkeydef (*(int *) data);
	case KBDDEFAULT:
		kbd_ovlinit();
		return 0;
	case KBDSCLRLYR:
		kbs.a0flag = (*(int*)data & 1) != 0;
		kbs.c0flag = (*(int*)data & 2) != 0;
		kbs.m0flag = (*(int*)data & 4) != 0; /*not used yet*/
		return 0;
	case KBDGSPECF:
		return kbd_gethotkey((struct kbd_hotkey *)data);
	case KBDSSPECF:
		return kbd_sethotkey((struct kbd_hotkey *)data);

	/* and now some special features which are cheap */
	case FIONREAD:
		s = spltty();
		n = RB_LEN(&co_buf);
		splx(s);
		*(off_t*)data = n;
		return 0;

	case TIOCSPGRP:
		consoftc.cs_pgid = *(int*)data;
		return 0;

	case TIOCGPGRP:
		*(int*)data = consoftc.cs_pgid;
		return 0;

	default:
		/* backward compatibility */
		error = vgaioctl(dev, cmd, data, flag);
		if (!error) return 0;
		else return ENOTTY;
	}
	/*NOTREACHED*/
}

/**********************************************************************
 *  utility functions for vgaioctl
 *********************************************************************/

static int chklim(struct vty *vp,int x0,int x1, int y0, int y1)
{
	if (x0 < 0 || x0 >= vp->ncol ||
	    y0 < 0 || y0 >= vp->nrow ||
	    x1 < 0 || x1 >= vp->ncol ||
	    y1 < 0 || y1 >= vp->nrow ||
	    x0 > x1 || y0  > y1)
	    	return 1;
	return 0;
}

static int cpyblk(u_char *from, u_char *to, int mode,
		  int x0, int x1, int y0, int y1, int ncol)
{
	register	x,y,n;
	u_char		*ofs,*ofs2;
	u_short		*ofs1;

	/* copy the requested data, could be optimized */

	ofs = from;
	switch (mode) {
	case VGA_ATTR:
		ofs++;
		/*FALLTHRU*/

	case VGA_TEXT:
		ofs2 = ofs + y0*ncol*2;
		for (n=0, y=y0; y<=y1; y++, ofs += (ncol*2) ) {
			for (ofs2=ofs,x=x0; x<=x1; x++,n++,ofs2+=2)
				*(to+n) = *ofs2;
		}
		break;

	case VGA_BOTH:
		ofs1 = ((u_short*)from) + x0;
		for (y=y0; y<=y1; y++, ofs1 += ncol)
			bcopy(ofs1, to, (x1-x0+1)*2);
		break;

	default:
		return EINVAL;
	}
	return 0;
}

static int getsz(struct vty *vp,struct vga_block *p)
{
	int sz;

	/* how much */
	if ((p->mode & 0x03) == VGA_SCREEN) {
		p->x0 = 0;
		p->y0 = 0;
		p->x1 = vp->ncol-1;
		p->y1 = vp->nrow-1;
		sz = vp->size;
	}
	else {
		if (chklim(vp,p->x0,p->x1,p->y0,p->y1))
			return 0;
		sz = (p->x1-p->x0+1) * (p->y1-p->y0+1);
	}

	if ((p->mode & 0x30) == VGA_BOTH)
		sz *= 2;

	return sz;
}

static int vioc_vgagetblock(struct vga_block *p)
{
	u_char		*data = 0;
	struct vty	*vp;
	int		sz,x0,y0,x1,y1;
	int		error;

	/* check vty */
	vp = n2vty(p->pagenum);
	if (!vp) return EINVAL;
	
	/* get size */
	if (!(sz=getsz(vp, p))) return EINVAL;
	
	data = (u_char*)malloc(sz, M_IOCTLOPS, M_WAITOK);

	/* copy the data into buffer */
	error = cpyblk(	(u_char*)vp->Crtat, data, p->mode&0x30,
			x0, x1, y0, y1, vp->ncol);

	/* to user process */
	if (!error) error = copyout(data, p->map, sz);

	/* clean up */
	free(data,M_IOCTLOPS);
	return error;
}

static int vioc_vgasetblock(struct vga_block *p)
{
	u_char		*data = 0;
	struct vty	*vp;
	int		sz,x0,y0,x1,y1;
	int		error;

	/* check vty */
	vp = n2vty(p->pagenum);
	if (!vp) return EINVAL;
	
	/* get size */
	if (!(sz=getsz(vp, p))) return EINVAL;
	
	data = (u_char*)malloc(sz, M_IOCTLOPS, M_WAITOK);

	error = copyin(p->map,data,sz);

	/* copy the requested data */
	error = cpyblk (data, (u_char*)vp->Crtat, p->mode&0x30,
			x0, x1, y0, y1, vp->ncol);

	/* clean up */
	free(data,M_IOCTLOPS);
	return error;
}

static int vioc_gettextpage(struct textpage *tp)
{
	int 		i;
	u_char	*p = ((u_char*)Crtat) + tp->ad;

	if (tp->ad > 1 || tp->pagenum > 7) return EINVAL;
	
	for (i=0; i<actvty->size; i++, p+=2) 
		tp->map[i] = *p;
	return 0;
}

static int vioc_settextpage(struct textpage *tp)
{
	int 		i;
	u_char	*p = ((u_char*)Crtat) + tp->ad;

	if (tp->ad > 1 || tp->pagenum > 7) return EINVAL;
	
	for (i=0; i<actvty->size; i++,p+=2) 
		*p = tp->map[i];
	return 0;
}

static int vioc_oldgetfontchar(struct fontchar *fc)
{
	int pg = fc->page;
	u_char *ofs = (u_char*)Crtat + pg*0x4000;

	if (vds.cardtype < VG_EGA || pg < 0 || pg > 7) return EINVAL;

	vga_enablecg(pg);
	bcopy(ofs+(fc->idx<<5),fc->cmap,VGA_FNTCSIZE);
	vga_disablecg();

	return 0;
}	

static int vioc_oldsetfontchar(struct fontchar *fc)
{
	int pg = fc->page;
	u_char *ofs = (u_char*)Crtat + pg*0x4000;

	if (vds.cardtype < VG_EGA || pg < 0 || pg > 7) return EINVAL;

	vga_enablecg(pg);
	bcopy(fc->cmap,ofs+(fc->idx<<5),VGA_FNTCSIZE);
	vga_disablecg();

	return 0;
}	

static int vioc_miscfunctions(struct miscfcns *f) 
{
	switch(f->cmd) {
	case 1:
		vds.encoding[1] = NOFONT;
		outw(0x3c4,0x0003);	/* SA=0,SB=0 */
		break;
	case 2:
		f->u.enc[0] = vds.encoding[0];
		f->u.enc[1] = vds.encoding[1];
		break;
	case 3:
		vds.scrtimeout = f->u.timeout;	/* seconds */
		vga_doblanking(BLANKSTART);
		break;
	case 4:
		f->u.timeout =  (vds.scrtimeout&0x3FFFFFFF) |
				((u_long)(vds.blanking&3)<<30);
		break;
	default:
		return EINVAL;
	}
	return 0;
}

static int vioc_oldsetfontmap(struct fontmap *data)
{
	int i,k;
	int pg = data->page;
	u_char *ofs = (u_char*)Crtat+pg*0x4000;

	if (vds.cardtype < VG_EGA || pg < 0 || pg > 7) return EINVAL;
	vds.encoding[pg] = data->encoding;

	vga_enablecg();
	bzero(ofs,8192);
	for(i=k=0; i<(VGA_FNTCSIZE*VGA_FNTNCHARS); k+=32,i+=VGA_FNTCSIZE) {
		bcopy(&data->map[i],ofs+k,VGA_FNTCSIZE);
	}
	vga_disablecg();

	/* enable SB/SBH when font 1 is loaded */
	if (pg==1 && data->encoding != NOFONT) outw(0x3c4,0x0403);	/* SA=1,SB=0 */

	return 0;
}

static int vioc_oldgetfontmap(struct fontmap *data)
{
	int i,k;
	int pg = data->page;
	u_char *ofs = (u_char*)Crtat + pg*0x4000;
	if (vds.cardtype < VG_EGA || pg < 0 || pg > 7) return EINVAL;

	data->encoding = vds.encoding[pg];

	vga_enablecg(pg);
	for(i=k=0; i<(VGA_FNTCSIZE*VGA_FNTNCHARS); k+=32,i+=VGA_FNTCSIZE)
		bcopy(ofs+k,&data->map[i],VGA_FNTCSIZE);
	vga_disablecg();

	return 0;
}

#define	FSPACE 256*32
#define FCHUNKS 32

static int vioc_setfontmap(struct fmap *f)
{
	u_char *ofs = (u_char*)Crtat + f->page*0x4000;
	u_char *buf;
	struct fchar *cibuf,fc[FCHUNKS];
	int error = 0;
	int i,j,k;
	int ec;

	/* XXX graphics */
	if (vds.cardtype < VG_EGA || f->page < 0 || f->page > 1 ||
		f->x > 9 || f->y > 16 || f->nr > 256) return EINVAL;

	if (f->nr > 0) {
		vds.f89bit = vds.cardtype >= VG_EGA ? 9 : 8;
		buf = (u_char*)malloc(FSPACE, M_IOCTLOPS, M_WAITOK);
		bzero(buf, FSPACE);

		/* fill the buffer */
		cibuf = f->fntmap;
		for (i = f->nr; i > 0; i -= FCHUNKS, cibuf+=FCHUNKS) {
			j = i >= FCHUNKS ? FCHUNKS : i;
			error = copyin(cibuf, fc, j*sizeof(struct fchar));
			if (error) goto erexit;
			for (k=0; k<j; k++) {
				ec = fc[k].encoding & 0xFF;
				bcopy(fc[k].map,buf+(ec<<5),f->y);
			}
		}

		/* set the extension bit (this is really switched in
		 * vga_disablecg()
		 */
		if (f->x < 9)
			vds.f89bit = 8;

		/* move the data into CG space */
		vga_enablecg();
		bcopy(buf, ofs, FSPACE);
		vga_disablecg();
	}

	/* this is a hack, as long as XCHAR == u_char */
	switch (f->start) {
	case 0:	
		i = XLAT2PC8; break;
	case 1: 
		i = NOFONT; break;
	default:
	case 2: 
		i = 0; break;
	}
	vds.encoding[f->page] = (u_short)i;

	/* enable SB/SBH when font 1 is loaded */
	if (f->page==1) {
		if (f->nr > 0)	outw(0x3c4,0x0403);	/* SA=1,SB=0 */
		else 		outw(0x3c4,0x0003);
	}

erexit:
	/* clean up */
	free(buf,M_IOCTLOPS);

	return error;
}

static int vioc_getfontmap(struct fmap *f)
{
	u_char *ofs = (u_char*)Crtat + f->page*0x4000;
	u_char *buf;
	struct fchar *cobuf,fc[FCHUNKS];
	int error = 0;
	int i,j,k;

	/* XXX graphics */
	if (vds.cardtype < VG_EGA || f->page < 0 || f->page > 1 ||
	    f->nr > 0 && (f->x > 9 || f->y > 16) || (f->nr+f->start) > 256) return EINVAL;

	if (f->nr > 0) {
		buf = (u_char*)malloc(FSPACE, M_IOCTLOPS, M_WAITOK);

		/* move the data from CG space */
		vga_enablecg();
		bcopy(ofs, buf, FSPACE);
		vga_disablecg();

		/* fill the buffer */
		cobuf = f->fntmap;
		for (k = i = 0; i<f->nr; i++,k++) {
			if (k==FCHUNKS) {
				error = copyout(fc,cobuf,FCHUNKS*sizeof(struct fchar));
				if (error) goto erexit;
				k = 0;
				cobuf += FCHUNKS;
			}

			fc[k].encoding = i+f->start;
			bzero(fc[k].map,VGA_MAXX/8*VGA_MAXY);
			bcopy(buf+32*(i+f->start),fc[k].map,f->y);
		}

		/* copy the rest */
		error = copyout(fc,cobuf,k*sizeof(struct fchar));

erexit:
		/* clean up */
		free(buf,M_IOCTLOPS);
	}

	f->start = vds.encoding[f->page];

	return error;
}

/*
 *  execute my own vga ioctls
 */
/*ARGSUSED*/
int vgaioctl(dev_t dev, int cmd, caddr_t data, int flag)
{
	switch(cmd) {
	/* CONSGINFO is mandatory ! */
	case CONSGINFO:
		*((struct consinfo*)data) = cons_capabilities;
		return 0;
	case OLDCONSGINFO:
		((struct oldconsinfo*)data)->info1 = cons_capabilities.info1;
		return 0;
	case CONSOLE_X_MODE:
		return kbd_setxserveriopl(*(int*)data);
	case VGASCURSOR:
		return vga_setcshape((struct cursorshape*)data);
	case VGAGCURSOR:
		return vga_getcshape((struct cursorshape*)data);
	case VGAGINFO:
		return vga_getvideoinfo((struct videoinfo*)data);
	case VGAGBLANK:
		*(int*)data =  (vds.scrtimeout&0x3FFFFFFF) |
				((u_long)(vds.blanking&3)<<30);
		return 0;
	case VGASBLANK:
		vds.scrtimeout = *(int*)data;	/* seconds */
		vga_doblanking(BLANKSTART);
		return 0;
	case VGAGBLOCK:
		return vioc_vgagetblock((struct vga_block*)data);
	case VGASBLOCK:
		return vioc_vgasetblock((struct vga_block*)data);
	case OLDVGASFNTMAP:
		return vioc_oldsetfontmap((struct fontmap*)data);
	case OLDVGAGFNTMAP:
		return vioc_oldgetfontmap((struct fontmap*)data);
	case VGASFONTMAP:
		return vioc_setfontmap((struct fmap*)data);
	case VGAGFONTMAP:
		return vioc_getfontmap((struct fmap*)data);
	case OLDVGAGCHAR:
		return vioc_oldgetfontchar((struct fontchar*)data);
	case OLDVGASCHAR:
		return vioc_oldsetfontchar((struct fontchar*)data);
	case VGAGPAGE:
		return vioc_gettextpage((struct textpage*)data);
	case VGASPAGE:
		return vioc_settextpage((struct textpage*)data);
	case VGAMISCFCNS:
		return vioc_miscfunctions((struct miscfcns*)data);
	case VGATAKECTRL:
		/* No op for now */
		return 0;
	case VGAGIVECTRL:
		/* No op for now */
		return 0;
	default:
		return ENOTTY;
	}
	/*NOTREACHED*/
}

#endif /*!MINITERM*/
#endif /*CODRV1*/
