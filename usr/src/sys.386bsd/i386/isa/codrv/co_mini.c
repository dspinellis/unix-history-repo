/*      Copyright 1992 by Holger Veit
 *	May be freely used with Bill Jolitz's port of 
 *	386bsd and may be included in a 386bsd collection
 *	as long as binary and source are available and reproduce the above
 *	copyright.
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
 *	@(#) $RCSfile$	$Revision$ (Contributed to 386bsd) $Date$
 *
 *	History: see CO_HISTORY
 */
static char *rcsid = "$Header$";

/* This file provides a real mini terminal emulator, basically one that
 * has only the cursor functions, screen clear/home, DEL, TAB, BEL,
 * but no attributes etc.
 * This interface is used for a installation kernel which must be 
 * small enough to fit on a diskette.
 */

/* check my optional symbol to avoid multiple inclusions */
#ifdef MINITERM

#define COMPAT_CO011
#include "co_hdr.h"
#include "vty.h"

#define ESC_NONE	0	/* No esc in progress */
#define ESC_WBRAC	1	/* got esc, wait for '[' or 'c' */
#define ESC_WPARAM	2	/* got esc [, wait for param, ';' or letter */

/*
 *	Do the local initialisations. Notice that many things are already done
 *	in vty_init
 */
void vtemul_init()
{
	char	*c,*id = "coinst";
	XCHAR	*xc;
	Keycap_def	*kp;
	struct Keycap2	*k2;

	/* fill the cons_capabilities structure
	 * Don't use strcpy here!
	 */
	c = id;
	xc = cons_capabilities.emul_name;
	while (*xc++ = xc_char2xc(*c++)) ;

#define copydef(src,dst)\
	xc_bcopy(src,dst,KBDDEFOVLKEYSIZE)
}

/*
 *	this routine does all the ESC and character processing stuff
 */
void vtemul_exec(struct vty *vp, XCHAR ch)
{
	int inccol, par1, par2;
	int sc = 1;	/* do scroll check */
	u_short at,sat;
	struct outmode *sk = vp->op;

	/* which attributes do we use? */
	at = sk->fg_at|sk->bg_at;
	sat= vp->so_at;

	/* translate to proper font */
	ch = vga_xlatiso646(vp,&at,&sat,ch);

	switch(ch) {
	case 0x1B:
		if(sk->escstate != ESC_NONE)
			emul_wrtchar(vp,ch,sat);
		else	
			sk->escstate = ESC_WBRAC;
		break;

	case '\t':
		inccol = (8 - vp->col % 8);	/* non-destructive tab */
		emul_cursorrelative(vp,inccol,0);
		break;

	case '\010':
		emul_cursorleft(vp, 1); 
		break;

	case '\r':
		emul_cursorrelative(vp,-vp->col,0);
		break;

	case '\n':
		emul_cursorrelative(vp,0,1);
		break;

	case 0x07:
		/* different sounds for different vtys possible */
		sysbeep (vp->pitch, vp->duration);
		break;
	default:
		/* ESC Processing */
		switch (sk->escstate) {

		default:
		case ESC_NONE:	
			/* NO ESC, normal processing */
			emul_wrtchar(vp,ch,vp->so ? sat : at);
			if (vp->col >= vp->ncol) vp->col = 0;
			break;

		case ESC_WBRAC:	
			/* has seen ESC, wait for [ or 'c' */
			if (ch=='[') {
				sk->escstate = ESC_WPARAM;
				sk->parcnt = 0;
				sk->param[0] = 0;
				sk->param[1] = 0;
			} 
			else if (ch == 'c') { 
				/* Clear screen & home */
				emul_clearcursor(vp, 2);
				emul_cursormove(vp, 0, 0);
				sk->escstate = ESC_NONE;
			} 
			else {	
				/* error */
				sk->escstate = ESC_NONE;
				emul_wrtchar(vp, ch, sat); 
			}
			break;

		case ESC_WPARAM:	/* has seen ESC [ wait for digit, ';' or letter */
			if (ch>='0' && ch<='9') {
				sk->param[sk->parcnt] *= 10;
				sk->param[sk->parcnt] += ch-'0';
			} 
			else if (ch==';') {
				if (sk->parcnt>=2) {
					sk->escstate = ESC_NONE;	/* error */
					emul_wrtchar(vp,ch,sat); 
				}
				else {
					sk->parcnt++;
					sk->param[sk->parcnt] = 0;
				}
			} 
			else if (ch>=' ' && ch<='~') {
				par1 = sk->param[0];
				par2 = sk->param[1];
				sk->parcnt++;
				sk->param[sk->parcnt] = 0;
				switch (ch) {
				case 'A': /* back cx rows */
					emul_cursorup(vp, par1);
					sc = 0;
					break;
				case 'B': /* down cx rows */
					emul_cursordown(vp, par1);
					sc = 0;
					break;
				case 'C': /* right cursor */
					emul_cursorright(vp, par1, 1);
					sc = 0;
					break;
				case 'D': /* left cursor */
					emul_cursorleft(vp, par1);
					sc = 0;
					break;
				case 'H': /* Cursor move */
					emul_cursormove(vp, par1, par2);
					break;
				default:
					emul_wrtchar(vp,ch,sat); 
				}
				sk->escstate = ESC_NONE;
			}
			break;
		}
	}
	
	if (sc && emul_checkcursor(vp) > 0) {
		if (consoftc.cs_flags&CO_OPEN) 
			do 
				(void)kbd_sgetc(1); 
			while (vp->scroll);

		emul_scrollup(vp, 1, 1);
	}
}

/*
 *	The following part provides a basic set of ioctls
 */
void coioctl_init() {}

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
		/* this is a relic from my youth mistakes */
		return 0;
	case KBDCOLDRESET:
		kbd_coldreset();
		kbd_ovlinit();
		return 0;
	case KBDGCKEY:
		return kbd_getckeydef (((Ovl_tbl *)data)->keynum, (Ovl_tbl *) data);
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
	default:
		/* backward compatibility */
		error = vgaioctl(dev, cmd, data, flag);
		if (!error) return 0;
		else return ENOTTY;
	}
	/*NOTREACHED*/
}

static int vioc_setfontmap(struct fontmap *data)
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

static int vioc_getfontmap(struct fontmap *data)
{
	int i,k;
	int pg = data->page;
	u_char *ofs = (u_char*)Crtat + pg*0x4000;
	if (vds.cardtype < VG_EGA || pg < 0 || pg > 1) return EINVAL;

	data->encoding = vds.encoding[pg];

	vga_enablecg(pg);
	for(i=k=0; i<(VGA_FNTCSIZE*VGA_FNTNCHARS); k+=32,i+=VGA_FNTCSIZE)
		bcopy(ofs+k,&data->map[i],VGA_FNTCSIZE);
	vga_disablecg();

	return 0;
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
	case VGAGINFO:
		return vga_getvideoinfo((struct videoinfo*)data);
	case VGASFNTMAP:
		return vioc_setfontmap((struct fontmap*)data);
	case VGAGFNTMAP:
		return vioc_getfontmap((struct fontmap*)data);
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

#endif /* MINITERM */
