 /*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz and Don Ahn.
 * Many improvements based on this source are made by Holger Veit.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#) $RCSfile: co_vga.c,v $	$Revision: 1.11 $ (Berkeley) $Date: 93/01/23 23:14:55 $
 */
static char rcsid[] = "$Header: /usr/src/sys.386bsd/i386/isa/codrv/RCS/co_vga.c,v 1.11 93/01/23 23:14:55 root Exp Locker: root $";

/*
 * History: see CO_HISTORY
 */

#include "co.h"
#include "pc.h"
#if NCO == 1 
#if NPC == 0

#include "co_hdr.h"

static	char	isinitialized = 0;	/* =1: console/vtys initialized */
u_short	*Crtat  = (u_short *)MONO_BUF;	/* The only absolute location that is
					 * known from video memory, everything
					 * else derived from it,
					 * modified by device probe */
static	int	scrblanksave;		/* save position cursor */
static	u_short	*scrbuf = 0;		/* buffer for screen saver */
static	char	*card2name(int id);

/* find video config */
static void probe_video()
{
	vga_whoami();
	if (vds.color) {
		Crtat += (CGA_BUF-MONO_BUF)/CHR;
		vds.iobase = 0x3D0;	/* color */
	} else
		vds.iobase = 0x3B0;	/* mono */

	vds.encoding[0] = XLAT2PC8;
	vds.encoding[1] = NOFONT;
	vds.f89bit = vds.cardtype >= VG_EGA ? 9 : 8;
}	

int coattach(struct isa_device *dev)
{
	struct cursorshape cs;
	struct kbd_hotkey spec;
	int i;

	/* re-initialize ALL vtys */
	vty_init(0);

	/* initialize the ioctl subsystem */
	coioctl_init();

	printf(" %s", card2name(vds.cardtype));
	if (vds.cardsubtype>=0)
		printf("/0x%02x", vds.cardsubtype);

	printf(" ram %dk io 0x%x kbd 0x%x", vds.ram, vds.iobase, kbs.id);

	if (nvty>1) printf(" vtys %d",nvty);

	/* create a save buffer for screen saver */
	if (!scrbuf)	/* XXX modify with different screen sizes */
		scrbuf = (u_short*)malloc(vtys[0].size*CHR, M_TEMP, M_NOWAIT);
	if (!scrbuf)
		panic("coattach: no screen buffer");

#ifdef	FAT_CURSOR
	cs.start = 0;
	cs.end = 18;
	vga_setcshape(&cs);
#endif	FAT_CURSOR

	vds.blanking = 0;
	vds.scrtimeout = BLANKTIMEOUT;
	vga_doblanking(BLANKSTART);
	vga_cursor(0);

	/* define the basic special function keys
	 * CTRL-ALT-DEL (Reset)
	 * CTRL-ALT-ESC (Debugger)
	 * CTRL-ALT-F1  (VTY-1)
	 * CTRL-ALT-F2	(VTY-2)
	 * CTRL-ALT-F3  (VTY-3)
	 * CTRL-ALT-F4	(VTY-4)
	 * CTRL-ALT-F5  (VTY-5)
	 * CTRL-ALT-F6	(VTY-6)
	 * CTRL-ALT-F7  (VTY-7)
	 * CTRL-ALT-F8	(VTY-8)
	 * CTRL-ALT-F9  (VTY-9)
	 * CTRL-ALT-F10	(VTY-10)
	 * CTRL-ALT-F11	(VTY-11)
	 * CTRL-ALT-F12	(VTY-12)
	 */
	spec.key	= 104;	/*DEL*/
	spec.modifier	= KBD_EXT_CA; /*CTRL-ALT*/
	spec.function	= KBD_RESETKEY;
	kbd_sethotkey(&spec);
#if NDDB > 0
	spec.key	= 110;	/*ESC*/
	spec.function	= KBD_DEBUGKEY;
	kbd_sethotkey(&spec);
#endif
	for (i=0; i<12; i++) {
		spec.key = 112+i;
		spec.function = i;
		kbd_sethotkey(&spec);
	}
}

/*
 * vga_cursor():
 *   reassigns cursor position, updated by the rescheduling clock
 *   which is a index (0-1999) into the text area. Note that the
 *   cursor is a "foreground" character, it's color determined by
 *   the fg_at attribute. Thus if fg_at is left as 0, (FG_BLACK),
 *   as when a portion of screen memory is 0, the cursor may dissappear.
 */
void vga_setcursorpos(int pos)
{
	outb(vds.iobase+4, M6845_CURSORH);
	outb(vds.iobase+5, pos>> 8);
	outb(vds.iobase+4, M6845_CURSORL);
	outb(vds.iobase+5, pos);
}

void vga_cursor(int a)
{ 	
	vga_setcursorpos(actvty->crtat-actvty->Crtat);

	if (a == 0 && actvty->visible == 1)
		timeout(vga_cursor, 0, hz/10);
}

#ifndef MINITERM
/* Screen saver function: 
 * XXX to be changed for graphics console
 *
 * if screenblanking is enabled, the display is switched off after delay
 * given in scrtimeout (in sec.) 
 * Any key hit restarts display again and restarts the timer.
 * scrtimeout = 0 disables blanking.
 * Also, blanking is suspended if console_x_mode is active, because it
 * is supposed that X11 has its own facilities to turn off display.
 */
static struct cursorshape playsave = { -1,-1 };

#if SCRSAVER == 1
/*ARGSUSED*/
static void display_off(int arg,int ctime)
{
	static int play1,play2,play3,play4;
	int dir[] = { -79,-81,79,81 };
	register s;
	int size = actvty->size;

	if (!actvty->visible) return;	/* no show on invisible screen */

	if (vds.blanking==1) {

	/* This idea was in parts borrowed from Linux, since I 
	 * was too lazy to play around with the video registers 
	 * (although there are suited bits to switch)
	 * screenplay originally by me (holgi)
	 */
		bcopy(actvty->Crtat,scrbuf,size*CHR);
		fillw(0x0820,actvty->Crtat, actvty->size);
		scrblanksave = actvty->crtat-actvty->Crtat;
		vds.blanking = 2;
		play1 = actvty->crtat-actvty->Crtat;
		play2 = time.tv_sec;
		play3 = 7;
		play4 = 1;
		untimeout(vga_cursor, 0);
	}
	
	/* some animation */
	s = spltty();
	vga_setcursorpos(play1);
	if (!play3) {
		play2 = (play2 % 1237) + 997;
		play3 = 4 + (play2 % 9);
		if (((play2>>3)&3)==play4) 
			play4=(play4+1)%4;
		else
			play4=(play2>>3)&3;
	} else play3--;
	play1 += dir[play4];
	if (play1<0) play1 += size;
	else if (play1 >= size) play1 -= size;
	timeout(display_off,0,hz/4);
	splx(s);
}

#else
#if SCRSAVER == 2	/* moving snake by Chr. Robitschko */

/* XXX This code could be merged with the standard saver, because there are
   not many modifications, but I need things to do for the next version */

static void display_off(int arg,int ctime)
{
	int dir[] = { -79,-81,79,81 };
	register s, f;
#ifdef NETBSD
	const char	snake[] = { "NetBSD" };
#else
	const char	snake[] = { "386BSD" };
#endif
	const int	snlen = sizeof(snake) - 1;
	static char	*snpos[snlen];
	static int play1,play2,play3,play4;
	struct cursorshape cs;

	int size = actvty->size;

	if (!actvty->visible) return;	/* no show on invisible screen */

	if (vds.blanking==1) {

	/* disable cursor -hv- */
	vga_getcshape(&playsave);
	cs.start = 31;
	cs.end = 30;
	vga_setcshape(&cs);

		bcopy(actvty->Crtat,scrbuf,size*CHR);
		fillw(0x0820,actvty->Crtat, actvty->size);
		scrblanksave = actvty->crtat-actvty->Crtat;
		vds.blanking = 2;
		play1 = actvty->crtat-actvty->Crtat;
		play2 = time.tv_sec;
		play3 = 7;
		play4 = 1;
		for (f=0; f<snlen; f++)
			snpos[f] = (char *)0;
		untimeout(vga_cursor, 0);
	}
	
	/* some animation */
	s = spltty();
	if (snpos[snlen-1]) *snpos[snlen-1] = ' ';
	for (f=snlen-1; f>0; f--)
		snpos[f] = snpos[f-1];
	snpos[0] = (char *)(Crtat + play1);
	for (f=snlen-1; f>=0; f--)
		if (snpos[f]) *snpos[f] = snake[f];

	if (!play3) {
		play2 = (play2 % 1237) + 997;
		play3 = 4 + (play2 % 9);
 		if (((play2>>3)&1)==play4%2) 
			play4=(play4+1)%4;
		else
			play4=(play2>>3)&3;
	} else play3--;
	play1 += dir[play4];
	if (play1<0) play1 += size;
	else if (play1 >= size) play1 -= size;
	timeout(display_off,0,hz/4);
	splx(s);
}

#else
ERROR! NEED SCRSAVER=1 OR 2
#endif /* SCRSAVER=2 */
#endif /* SCRSAVER=1 */

/* XXX graphics */
static void display_on()
{
	/* reload old content */
	if (vds.blanking==2) {
		bcopy(scrbuf,actvty->Crtat,actvty->size*CHR);
		if (playsave.start != -1) vga_setcshape(&playsave);
		vga_setcursorpos(scrblanksave);
		vga_cursor(0);
	}
}

/* XXX graphics */
void vga_doblanking(int fct)
{
	/* restore display, just in case */
	display_on();

	/* timer was started, stop */
	if (vds.blanking<3)
		untimeout(display_off,0);

	vds.blanking = 0;

	if (fct==BLANKSTART) {
		if (vds.scrtimeout) {
			/* start it */
			timeout(display_off,0,vds.scrtimeout*hz);
			vds.blanking = 1;
		}
	} 
	/*else if(fct==BLANKSTOP) vds.blanking = 0;*/
}

#else /* MINITERM */
/*ARGSUSED*/
static void display_off(int arg,int ctime) {}
static void display_on() {}
void vga_doblanking(int fct) {}
#endif

/* normally called by init_main to do local initialisation */
/* called by sput, if anyone is faster than init_main, e.g. ddb */
void consinit () 
{
	if (isinitialized == 0)
	{
		/* find out video card */
		probe_video();

		/* initialize video device */
		emul_initvideo();

		/* disable screensaver */
		vds.blanking = 3;

		/* initialize a single terminal */
		vty_init(1);

		isinitialized = 1;

		/* NetBSD would like to have this, 
		 * and for 386bsd it doesn't make problems */
		cons_normal();
	}
}

/*
 *   sput calls vtemul_exec for terminal emulation
 *   if ka, use kernel attributes.
 */
void sput(int vtynum, XCHAR c, int ka)
{
	register struct vty *act;

	if (!isinitialized)
		consinit();

	/* necessary: turn on display again */
	if (vds.blanking==2) vga_doblanking(BLANKSTART);

	/* select vty */
	act = &vtys[vtynum];

	/* select attribute set */
	act->op = ka ? &act->om[1] : &act->om[0];

	/* process input */
	vtemul_exec(act, c);

	/* re-set cursor position */
	if (ka) vga_cursor(1);
}

/* both routines used by init_main for more portability */
void cons_highlight() 
{
	register struct vty *p = &vtys[0];

	p->op = &p->om[1];
	emul_setattributes(p,3,15);
}

void cons_normal()
{
	register struct vty *p = &vtys[0];
	p->op = &p->om[1];
	emul_setattributes(p,0,0);
}

/* 
 * vga_whoami: try to detect whether a SVGA is present
 * see Ferraro, Programmer's guide to the EGA/VGA cards for more info
 */
/*
 * Try to identify, which video card is in the system
 */
void vga_whoami()
{
	register int iobase;
	u_char	*cp;
	u_char  *videobios;
	u_short *vp;
	u_short vsave;
	volatile u_char	lock,save1,save2,save3,save4;

	vds.color = 1;			/* default */
	vds.cardtype = VG_UNKNOWN;
	vds.cardsubtype = -1;
	vds.ram = 64;
	
	/* first look whether there is RAM in the COLOR region, if this is
	 * a mono card, there is none
	 */
	vp = Crtat + (CGA_BUF-MONO_BUF)/CHR;
	vsave = *vp;
	*vp = 0xCC33;
	DELAY(100); /* wait 100 us, doesn't help if you buffer your isa 
		     * bus lines with large capacitors  :-)
	             */
	if (*vp == 0xCC33) goto not_a_mono;
	else {
		/* we are paranoid */
		*vp = 0xA55A;
		DELAY(100);
		if (*vp == 0xA55A) goto not_a_mono;
	}

	/* oh, that poor owner has only a MDA/Hercules card :-) */
	*vp = vsave;
	vds.ram = 16;
	vds.color = 0;
	vds.cardtype = VG_MONO;
	return;

not_a_mono:
	*vp = vsave;

	/* there is RAM in the ???B8000 region, look which color card we have
	 * Hmmm... the ancient CGA was supported by the ROMBIOS, and had no ROM
	 * on board, but the EGA's and VGA's have
	 */
	videobios = cp = (u_char*)Crtat + (EGA_BIOS-MONO_BUF);

	/* Some machines map video BIOS to e0000 instead of c0000.
	 * Check it now.
	 */
	if (*cp != 0x55 || *(cp+1) != 0xAA)
		videobios = cp = (u_char*)Crtat + (ALTEGA_BIOS-MONO_BUF);

	if (*cp != 0x55 || *(cp+1) != 0xAA) {
		/* There is no ROM ID.
		 * you do not plan to run X11 with this card, do you?
		 */
		vds.ram = 16;
		vds.cardtype = VG_CGA;
		return;
	}

	/* this is an EGA or a VGA or a SVGA or something else quite strange
	 * so lets look whether this is a VGA. We know that several EGA 
	 * registers are readonly, which is really a design fault.
	 * so just look, whether we can write and read something into 
	 * such a register: this will hold for the CRT address register at
	 * iobase+4 and the graphics control address
	 */

	if (inb(0x3cc) & 1) {
		iobase = vds.iobase = 0x3D0;
	} else
	{
		iobase = vds.iobase = 0x3B0;
		vds.color = 0;
	}

	outb(iobase+4, 7);
	DELAY(100);
	if (inb(iobase+4) != 7) vds.cardtype = VG_EGA;
	else {
		outb(0x3ce,2);
		DELAY(100);
		if (inb(0x3ce) != 2) vds.cardtype = VG_EGA;
	}
	if (vds.cardtype==VG_EGA) {
		vds.ram = 64;	/* for simplicity assume the worst */
		return;
	}

	/* There is a problem now: 8514/A adapters also have ROM, unfortunately
	 * I don't have sufficient information about the 8514/A and the XGA,
	 * so just don't try 386BSD with it !
	 */

	/* I don't know whether this already qualifies for a VGA, so
	 * someone may add some code here, but for now I am persuaded that I
	 * have a VGA at this point. This might be a SVGA, however, so let's 
	 * look if this assumption is true. Hard stuff follows...
	 * refer to Ferraro: Programmer's Guide to the EGA and VGA Cards
	 * 2nd ed, Addison-Wesley, 1990
	 */
	vds.cardtype = VG_VGA;

	/* unlock paradise registers */
	outb(iobase+4, 0x11); lock = inb(iobase+5); outb(iobase+5, lock & 0x7f);

	/* now check for a PVGA1 */
	cp = videobios + 0x007d;
	if (*cp=='V' && *(cp+1)=='G' && *(cp+2)=='A' && *(cp+3)=='=') {
		/* this is a paradise, now check which one */
		vds.cardtype = VG_PARADISE;
		outw(0x3CE, 0x050F);
		outw(iobase+4, 0x8529);
		outb(iobase+4,0x2b); save1 = inb(iobase+5);
		outb(iobase+5,0xaa); save2 = inb(iobase+5);
		outb(iobase+5,save1);
		if (save2 != 0xaa) { vds.cardsubtype = 0x01; goto foundp2; }
		outb(0x3c4,0x12); save1 = inb(0x3c5);
		outb(0x3c5,save1 & 0xbf); save2 = inb(0x3c5) & 0x40;
		if (save2) { vds.cardsubtype = 0x01; goto foundp2; }
		outb(0x3c5,save1 | 0x40); save2 = inb(0x3c5) & 0x40;
		if (!save2) { vds.cardsubtype = 0x02; goto foundp; }
		outb(0x3c5,save1);
		save3 = 0;
		outb(0x3c4,0x10); save1 = inb(0x3c5);		
		outb(0x3c5, save1 & 0xfb); save2 = inb(0x3c5) & 0x04;
		if (save2) save3 = 1;
		outb(0x3c5, save1 | 0x04); save2 = inb(0x3c5) & 0x04;
		if (!save2) save3 = 1;
		vds.cardsubtype = save3 ? 0x03 : 0x04;
foundp:		outb(0x3c5,save1);
foundp2:	if (vds.cardsubtype != -1) {				
			/* find ram */
			outb(0x3ce,0x0b);
			switch(inb(0x3cf) & 0xc0) {
			case 0x80: vds.ram = 512; break;
			case 0xc0: vds.ram = 1024; break;
			default:   vds.ram = 256;
			}
			return;
		}
	}

/* Strategy: The ET4000 uses the reg 3x5/33, the ET3000 uses the 3x5/23
 * uniquely, the WD paradises also use 3x5/33. So check for Paradise
 * first, and if it's not, check for 3000/4000 with these registers
 */
	/* Is this a TSENG? */
	outb(0x3bf, 3);
	outb(iobase+8,0xa0);

	save1 = inb(iobase+0x0A); 
	outb(0x3C0, 0x20|0x16); save2 = inb(0x3C1);
	outb(0x3C0, save2 ^ 0x10);
	outb(0x3C0, 0x20|0x16); save3 = inb(0x3C1);
      	outb(0x3C0, save2);
	if (save3 == (save2 ^ 0x10)) {
		/* assume it is a Tseng, but which one */
		outb(iobase+4, 0x23); save2 = inb(iobase+5);
		outb(iobase+5, save2 ^ 0x07); save3 = inb(iobase+5);
		outb(iobase+5, save2);
		if (save3 == (save2 ^ 0x07)) {
			vds.ram = 512;
			vds.cardtype = VG_ET3000;
		} else
		{
			/* same experiment for reg 33 */
			outb(iobase+4, 0x33); save2 = inb(iobase+5);
			outb(iobase+5, save2 ^ 0x0f); save3 = inb(iobase+5);
			outb(iobase+5, save2);
			if (save3 == (save2 ^ 0x0f)) {
				vds.cardtype = VG_ET4000;
				outb(iobase+4,0x37); 
				switch (inb(iobase+5) & 03) {
				case 1: vds.ram = 256; break;
				case 2: vds.ram = 512; break;
				case 3: vds.ram = 1024; break;
				}
			}
		}
	}
	/* cleanup for TSENG */
	outb(0x3bf, 1);
	outb(iobase+8, 0xa0);

	if (vds.cardtype != VG_VGA) return;

	/* now look for the GENOAs */
	cp = videobios + 0x37;
	cp += (u_char)*cp;	/* at 0x37 is the offset to the real signature */
	if (*cp==0x77 && *(cp+2)==0x66 && *(cp+3)==0x99) {
		/* this is a GENOA. Note that the GENOAs 5xxx are 
		 * really TSENG ET3000, so they should have fallen 
		 * through the sieve above already, if not, something's 
		 * quite strange here, so I don't believe its a GENOA
		 */
		vds.cardtype = VG_GENOA;
		vds.cardsubtype = *(cp+1);
		switch (vds.cardsubtype) {
		default:
		case 0x33: /* shouldn't reach this */
		case 0x55: vds.cardtype = VG_VGA; return;
		case 0x22: 
		case 0x00: vds.ram = 256; return;
		case 0x11: vds.ram = 512; return;
		}
		/*NOTREACHED*/
	}

	/*
	 * Look for the Tridents
	 */
	outb(0x3c4, 0x0e); save1 = inb(0x3c5);
	outb(0x3c5, 0); save2 = inb(0x3c5) & 0x0f;
	outb(0x3c5, save1 ^ 0x02);
	if (save2==0x02) {
		/* is a Trident */
		vds.cardtype = VG_TVGA;
		outb(0x3c4, 0x0b);
		outb(0x3c5, 0);
		save1 = inb(0x3c5);
		/* restore "old mode", recommended by 
		 * chmr@edvz.tu-graz.ac.at (Christoph Robitschko) 
		 */
		outb(0x3c5, 0);	
		vds.cardsubtype = save1;
		if (save1== 0xfd) /*TVGA 9000*/
			vds.ram = 1024;
		else
			vds.ram = 512; /* There is 3c4/1f, but I don't know the encoding */

		return;
	}

	/* for the moment X11 does not know any more chips, but
	 * I found some more in Ferraro's book
	 *
	 * Video 7 
	 */
	outw(0x3c4,0xea06);
	outb(iobase+4,0x0c); save1 = inb(iobase+5);
	outb(iobase+5,0x55); save2 = inb(iobase+5);
	outb(iobase+4,0x1f); save3 = inb(iobase+5);
	outb(iobase+4,0x0c); outb(iobase+5,save1);
	if (save3 == (0x55^0xea)) {
		/* this is a video 7 */
		vds.cardtype = VG_VIDEO7;
		outb(0x3c4, 0x8e);
		vds.cardsubtype = inb(0x3c5);
		if (vds.cardsubtype > 0x40 && vds.cardsubtype < 0x4a)
			vds.ram = 1024;	/* 1024i */
		else
			vds.ram = 512;
		return;
	}

	/* C&T Chips, I'm not sure about that, in particular the
	 * strange 46E8 port, I somewhere  heard that I/O space is 
	 * 0000-03FF only, but I don't know whether any chipset
	 * clips this range. Anyway...
	 */
	outb(0x46e8,0x1e);
	if (inb(0x104)==0xa5) {
		outb(0x103, 0x80); outb(0x46e8, 0x0e);
		outb(0x3d6, 0);
		vds.cardtype = VG_CHIPS;
		vds.cardsubtype = inb(0x3d7) & 0xf0;
		switch(vds.cardsubtype) {
		case 0x10:
			outb(0x3d6, 0x3a); save1=inb(0x3d7);
			outb(0x3d7, 0xaa); save2=inb(0x3d7);
			outb(0x3d7, save1);
			if (save2==0xaa) {
				vds.cardsubtype = 0x11;
				vds.ram = 1024;
			} else
				vds.ram = 256;
			break;
		case 0x20: vds.ram = 256; break;
		case 0x30: vds.ram = 1024; break;
		case 0x50: vds.ram = 256;
		} 
		outb(0x46e8, 0x1e); outb(0x103, 0);
	}
	outb(0x46e8, 0x0e);

	/* the two ATI's */
	cp = videobios + 0x0031;
	if (!bcmp(cp,"761295520",9)) {
		/* is an ATI, also unsure about the numbers */
		vds.cardtype = VG_ATI;
		cp = videobios + 0x0042;
		vds.cardsubtype = *cp | (*(cp+1)<<8);
		vp = (u_short *) videobios + 0010;
		vds._atiext = *vp;
		outb(*vp,0xbb); save1 = inb((*vp)+1);
		vds.ram = (save1 & 0x20) ? 512 : 256;

		return;
	}

	/* I don't know more types, so assume 
	 * it is a VGA (at least looks as if) 
	 */
	vds.ram = 256;
	vds.cardtype = VG_VGA;
	return;

}

static char *card2name(int id)
{
	static char *nametable[] = {
	/* 0 */		"UNKNOWN",
	/* 1 */		"MDA/Hercules",
	/* 2 */		"CGA",
	/* 3 */		"EGA",
	/* 4 */		"VGA",
	/* 5 */		"C&T",
	/* 6 */		"Genoa VGA",
	/* 7 */		"Paradise VGA",
	/* 8 */		"Trident VGA",
	/* 9 */		"Tseng ET3000",
	/* 10 */	"Tseng ET4000",
	/* 11 */	"Video7 VGA",
	/* 12 */	"ATI VGA",
			0,
	};

	return nametable[id];
}

/*
 * set cursor shape
 */
int vga_setcshape(struct cursorshape *data)
{
	register s,e;
	
	s = data->start;
	e = data->end;

	/* I don't do error correction here, the user should pass 
	 * correct and checked data! Note: The registers allow 5 Bits == 31
	 */ 
	if (s<0 || s>31 || e<0 || e>31) return EINVAL;

	/* the Tridents seem to have the feature to switch off
	 * cursor when start is 0 (not verified)
	 */
	if (vds.cardtype==VG_TVGA && s==0) s=1;

	outb(vds.iobase+4, M6845_CURSTART);
	outb(vds.iobase+5, s);
	outb(vds.iobase+4, M6845_CUREND);
	outb(vds.iobase+5, e);
}

#ifndef MINITERM
int vga_getcshape(struct cursorshape *data)
{
	/* update from registers */
	outb(vds.iobase+4, M6845_CURSTART);
	data->start = inb(vds.iobase+5) & 0x1F;
	outb(vds.iobase+4, M6845_CUREND);
	data->end = inb(vds.iobase+5) & 0x1F;
	
	return 0;
}
#endif

int vga_getvideoinfo(struct videoinfo *data)
{
	int	i;
	
	bcopy(card2name(vds.cardtype),data->name,20);
	data->type = vds.cardtype;
	data->subtype = vds.cardsubtype;
	data->ram = vds.ram;
	data->iobase = vds.iobase;
	return 0;	
}

static char modesave;

void vga_enablecg()
{
	register iobase = vds.iobase;
	
	while((inb(iobase+10)&0x08)==0);	/* wait for vertical blanking */
	inb(iobase+10);
	outb(0x3C0,0x30); modesave = inb(0x3C1);
	inb(iobase+10);
	outb(0x3C0,0x30); outb(0x3C0, 0x01); /* graphics mode */
/*	outw(0x3c4,0x0100);*/	/* hold sequencer synch reset */
	outw(0x3c4,0x0402);	/* write enable map "page" */
	outw(0x3c4,0x0604);	/* set sequential addressing */
/*	outw(0x3c4,0x0300);*/	/* release sequencer synch reset */
	outw(0x3ce,0x0204);	/* select map "page" for cpu reading */
	outw(0x3ce,0x0005);	/* RM=0,O/E=0,WM=00 */
	outw(0x3ce,0x0d06);	/* map at "B8000" */
}

void vga_disablecg()
{
	register iobase10 = vds.iobase+10;
	char seq;
	
	inb(iobase10);
	outb(0x3C0,0x30); outb(0x3C0, modesave); /* alpha mode */
/*	outw(0x3c4,0x0100);*/	/* hold sequencer synch reset */
	outw(0x3c4,0x0302);	/* write enable text pages */
	outw(0x3c4,0x0304);	/* set O/E addressing */
/*	outw(0x3c4,0x0300);*/	/* release sequencer synch reset */
	outw(0x3ce,0x0004);	/* select map 0 for cpu reading */
	outw(0x3ce,0x1005);	/* RM=0,O/E=0,WM=00 */
	outw(0x3ce,0x0e06);	/* map at "B8000", Chain O/E */

	/* set the 8/9 bit according to the state of vds.f89 
	 * The little problem is that with a EGA we have to guess
	 * the old value (but it is usually 0x03)
	 */
	if (vds.cardtype != VG_EGA) {
		inb(iobase10);
		outb(0x3c0,0x20|0x10);
		seq = inb(0x3c1);
	}
	else
		seq = 0x03;
	inb(iobase10);
	outb(0x3c0,0x20|0x10);
	if (vds.f89bit == 9) outb(0x3c0, seq | 0x04);
	else outb(0x3c0, seq & ~0x04);

	while(inb(iobase10)&0x08);	/* wait for end of vertical blanking */
}

static u_char iso2pc8[] = {
/*A0*/	0xff,0xad,0x9b,0x9c,0xfe,0x9d,0x7c,0x15,
/*A8*/	0xfe,0xfe,0xa6,0xae,0xaa,0x2d,0xfe,0xfe,
/*B0*/	0xf8,0xf1,0xfd,0xfe,0xfe,0xe6,0x14,0xf9,
/*B8*/	0xfe,0xfe,0xa7,0xaf,0xac,0xfd,0xfe,0xa8,
/*C0*/	0xfe,0xfe,0xfe,0xfe,0x8e,0x8f,0x92,0x80,
/*C8*/	0xfe,0x90,0xfe,0xfe,0xfe,0xfe,0xfe,0xfe,
/*D0*/	0xfe,0xa5,0xfe,0xfe,0xfe,0xfe,0x99,0xfe,
/*D8*/	0xfe,0xfe,0xfe,0xfe,0x9a,0xfe,0xfe,0xe1,
/*E0*/	0x85,0xa0,0x83,0xfe,0x84,0x86,0x91,0x87,
/*E8*/	0x8a,0x82,0x88,0x89,0x8d,0xa1,0x8c,0x8b,
/*F0*/	0xfe,0xa4,0x95,0xa2,0x93,0xfe,0x94,0xf6,
/*F8*/	0xfe,0x97,0xa3,0x96,0x81,0xfe,0xfe,0x98
};

/* needs attribute byte to select font */
int vga_xlatiso646(struct vty *vp,u_short *at,u_short *sat,int c)
{
	int f2flag = vp->op->f2;
	
	if (vds.encoding[1] != NOFONT) {	/* second font loaded? */
		if (f2flag) {
			*at |= 0x08;	/* select font 2 */
			*sat |= 0x08;
		} else
		{
			*at &= 0xF7;
			*sat &= 0xF7;
		}
	} else f2flag = 0;			/* ignore font switch */

	/* need to translate character? */
	if (f2flag==0 && vds.encoding[0]==XLAT2PC8)
		return (c<0xa0) ? c : iso2pc8[c-0xa0];
	else		
		return c;
}

/***********************************************************************
 * Support functions for the terminal emulator
 **********************************************************************/
#ifndef GFX_CONSOLE
/*
 *	moves the cursor up  n  lines
 */
void vga_cursorup(struct vty *vp, int n)
{
	register pos;

	if (n <= 0) n = 1;

	pos = vp->crtat - vp->Crtat;
	pos -= vp->ncol * n;
	vp->row--;
	if (pos < 0) {
		pos += vp->size;
		vp->row = vp->nrow-1;
	}
	vp->crtat = vp->Crtat + pos;
}

/*
 *	moves the cursor down  n  lines
 */
void vga_cursordown(struct vty *vp, int n)
{
	register pos;

	if (n <= 0) n = 1;
	pos = vp->crtat - vp->Crtat;
	pos += vp->ncol * n;
	vp->row++;
	if (pos >= vp->size) {
		pos -= vp->size;
		vp->row = 0;
	}
	vp->crtat = vp->Crtat + pos;
}

/*
 * moves the cursor left  n  columns
 */
void vga_cursorleft(struct vty *vp, int n)
{
	register pos;

	if (n <= 0) n = 1;
	pos = vp->crtat - vp->Crtat;
	pos -= n; vp->col -= n;
	if (vp->col < 0) {
		vp->col += vp->ncol;
		pos += vp->ncol;     /* cursor stays on same line */
	}
	vp->crtat = vp->Crtat + pos;
}

/*
 * moves the cursor right  n  columns
 * wrap=1: cursor stays on same line
 */
void vga_cursorright(struct vty *vp, int n, int wrap)
{
	register pos;

	if (n <= 0) n = 1;
	pos = vp->crtat - vp->Crtat;
	pos += n; vp->col += n;
	if (wrap && vp->col >= vp->ncol) {
		vp->col -= vp->ncol;
		pos -= vp->ncol;     /* cursor stays on same line */
	}
	vp->crtat = vp->Crtat + (pos % vp->size);
}

/* 
 * scrollup n lines and fill free space with default attribute
 * cm = 1: also move cursor
 */
void vga_scrollup(struct vty *vp,int n, int cm)
{
	u_short attr = vp->op->def_at;
	int	blkx,blky;

	if (n <= 0) n = 1;
	blkx = vp->ncol*n;
	blky = vp->size - blkx;

	bcopy(vp->Crtat+blkx, vp->Crtat, blky*CHR);
	fillw((attr <<8)+' ', vp->Crtat+blky, blkx);
	if (cm)  { 
		vp->crtat -= blkx;
		vp->row-= n;
	}
}

#ifndef MINITERM
/* 
 * scroll down n lines and fill free space with default attribute
 */
void vga_scrolldown(struct vty *vp, int n)
{
	u_short attr = vp->op->def_at;
	int blkx;
	
	if (n <= 0) n = 1;
	blkx = vp->ncol*n;

	bcopy(vp->Crtat, vp->Crtat+blkx, (vp->size-blkx)*CHR);
	fillw((attr <<8)+' ', vp->Crtat, blkx);
	/* XXX vp->crtat += blkx; */
}
#endif /*!MINITERM*/

/*
 * set the absolute cursor position (row,col = 0..max)
 */
void vga_cursormove(struct vty *vp, int row, int col)
{
	if (row==0 || col==0) {
		vp->crtat = vp->Crtat;
		vp->row = 0;
		vp->col = 0;
	} else if (col <= vp->ncol && row <= vp->nrow) {
		vp->crtat = vp->Crtat + (row - 1) * vp->ncol + col - 1;
		vp->col = col - 1;
		vp->row = row - 1;
	}
}

/* 
 * cursorrelative: move cursor relative, don't check for outside of screen
 */
void vga_cursorrelative(struct vty *vp, int dx, int dy)
{
	int incr = dy*vp->ncol + dx;
	int pos = vp->crtat - vp->Crtat;
	
/*	if ((pos+incr) < 0 || (pos+incr) >= vp->size) 
		return;
*/	vp->crtat += incr;
	vp->col = (vp->col+dx) % vp->ncol;
	vp->row = (vp->row+dy) % vp->nrow;
}

/*
 * Clear screen with default attr
 * mode = 0: from cursor to end of screen
 *      = 1: cursor position only
 *	= 2: whole display
 */
void vga_clearcursor(struct vty *vp, int mode)
{
	u_short attr = (vp->op->def_at << 8) | ' ';
	
	/* clear ... */
	switch (mode) {
	case 0:	/* ... to end of display */
		fillw(attr, vp->crtat, vp->Crtat + vp->size - vp->crtat);
		break;
	case 1:	/* ... to next location */
		fillw(attr, vp->Crtat, vp->crtat - vp->Crtat + 1);
		break;
	case 2:	/* ... whole display */
		fillw(attr, vp->Crtat, vp->size);
	}
}

/*
 * Clear line and fill with default attr
 * mode = 0: from cursor to end of line
 * 	= 1: from beginning to cursor
 *	= 2: whole line
 */
#ifndef MINITERM
void vga_clearline(struct vty *vp, int mode)
{
	u_short attr = (vp->op->def_at << 8) | ' ';
	
	/* clear ... */
	switch (mode) {
	case 0:	/* ... current to EOL */
		fillw(attr, vp->crtat, 
			vp->ncol - (vp->crtat - vp->Crtat) % vp->ncol);
		break;
	case 1:	/* ... beginning to next */
		fillw(attr,
			vp->crtat - (vp->crtat - vp->Crtat) % vp->ncol,
			((vp->crtat - vp->Crtat) % vp->ncol) + 1);
		break;
	case 2:	/* ... entire line */
		fillw(attr, vp->crtat - (vp->crtat - vp->Crtat) % vp->ncol,
			vp->ncol);
	}
}

/*
 * delete n lines from cursor position to end, pull up lines with default attr
 */
void vga_deleteline(struct vty *vp, int n)
{
	u_short *crtend,*pp;
	u_short attr = (vp->op->def_at <<8) | ' ';
	
	if (n <= 0) n = 1;

	/* Work from beginning of line */
	vp->crtat -=  (vp->crtat - vp->Crtat) % vp->ncol; 
	vp->col = 0;
	crtend = vp->Crtat + vp->size;

	/* Cap affected area at bottom of screen */
	if ((pp = (vp->crtat + n*vp->ncol)) > crtend) {
		n = (crtend-vp->crtat) / vp->ncol;
		pp = crtend;
	}

	/* If there's visible lines left, move them up */
	if (pp < crtend) {
		bcopy(pp, vp->crtat, (crtend-pp)*CHR);
	}
	fillw(attr, crtend - n*vp->ncol, n*vp->ncol);
}

/*
 * insert n lines from cursor position to end, fill with default attr
 */
void vga_insertline(struct vty *vp, int n)
{
	u_short *crtend,*pp;
	u_short attr = (vp->op->def_at << 8) | ' ';

	if (n <= 0) n = 1;

	/* Work from beginning of line */
	vp->crtat -=  (vp->crtat - vp->Crtat) % vp->ncol; 
	vp->col = 0;
	crtend = vp->Crtat + vp->size;

	/* Cap affected area at bottom of screen */
	if ((pp = (vp->crtat + n*vp->ncol)) > crtend) {
		n = (crtend-vp->crtat) / vp->ncol;
		pp = crtend;
	}

	/* If there's visible lines left, move them down */
	if (pp < crtend) {
		bcopy(vp->crtat, pp, (crtend-pp)*CHR);
	}
	fillw(attr, vp->crtat, n*vp->ncol);
}

/*
 * Delete n chars in line, fill free space with default attr
 */
void vga_deletechars(struct vty *vp, int n)
{
	u_short *crtend,*pp;
	u_short attr = (vp->op->def_at << 8) | ' ';

	if (n <= 0) n = 1;

	/* Point to end of current line */
	pp = vp->crtat + (vp->ncol - (vp->crtat-vp->Crtat) % vp->ncol);

	/* Cap delete to end of current line */
	if ((vp->crtat+n) > pp)
		n = pp-vp->crtat;

	/* If visible characters, move in */
	if ((vp->crtat+n) < pp)
		bcopy(vp->crtat+n, vp->crtat, ((pp-vp->crtat)-n)*CHR);

	/* Blank out space at end of line */
	fillw(attr, pp-n, n);
}

/*
 * Delete n chars in line, fill free space with default attr
 */
void vga_insertchars(struct vty *vp, int n)
{
	u_short *crtend,*pp;
	u_short attr = (vp->op->def_at << 8) | ' ';

	if (n <= 0) n = 1;

	/* Point to end of current line */
	pp = vp->crtat + (vp-> ncol - (vp->crtat-vp->Crtat) % vp->ncol);

	/* Cap insert to end of current line */
	if ((vp->crtat+n) > pp)
		n = pp-vp->crtat;

	/* If visible characters, move out */
	if ((vp->crtat) < pp)
		bcopy(vp->crtat, vp->crtat+n, ((pp-vp->crtat)-n)*CHR);

	/* Blank out space at new positions */
	fillw(attr, vp->crtat, n);
}
#endif /*!MINITERM*/

static u_short fgansitopc[] =
{	FG_BLACK, FG_RED, FG_GREEN, FG_BROWN, FG_BLUE,
	FG_MAGENTA, FG_CYAN, FG_LIGHTGREY
};

static u_short bgansitopc[] =
{	BG_BLACK, BG_RED, BG_GREEN, BG_BROWN, BG_BLUE,
	BG_MAGENTA, BG_CYAN, BG_LIGHTGREY
};

/*
 * set attributes
 * mode = 0: reset normal attributes, attr=0: std, attr=1: kernel
 * mode = 1: set ansi color background
 * mode = 2: set ansi color foreground
 * mode = 3: set PC attribute
 * mode = 4: reset standout attributes (extension!)
 */
void vga_setattributes(struct vty *vp, int mode, int attr)
{
	struct outmode *o = vp->op;

	switch (mode) {
	case 0:	/* reset to normal attributes */
		if (o == &vp->om[0]) {	/* std */
			o->fg_at = vds.color ? DEF_STD_C_FGAT : DEF_STD_M_FGAT;
			o->bg_at = DEF_STD_BGAT;
		} else
		{	/* kernel */
			o->fg_at = vds.color ? DEF_KERN_C_FGAT : DEF_KERN_M_FGAT;
			o->bg_at = DEF_KERN_BGAT;
		}
		/* asa@kiae.su: fix to allow non-black background */
		/*o->def_at = o->fg_at | o->bg_at;*/
		break;

	case 1:	/* ansi background */
		if (vds.color)
			o->bg_at = bgansitopc[attr & 7];
		break;

	case 2:	/* ansi foreground */
		if (vds.color)
			o->fg_at = fgansitopc[attr & 7];
		break;

	case 3:	/* pc text attribute */
		o->fg_at = attr & 0x8f;
		o->bg_at = attr & 0x70;
		break;

	case 4:	/* set so attribute to default */
		vp->so_at = vds.color ? DEF_SO_C_AT : DEF_SO_M_AT;
 	}
 	o->def_at = o->fg_at | o->bg_at;	/* asa@kiae.su */
}

void vga_selectfont(struct vty *vp,int fontnr)
{
	vp->op->f2 = (fontnr==2);
}

void vga_wrtchar(struct vty *vp, u_int c, u_int at)
{
	*(vp->crtat++) = (u_short)(at<<8)|c; 
	vp->col++; 
}

/*
 *	check whether crtat points to outside the screen
 *	= 0: no, = -1: less than top of screen, =1: below bottom of screen
 */
int vga_checkcursor(struct vty *vp)
{
	if (vp->crtat < vp->Crtat) return -1;
	if (vp->crtat >= vp->Crtat+vp->size) return 1;
	return 0;
}

/*
 * return a char to the vty, i.e. simulate an (ASCII) keypress,
 * used to return status messages (not for pc3)
 */
void vga_sendchar(struct vty *vp, XCHAR c)
{
	/* XXX modify for real (=char) XCHAR type */

	/* is it opened? */
	if (vp->ttycnt) 
		(*linesw[vp->ttyp->t_line].l_rint)(c & 0xFF, vp->ttyp);
}

void vga_initvideo() {}

#endif
#endif /* NPC=0 */
#endif /* NCO=1 */
