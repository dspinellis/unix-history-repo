/*-
 *      Copyright 1992,1993 by Holger Veit
 *	Some parts of this code may still resemble code
 *	derived from software contributed to Berkeley by William Jolitz 
 *	and Don Ahn.
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
 *
 *	@(#) $RCSfile: co_kbd.c,v $	$Revision: 1.12 $ (Contributed to 386bsd) $Date: 93/01/23 23:14:42 $
 */
static char rcsid[] = "$Header: /usr/src/sys.386bsd/i386/isa/codrv/RCS/co_kbd.c,v 1.12 93/01/23 23:14:42 root Exp Locker: root $";

/*
 * History: see CO_HISTORY
 */

/*
 * code to work keyboard & display for PC-style console
 */
#include "co.h"
#include "pc.h"
#if NCO == 1 
#if NPC == 0
#include "vty.h"

#define GLOBAL
#include "co_hdr.h"
#undef GLOBAL

struct consinfo cons_capabilities = 
{
	  CONS_ISCO | CONS_HASKBD | CONS_HASKEYNUM 
	| CONS_USEPC8 | CONS_HASKCAP
	| CONS_HASFNT
	| CONS_HASPX386
#if NVTY > 1
	| CONS_HASVTY
#endif
#ifdef PC3
	| CONS_HASPC3
#endif
	,0l, 0l, 0l,

	/* dont write this as a "string", this is XCHAR data! */
	{ 'c', 'o', 'd', 'r', 'v', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },

	/* filled in by vtemul_init */
	{   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },

	/* reserved */
	{   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
  	{   0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
};

/*
 *  static variables for co_kbd
 */
static	XCHAR	*more_chars;
static	u_char	lastkey = 0;	/* save for last key */
static	u_char	extended = 0;	/* extended scan code */
static	char	resetdone = 0;	/* reset flag */
static	u_char	altkpflag = 0;	/* in ALT-KP mode */
static	u_short	altkpval = 0;	/* accumulated value of ALT-KP */

static	XCHAR	*kbd_xlatkey2ascii(u_int key);
static	u_int	kbd_sgetk(int);

/* CO device support */
struct	isa_driver codriver = {
	coprobe, coattach, "co",
};

#define CO_RDPRI	(PZERO+1)

void reset_kbd_flags()
{
	/* when closing pccons, it may be that a ctrl key is still pending */
	kbs.shift_down =
	kbs.ctrl_down =
	kbs.meta_down =
	kbs.altgr_down =
	extended =
	altkpflag = 0;
}	

/*
 * CO is the raw keyboard device == /dev/kbd
 * PC is the TTY-type keyboard device == /dev/console
 * CO has precedence over PC, so if the XSERVER opens /dev/kbd,
 * no word will ever be heard from /dev/console, until X closes it again
 *
 */

/* ARGSUSED */
int coopen(dev_t dev, int flag, int mode, struct proc *p)
{
	consoftc.cs_mymajor = major(dev);

	/* minor 128 is /dev/vga */
	if (minor(dev)==0x80) return 0;
	
	/* exclusive open only */
	if (consoftc.cs_flags & CO_OPENRAW)
		return EBUSY;

	/* someone wants to write to the keyboard? */
	if (flag & (FWRITE|FAPPEND))
		return ENODEV;	

	consoftc.cs_flags |= CO_OPENRAW;

	/* save constty for later */
	consoftc.cs_constty = constty;

	/* save active vty */
	consoftc.cs_actvty = actvty;

	/* initialize the buffer */
	consoftc.cs_flags |= CO_INITRB;
	initrb(&co_buf);

	/* signal opening process only */
#ifdef OLDPATCHKIT
	consoftc.cs_selp = 0;
#else
	consoftc.cs_rsel = 0;
#endif
	consoftc.cs_pgid = p->p_pid;

	/* lex xserver */
	if (minor(dev)==1) {
		kbd_setxserveriopl(1);	
		vty_setactive(-1,0);
	}

	return 0;
}

/* ARGSUSED */
int coclose(dev_t dev, int flag, int mode, struct proc *p)
{
	register struct tty *tp = actvty->ttyp;

	/* minor 128 is /dev/vga */
	if (minor(dev)==0x80) return 0;

	consoftc.cs_flags &= ~(CO_OPENRAW|CO_ASYNC);
#ifdef OLDPATCHKIT
	consoftc.cs_selp = 0;
#else
	consoftc.cs_rsel = 0;
#endif
	consoftc.cs_pgid = 0;

	/* do we need to restore console?
	 * check whether we had a console and now have no longer one:
	 * then someone grabbed it (xterm -C) and died,
	 * of course the old constty must still be open
	 */
	if (consoftc.cs_constty && !constty && consoftc.cs_constty->t_state) {
		constty = consoftc.cs_constty;
		constty->t_state |= TS_CARR_ON;
		ttwakeup(constty);
	}

	if ((tp->t_state & TS_ISOPEN) && (tp->t_state & TS_CARR_ON)==0) {
		/* usually console hangs in ttopen (hack) */
		tp->t_state |= TS_CARR_ON;
		ttwakeup(tp);
	}

	if (minor(dev)==1) {
		kbd_setxserveriopl(0);
		vty_setactive(actvty->vtynum, 3);
	}

	return(0);
}

/* ARGSUSED */
int coread(dev_t dev, struct uio *uio, int flag)
{
	register c, n = 0;
	int error,s;

	/* minor 128 is /dev/vga */
	if (minor(dev)==0x80) return ENXIO;

	s = spltty();	
	while ( (n=RB_LEN(&co_buf))<=0 ) {
		if (flag & IO_NDELAY) {
			splx(s);
			return EWOULDBLOCK;
		}
		consoftc.cs_flags |= CO_RDWAIT;
		if (error= tsleep((caddr_t)&co_buf,CO_RDPRI|PCATCH,"kbd",0)) {
			splx(s);
			return error;
		}
	}
	splx(s);
	consoftc.cs_flags &= ~CO_RDWAIT;

	while ((c = getc(&co_buf)) > 0) {
		error = ureadc(c,uio);
		if (error || uio->uio_resid == 0) 
			break;
	}
	return error;
}

/*
 * Got a console receive interrupt -
 * the console processor wants to give us a character.
 * Catch the character in a buffer
 */
void cointr(dev_t dev, int irq, int cpl)
{
	int c, error;
	XCHAR	*cp;
	register struct tty *tp;

	/* open buffer ? */
	if ((consoftc.cs_flags & CO_INITRB) == 0) {
		initrb(&co_buf);
		consoftc.cs_flags |= CO_INITRB;
	}

	/* vt switch in progress? */
	if (vtswsema) return;

	/* get a keynum */
	c = kbd_sgetk(1);

	/* spurious? */
	if ((c&0x7F)==0) return;

	/* must restore display? */
	if (vds.blanking) vga_doblanking(BLANKSTART);

	/* in poll mode ? */
	if (consoftc.cs_flags & CO_POLLING) return;

	/* no open device */
	if ((consoftc.cs_flags & (CO_OPEN|CO_OPENRAW)) == 0)
		return;

	if (consoftc.cs_flags & CO_OPENRAW) {

		/* has the vty, which was active when /dev/kbd was opened
		 * still control? If yes, move data to /dev/kbd, else
		 * to the active vty
		 */
		if (actvty==consoftc.cs_actvty) {
			/* save byte */
			error = putc(c,&co_buf);
			if (error) {	/* buffer full */
				printf("Keyboard buffer overflow");
				initrb(&co_buf);	/* flushing all */
				consoftc.cs_ovfl++;
			}
			/* if anyone is interested, tell it that there is data */
			cowakeup();
			return;
		}
	}
	
	/* convert to ASCII and give to tty driver */
	cp = kbd_xlatkey2ascii(c);
	if (cp==0) return;

	tp = actvty->ttyp;
	if (actvty->ttycnt==0) return;	/* ignore if not open */

	/* notice: a \000 Byte is an ASCIZ string \000\000 actually */
	if (*cp==0 && *(cp+1)==0)
		(*linesw[tp->t_line].l_rint)((*cp++ & 0xff), tp);
	else while (*cp)
		(*linesw[tp->t_line].l_rint)((*cp++ & 0xff), tp);
	return;
}

void cowakeup()
{
	struct proc *p;

	if (!(consoftc.cs_flags & CO_OPENRAW))
		return;
#ifdef OLDPATCHKIT
	if (consoftc.cs_selp) {
		selwakeup(consoftc.cs_selp,0);
		consoftc.cs_selp = 0;
	}
#else
	if (consoftc.cs_rsel) {
		selwakeup(consoftc.cs_rsel,0);
		consoftc.cs_rsel = 0;
	}
#endif
	if (consoftc.cs_flags & CO_ASYNC) {
		if (consoftc.cs_pgid < 0)
			gsignal(-consoftc.cs_pgid,SIGIO);
		else if (p=pfind(consoftc.cs_pgid))
			psignal(p,SIGIO);
	}
	if (consoftc.cs_flags & CO_RDWAIT) {
		wakeup((caddr_t)&co_buf);
		consoftc.cs_flags &= ~CO_RDWAIT;
	}
}

int coselect(dev_t dev, int rw, struct proc *p)
{
	register nread;
	int s = spltty();
	if (rw==FREAD) {
		nread = RB_LEN(&co_buf);
		if (nread > 0) {
			splx(s);
			return 1;
		}
#ifdef OLDPATCHKIT
		consoftc.cs_selp = p;
#else
		consoftc.cs_rsel = p->p_pid;
#endif
	}
	splx(s);
	return 0;
}

int coioctl(dev_t dev, int cmd, caddr_t data, int flag)
{
	register error;

	switch (minor(dev)) {
	case 0:	
	case 1:
		/* /dev/kbd */
		return kbdioctl(dev,cmd,data,flag);
	case 128:
		/* /dev/vga */
		return vgaioctl(dev,cmd,data,flag);
	default:
		return ENOTTY;
	}
	/*NOTREACHED*/
}

/* might be put into vga.c, because it is its memory */
int comap(dev_t dev, int offset, int nprot)
{
	if (offset > 0x20000)
		return -1;
	return i386_btop((0xa0000 + offset));
}

void kbd_wait_ibf()
{
	int i;

	for (i=0; i<KBDTIMEOUT; i++) {
		if ((inb(KBSTATP) & KBS_IBF)==0) return;
	}
	/* keyboard controller seems to hang */
	/* may consider reset ? */
}
	
void kbd_wait_obf() 
{
	int i;
	for (i=0; i<KBDTIMEOUT; i++) {
		if (inb(KBSTATP) & KBS_DIB) return;
	}
	/* timeout, expected answer did not arrive in time */
	/* data read now may be void ? */
}

#define kbd_ctrlcmd(cmd) kbc_8042cmd(cmd)
#define kbd_flush() if (inb(KBSTATP) & KBS_DIB) (void)inb(KBDATAP)

static unsigned kbd_cmd(cmd) {
	kbd_wait_ibf(); 		/* wait until buffer is free for data */
	if (cmd) outb(KBOUTP, cmd);	/* output the data */
	kbd_wait_ibf();			/* wait until buffer is available again */
	return inb(KBDATAP); 		/* read something (!) from the data port */
}

void kbd_warmreset()
{
	/* flush the keyboard buffer */
	kbd_flush();
	
	/* set the cntrl command byte */
#ifdef XTKBDMODE
#define KC8_CTRL	(KC8_TRANS|KC8_IGNSEC|KC8_CPU|KC8_IEN)	
#else
#define KC8_CTRL	(KC8_IGNSEC|KC8_CPU|KC8_IEN)
#endif

	/* Enable interrupts and keyboard controller */
	kbd_wait_ibf();
	outb(KBCMDP,KBC_CMDWRITE);
	kbd_wait_ibf();
	outb(KBOUTP,KC8_CTRL);
}

void kbd_coldreset()
{
	u_char c;
	int i, again = 0;
	
	/* flush the input buffer and set the mode */
	kbd_warmreset();

	/* Start keyboard stuff RESET */
	DELAY(1000);				/* 11 Sep 92 : !!CROCK!!*/
	kbd_cmd(KBC_RESET);
	i = KBDRETRIES;
	while(i-- && (c = inb(KBDATAP)) != KBR_ACK) {
		if ((c == KBR_RESEND) ||  (c == KBR_OVERRUN)) {
			if(!again)printf("KEYBOARD disconnected: RECONNECT \n");
			DELAY(1000);		/* 11 Sep 92 : !!CROCK!!*/
			kbd_cmd(KBC_RESET);
			again = 1;
		}
	}

	i = KBDTIMEOUT;
	while(i-- && (c=inb(KBDATAP)) != KBR_RSTDONE);

	/* get keyboard type */
	kbd_wait_ibf();
	outb(KBCMDP,KBC_RDID);
	i = KBDTIMEOUT;
	while (i-- > 0  && (kbs.id=inb(KBDATAP)) == KBR_RSTDONE);
/*	kbd_wait_obf();*/
	if (!i) kbs.id = 0xFF;
/*	kbs.id = inb(KBDATAP);*/

	resetdone = 1;
}

/* set the LEDs */
void kbd_setleds(int ledval)
{
	kbs.ledstate = ledval;

	kbd_cmd(KBC_STSIND);
	outb(KBOUTP,ledval);
}

#ifndef MINITERM
/* set the typematic rate */
void kbd_settpmrate(int rate)
{
	kbs.tpmrate = rate & 0x7f;

	kbd_cmd(KBC_SETTPM);
	outb(KBOUTP, kbs.tpmrate);
}
#endif /*!MINITERM*/

int coprobe(struct isa_device *dev)
{
	register struct consoftc *p = &consoftc;
	register struct kbdstate *k = &kbs;

	/* reset consoftc structure */
	p->cs_flags = 0;
	p->cs_timo = 0;
	p->cs_wedgecnt = 0l;
	p->cs_ovfl = 0l;
	p->cs_opencnt = 0;

	k->pitch = 0x31b;
	k->duration = hz/4;
	k->ledstate = 0;
	k->repeat = 1;

	extended = 
	vds.blanking = 0;

	/* VT switch semaphore */
	vtswsema = 0;

	/* init keyboard ring buffer */
	p->cs_flags |= CO_INITRB;
	initrb(&co_buf);

	/* reset keyboard */
	kbd_coldreset();

	kbd_setleds(leds(actvty));

#ifndef MINITERM
	k->tpmrate = /*KBD_TPD500|KBD_TPM100*/ 0x20|0x0c;
	kbd_settpmrate(k->tpmrate);
#endif

	/* clear the overlay table */
	kbd_ovlinit();

	return 1;
}

#define MAXKEYNUM	127
#ifndef XTKBDMODE

#define KMASK 0xFF
#define BREAKKEY	0x80
#define PRINTKEY	0x7C

/* This one is for AT scan codes (preferred) */
static char scantokey[] = {
/*      -0-  -1-  -2-  -3-  -4-  -5-  -6-  -7-    This layout is valid for US only */
/*00*/   0, 120,   0, 116, 114, 112, 113, 123,  /* ??  F9  ??  F5  F3  F1  F2  F12 */
/*08*/   0, 121, 119, 117, 115,  16,   1,   0,  /* ??  F10 F8  F6  F4  TAB `   ??  */
/*10*/   0,  60,  44,   0,  58,  17,   2,   0,  /* ??  ALl SHl ??  CTl Q   1   ??  */
/*18*/   0,   0,  46,  32,  31,  18,   3,   0,  /* ??  Z   S   A   W   2   ??  ??  */
/*20*/   0,  48,  47,  33,  19,   5,   4,   0,  /* ??  C   X   D   E   4   3   ??  */
/*28*/   0,  61,  49,  34,  21,  20,   6,   0,  /* ??  SP  V   F   T   R   5   ??  */
/*30*/   0,  51,  50,  36,  35,  22,   7,   0,  /* ??  N   B   H   G   Y   6   ??  */
/*38*/   0,   0,  52,  37,  23,   8,   9,   0,  /* ??  ??  M   J   U   7   8   ??  */
/*40*/   0,  53,  38,  24,  25,  11,  10,   0,  /* ??  ,   K   I   O   0   9   ??  */
/*48*/   0,  54,  55,  39,  40,  26,  12,   0,  /* ??  .   /   L   ;   P   -   ??  */
/*50*/   0,   0,  41,   0,  27,  13,   0,   0,  /* ??  ??  "   ??  [   =   ??  ??  */
/*58*/  30,  57,  43,  28,   0,  29,   0,   0,  /* CAP SHr ENT ]   ??  \   ??  ??  */
/*60*/   0,  45,   0,   0,   0,   0,  15,   0,  /* ??  NL1 ??  ??  ??  ??  BS  ??  */
/*68*/   0,  93,   0,  92,  91,   0,   0,   0,  /* ??  KP1 ??  KP4 KP7 ??  ??  ??  */
/*70*/  99, 104,  98,  97, 102,  96, 110,  90,  /* KP0 KP. KP2 KP5 KP6 KP8 ESC NUM */
/*78*/ 122, 106, 103, 105, 100, 101, 125,   0,  /* F11 KP+ KP3 KP- KP* KP9 LOC ??  */
/*80*/ 126,   0,   0, 118,                      /* BREAK  ??  ??  F7 */
/*     ^^^
 * -hv- we use 0x80 for a pseudo scan code for this shit break key
 */
};

static char extscantokey[] = {
/*      -0-  -1-  -2-  -3-  -4-  -5-  -6-  -7-     This layout is valid for US only */
/*00*/   0, 120,   0, 116, 114, 112, 113, 123,  /* ??  F9  ??  F5  F3  F1  F2  F12 */
/*08*/   0, 121, 119, 117, 115,  16,   1,   0,  /* ??  F10 F8  F6  F4  TAB `   ??  */
/*10*/   0,  62, 124,   0,  64,  17,   2,   0,  /* ??  ALr PSc ??  CTr Q   1   ??  */
/*18*/   0,   0,  46,  32,  31,  18,   3,   0,  /* ??  Z   S   A   W   2   ??  ??  */
/*20*/   0,  48,  47,  33,  19,   5,   4,   0,  /* ??  C   X   D   E   4   3   ??  */
/*28*/   0,  61,  49,  34,  21,  20,   6,   0,  /* ??  SP  V   F   T   R   5   ??  */
/*30*/   0,  51,  50,  36,  35,  22,   7,   0,  /* ??  N   B   H   G   Y   6   ??  */
/*38*/   0,   0,  52,  37,  23,   8,   9,   0,  /* ??  ??  M   J   U   7   8   ??  */
/*40*/   0,  53,  38,  24,  25,  11,  10,   0,  /* ??  ,   K   I   O   0   9   ??  */
/*48*/   0,  54,  95,  39,  40,  26,  12,   0,  /* ??  .   KP/ L   ;   P   -   ??  */
/*50*/   0,   0,  41,   0,  27,  13,   0,   0,  /* ??  ??  "   ??  [   =   ??  ??  */
/*58*/  30,  57, 108,  28,   0,  29,   0,   0,  /* CAP  SHr KPE ]   ??  \  ??  ??  */
/*60*/   0,  45,   0,   0,   0,   0,  15,   0,  /* ??  NL1 ??  ??  ??  ??  BS  ??  */
/*68*/   0,  81,   0,  79,  80,   0,   0,   0,  /* ??  END ??  LA  HOM ??  ??  ??  */
/*70*/  75,  76,  84,  97,  89,  83, 110,  90,  /* INS DEL DA  KP5 RA  UA  ESC NUM */
/*78*/ 122, 106,  86, 105, 124,  85, 125,   0,  /* F11 KP+ PD  KP- PSc PU  LOC ??  */
/*80*/ 126,   0,   0, 118,                      /* BREAK  ??  ??  F7 */
};
#else

/* in SNAFU mode, try this ones (XTKBDMODE) */

#define KMASK 0x7F
#define BREAKKEY	0x7F
#define PRINTKEY	0x7E

static char scantokey[] = {
/*      -0-  -1-  -2-  -3-  -4-  -5-  -6-  -7-    This layout is valid for US only */
/*00*/    0, 110,   2,   3,   4,   5,   6,   7,  /* ??  F9  1   2   3   4   5   6   */
/*08*/    8,   9,  10,  11,  12,  13,  15,  16,  /* 7   8   9   0   -   =   BS  TAB */
/*10*/   17,  18,  19,  20,  21,  22,  23,  24,  /* Q   W   E   R   T   Y   U   I   */
/*18*/   25,  26,  27,  28,  43,  58,  31,  32,  /* O   P   [   ]   ENT CTl A   S   */
/*20*/   33,  34,  35,  36,  37,  38,  39,  40,  /* D   F   G   H   J   K   L   ;   */
/*28*/   41,   1,  44,  29,  46,  47,  48,  49,  /* '   '   SHl NL1 Z   X   C   V   */
/*30*/   50,  51,  52,  53,  54,  55,  57, 100,  /* B   N   M   ,   .   /   SHr KP* */
/*38*/   60,  61,  30, 112, 113, 114, 115, 116,  /* ALl SP  CAP F1  F2  F3  F4  F5  */
/*40*/  117, 118, 119, 120, 121,  90, 125,  91,  /* F6  F7  F8  F9  F10 NUM LOC KP7 */
/*48*/   96, 101, 105,  92,  97, 102, 106,  93,  /* KP8 KP9 KP- KP4 KP5 KP6 KP+ KP1 */
/*50*/   98, 103,  99, 104,   0,   0,  56, 122,  /* KP2 KP3 KP0 KP. ??  ??  NL2 F11 */
/*58*/  123,   0,   0,   0,   0,   0,   0,   0,  /* F12 ??  ??  ??  ??  ??  ??  ??  */
/*60*/    0,   0,   0,   0,   0,   0,   0,   0,  /* ??  ??  ??  ??  ??  ??  ??  ??  */
/*68*/    0,   0,   0,   0,   0,   0,   0,   0,  /* ??  ??  ??  ??  ??  ??  ??  ??  */
/*70*/    0,   0,   0,   0,   0,   0,   0,   0,  /* ??  ??  ??  ??  ??  ??  ??  ??  */
/*78*/    0,   0,   0,   0,   0,   0, 124, 126,  /* ??  ??  ??  ??  ??  ??  ??  ??  */
/*                                    ^^^  ^^^
 * -hv- we use 0x7E for a pseudo scan code for the printscreen kludge
 * -hv- we use 0x7f for a pseudo scan code for this shit break key
 */
};

static char extscantokey[] = {
/*      -0-  -1-  -2-  -3-  -4-  -5-  -6-  -7-    This layout is valid for US only */
/*00*/    0, 110,   2,   3,   4,   5,   6,   7,  /* ??  F9  1   2   3   4   5   6   */
/*08*/    8,   9,  10,  11,  12,  13,  15,  16,  /* 7   8   9   0   -   =   BS  TAB */
/*10*/   17,  18,  19,  20,  21,  22,  23,  24,  /* Q   W   E   R   T   Y   U   I   */
/*18*/   25,  26,  27,  28, 108,  64,  31,  32,  /* O   P   [   ]   KPE CTr A   S   */
/*20*/   33,  34,  35,  36,  37,  38,  39,  40,  /* D   F   G   H   J   K   L   ;   */
/*28*/   41,   1,  44,  29,  46,  47,  48,  49,  /* '   '   SHl NL1 Z   X   C   V   */
/*30*/   50,  51,  52,  53,  54,  95,  57, 100,  /* B   N   M   ,   .   KP/ SHr KP* */
/*38*/   62,  61,  30, 112, 113, 114, 115, 116,  /* ALr SP  CAP F1  F2  F3  F4  F5  */
/*40*/  117, 118, 119, 120, 121,  90, 125,  80,  /* F6  F7  F8  F9  F10 NUM LOC HOM */
/*48*/   83,  85, 105,  79,  97,  89, 106,  81,  /* UA  PU  KP- LA  KP5 RA  KP+ END */
/*50*/   84,  86,  75,  76,   0,   0,  56, 122,  /* DA  PD  INS DEL ??  ??  NL2 F11 */
/*58*/  123,   0,   0,   0,   0,   0,   0,   0,  /* F12 ??  ??  ??  ??  ??  ??  ??  */
/*60*/    0,   0,   0,   0,   0,   0,   0,   0,  /* ??  ??  ??  ??  ??  ??  ??  ??  */
/*68*/    0,   0,   0,   0,   0,   0,   0,   0,  /* ??  ??  ??  ??  ??  ??  ??  ??  */
/*70*/    0,   0,   0,   0,   0,   0,   0,   0,  /* ??  ??  ??  ??  ??  ??  ??  ??  */
/*78*/    0,   0,   0,   0,   0,   0, 124, 126,  /* ??  ??  ??  ??  ??  ??  ??  ??  */
/*                                    ^^^  ^^^
 * -hv- we use 0x7E for a pseudo scan code for the printscreen kludge
 * -hv- we use 0x7f for a pseudo scan code for this shit break key
 */
};
#endif

static int key2scan(int keynum) 
{
	int i = 0;
	while (i<=MAXKEYNUM && keynum != scantokey[i]) i++;
	return i==MAXKEYNUM ? -1 : i;
}

#define KBD_ALPHA	KBD_ASCII|KBD_DOCAPS
Keycap_def	kbd_keytab[] =
{
/*      type       ptr unshift   	shift     	ctrl    */
/* DONT EVER OVERLOAD KEY 0, THIS IS A KEY THAT MUSTN'T EXIST */
/*  0*/ KBD_NONE,   0, XC2('d','f'),	XC0,		XC0,
/*  1*/ KBD_ASCII,  0, XC1('`'),	XC1('~'),	XC1('`'),
/*  2*/ KBD_ASCII,  0, XC1('1'),	XC1('!'),	XC1('!'),
/*  3*/ KBD_ASCII,  0, XC1('2'),	XC1('@'),	XC1(0),
/*  4*/ KBD_ASCII,  0, XC1('3'),	XC1('#'),	XC1('#'),
/*  5*/ KBD_ASCII,  0, XC1('4'),	XC1('$'),	XC1('$'),
/*  6*/ KBD_ASCII,  0, XC1('5'),	XC1('%'),	XC1('%'),
/*  7*/ KBD_ASCII,  0, XC1('6'),	XC1('^'),	XC1('\036'),
/*  8*/ KBD_ASCII,  0, XC1('7'),	XC1('&'),	XC1('&'),
/*  9*/ KBD_ASCII,  0, XC1('8'),	XC1('*'),	XC1('\010'),
/* 10*/ KBD_ASCII,  0, XC1('9'),	XC1('('),	XC1('('),
/* 11*/ KBD_ASCII,  0, XC1('0'),	XC1(')'),	XC1(')'),
/* 12*/ KBD_ASCII,  0, XC1('-'),	XC1('_'),	XC1('\037'),
/* 13*/ KBD_ASCII,  0, XC1('='),	XC1('+'),	XC1('+'),
/* 14*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 15*/ KBD_ASCII,  0, XC1('\177'),	XC1('\177'),	XC1('\010'),
/* 16*/ KBD_ASCII,  0, XC1('\t'),	XC2('\177','\t'),	XC1('\t'),
/* 17*/ KBD_ALPHA,  0, XC1('q'),	XC1('Q'),	XC1('\021'),
/* 18*/ KBD_ALPHA,  0, XC1('w'),	XC1('W'),	XC1('\027'),
/* 19*/ KBD_ALPHA,  0, XC1('e'),	XC1('E'),	XC1('\005'),
/* 20*/ KBD_ALPHA,  0, XC1('r'),	XC1('R'),	XC1('\022'),
/* 21*/ KBD_ALPHA,  0, XC1('t'),	XC1('T'),	XC1('\024'),
/* 22*/ KBD_ALPHA,  0, XC1('y'),	XC1('Y'),	XC1('\031'),
/* 23*/ KBD_ALPHA,  0, XC1('u'),	XC1('U'),	XC1('\025'),
/* 24*/ KBD_ALPHA,  0, XC1('i'),	XC1('I'),	XC1('\011'),
/* 25*/ KBD_ALPHA,  0, XC1('o'),	XC1('O'),	XC1('\017'),
/* 26*/ KBD_ALPHA,  0, XC1('p'),	XC1('P'),	XC1('\020'),
/* 27*/ KBD_ASCII,  0, XC1('['),	XC1('{'),	XC1('\033'),
/* 28*/ KBD_ASCII,  0, XC1(']'),	XC1('}'),	XC1('\035'),
/* 29*/ KBD_ASCII,  0, XC1('\\'),	XC1('|'),	XC1('\034'),
/* 30*/ KBD_CAPS,   0, XC0,		XC0,		XC0,
/* 31*/ KBD_ALPHA,  0, XC1('a'),	XC1('A'),	XC1('\001'),
/* 32*/ KBD_ALPHA,  0, XC1('s'),	XC1('S'),	XC1('\023'),
/* 33*/ KBD_ALPHA,  0, XC1('d'),	XC1('D'),	XC1('\004'),
/* 34*/ KBD_ALPHA,  0, XC1('f'),	XC1('F'),	XC1('\006'),
/* 35*/ KBD_ALPHA,  0, XC1('g'),	XC1('G'),	XC1('\007'),
/* 36*/ KBD_ALPHA,  0, XC1('h'),	XC1('H'),	XC1('\010'),
/* 37*/ KBD_ALPHA,  0, XC1('j'),	XC1('J'),	XC1('\n'),
/* 38*/ KBD_ALPHA,  0, XC1('k'),	XC1('K'),	XC1('\013'),
/* 39*/ KBD_ALPHA,  0, XC1('l'),	XC1('L'),	XC1('\014'),
/* 40*/ KBD_ASCII,  0, XC1(';'),	XC1(':'),	XC1(';'),
/* 41*/ KBD_ASCII,  0, XC1('\''),	XC1('"'),	XC1('\''),
/* 42*/ KBD_ASCII,  0, XC1('\\'),	XC1('|'),	XC1('\034'),	/* special */
/* 43*/ KBD_ASCII,  0, XC1('\r'),	XC1('\r'),	XC1('\n'),	/* RETURN */
/* 44*/ KBD_SHIFT,  0, XC0,		XC0,		XC0,	/* SHIFT left */
/* 45*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 46*/ KBD_ALPHA,  0, XC1('z'),	XC1('Z'),	XC1('\032'),
/* 47*/ KBD_ALPHA,  0, XC1('x'),	XC1('X'),	XC1('\030'),
/* 48*/ KBD_ALPHA,  0, XC1('c'),	XC1('C'),	XC1('\003'),
/* 49*/ KBD_ALPHA,  0, XC1('v'),	XC1('V'),	XC1('\026'),
/* 50*/ KBD_ALPHA,  0, XC1('b'),	XC1('B'),	XC1('\002'),
/* 51*/ KBD_ALPHA,  0, XC1('n'),	XC1('N'),	XC1('\016'),
/* 52*/ KBD_ALPHA,  0, XC1('m'),	XC1('M'),	XC1('\r'),
/* 53*/ KBD_ASCII,  0, XC1(','),	XC1('<'),	XC1('<'),
/* 54*/ KBD_ASCII,  0, XC1('.'),	XC1('>'),	XC1('>'),
/* 55*/ KBD_ASCII,  0, XC1('/'),	XC1('?'),	XC1('\177'),
/* 56*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 57*/ KBD_SHIFT,  0, XC0,		XC0,		XC0,	/* SHIFT right */
/* 58*/ KBD_CTL,    0, XC0,		XC0,		XC0,	/* CTL left */
/* 59*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 60*/ KBD_META,   0, XC0,		XC0,		XC0,	/* ALT left */
/* 61*/ KBD_ASCII,  0, XC1(' '),	XC1(' '),	XC1(' '),         /* SPACE */
/* 62*/ KBD_META,   0, XC0,		XC0,		XC0,	/* ALT right */
/* 63*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 64*/ KBD_CTL,    0, XC0,		XC0,		XC0,	/* CTL right */
/* 65*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 66*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 67*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 68*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 69*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 70*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 71*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 72*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 73*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 74*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 75*/ KBD_FUNC,   0, XE3('[','L'),	XE3('|','a'),	XE3('|','b'),    /* INS */
/* 76*/ KBD_FUNC,   0, XC1('\177'),	XE3('|','c'),	XE3('|','d'),    /* DEL */
/* 77*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 78*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 79*/ KBD_FUNC,   0, XE3('[','D'),	XE3('|','e'),	XE3('|','f'),    /* CU <- */
/* 80*/ KBD_FUNC,   0, XE3('[','H'),	XE3('|','g'),	XE3('|','h'),    /* HOME */
/* 81*/ KBD_FUNC,   0, XE3('[','F'),	XE3('|','i'),	XE3('|','j'),    /* END */
/* 82*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 83*/ KBD_FUNC,   0, XE3('[','A'),	XE3('|','k'),	XE3('|','l'),	/* CU ^ */
/* 84*/ KBD_FUNC,   0, XE3('[','B'),	XE3('|','m'),	XE3('|','n'),	/* CU v */
/* 85*/ KBD_FUNC,   0, XE3('[','I'),	XE3('|','o'),	XE3('|','p'),	/* PG UP */
/* 86*/ KBD_FUNC,   0, XE3('[','G'),	XE3('|','q'),	XE3('|','r'),	/* PG DN */
/* 87*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 88*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 89*/ KBD_FUNC,   0, XE3('[','C'),	XE3('|','s'),	XE3('|','t'),	/* CU -> */
/* 90*/ KBD_NUM,    0, XC0,		XC0,		XC0,
/* 91*/ KBD_KP,     0, XC1('7'),	XE3('[','H'),	XC1('7'),
/* 92*/ KBD_KP,     0, XC1('4'),	XE3('[','D'),	XC1('4'),
/* 93*/ KBD_KP,     0, XC1('1'),	XE3('[','F'),	XC1('1'),
/* 94*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/* 95*/ KBD_KP,     0, XC1('/'),	XC1('/'),	XC1('/'),
/* 96*/ KBD_KP,     0, XC1('8'),	XE3('[','A'),	XC1('8'),
/* 97*/ KBD_KP,     0, XC1('5'),	XE3('[','E'),	XC1('5'),
/* 98*/ KBD_KP,     0, XC1('2'),	XE3('[','B'),	XC1('2'),
/* 99*/ KBD_KP,     0, XC1('0'),	XE3('[','L'),	XC1('0'),
/*100*/ KBD_KP,     0, XC1('*'),	XC1('*'),	XC1('*'),
/*101*/ KBD_KP,     0, XC1('9'),	XE3('[','I'),	XC1('9'),
/*102*/ KBD_KP,     0, XC1('6'),	XE3('[','C'),	XC1('6'),
/*103*/ KBD_KP,     0, XC1('3'),	XE3('[','G'),	XC1('3'),
/*104*/ KBD_KP,     0, XC1('.'),	XC1('\177'),	XC1('.'),
/*105*/ KBD_KP,     0, XC1('-'),	XC1('-'),	XC1('-'),
/*106*/ KBD_KP,     0, XC1('+'),	XC1('+'),	XC1('+'),
/*107*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/*108*/ KBD_ASCII,  0, XC1('\r'),	XC1('\r'),	XC1('\n'),	/* RETURN */
/*109*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/*110*/ KBD_ASCII,  0, XC1('\033'),	XC1('\033'),	XC1('\033'),
/*111*/ KBD_NONE,   0, XC0,		XC0,		XC0,
/*112*/ KBD_FUNC,   0, XE3('[','M'),	XE3('[','Y'),	XE3('[','k'),	/* F1 */
/*113*/ KBD_FUNC,   0, XE3('[','N'),	XE3('[','Z'),	XE3('[','l'),	/* F2 */
/*114*/ KBD_FUNC,   0, XE3('[','O'),	XE3('[','a'),	XE3('[','m'),	/* F3 */
/*115*/ KBD_FUNC,   0, XE3('[','P'),	XE3('[','b'),	XE3('[','n'),	/* F4 */
/*116*/ KBD_FUNC,   0, XE3('[','Q'),	XE3('[','c'),	XE3('[','o'),	/* F5 */
/*117*/ KBD_FUNC,   0, XE3('[','R'),	XE3('[','d'),	XE3('[','p'),	/* F6 */
/*118*/ KBD_FUNC,   0, XE3('[','S'),	XE3('[','e'),	XE3('[','q'),	/* F7 */
/*119*/ KBD_FUNC,   0, XE3('[','T'),	XE3('[','f'),	XE3('[','r'),	/* F8 */
/*120*/ KBD_FUNC,   0, XE3('[','U'),	XE3('[','g'),	XE3('[','s'),	/* F9 */
/*121*/ KBD_FUNC,   0, XE3('[','V'),	XE3('[','h'),	XE3('[','t'),	/* F10 */
/*122*/ KBD_FUNC,   0, XE3('[','W'),	XE3('[','i'),	XE3('[','u'),	/* F11 */
/*123*/ KBD_FUNC,   0, XE3('[','X'),	XE3('[','j'),	XE3('[','v'),	/* F12 */
/*124*/ KBD_KP,     0, XE3('[','w'),	XE3('[','x'),	XE3('[','y'),	
/*125*/ KBD_SCROLL, 0, XC0,		XC0,		XC0,
/*126*/ KBD_BREAK,  0, XC0,		XC0,		XC0,
/*127*/ KBD_NONE,   0, XC0,		XC0,		XC0,
};

static char	keypad2num[] = {
	7, 4, 1, -1, -1, 8, 5, 2, 0, -1, 9, 6, 3, -1, -1, -1, -1
};

/*
 * Overloaded definitions are stored in Ovl_tbl structure,
 * allocated via malloc() and pointed by kbd_keytab[].ovlptr. -vak-
 * A table entry consists of
 * a short, holding the new type attribute and 4 entries for a new 
 * keydef. 
 * Note that as long there is enough space, you may overload any key:
 * You may assign "rm -rf /\n" to the ENTER key, so you have it directly,
 * when you need it, but you can also make a big SHIFT key by overloading
 * the SPACE bar. But these are the more uncommon cases.
 */

void kbd_ovlinit()
{
	register i;
	
	for (i=0; i<=MAXKEYNUM; i++)
		kbd_keytab[i].type &= (KBD_MASK|KBD_DOCAPS);
	
	kbs.m0flag = kbs.c0flag = kbs.a0flag = 0;
}

/* get original key def */
int kbd_getokeydef(u_int key,Ovl_tbl *thisdef)
{
	if (key==0 || key>MAXKEYNUM) return EINVAL;

	thisdef->keynum = key;
	thisdef->type = kbd_keytab[key].type;

#define copydef(src,dst)\
	xc_bcopy(src,dst,KBDDEFOVLKEYSIZE)

	copydef(kbd_keytab[key].unshift,thisdef->unshift);
	copydef(kbd_keytab[key].shift,thisdef->shift);
	copydef(kbd_keytab[key].ctrl,thisdef->ctrl);

	copydef(kbd_keytab[key].unshift,thisdef->altgr); /* deliver at least anything */
	copydef(kbd_keytab[key].shift,thisdef->shiftaltgr);
	copydef(kbd_keytab[key].unshift,thisdef->meta);
/* this defeated EMACS META-SPACE */
/*	if (thisdef->meta [0]) */
		thisdef->meta[0] ^= 0x80;
#undef copydef

	return 0;
}

/* get current key def */
int kbd_getckeydef(u_int key,Ovl_tbl *thisdef)
{
	if (key>MAXKEYNUM) 
		return EINVAL;

	if (kbd_keytab[key].type & KBD_OVERLOAD)
		*thisdef = *kbd_keytab[key].ovlptr;
	else
		kbd_getokeydef(key,thisdef);

	return 0;
}

Spec_tbl *spec_tbl = NULL;
static int kbd_nrspec = 0;

#define SPECINVALID	((Spec_tbl*)0xFFFFFFFF)

static Spec_tbl *kbd_findspec(u_short key,u_short modifier)
{
	int i;

	if (!spec_tbl) spec_tbl = malloc (
		MAXNROFSPEC*sizeof(Spec_tbl), M_DEVBUF, M_WAITOK);
	/* hv must check whether space is really there */
	if (!spec_tbl) return SPECINVALID;

	for (i=0; i<kbd_nrspec; i++)
		if (key==spec_tbl[i].key)
			if (modifier==0 || 
			    modifier == spec_tbl[i].modifier)
				return &spec_tbl[i];
	return 0;
}

int kbd_gethotkey(struct kbd_hotkey *data)
{
	Spec_tbl *p;

	if (data->key==0 || data->key > MAXKEYNUM)
		return EINVAL;
	p = kbd_findspec(data->key,data->modifier);
	if (p==SPECINVALID) return ENOMEM;
	if (p) {
		data->key = p->key;
		data->modifier = p->modifier;
		data->function = p->function;
	} else
		/* not found, clear all */
		data->key = data->modifier = data->function = 0;

	return 0;
}

int kbd_sethotkey(struct kbd_hotkey *data)
{
	int k;
	Spec_tbl *p;

	if (data->key > MAXKEYNUM)
		return EINVAL;
	p = kbd_findspec(data->key,data->modifier);
	if (p==SPECINVALID) return ENOMEM;
	if (!p) {
		/* not found: enter. Find empty slot */
		for (k=0; k<kbd_nrspec; k++)
			if (spec_tbl[k].key==0 && spec_tbl[k].modifier==0)
				break;
		if (k>=MAXNROFSPEC) return ENOMEM;
		p = &spec_tbl[k];
	} else k = -1;

	if (data->modifier == KBD_HOTKEYDELETE) {
		p->key = 0;
		p->modifier = 0;
		p->function = 0;
		p->scan = 0;
	} else
	{
		p->key = data->key;
		p->modifier = data->modifier;
		p->function = data->function;
		p->scan = key2scan(p->key);
		if (k==kbd_nrspec) kbd_nrspec++;
	}
	return 0;
}

#ifndef MINITERM
/*
 * management of diacritical characters
 * in the future this table will be editable
 */
static char *diacriticals[] =
{
	0,
	"` ```A\300a\340E\310e\350I\314i\354O\322o\362U\331u\371",
	"' '''A\301a\341E\311e\351I\315i\355O\323o\363U\332u\372Y\335",
	"^ ^^^A\302a\342E\312e\312I\316i\356O\324o\364U\333u\373",
	"~ ~~~A\303a\343N\321n\361O\325o\365",
	"\250A\304a\344\E\313e\353I\316i\357O\326o\366U\334u\374y\377",
	"\270C\307c\347",
	"\260\260\260A\305a\345",
	0
};
static char diacflag = 0;

static XCHAR *kbd_xlatdiac(u_short type,XCHAR *ch)
{
	int i;
	char *s;

	/* may by NULL pointer, a rich source for crashes */
	if (!ch) return ch;
	
	if (diacflag) {
		for (s=diacriticals[diacflag]+1, diacflag=0; *s; s+=2)
			if (*s==(char)*ch) {
				*ch= (XCHAR)*(s+1);
				return ch;
			}
		return ch;
	} else
	{
		if (type & KBD_DIACPFX) {
			for (i=1; diacriticals[i]; i++)
				if (*ch== *(diacriticals[i])) {
					diacflag = i;
					return 0;
				}
		}
		return ch;
	}
}
#endif /*!MINITERM*/

/*
 * kbd_xlatkey2ascii: takes a keynum and the current shift/ctrl/etc. state and
 * delivers an appropriate ASCII string
 */
static XCHAR *kbd_xlatkey2ascii(u_int key)
{
	struct vty	*vp = actvty;
	struct consoftc	*cs = &consoftc;
	static XCHAR	metachar[2];
	static Ovl_tbl	thisdef;
	u_short		n,type;
	char		isreleased = key & 0x80;
	int		mask;
	int		keycode= key & 0x7f;
	
	/* ignore the NON-KEY */
	if (keycode==0)
		return 0;

	/* get the current ASCII value */
	kbd_getckeydef(keycode, &thisdef);
	type = thisdef.type & KBD_MASK;

	switch (type) {
	case KBD_SHFTLOCK:
		if (!isreleased) {
			vp->shiftlock ^= 1;
			kbd_setleds (leds (vp));
		}
		return 0;
	case KBD_ALTGRLOCK:
		if (!isreleased) {
			vp->altgrlock ^= 1;
			kbd_setleds (leds (vp));
		}
		return 0;
	case KBD_NUM:
		if (!isreleased) {
			vp->num ^= 1;
			kbd_setleds (leds (vp));
		}
		return 0;
	case KBD_CAPS:
		if (!isreleased) {
			vp->caps ^= 1;
			kbd_setleds (leds (vp));
		}
		return 0;
	case KBD_SCROLL:
		if (!isreleased) {
			vp->scroll ^= 1;
			kbd_setleds (leds (vp));
		}
		return 0;
	case KBD_SHIFT:
		kbs.shift_down = isreleased ? 0 : 1;
		return 0;
	case KBD_META:
		kbs.meta_down = isreleased ? 0 : 0x80;
		goto altnumpad;
	case KBD_ALTGR:
		kbs.altgr_down = isreleased ? 0 : 1;
altnumpad:
		/* special handling of ALT-KEYPAD */
		/* have we been in altkp mode? */
		if (isreleased) {
			if (altkpflag) {
				metachar[0] = altkpval;
				metachar[1] = 0;
				altkpflag = 0;
				altkpval = 0;
				kbs.altgr_down = 0;	/* -hv- was: hang in ALT-KP */
				return metachar;
			}
		}
		return 0;
	case KBD_CTL:
		kbs.ctrl_down = isreleased ? 0 : 1;
		/*fall thru*/
	default:
	case KBD_NONE:
		return 0;

	case KBD_BREAK:
	case KBD_ASCII:
	case KBD_FUNC:
		if (isreleased) return 0;
		more_chars = NULL;
		
		if (kbs.ctrl_down) {
			if (!kbs.c0flag || thisdef.type&KBD_OVERLOAD)
				more_chars = thisdef.ctrl;
		} else if (kbs.meta_down) {
			if (thisdef.type&KBD_OVERLOAD)
				more_chars = thisdef.meta;
			else {
				metachar[0] = thisdef.unshift[0] ^ 0x80;
				metachar[1] = 0;
				more_chars = metachar;
			}
		} else if (kbs.altgr_down || vp->altgrlock) {
			if (!kbs.a0flag || thisdef.type&KBD_OVERLOAD)
				if (kbs.shift_down || vp->shiftlock ||
				    vp->caps && (thisdef.type & KBD_DOALTCAPS))
					more_chars = thisdef.shiftaltgr;
				else
					more_chars = thisdef.altgr;
		} else if (kbs.shift_down || vp->shiftlock || vp->caps &&
			   (thisdef.type & KBD_DOCAPS))
			more_chars = thisdef.shift;
		else
			more_chars = thisdef.unshift;
#ifndef MINITERM
		return kbd_xlatdiac(thisdef.type,more_chars);
#else
		return more_chars;
#endif

	case KBD_KP:
		if (isreleased) return 0;

		more_chars = NULL;

		/* there is special if used with keypad */
		if (kbs.meta_down || kbs.altgr_down) {
			n = keypad2num[keycode-91];
			if (n>=0) {
				if (!altkpflag) {
					/* start ALT-KP mode */
					altkpflag = 1;
					altkpval = 0;
				}
				altkpval *= 10;
				altkpval += n;

				/* discard ALT-keypad mode if over 255 */
				if (altkpval > 255) {
					altkpflag = 0;
					altkpval = 0;
				}
			} else altkpflag = 0;
			/* will be emitted when ALT released */
			return 0;
		} 

		if (kbs.shift_down || vp->shiftlock || kbs.ctrl_down || !vp->num)
			more_chars = (kbs.altgr_down || vp->altgrlock) ?
				thisdef.shiftaltgr : thisdef.shift;
		else
			more_chars = (kbs.altgr_down || vp->altgrlock) ?
				thisdef.altgr : thisdef.unshift;

		return(more_chars);
	}
}

/*
 *   kbd_sgetc (noblock):  get ASCII strings from  the  keyboard.  If
 *   noblock  ==  0  wait  until a key is gotten. Otherwise return a NULL
 *   if no characters are present.
 *
 */
XCHAR *kbd_sgetc(int noblock)
{
	u_int key;

	if (!resetdone) kbd_warmreset(); /* allows proper operation of ddb when 
					 * kernel dies before probing pc0
					 */
	key = kbd_sgetk(noblock);
	return kbd_xlatkey2ascii(key);
}

/* compatibility */
int sgetc(int noblock) {
	return (int)*kbd_sgetc(noblock);
}

/* 
 * kbd_sgetk: does most of the work and returns keynums
 */
static	u_char	breakseen = 0;
static	u_char	breakshit = 0;	/* -hv- for the polite one: 
				   read as "break's hit" :-)
				   Why couldn't IBM use the normal
				   convention for the break key? */
static	u_char	altseen = 0;
static	u_char	ctrlseen = 0;
static	u_char	shiftseen = 0;
static	u_char	syskeyseen = 0;
static	u_char	resetcnt = 0;
static	u_char	in_Debugger = 0;
static u_char	lockhotkey = 0xff;
#ifdef XTKBDMODE
static	u_char	lsh = 0;
#endif

static Spec_tbl *kbd_ishotkey(int scan) 
{
	int i;

	for (i=0; i<kbd_nrspec; i++)
		if (spec_tbl[i].scan == scan) return &spec_tbl[i];
	return 0;
}

static int kbd_dohotkey(Spec_tbl *ptr)
{
	int func;

	/* bit0=shift,bit1=ctrl,bit2=alt,bit3=syskey */
	static u_char masktbl[] = {
		KBD_EXT_N, KBD_EXT_S, KBD_EXT_C, KBD_EXT_C,
		KBD_EXT_A, KBD_EXT_A, KBD_EXT_CA,KBD_EXT_CA,
		KBD_EXT_SK,KBD_EXT_SK,KBD_EXT_SK,KBD_EXT_SK,
		KBD_EXT_SK,KBD_EXT_SK,KBD_EXT_CA,KBD_EXT_CA
	};
#define MASK masktbl[shiftseen|(ctrlseen<<1)|(altseen<<2)|(syskeyseen<<3)]

	if (ptr->modifier & MASK) {
		/* has triggered a hotkey function, now which */
		/* block function if repeated */
		if (lockhotkey==(func=ptr->function)) return 0;

		/* now perform the function */
		switch (func) {
		default:
#ifndef MINITERM
			if (func>=KBD_VTY0 && func<nvty)
				vty_setactive(func,1);
			break;
		case KBD_VTYUP:
			if (!vtswsema && (consoftc.cs_flags & CO_OPENRAW)==0)
				vty_next(); 
			break;
		case KBD_VTYDOWN:
			if (!vtswsema && (consoftc.cs_flags & CO_OPENRAW)==0)
				vty_previous(); 
#endif /*!MINITERM*/
			break;
		case KBD_RESETKEY:
			if (resetcnt==1)
				cpu_reset();
			else {
				vty_broadcast("WARNING: RESET WITHOUT SYNC. TO EXECUTE PRESS ctrl-alt-del AGAIN\n");
				resetcnt=1;
			}
			return;
		case KBD_DEBUGKEY:
#if NDDB > 0
			if (!in_Debugger) {
				in_Debugger = 1;
				Debugger();
				in_Debugger = 0;
				return 1;
			}
#endif
			break;
		}
		lockhotkey = func;
		return 1;	/* hotkey was done */
	}
	return 0;		/* was not done */
}

static u_int kbd_sgetk(int noblock)
{
	u_char		dt;
	u_int		key;
	u_short		type;
	int		delay;
	Spec_tbl	*sp;
	static char	keybuf[2];

loop:
	/*
	 *   First see if there is something in the keyboard port
	 *   Some keyboards may raise interrupt before putting characters
	 *   in the buffer (cf. Mach kd.c), so allow a little delay
	 *   and don't time out immediately
	 *   (sgetk is called from cointr!)
	 */
	delay = 1000;
	while (!(inb(KBSTATP) & KBS_DIB)) {
		if (noblock && delay-- ==0) return 0;
	}

	/* get the byte */
	dt = inb(KBDATAP);

	/* look what we got */
	switch (dt) {
	case KBR_DIAGFAIL:
		printf("Keyboard irq: keyboard sent DIAGNOSTIC FAILURE\n");
		kbd_coldreset();
		break;
	case KBR_RESEND:
	case KBR_ACK:
	case KBR_RSTDONE:
	case KBR_OVERRUN:
	case KBR_ECHO:		/* this is junk we do not want to see */
		break;
	case KBR_E0:		/* extended code? */
		extended = 1;
		break;
	case KBR_F0:		/* break code? */
		breakseen = 1;
		if (breakshit)		/* =5: wait for 14 */ 
			breakshit++;	/* =7: wait for 77 */
		break;
	case KBR_E1:		/* -hv- this shit BREAK key */
		/* we have to collect a sequence E1 14 77 E1 for make
		   and F0 14 F0 77 for break */
		breakshit++;	/* state=1: wait for 14, state=4: have make */
		if (breakshit==4) {
			dt = 0x80;	/* build replacement code */
			goto regular;
		}
		break;
		/* I don't know why the keyboard sends additional E0 F0 12 (make!)
		 * and E0 12 (break!), when an extended key is used with 
		 * shift, but this is certainly junk, which we avoid here
		 */
	case 0x12:
		if (extended) {
			extended = 0;
			breakseen= 0;
			break;
		} else
			goto regular;
	case 0x14:
		if (breakshit) 
			breakshit++;	/* =2: wait for 77 */ 
		else			/* =6: needs second break; */
			goto regular; 
		break;
	case 0x77:
		if (breakshit) 
			breakshit++;	/* =3: wait for E1 */ 
		else			/* =7: needs second break; */
			goto regular; 
		if (breakshit==8) {
			dt = BREAKKEY;	/* build replacement code */
			breakshit = 0;
			goto regular;
		}
		break;
	default:
		goto regular;	/* regular key */
	}		

rescan:
	if (noblock)
		return 0;
	else
		goto loop;

	/* got a normal scan key */
regular:

	/* process special non-movable key combinations */
	switch (dt) {
	case 0x14:	/* SCAN LEFT CTRL */
		ctrlseen = breakseen ? 0 : 1;
		break;
	case 0x11:	/* SCAN LEFT ALT */
		altseen = breakseen ? 0 : 1;
		break;
	case 0x12:	/* SCAN LEFT SHIFT */
		shiftseen = breakseen ? 0 : 1;
		break;
	case 0x7C:	/* Print/SysRq */
		if (breakseen)
			syskeyseen ^= 1;
		resetcnt = 0;
		break;
	default:
		/* check and process hotkey functions */
		if ((sp=kbd_ishotkey(dt))) {
			if (kbd_dohotkey(sp))
				goto rescan;
		} else
			lockhotkey = 0xff;
	}
	if (!breakseen) resetcnt = 0;

	/*
	 *   make a keycode from scan code 
	 */
	key = extended ? extscantokey[dt&KMASK] : scantokey[dt&KMASK];
	extended = 0;

	/* in NOREPEAT MODE ignore the key if it was the same as before */
	if (!kbs.repeat && key == lastkey && !breakseen) {
		if (noblock)
			return 0;
		else
			goto loop;
	}
	
	/* -hv- processing of special keys moved to kbd_xlatkey2ascii */
	if (breakseen) {
		key |= 0x80;
		breakseen = 0;
		lastkey = 0;		/* -hv- I know this is a bug with */
	} else				/* N-Key-Rollover, but I ignore that */
	{				/* because avoidance is too complicated */
		lastkey = key;
	}
	
	/* have a key */
	return key;
}

/*
 *   Utility functions for IOCTL
 */
int kbd_rmkeydef(u_int key)
{
	register Ovl_tbl *ref;
	
	if (key==0 || key>MAXKEYNUM) return EINVAL;

	if (kbd_keytab[key].type & KBD_OVERLOAD)
		kbd_keytab[key].type &= (KBD_MASK|KBD_DOCAPS);

	return 0;
}	

int kbd_setkeydef(Ovl_tbl *data)
{
	/* valid */
	if (data->keynum>MAXKEYNUM ||
	   (data->type&KBD_MASK)==KBD_BREAK ||
	   (data->type&KBD_MASK) > KBD_ALTGRLOCK)
		return EINVAL;

	/* limit junk */
	data->unshift[KBDMAXOVLKEYSIZE] =
	data->shift[KBDMAXOVLKEYSIZE] =
	data->ctrl[KBDMAXOVLKEYSIZE] =
	data->meta[KBDMAXOVLKEYSIZE] =
	data->shiftaltgr[KBDMAXOVLKEYSIZE] =
	data->altgr[KBDMAXOVLKEYSIZE] = 0;

	/* mark overload */
	data->type |= KBD_OVERLOAD;

	/* if key already overloaded, use this slot */
	if (! kbd_keytab[data->keynum].ovlptr) {
		/* allocate new slot */
		kbd_keytab[data->keynum].ovlptr =
			(Ovl_tbl *) malloc (sizeof (Ovl_tbl), M_DEVBUF, M_WAITOK);
		if (! kbd_keytab[data->keynum].ovlptr)
			return ENOSPC;	/* no space, abuse of ENOSPC(!) */
	}

	/* copy */
	*kbd_keytab[data->keynum].ovlptr = *data;

	/* mark key */
	kbd_keytab[data->keynum].type |= KBD_OVERLOAD;
	return 0;
}

int kbd_cvtsound(int ipitch, int *opitch, int idur, int *odur)
{
	if (ipitch <= 0)
		ipitch = 1500, idur = 250;
	else if (idur<=0 || idur>=10000)
		idur = 250;
	*opitch = 1193180 / ipitch;
	*odur = idur * hz / 1000;
	return 0;
}

int pc_xmode = 0;	/* someone looks for this in the symbol table */

int kbd_setxserveriopl(int mode)
{
	struct syscframe *fp;
	int error;
	
	/* do we have the necessary privilege? */
	if (error=suser(curproc->p_ucred, &curproc->p_acflag))
		return error;

	/* we have it, caller knows what he does :-) */
	fp = (struct syscframe *)curproc->p_regs;

	if (mode) {	/* switch on */
		if (pc_xmode) return 0;
		pc_xmode = 1;
		fp->sf_eflags |= PSL_IOPL;
		vga_doblanking(BLANKSTOP);	/* does not really belong here */
	} else
	{	/* switch off */
		if (!pc_xmode) return 0;
		pc_xmode = 0;
		fp->sf_eflags &= ~PSL_IOPL;
		vga_doblanking(BLANKSTART);	/* does not really belong here */
	}
	return 0;
}

#endif /* NPC=0 */
#endif /* NCO=1 */
