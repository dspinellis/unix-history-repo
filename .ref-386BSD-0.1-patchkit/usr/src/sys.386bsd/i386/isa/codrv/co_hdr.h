/*
 * General declarations for CO driver
 *
 *      Copyright 1992 by Holger Veit
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
 *	@(#) $RCSfile: co_hdr.h,v $	$Revision: 1.11 $ (Contributed to 386bsd) $Date: 93/01/23 23:14:39 $
 *
 *	History: see CO_HISTORY
 */

#ifndef _CO_HDR_H_
#define _CO_HDR_H_
#ifndef GLOBAL
#define GLOBAL extern
#endif

#include "param.h"
#include "malloc.h"
#include "conf.h"
#include "ioctl.h"
#include "proc.h"
#include "user.h"
#include "tty.h"
#include "uio.h"
#include "fcntl.h"
#include "i386/isa/isa_device.h"
#include "callout.h"
#include "systm.h"
#include "kernel.h"
#include "vnode.h"
#include "syslog.h"
#include "i386/isa/icu.h"
#include "i386/i386/cons.h"
#include "i386/isa/isa.h"
#include "i386/isa/ic/i8042.h"
#include "i386/isa/kbd.h"
#include "machine/pc/display.h"
#include "machine/psl.h"
#include "machine/frame.h"
#include "ioctl_pc.h"

/* 
 *  user adjustable constants
 */

/*#define NETBSD*/		/* define this if you have NetBSD */

#define SCRSAVER	2	/* 1=standard moving block */
				/* 2=moving snake (thanks: Christoph Robitschko) */

#define	BLANKTIMEOUT	10*60	/* Default Screen blanking after 10*60 seconds */
#define KBDTIMEOUT	0x15000l /* timeout parameter for a keyboard response
				  * adjust for slow keyboard at a fast computer
				  * (e.g. 486DX laptop)
				  */
#define KBDRETRIES	5	/* max. no. of tries to reset the keyboard in
				 * probing loop, avoids hanging forever if
				 * keyboard does not reply like expected
				 */

#define	FAT_CURSOR		/* full size cursor, undef if to leave unchanged */


/*  default screen attributes */
#define DEF_STD_BGAT	BG_BLACK	/* standard background */
#define DEF_STD_M_FGAT	FG_LIGHTGREY	/* standard mono foreground */
#define DEF_STD_C_FGAT	FG_LIGHTGREY	/* standard color foreground */
#define DEF_KERN_BGAT	BG_BLACK	/* kernelmode background */
#define DEF_KERN_M_FGAT	FG_UNDERLINE	/* kernelmode mono foreground */
#define DEF_KERN_C_FGAT	FG_LIGHTBLUE	/* XXX (to distinguish) kernelmode color foreground */
#define DEF_SO_M_AT	FG_INTENSE|BG_LIGHTGREY	/* standout mono mode */
#define DEF_SO_C_AT	FG_YELLOW|BG_BLACK	/* standout color mode */

/*****************NO USER DEFINABLE DATA BELOW THIS LINE*******************/

/* this is an anachronism in the moment: there is no ioctl other than CODRV1
 * public available. If you want to write a different ioctl set
 * based on the core system, read co_codrv1.c as a template.
 */
#define CODRV1	1

/* I dunno why this had to be changed! */
#ifndef NETBSD
#include "ddb.h"
#else /* is NetBSD */
#undef NDDB
#ifdef DDB
#define NDDB 1
#else
#define NDDB 0
#endif
#endif /* NetBSD */

#define	DEFAULTCOL	80
#define	DEFAULTROW	25
#define	CHR		2
			/* Don't think these locations are true virtual addrs! */
#define MONO_BUF	0xfe0b0000
#define CGA_BUF		0xfe0b8000
#define EGA_BIOS	0xfe0c0000
#define ALTEGA_BIOS     0xfe0e0000      /* alternate video BIOS location */
#define IOPHYSMEM	0xa0000

#define MONO_BASE	0x3b4
#define CGA_BASE	0x3d4

/* 6845 register definitions */
#define	M6845_ADDR	0
#define M6845_HTOTAL	0
#define M6845_HDISPL	1
#define M6845_HSYNC	2
#define M6845_SYNCWIDTH	3
#define M6845_VTOTAL	4
#define M6845_VTOTADJ	5
#define M6845_VDISPL	6
#define M6845_VSYNC	7
#define M6845_IMODE	8
#define M6845_MAXSCAN	9
#define M6845_CURSTART	0x0A
#define M6845_CUREND	0x0B
#define M6845_STARTADRH	0x0C
#define M6845_STARTADRL	0x0D
#define M6845_CURSORH	0x0E
#define M6845_CURSORL	0x0F
#define M6845_LPENH	0x10
#define M6845_LPENL	0x11

/* for screentimeout */
#define	BLANKSTART	0	/* for reset and change */
#define BLANKSTOP	1	/* temporarily suspend */

#define MAXESCPARAM	10	/* max # of ESC parameters */

/* for fonts */
#define XLAT2PC8	0xFFFE
#define NOFONT		0xFFFD
#define ISOLATIN1	0

/* abbreviation for LED control */
#define leds(vp) ((vp->altgrled ? \
	vp->altgrlock : vp->shiftled ? \
	vp->shiftlock : vp->caps) << 2 | \
	vp->num<<1 | vp->scroll)

/*
 *  local types
 */


#define KBDMAXOVLKEYSIZE	15	/* excl. zero byte */
#define KBDDEFOVLKEYSIZE	4	/* excl. zero byte */

/* This defines the actually used table for key assignments, and is
 * dynamically assigned.
 */
typedef struct kbd_ovlkey Ovl_tbl;

/* This defines the default table for key assignments. This table does
 * not have entries for META, ALTGR, SHIFTALTGR like the overloaded key
 * table
 */
typedef struct {
	u_short	type;				/* type of key */
	Ovl_tbl	*ovlptr; 		     	/* -vak pointer into overload table */
	XCHAR	unshift[KBDDEFOVLKEYSIZE+1];	/* default codes */
	XCHAR	shift[KBDDEFOVLKEYSIZE+1];
	XCHAR	ctrl[KBDDEFOVLKEYSIZE+1];
} Keycap_def;

#define XC0		{ 0,1,1,1 }
#define XC1(a)		{ a,0,1,1 }
#define XC2(a,b)	{ a,b,0,1 }
#define XC3(a,b,c)	{ a,b,c,0 }
#define XC4(a,b,c,d)	{ a,b,c,d }
#define XE3(a,b)	{ '\033',a,b,0 }
#if XCHAR == u_char
#define xc_bcopy(src,dst,sz)	bcopyb(src,dst,sz)
#define xc_char2xc(X) (X)
#else
ERROR! CHANGE xc_bcopy!!!!!!
#endif

/* This is the special functions lookup table (local version of 
 * struct kbd_special
 */
 
#define MAXNROFSPEC 30	/* so many keys may have special functions */
typedef struct {
	u_short	key;
	u_short modifier;
	u_short	scan; 
	u_short	function;
} Spec_tbl;

/* noise, noise, noise (local structure) */
struct kbd_sound {
	int pitch;	/* Frequency in Hz */
	int duration;	/* Time in msec */
};

/*
 * global variables
 */
GLOBAL	struct consinfo	cons_capabilities;
GLOBAL	volatile char	vtswsema;
extern	u_short		*Crtat;	/* only absolute address in video area */
extern	Keycap_def	kbd_keytab[];

GLOBAL	struct consoftc {
	int	cs_mymajor;
	int	cs_flags;
#define CO_INITRB	0x001
#define CO_INITTTY	0x002
#define CO_OPEN		0x004	/* cs_opencnt != 0 */
#define CO_OPENRAW	0x008
#define	CO_POLLING	0x010	/* polling for input */
#define	CO_ACTIVE	0x020	/* timeout active (unused) */
#define CO_ASYNC	0x040	/* Async I/O mode */
#define CO_RDWAIT	0x080	/* blocked on read */

	int		cs_timo;	/* timeouts since interrupt (unused) */
	u_long		cs_wedgecnt;	/* times restarted (unused)*/
	u_long		cs_ovfl;	/* buffer overflows */
#ifdef OLDPATCHKIT
	struct proc 	*cs_selp;	/* Process waiting for select call */
#else
	pid_t		cs_rsel;	/* Process waiting for select call */
#endif
	int		cs_pgid;	/* Process group for ASYNC I/O */
/*XXX*/	struct tty 	*cs_constty;	/* used to restore constty if */
					/* anyone dares to steal CONSOLE during raw open */
	struct vty	*cs_actvty;	/* active vty which owns the /dev/kbd */
	int		cs_opencnt;	/* counter for opens from vtys */
} consoftc;

GLOBAL struct kbdstate {

	int	pitch;			/* default pitch of beep */
	int	duration;		/* default duration of beep */

/* XXX could be bitfields. But why? */
	u_char	m0flag;			/* clear META-map flag */
	u_char	c0flag;			/* clear CRTL-map flag */
	u_char	a0flag;			/* clear ALT-map flag */
	u_char	ledstate;		/* current state of kbd LEDS */	

	u_char 	shift_down;		/* shift pressed */
	u_char	ctrl_down;		/* ctrl pressed */
	u_char	meta_down;		/* meta pressed */
	u_char	altgr_down;		/* altgr pressed */
	u_char	repeat;			/* allow key repetition */
	u_char	id;			/* keyboard id */
	int	tpmrate;		/* repetition rate/delay */
} kbs;	/* kbd state */

GLOBAL struct videostate {
	char	blanking;
	char	color;
	char	f89bit;		/* 8/9 bit flag */
	short	cardtype;	/* set by device probe */
	short	cardsubtype;	/* chipset version */
	short	ram;		/* set by whoami */
	int	iobase;
	int	scrtimeout;	/* Timeout for screenblanker */
	u_short	encoding[2];	/* font encoding */
	short	_atiext;	/* ATI VGA special regs */
} vds;

/* poor man's C++. The usage of these attributes is NOT FOR NONSENSE */
#define PRIVATE	
#define PUBLIC

/* VT central data structure */
struct vty {
PRIVATE	u_short		*Crtat;		/* ptr to virtual video page */
PRIVATE	u_short		*crtat;		/* ptr to virtual cursor position */
PRIVATE	u_short		*vbuf;		/* video buffer */

PUBLIC	char		so;		/* in standout mode? */
PUBLIC	char		vtynum;		/* to get vt# from vtyptr */
PUBLIC	u_short		ttycnt;		/* open reference counter */
PUBLIC	struct tty	*ttyp;		/* pointer to virtual tty information */

PUBLIC	u_short		so_at;		/* standout attribute */

PUBLIC	struct outmode {
		u_short	fg_at,bg_at;	/* kernel attributes */
		u_short	def_at;		/* default attribute */
		char	f2;		/* select second font */
		char	escstate;	/* escape state */
		char	parcnt;		/* param count */
		int	param[MAXESCPARAM]; /* ESC parameters */
	} om[2]; 			/* 0 = std, 1 = kernel */
PUBLIC	struct outmode	*op;		/* pointer to actual set */

PUBLIC	short 		row, col;	/* current cursor position */
PUBLIC	short		nrow, ncol;	/* current screen geometry */
PUBLIC	short		size;		/* size of video space */
PUBLIC	u_char		visible;	/* =1 is visible */
PUBLIC	u_char		scroll;		/* =1, scrolllock active */
PUBLIC	u_char		caps;		/* caps lock active */
PUBLIC	u_char		num;		/* num lock active */ 
PUBLIC	u_char		shiftlock;	/* shift lock active */
PUBLIC	u_char  	altgrlock;	/* altgr lock active */
PUBLIC	u_char  	altgrled;	/* -vak- CAPS led is ALTGR-LOCK */
PUBLIC	u_char  	shiftled;	/* -vak- CAPS led is CAPS-LOCK */

PUBLIC	int		pitch;		/* vty dependent sound */
PUBLIC	int		duration;
};

/* TTY structure for virtual terminals */
extern	struct vty	vtys[];			/* vty data */
extern	struct tty	pccons[];		/* vty-tty buffers */
extern	int		nvty;			/* # of available vtys */
extern	struct tty	*constty;		/* console tty */
GLOBAL	struct vty	*actvty;		/* pointer to actual vty */

/* Ring buffer of the raw co device */
GLOBAL	struct ringb	co_buf;

/*
 *  prototypes
 */

/* in co_cons.c */
extern	struct tty *dev2tty(dev_t dev);
extern	int	pcopen(dev_t dev, int flag, int mode, struct proc*);
extern	int	pcclose(dev_t dev, int flag, int mode, struct proc*);
extern	int	pcread(dev_t dev, struct uio *uio, int flag);
extern	int	pcwrite(dev_t dev, struct uio *uio, int flag);
extern	int	pcioctl(dev_t dev, int cmd, caddr_t data, int flag);
extern	int	pcstart(struct tty *tp);
extern	void	pccnprobe(struct consdev *cp);
extern	void	pccninit(struct consdev *cp);
extern	void	pccnputc(dev_t dev, int c);
extern	int	pccngetc(dev_t dev);
extern	int	pcparam(struct tty*,struct termios*);
extern	int	pcpoll(int onoff);
extern	int 	pg(char *p,int q,int r,int s,int t,int u,int v,int w,int x,int y,int z);
extern	int	getchar();

/* in co_kbd.c */
extern	void	reset_kbd_flags();
extern	int	coopen(dev_t dev, int flag, int mode, struct proc *p);
extern	int	coclose(dev_t dev, int flag, int mode, struct proc *p);
extern	int	coread(dev_t dev, struct uio *uio, int flag);
extern	void	cointr(dev_t dev, int irq, int cpl);
extern	void	cowakeup();
extern	int	coselect(dev_t dev, int rw, struct proc *p);
extern	int	coioctl(dev_t dev, int cmd, caddr_t data, int flag);
extern	int	comap(dev_t dev, int offset, int nprot);
extern	int	coprobe(struct isa_device *dev);
extern	void	kbd_setleds(int ledval);
extern	void	kbd_ovlinit();
extern	int	kbd_getokeydef(u_int key,Ovl_tbl *thisdef);
extern	int	kbd_getckeydef(u_int key,Ovl_tbl *thisdef);
extern	int	kbd_getspecial(struct kbd_hotkey *data);
extern	int	kbd_setspecial(struct kbd_hotkey *data);
extern	int	sgetc(int noblock); /* compatibility */
extern	XCHAR	*kbd_sgetc(int noblock);
extern	void	kbd_settpmrate(int rate);
extern	int	kbd_rmkeydef(u_int key);
extern	int	kbd_setkeydef(Ovl_tbl *data);
extern	int	kbd_cvtsound(int ipitch, int *opitch, int idur, int *odur);

/* in co_vga.c */
extern	void	cons_highlight();
extern	void	cons_normal();
extern	int	coattach(struct isa_device *dev);
extern	void	vga_setcursorpos(int pos);
extern	void	vga_cursor(int a);
extern	void	vga_doblanking(int fct);
extern	void	sput(int vtynum, XCHAR c, int ka);
extern	void	consinit();
extern	void	vga_whoami();
extern	int	vga_setcshape(struct cursorshape *data);
extern	int	vga_getcshape(struct cursorshape *data);
extern	int	vga_getvideoinfo(struct videoinfo *data);
extern	void	vga_enablecg();
extern	void	vga_disablecg();
extern	int	vga_xlatiso646(struct vty *vp,u_short *at,u_short *sat,int c);
/* emulator support */
extern	void	vga_cursorup(struct vty *vp, int n);
extern	void	vga_cursordown(struct vty *vp, int n);
extern	void	vga_cursorleft(struct vty *vp, int n);
extern	void	vga_cursorright(struct vty *vp, int n, int wrap);
extern	void	vga_scrollup(struct vty *vp,int n, int cm);
extern	void	vga_scrolldown(struct vty *vp, int n);
extern	void	vga_cursormove(struct vty *vp, int x, int y);
extern	void	vga_cursorrelative(struct vty *vp, int dx, int dy);
extern	void	vga_clearcursor(struct vty *vp, int mode);
extern	void	vga_clearline(struct vty *vp, int mode);
extern	void	vga_deleteline(struct vty *vp, int n);
extern	void	vga_insertline(struct vty *vp, int n);
extern	void	vga_deletechars(struct vty *vp, int n);
extern	void	vga_insertchars(struct vty *vp, int n);
extern	void	vga_setattributes(struct vty *vp, int mode, int attr);
extern	void	vga_selectfont(struct vty *vp,int fontnr);
extern	void	vga_wrtchar(struct vty *vp, u_int c, u_int at);
extern	int	vga_checkcursor(struct vty *vp);
extern	void	vga_sendchar(struct vty *vp, XCHAR c);
extern	void	vga_initvideo();

/* in co_pc3.c */
extern	void	vtemul_init();		/* initialize terminal emulator */
extern	void	vtemul_exec(struct vty*,XCHAR);	/* process data */

/* in co_vty.c */
extern	struct vty	*dev2vty(dev_t dev);
extern	void	vty_init(int first);
extern	void	vty_setactive(int vtyno,int sw);
extern	void	vty_next();
extern	void	vty_previous();
extern	void	vty_broadcast(const char *fmt,...);

/* in co_codrv1.c/co_codrv2.c, etc. */
extern	void	coioctl_init();
extern	int	consioctl(dev_t dev, int cmd, caddr_t data, int flag);
extern	int	kbdioctl(dev_t dev, int cmd, caddr_t data, int flag);
extern	int	vgaioctl(dev_t dev, int cmd, caddr_t data, int flag);

/*******************************************************************
 * The video console multiplexer (not yet)
 ******************************************************************/

/* in future this will be a struct of pointers to functions, and
 * will be filled by a "module_init call
 */

#ifdef GFX_CONSOLE
#define emul_cursorup		gfx_cursorup
#define emul_cursordown		gfx_cursordown
#define emul_cursorleft		gfx_cursorleft
#define emul_cursorright	gfx_cursorright
#define emul_scrollup		gfx_scrollup
#define emul_scrolldown		gfx_scrolldown
#define emul_cursormove		gfx_cursormove
#define emul_cursorrelative	gfx_cursorrelative
#define emul_clearcursor	gfx_clearcursor
#define emul_clearline		gfx_clearline
#define emul_deleteline		gfx_deleteline
#define emul_insertline		gfx_insertline
#define emul_deletechars	gfx_deletechars
#define emul_insertchars	gfx_insertchars
#define emul_setattributes	gfx_setattributes
#define emul_selectfont		gfx_selectfont
#define emul_wrtchar		gfx_wrtchar
#define emul_checkcursor	gfx_checkcursor
#define emul_sendchar		gfx_sendchar
#define emul_initvideo		gfx_initvideo
#else
#define emul_cursorup		vga_cursorup
#define emul_cursordown		vga_cursordown
#define emul_cursorleft 	vga_cursorleft
#define emul_cursorright	vga_cursorright
#define emul_scrollup		vga_scrollup
#define emul_scrolldown		vga_scrolldown
#define emul_cursormove		vga_cursormove
#define emul_cursorrelative	vga_cursorrelative
#define emul_clearcursor	vga_clearcursor
#define emul_clearline		vga_clearline
#define emul_deleteline		vga_deleteline
#define emul_insertline		vga_insertline
#define emul_deletechars	vga_deletechars
#define emul_insertchars	vga_insertchars
#define emul_setattributes	vga_setattributes
#define emul_selectfont		vga_selectfont
#define emul_wrtchar		vga_wrtchar
#define emul_checkcursor	vga_checkcursor
#define emul_sendchar		vga_sendchar
#define emul_initvideo		vga_initvideo
#endif

#endif /* _CO_HDR_H_

