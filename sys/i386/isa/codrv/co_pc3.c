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
 *	@(#) $RCSfile: co_pc3.c,v $	$Revision: 1.6 $ (Contributed to 386bsd) $Date: 93/01/23 23:14:46 $
 *
 *	History: see CO_HISTORY
 */
static char *rcsid = "$Header: /usr/src/sys.386bsd/i386/isa/codrv/RCS/co_pc3.c,v 1.6 93/01/23 23:14:46 root Exp Locker: root $";

/* This file provides a plug-in interface for different terminal emulations.
 * As a reference, the pc3/pc3x/pc3n/pc3nc interface is shown.
 * 
 * Programmers are invited to provide different emulations, e.g. 
 * vt100/vt220/vt320, adm3a/adm31, hp262x, etc.
 *
 * The following conventions have to be met:
 *
 * The video subsystem provides a number of support functions to 
 * manipulate screen data, such as writing to a VTY screen, or moving the 
 * cursor, etc.
 *
 * All these routines are named "emul_*". See co_hdr.h for a list of available
 * facilities. You will see that these are defines to either a set of
 * "vga_*" or "gfx_*" routines (not yet available). To make a migration to
 * the future graphics console as smooth as possible, avoid using other
 * routines to access the screen, even if you know tricky code
 * that runs 10 times as fast. There is time enough to do optimizations
 * when the graphics subsystem is running.
 *
 ***************************************************************************
 *
 * This file contains one central routine, 'vtemul_exec', which is called
 * by the console driver, with the following arguments:
 *
 * vp:	Pointer to the actual vty
 * ch:	Character to be processed (note that this is not a "u_char"!)
 *
 ***************************************************************************
 *
 * A second call is used during coattach: 'vtemul_init', which 
 * can be used for local initialisation. vtemul_init also MUST fill an
 * identifier into the field 'emul_name' of struct consinfo cons_capabilities.
 *
 ***************************************************************************
 *
 * Only use the PUBLIC fields in struct vty!
 * Don't rely on the PRIVATE members of the structure, even if you
 * can access them (PRIVATE and PUBLIC are #defines only).
 * The reason is, that a future version may run in graphics mode, and
 * will interpret these fields in a different way you may think they
 * might be used. This holds in particular for Crtat and Crtat!!!!
 *
 * If you need more special variables, use a local structure
 * use #include "vty.h" to find out the number of vtys available.
 * But even in the local structure, don't think that there is 
 * direct accessible video memory, even if using this this speeds up 
 * everything extremely!
 */

/* example code follows: */

/* check my optional symbol to avoid multiple inclusions */
#ifdef PC3
#ifndef MINITERM

#include "co_hdr.h"
#include "vty.h"

#define ESC_NONE	0	/* No esc in progress */
#define ESC_WBRAC	1	/* got esc, wait for '[' or 'c' */
#define ESC_WPARAM	2	/* got esc [, wait for param, ';' or letter */

/* !!! this may look like a Keycap_def, like in co_kbd.c, but isn't ! :-) */
static struct Keycap2 {
	short	key;
	u_short	type;				/* type of key */
	XCHAR	unshift[KBDDEFOVLKEYSIZE+1];	/* default codes */
	XCHAR	shift[KBDDEFOVLKEYSIZE+1];
	XCHAR	ctrl[KBDDEFOVLKEYSIZE+1];
} pc3keys[] = {
75,	KBD_FUNC,	XE3('[','L'),	XE3('|','a'),	XE3('|','b'),    /* INS */
76,	KBD_FUNC,	XC1('\177'),	XE3('|','c'),	XE3('|','d'),    /* DEL */
79,	KBD_FUNC,	XE3('[','D'),	XE3('|','e'),	XE3('|','f'),    /* CU <- */
80,	KBD_FUNC,	XE3('[','H'),	XE3('|','g'),	XE3('|','h'),    /* HOME */
81,	KBD_FUNC,	XE3('[','F'),	XE3('|','i'),	XE3('|','j'),    /* END */
83,	KBD_FUNC,	XE3('[','A'),	XE3('|','k'),	XE3('|','l'),	/* CU ^ */
84,	KBD_FUNC,	XE3('[','B'),	XE3('|','m'),	XE3('|','n'),	/* CU v */
85,	KBD_FUNC,	XE3('[','I'),	XE3('|','o'),	XE3('|','p'),	/* PG UP */
86,	KBD_FUNC,	XE3('[','G'),	XE3('|','q'),	XE3('|','r'),	/* PG DN */
89,	KBD_FUNC,	XE3('[','C'),	XE3('|','s'),	XE3('|','t'),	/* CU -> */
91,	KBD_KP,		XC1('7'),	XE3('[','H'),	XC1('7'),
92,	KBD_KP,		XC1('4'),	XE3('[','D'),	XC1('4'),
93,	KBD_KP,		XC1('1'),	XE3('[','F'),	XC1('1'),
95,	KBD_KP,		XC1('/'),	XC1('/'),	XC1('/'),
96,	KBD_KP,		XC1('8'),	XE3('[','A'),	XC1('8'),
97,	KBD_KP,		XC1('5'),	XE3('[','E'),	XC1('5'),
98,	KBD_KP,		XC1('2'),	XE3('[','B'),	XC1('2'),
99,	KBD_KP,		XC1('0'),	XE3('[','L'),	XC1('0'),
100,	KBD_KP,		XC1('*'),	XC1('*'),	XC1('*'),
101,	KBD_KP,		XC1('9'),	XE3('[','I'),	XC1('9'),
102,	KBD_KP,		XC1('6'),	XE3('[','C'),	XC1('6'),
103,	KBD_KP,		XC1('3'),	XE3('[','G'),	XC1('3'),
104,	KBD_KP,		XC1('.'),	XC1('\177'),	XC1('.'),
105,	KBD_KP,		XC1('-'),	XC1('-'),	XC1('-'),
106,	KBD_KP,		XC1('+'),	XC1('+'),	XC1('+'),
112,	KBD_FUNC,	XE3('[','M'),	XE3('[','Y'),	XE3('[','k'),	/* F1 */
113,	KBD_FUNC,	XE3('[','N'),	XE3('[','Z'),	XE3('[','l'),	/* F2 */
114,	KBD_FUNC,	XE3('[','O'),	XE3('[','a'),	XE3('[','m'),	/* F3 */
115,	KBD_FUNC,	XE3('[','P'),	XE3('[','b'),	XE3('[','n'),	/* F4 */
116,	KBD_FUNC,	XE3('[','Q'),	XE3('[','c'),	XE3('[','o'),	/* F5 */
117,	KBD_FUNC,	XE3('[','R'),	XE3('[','d'),	XE3('[','p'),	/* F6 */
118,	KBD_FUNC,	XE3('[','S'),	XE3('[','e'),	XE3('[','q'),	/* F7 */
119,	KBD_FUNC,	XE3('[','T'),	XE3('[','f'),	XE3('[','r'),	/* F8 */
120,	KBD_FUNC,	XE3('[','U'),	XE3('[','g'),	XE3('[','s'),	/* F9 */
121,	KBD_FUNC,	XE3('[','V'),	XE3('[','h'),	XE3('[','t'),	/* F10 */
122,	KBD_FUNC,	XE3('[','W'),	XE3('[','i'),	XE3('[','u'),	/* F11 */
123,	KBD_FUNC,	XE3('[','X'),	XE3('[','j'),	XE3('[','v'),	/* F12 */
124,	KBD_KP,		XE3('[','w'),	XE3('[','x'),	XE3('[','y'),	
0,	KBD_NONE,	XC0,		XC0,		XC0
};

/*
 *	Do the local initialisations. Notice that many things are already done
 *	in vty_init
 */
void vtemul_init()
{
	char	*c,*id = "pc3n";
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

	/* set the function keys new */
	for (k2 = pc3keys; k2->key; k2++) {
		kp = &kbd_keytab[k2->key];
		kp->type = k2->type;
		kp->ovlptr = 0;
		copydef(k2->unshift,kp->unshift);
		copydef(k2->shift,kp->shift);
		copydef(k2->ctrl,kp->ctrl);
	}
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
	case 016:	/* SO, select font 2 */
		emul_selectfont(vp,2);
		break;
	case 017:	/* SI, select font 1 */
		emul_selectfont(vp,1);
		break;
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
				case 'f': /* in system V consoles */
				case 'H': /* Cursor move */
					emul_cursormove(vp, par1, par2);
					break;
				case 'J': /* Clear ... */
					emul_clearcursor(vp, par1);
					break;
				case 'K': /* Clear line ... */
					emul_clearline(vp, par1);
					break;
				case 'L':  /* Insert Line */
					emul_insertline(vp, par1);
					break;
				case 'M':  /* Delete Line */
					emul_deleteline(vp, par1);
					break;
				case 'P':  /* delete spaces */
					emul_deletechars(vp, par1);
					break;
				case 'S':  /* scroll up cx lines */
					emul_scrollup(vp, par1, 0);
					break;
				case 'T':  /* scroll down cx lines */
					emul_scrolldown(vp, par1);
					break;
				case 'm':	/* standout mode */
					if (par1) vp->so = 1;
					else vp->so = 0;
					break;
				case 'r':
					vp->so_at = (par1 & 0x0f) | ((par2 & 0x0f) << 4);
					break;
				case 'x': /* set attributes */
					emul_setattributes(vp, par1, par2);
					break;
				case '@':  /* insert spaces */
					emul_insertchars(vp, par1);
					break;
				case '!':  /* set beep pitch and duration */
					kbd_cvtsound(par1, &vp->pitch, 
						     par2, &vp->duration);
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

#endif /* !MINITERM */
#endif /* PC3 */
