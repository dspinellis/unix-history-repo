/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: vt100esc.c,v 4.300 91/06/09 06:14:59 root Rel41 $ SONY
 *
 *	@(#)vt100esc.c	7.4 (Berkeley) %G%
 */

/*
 *  vt100 escape sequence handler
 */

#include <machine/fix_machine_type.h>

#ifdef IPC_MRX
#include "../../h/param.h"
#include "../../h/systm.h"
#include "../../iop/framebuf.h"
#else
#include <sys/param.h>
#include <sys/systm.h>
#include <news3400/iop/framebuf.h>
#endif

#include <news3400/bm/vt100.h>
#include <news3400/bm/bitmapif.h>

#include <news3400/fb/fbdefs.h>

#ifdef IPC_MRX
#include "../../iop/kbreg.h"
#include "../../iop/keyboard.h"
#else
#include <news3400/iop/kbreg.h>
#include <news3400/iop/keyboard.h>
#endif

#if CPU_SINGLE
#include <news3400/sio/scc.h>
#endif

#ifdef IPC_MRX
#include "config.h"
#define kbd_ioctl(chan, cmd, argp) { \
	if (kb_ioctl) \
		(*kb_ioctl)(chan, cmd, argp); \
}
#endif

/*
 *  escape sequece functions
 */
int	esc_csi();
int	esc_csi_ansi();
int	esc_csi_dec();
int	esc_store_csr();
int	esc_restore_csr();
int	esc_index();
int	esc_next_line();
int	esc_tab_set();
int	esc_rev_index();
int	esc_numeric_kpad();
int	esc_application_kpad();
int	esc_line_size();
int	esc_char_setr();
int	esc_char_setl();
int	esc_kanji_set();
int	esc_parm_set();
int	esc_pf_define();
int	esc_ignore();

struct  esc_sequence esc_seq_table[] = {
	{'[', "ABCDfgHhJKLlMmnPr", esc_csi},
	{'7', "", esc_store_csr},
	{'8', "", esc_restore_csr},
	{'D', "", esc_index},
	{'E', "", esc_next_line},
	{'H', "", esc_tab_set},
	{'M', "", esc_rev_index},
	{'=', "", esc_application_kpad},
	{'>', "", esc_numeric_kpad},
	{'#', "34568", esc_line_size},
	{'(', "0ABJH", esc_char_setr},
	{')', "0AB", esc_char_setl},
	{'$', "B@", esc_kanji_set},
	{'~', "fcpsomdbDiGCBTtE", esc_parm_set},
	{'P', "pPZiI", esc_pf_define},
	{'\0', "", esc_ignore},
};

struct	key_pad	key_pad[] = {
	{ '0', 'p' },	/*	0	*/
	{ '1', 'q' },	/*	1	*/
	{ '2', 'r' },	/*	2	*/
	{ '3', 's' },	/*	3	*/
	{ '4', 't' },	/*	4	*/
	{ '5', 'u' },	/*	5	*/
	{ '6', 'v' },	/*	6	*/
	{ '7', 'w' },	/*	7	*/
	{ '8', 'x' },	/*	8	*/
	{ '9', 'y' },	/*	9	*/
	{ '.', 'n' },	/*  period	*/
	{ '-', 'm' },	/*  minus	*/
	{ '+', 'k' },	/*  plus	*/
	{ ',', 'l' },	/*  comma	*/
	{ '\n', 'M' },	/*  enter	*/
	{ 'A', 'A' },	/*  cursor up	*/
	{ 'B', 'B' },	/*  cursor down	*/
	{ 'C', 'C' },	/*  cursor right */
	{ 'D', 'D' },	/*  cursor left	*/
	{ '\0', '\0' }	/*	*/
};

static	char	esc_buf[ESC_BUF_SIZ];
static	char	*esc_bp = esc_buf;
extern	char	c_pos_mess[];

static change_csr_key_pad(), change_aux_key_pad(), itoa();

Key_string	key_str;
Pfk_string	pfk_str;

unsigned  int	first_jcode;

/*
 *  put out jis-code kanji
 */
jiskanji(sp, c)
	register SCREEN *sp;
	register unsigned int c;
{
	if (first_jcode) {
		addch(sp, c | (first_jcode << 8));
		first_jcode = 0;
	} else {
		first_jcode = c;
	}
}

/*
 *  This routine is the command analiser using second character.
 *  If a command has found then switch to particular escape handling
 *  routine, and directly called by mother routine. 
 *  The arguments are passed through the routine.
 */
esc_top_level(sp, c)
	register SCREEN	*sp;
	char c;
{
	register  struct  esc_sequence	*estp;

	for (estp = esc_seq_table; estp->command ; estp++) {
		if (estp->command == c) { 
					/* command found  */
			sp->s_estp = estp;
			if (*estp->terminators == '\0') {
				(*estp->esc_func)(sp);
				sp->s_current_stat &= ~ESCAPE;
			} else {
				sp->s_esc_handler = estp->esc_func;
			}
			return;
		}
	}
	sp->s_current_stat &= ~ESCAPE;
}

/*
 *  Undo the ESCAPE flag, escape buffer
 *  and the esc_handler routine
 *  This routine has to be called when escape sequence has started.
 */
recover(sp)
	register SCREEN *sp;
{
	register int *ip = (int *) esc_buf;
	register int *sup = (int *) (esc_buf + ESC_BUF_SIZ);

	sp->s_current_stat &= ~ESCAPE;
	sp->s_esc_handler = esc_top_level;
	while (ip < sup)
		*ip++ = 0;
	esc_bp = esc_buf;
}

/*
 *  This routine in_str(c, string) returns
 *  if string contains c then TRUE (1) else FALSE (0)
 */
in_str(c, string)
	char c;
	register char *string;
{
	while(*string)
		if (c == *string++)
			return(TRUE);
	return(FALSE);
}

/*
 *  Control sequence introducer (CSI)
 *  Which begins `^[[' and terminates one of `ABCDfgHhJKLlMmPr'
 */
esc_csi(sp, c)
	register SCREEN *sp;
	unsigned int c;
{
	static int bufc = 0;

	if (in_str(c, sp->s_estp->terminators)) {
		esc_csi_ansi(sp, esc_bp, c);
		sp->s_current_stat &= ~ESCAPE;
		bufc = 0;
		return;
	}
	/*  buffering arguments  */
	if (bufc < ESC_BUF_SIZ) {
		if (c >= '0' && c <= '9') {
			*esc_bp = *esc_bp *10 + (c - '0');
		} else if (c == ';') {
			esc_bp++;
			bufc++;
		} else if (c == '?') {
			if (esc_bp == esc_buf) {
				sp->s_esc_handler = esc_csi_dec;
			} else {
				esc_buf[0] = INVALID;
			}
		} else {
			sp->s_current_stat &= ~ESCAPE;
			bufc = 0;
		}
	}
}

#ifdef IPC_MRX
#define SCC_KEYBOARD	0
#endif

/*
 *  Ansi standard csi handler
 */
esc_csi_ansi(sp, esc_bp, terminator)
	register SCREEN *sp;
	char *esc_bp;
	char terminator;
{
	register char *cp = esc_buf;
	register struct cursor *spc = &sp->s_csr;
	register char *p;
	register int i;

	if (*cp == INVALID)
		return;

	cursor_off();
	switch (terminator) {
	case 'A':		/*  CUU	 */
		if (spc->csr_y < sp->s_region.top_margin) {
			spc->csr_y = max(spc->csr_y - max(*cp, 1)
					,TOP_M);
		} else {
			spc->csr_y = max(spc->csr_y - max(*cp, 1)
					,sp->s_region.top_margin);
		}
		spc->csr_p.y = (spc->csr_y - 1) * char_h + y_ofst;
		sp->s_current_stat &= ~WRAP;
		break;
	case 'B':		/*  CUD	 */
		if (spc->csr_y > sp->s_region.btm_margin) {
			spc->csr_y = min(spc->csr_y + max(*cp, 1)
					,btm_m);
		} else {
			spc->csr_y = min(spc->csr_y + max(*cp, 1)
					,sp->s_region.btm_margin);
		}
		spc->csr_p.y = (spc->csr_y - 1) * char_h + y_ofst;
		sp->s_current_stat &= ~WRAP;
		break;
	case 'C':		/*  CUF	 */
		spc->csr_x = min(spc->csr_x + max(*cp, 1), rit_m);
		spc->csr_p.x = (spc->csr_x - 1) * char_w + x_ofst;
		sp->s_current_stat &= ~WRAP;
		break;
	case 'D':		/*  CUB	 */
		spc->csr_x = max(spc->csr_x - max(*cp, 1), LFT_M);
		spc->csr_p.x = (spc->csr_x - 1) * char_w + x_ofst;
		sp->s_current_stat &= ~WRAP;
		break;
	case 'g':		/*  TBC	 */
		switch (*cp) {
		case 0:
			sp->s_tab_pos[spc->csr_x] = 0;
			break;
		case 3:
			for (i = 0; i <= rit_m; i++)
				sp->s_tab_pos[i] = 0;
			break;
		default:
			break;
		}
		break;
	case 'f':		/*  HVP	 */
	case 'H':		/*  CUP  same as HVP	*/
		csr_pos(sp, cp[1], cp[0]);
		sp->s_current_stat &= ~WRAP;
		break;
	case 'J':		/*  ED	*/
		erase_disp(sp, cp[0]);
		sp->s_current_stat &= ~WRAP;
		break;
	case 'K':		/*  EL	*/
		erase_line(sp, cp[0]);
		sp->s_current_stat &= ~WRAP;
		break;
	case 'L':		/*  IL	*/
		insert_line(sp, cp[0]);
		break;
	case 'M':		/*  DL	*/
		delete_line(sp, cp[0]);
		break;
	case 'P':		/*  DCH	 */
		delete_char(sp, cp[0]);
		sp->s_current_stat &= ~WRAP;
		break;
	case 'r':		/*  DECSTBM	*/
		cp[2] = max(cp[0] == 0 ? TOP_M: cp[0], TOP_M);
		cp[3] = min(cp[1] == 0 ? btm_m: cp[1], btm_m);
		if (cp[2] >= cp[3])
			break;

		sp->s_region.top_margin = cp[2];
		sp->s_region.btm_margin = cp[3];

		spc->csr_x = LFT_M;
		spc->csr_p.x = x_ofst;
		if (sp->s_term_mode & DECOM) {
			spc->csr_y = sp->s_region.top_margin;
			spc->csr_p.y = (spc->csr_y - 1) * char_h + y_ofst;
		} else {
			spc->csr_y = TOP_M;
			spc->csr_p.y = y_ofst;
		}
		break;
	case 'm':		/*  CRA	 */
		while (cp <= esc_bp) {
			switch (*cp++) {
			case 0:
				spc->csr_attributes &= NORMALM;
				if (sp->s_term_mode & DECSCNM) {
					fcolor = sp->s_bgcol;
					bcolor = sp->s_plane;
				}
				else {
					fcolor = sp->s_plane;
					bcolor = sp->s_bgcol;
				}
				break;
			case 1:		/*  bold	*/
				spc->csr_attributes |= BOLD;
				break;
			case 4:		/*  under score	 */
				spc->csr_attributes |= USCORE;
				break;
			case 5:		/*  blinking	*/
				spc->csr_attributes |= BLINK;
				break;
			case 7:		/*  reverse	*/
				spc->csr_attributes |= REVERSE;
				if (sp->s_term_mode & DECSCNM) {
					fcolor = sp->s_plane;
					bcolor = sp->s_bgcol;
				}
				else {
					fcolor = sp->s_bgcol;
					bcolor = sp->s_plane;
				}
				break;
			case 22:	/*  unbold	*/
				spc->csr_attributes &= ~BOLD;
				break;
			case 24:	/*  no under score	*/
				spc->csr_attributes &= ~USCORE;
				break;
			case 25:	/*  no blinking	 */
				spc->csr_attributes &= ~BLINK;
				break;
			case 27:	/*  re-reverse	*/
				spc->csr_attributes &= ~REVERSE;
				if (sp->s_term_mode & DECSCNM) {
					fcolor = sp->s_bgcol;
					bcolor = sp->s_plane;
				}
				else {
					fcolor = sp->s_plane;
					bcolor = sp->s_bgcol;
				}
				break;
			default:
				break;
			}
		}
		break;
	case 'n':
		while (cp <= esc_bp) {	/*  DSR(status request)	*/
			switch (*cp++) {
			case 6:		/*  inquiry cursor position	*/
				key_str.key_string = c_pos_mess;
				key_str.key_length = spr(c_pos_mess,
				    "\033[%d;%dR", (sp->s_term_mode & DECOM) ?
				    spc->csr_y - sp->s_region.top_margin + 1:
				    spc->csr_y, spc->csr_x);
				kbd_ioctl(SCC_KEYBOARD, KIOCBACK, &key_str);
				break;
			default:
				break;
			}
		}
		break;
	case 'h':		/*  SM	*/
		while (cp <= esc_bp) {
			switch (*cp++) {
			case 2:		/*  Keyboard Action	*/
				sp->s_term_mode |= KAM;
				break;
			case 4:		/*  Insert Replace	*/
				sp->s_term_mode |= IRM;
				break;
			case 12:	/*  Local echo disable	*/
				sp->s_term_mode |= SRM;
				break;
			case 20:	/*  Linefeed newline	*/
				sp->s_term_mode |= LNM;
				break;
			default:
				break;
			}
		}
		break;
	case 'l':		/*  RM	*/
		while (cp <= esc_bp) {
			switch (*cp++) {
			case 2:		/*  Keyboard Action	*/
				sp->s_term_mode &= ~KAM;
				break;
			case 4:		/*  Insert Replace	*/
				sp->s_term_mode &= ~IRM;
				break;
			case 12:	/*  Local echo disable	*/
				sp->s_term_mode &= ~SRM;
				break;
			case 20:	/*  Linefeed newline	*/
				sp->s_term_mode &= ~LNM;
				break;
			default:
				break;
			}
		}
		break;
	default:
		break;
	}
	cursor_on(&spc->csr_p);
	sp->s_current_stat &= ~ESCAPE;
}


/*
 *  Cursor position.
 *  csr_pos(sp, x, y) moves the cursor to (x, y).
 */
csr_pos(sp, x, y)
	register SCREEN *sp;
	register int x, y;
{
	if (sp->s_term_mode & DECOM) {
		sp->s_csr.csr_y = min(sp->s_region.top_margin +
				max(y, 1) - 1, sp->s_region.btm_margin);
	} else {
		sp->s_csr.csr_y = min(TOP_M + max(y, 1) - 1, btm_m);
	}
	sp->s_csr.csr_x = max(min(x, rit_m), LFT_M);
	sp->s_csr.csr_p.x = (sp->s_csr.csr_x -1) * char_w + x_ofst;
	sp->s_csr.csr_p.y = (sp->s_csr.csr_y -1) * char_h + y_ofst;
}


/*
 *  Erase in display.
 *  erase_disp(sp, pn) erases display from the cursor to the end, from 
 *  the beginning to the cursor or completely according to pn = 0, 1 or 2
 *  respectively.
 */
erase_disp(sp, pn)
	register SCREEN *sp;
	register int pn;
{
	register  struct  cursor  *spc = &sp->s_csr;

	switch (pn) {
	case 0:		/*  cursor to end	*/
		erase_line(sp, 0);
		clear_lines(min(spc->csr_y + 1, btm_m),
			btm_m - spc->csr_y, sp->s_term_mode & DECSCNM,
			sp->s_plane, sp->s_bgcol);
		break;
	case 1:		/*  beginning to cursor	*/
		erase_line(sp, 1);
		clear_lines(TOP_M, spc->csr_y - TOP_M, sp->s_term_mode & DECSCNM,
			sp->s_plane, sp->s_bgcol);
		break;
	case 2:		/*  whole	*/
		clear_lines(TOP_M, btm_m - TOP_M + 1,
			sp->s_term_mode & DECSCNM,
			sp->s_plane, sp->s_bgcol);
		break;
	default:
		break;
	}
}



/*
 *  Erase in line.
 *  erase_line(sp, pn) erases line from the cursor to the end, from the 
 *  beginning to the cursor or completely according to pn = 0, 1 or 2
 *  respectively.
 */
erase_line(sp, pn)
	register SCREEN *sp;
	register int pn;
{
	register struct cursor *spc = &sp->s_csr;

	switch(pn) {
	case 0:
		clear_chars(spc->csr_x, spc->csr_y,
			rit_m - spc->csr_x + 1, sp->s_term_mode & DECSCNM,
			sp->s_plane, sp->s_bgcol);
		break;
	case 1:
		clear_chars(LFT_M, spc->csr_y, 
			spc->csr_x - LFT_M + 1, sp->s_term_mode & DECSCNM,
			sp->s_plane, sp->s_bgcol);
		break;
	case 2:
		clear_lines(spc->csr_y, 1, sp->s_term_mode & DECSCNM,
			sp->s_plane, sp->s_bgcol);
		break;
	default:
		break;
	}
}

/*
 *  Insert line.
 *  insert_line(sp, pn) inserts pn lines in scroll region
 */
insert_line(sp, pn)
	register SCREEN *sp;
	register int pn;
{
	register struct cursor *spc = &sp->s_csr;
	register struct region *spr = &sp->s_region;

	pn = max(pn, 1);
	if (spc->csr_y < spr->top_margin || spc->csr_y > spr->btm_margin) 
		return;
	if (pn <= spr->btm_margin - spc->csr_y) {
		move_lines(spc->csr_y, spr->btm_margin - pn - spc->csr_y + 1,
			spc->csr_y + pn);
	}
	clear_lines(spc->csr_y,
		min(spc->csr_y + pn - 1, spr->btm_margin) - spc->csr_y + 1,
		sp->s_term_mode & DECSCNM, sp->s_plane, sp->s_bgcol);
	spc->csr_x = LFT_M;
	spc->csr_p.x = x_ofst;
}
			
/*
 *  Delete line.
 *  delete_line(sp, pn) deletes pn lines in scroll region
 */
delete_line(sp, pn)
	register SCREEN *sp;
	register int pn;
{
	register struct cursor *spc = &sp->s_csr;
	register struct region *spr = &sp->s_region;
	register int aux;

	pn = max(pn, 1);
	if (spc->csr_y < spr->top_margin || spc->csr_y > spr->btm_margin)
		return;
	if (pn <= spr->btm_margin - spc->csr_y) {
		aux = spc->csr_y + pn;
		move_lines(aux, spr->btm_margin - aux + 1, spc->csr_y);
	}
	aux = max(spr->btm_margin - pn + 1, spc->csr_y);
	clear_lines(aux, spr->btm_margin - aux + 1, sp->s_term_mode & DECSCNM,
		sp->s_plane, sp->s_bgcol);
	spc->csr_x = LFT_M;
	spc->csr_p.x = x_ofst;
}

/*
 *  Delete character.
 *  delete_char(sp, pn) deletes pn characters right side of the cursor.
 */
delete_char(sp, pn)
	register SCREEN *sp;
	register int pn;
{
	register struct cursor *spc = &sp->s_csr;
	register int aux;

	pn = max(pn, 1);
	if (pn < rit_m - spc->csr_x + 1) {
		move_chars(spc->csr_x + pn, spc->csr_y,
			rit_m - spc->csr_x - pn + 1 ,spc->csr_x);
	}
	aux = max(rit_m - pn + 1, spc->csr_x);
	clear_chars(aux, spc->csr_y, rit_m - aux + 1,
		sp->s_term_mode & DECSCNM, sp->s_plane, sp->s_bgcol);
}

/*
 *  This escape control sequence begins `^[[?' and ends `h' or `l'
 */
esc_csi_dec(sp, c)
	register SCREEN *sp;
	char c;
{
	register char *cp;

	if (in_str(c, sp->s_estp->terminators)) {
		if (esc_buf[0] != INVALID) {
			cursor_off();
			switch (c) {
			case 'h':	/*  set mode	*/
			for (cp = esc_buf; cp <= esc_bp; cp++) {
				switch (*cp) {
				case 1:		/*  cursor key application  */
					sp->s_term_mode |= DECCKM;
					change_csr_key_pad(APPLIC);
					break;
				case 3:		/*  132 column mode	*/
					sp->s_term_mode |= DECCOLM;
					break;
				case 4:		/*  jump scroll	*/
					sp->s_term_mode |= DECSCLM;
					break;
				case 5:		/*  reverse	*/
					if ((sp->s_term_mode & DECSCNM) == 0)
						reverse_rec(sp->s_bgcol,
							sp->s_plane);
					sp->s_term_mode |= DECSCNM;
					if (sp->s_csr.csr_attributes & REVERSE)
					{
						fcolor = sp->s_plane;
						bcolor = sp->s_bgcol;
					} else {
						fcolor = sp->s_bgcol;
						bcolor = sp->s_plane;
					}
					break;
				case 6:		/*  origin	*/
					sp->s_term_mode |= DECOM;
					sp->s_csr.csr_x = LFT_M; 
					sp->s_csr.csr_y =
						sp->s_region.top_margin;
					sp->s_csr.csr_p.x = x_ofst;
					sp->s_csr.csr_p.y =
					   (sp->s_csr.csr_y - 1) * char_h +
						y_ofst;
					break;
				case 7:		/*  auto wrap	*/
					sp->s_term_mode |= DECAWM;
					break;
				case 8:		/*  auto repeat	 */
					if ((sp->s_term_mode & DECARM) == 0) {
						kbd_ioctl(SCC_KEYBOARD, KIOCREPT,
							  (int *)0);
					}
					sp->s_term_mode |= DECARM;
					break;
				case 25:	/* cursor active */
					sp->s_term_mode |= DECCSR_ACTV;
					break;
				default:
					break;
				}
			}
			break;
			case 'l':	/*  reset mode	*/
			for (cp = esc_buf; cp <= esc_bp; cp++) {
				switch (*cp) {
				case 1:		/*  cursor key application  */
					sp->s_term_mode &= ~DECCKM;
					change_csr_key_pad(NUMERIC);
					break;
				case 3:		/*  132 column mode	*/
					sp->s_term_mode &= ~DECCOLM;
					break;
				case 4:		/*  jump scroll	*/
					sp->s_term_mode &= ~DECSCLM;
					break;
				case 5:		/*  reverse	*/
					if (sp->s_term_mode & DECSCNM)
						reverse_rec(sp->s_plane,
							sp->s_bgcol);
					sp->s_term_mode &= ~DECSCNM;
					if (sp->s_csr.csr_attributes & REVERSE)
					{
						fcolor = sp->s_bgcol;
						bcolor = sp->s_plane;
					} else {
						fcolor = sp->s_plane;
						bcolor = sp->s_bgcol;
					}
					break;
				case 6:		/*  origin	*/
					sp->s_term_mode &= ~DECOM;
					sp->s_csr.csr_x = LFT_M;
					sp->s_csr.csr_y = TOP_M;
					sp->s_csr.csr_p.x = x_ofst;
					sp->s_csr.csr_p.y = y_ofst;
					break;
				case 7:		/*  auto wrap	*/
					sp->s_term_mode &= ~DECAWM;
					break;
				case 8:		/*  auto repeat	 */
					if (sp->s_term_mode & DECARM) {
						kbd_ioctl(SCC_KEYBOARD, KIOCNRPT,
							(int *) 0);
					}
					sp->s_term_mode &= ~DECARM;
					break;
				case 25:	/* cursor non-active */
					sp->s_term_mode &= ~DECCSR_ACTV;
					break;
				default:
					break;
				}
			}
			break;
			default:
				break;
			}
			cursor_on(&sp->s_csr.csr_p);
		}
		sp->s_current_stat &= ~ESCAPE;
	} else {	/*  buffering  arguments	*/
		if (c >= '0' && c <= '9') {
			*esc_bp = *esc_bp * 10 + (c - '0');
		} else if (c == ';') {
			esc_bp++;
		} else if (c == '?') {
			esc_buf[0] = INVALID;
		} else {
			sp->s_current_stat &= ~ESCAPE;
		}
	}
}

/*
 *  changes cursor key pad to ansi_ctl
 */
static
change_csr_key_pad(applic)
	register int applic;
{
	char pad[4];
	register Pfk_string *pfk = &pfk_str;
	register Key_string *kys = &pfk_str.pfk_string;
	register struct key_pad  *kpd;
	register int i;

	kpd = &key_pad[UP-N0];
	pad[0] = '\033';
	pad[1] = (applic) ? 'O': '[';
	for (i = UP; i <= LEFT; i++) {
		pfk->pfk_num = i;
		kys->key_length = (applic) ? 3: 3;
		pad[2] = (applic) ? kpd->kpd_applic: kpd->kpd_numeric;
		kys->key_string = pad;
		kpd++;
		pfk->pfk_shift = PF_NORMAL;
		kbd_ioctl(SCC_KEYBOARD, KIOCSETS, pfk); 
		pfk->pfk_shift = PF_SHIFT;
		kbd_ioctl(SCC_KEYBOARD, KIOCSETS, pfk); 
	}
}

extern struct cursor inner_buf_csr;
extern int inner_buf_tstat;
/*
 *  Store cursor position and attributes.
 *  The SCREEN structure is stored inner structure.
 */
esc_store_csr(sp)
	register SCREEN *sp;
{
	inner_buf_csr = sp->s_csr;
	inner_buf_tstat = (DECOM|DECAWM) & sp->s_term_mode;
}

/*
 *  Restore cursor position and attributes.
 *  The SCREEN structure  is restored from inner structure.
 *  Prevail error from unexpected use of this command, inner structure
 *  must be initialized.
 */
esc_restore_csr(sp)
	register SCREEN *sp;
{
	cursor_off();
	sp->s_csr = inner_buf_csr;
	sp->s_term_mode = (sp->s_term_mode & ~(DECOM|DECAWM)) | inner_buf_tstat;
	cursor_on(&sp->s_csr.csr_p);
}

/*
 *  index()
 *  esc_index(sp) moves the cursor down if the cursor is not at
 *  bottom margin. If the cursor is at the bottom margin then
 *  scroll up.
 */
esc_index(sp)
	register SCREEN *sp;
{
	cursor_off();
	if (sp->s_csr.csr_y == sp->s_region.btm_margin)
		scroll_up(sp->s_region.top_margin,
			sp->s_region.btm_margin, sp->s_term_mode & DECSCNM,
			sp->s_plane, sp->s_bgcol);
	else {
		if (sp->s_csr.csr_y < btm_m) {
			sp->s_csr.csr_y += 1;
			sp->s_csr.csr_p.y += char_h;
		}
	}
	sp->s_current_stat &= ~WRAP;
	cursor_on(&sp->s_csr.csr_p);
}

/*
 *  next line
 *  esc_next_line(sp) moves the cursor down like index but the cursor
 *  position is the beginning of the next line.
 */
esc_next_line(sp)
	register SCREEN *sp;
{
	sp->s_csr.csr_x = LFT_M;
	sp->s_csr.csr_p.x = x_ofst;
	esc_index(sp);
}

/*
 *  tabulation set
 *  esc_tab_set(sp) sets tabulation stop at the current cursor point.
 */
esc_tab_set(sp)
	register SCREEN *sp;
{
	sp->s_tab_pos[sp->s_csr.csr_x] = 1;
}

/*
 *  reverse index
 *  esc_rev_index(sp) moves the cursor up if the cursor is not at the top 
 *  margin. If the cursor is at the top margin then the screen takes place
 *  scroll down.
 */
esc_rev_index(sp)
	register SCREEN *sp;
{
	cursor_off();
	if (sp->s_csr.csr_y == sp->s_region.top_margin)
		scroll_down(sp->s_region.top_margin,
			sp->s_region.btm_margin, sp->s_term_mode & DECSCNM,
			sp->s_plane, sp->s_bgcol);
	else {
		if (sp->s_csr.csr_y > TOP_M) {
			sp->s_csr.csr_y -= 1;
			sp->s_csr.csr_p.y -= char_h;
		}
	}
	sp->s_current_stat &= ~WRAP;
	cursor_on(&sp->s_csr.csr_p);
}

/*
 *  numeric key pad
 *  esc_numeric_kpad(sp) changes key pad of cursor to numeric one.
 *  This sequence is used in vi.
 *  currently not supported
 */
esc_numeric_kpad(sp)
	register SCREEN *sp;
{
	change_aux_key_pad(NUMERIC);
	sp->s_current_stat &= ~ESCAPE;
}

/*
 *  application key pad
 *  esc_application_kpad(sp) changes key pad of cursor to application one.
 *  This sequence is also used in vi.
 *  currently not supported.
 */
esc_application_kpad(sp)
	register SCREEN *sp;
{
	change_aux_key_pad(APPLIC);
	sp->s_current_stat &= ~ESCAPE;
}

/*
 *  change auxiliary keypad
 */
static
change_aux_key_pad(applic)
	register int applic;
{
	char pad[4];
	register Pfk_string *pfk = &pfk_str;
	register Key_string *kys = &pfk_str.pfk_string;
	register struct key_pad *kpd;
	register int i;

	kpd = &key_pad[0];
	if (applic) {
		pad[0] = '\033';
		pad[1] = 'O';
	}
	for (i = N0; i <= NENTER; i++) {

		pfk->pfk_num = i;
		kys->key_length = (applic) ? 3: 1;
		if (applic) {
			pad[2] = kpd->kpd_applic;
		} else {
			pad[0] = kpd->kpd_numeric;
		}
		kys->key_string = pad;
		kpd++;
		pfk->pfk_shift = PF_NORMAL;
		kbd_ioctl(SCC_KEYBOARD, KIOCSETS, pfk); 
		pfk->pfk_shift = PF_SHIFT;
		kbd_ioctl(SCC_KEYBOARD, KIOCSETS, pfk); 
	}
	if (!applic) {
		pfk->pfk_shift = PF_SHIFT;
		kys->key_length = 1;

		pfk->pfk_num = MINUS;
		kys->key_string = "/";
		kbd_ioctl(SCC_KEYBOARD, KIOCSETS, pfk); 

		pfk->pfk_num = PLUS;
		kys->key_string = "*";
		kbd_ioctl(SCC_KEYBOARD, KIOCSETS, pfk);

		pfk->pfk_num = COMMA;
		kys->key_string = "=";
 		kbd_ioctl(SCC_KEYBOARD, KIOCSETS, pfk);
	}
}

extern	struct	csr_buf	local_csr_buf;
/*
 *  change line size
 *  esc_line_size(sp, pn) changes line size.
 *	c = `3'	double side double height(top half)
 *	c = `4'	double side double height(bottom half)
 *	c = `5'	sigle width line
 *	c = `6'	double width line
 *  currently not supported
 */
esc_line_size(sp, c)
	register SCREEN *sp;
	char c;
{
	register int i;
	register int j;
	int save_f, save_b;

	cursor_off();
	switch (c) {
	case '5':
		local_csr_buf.csr_number = 1;
		break;
	case '6':
		local_csr_buf.csr_number = 2;
		break;
	case '8':
		sp->s_region.top_margin = TOP_M;
		sp->s_region.btm_margin = btm_m;
		save_f = fcolor;
		save_b = bcolor;
		fcolor = sp->s_bgcol;
		bcolor = sp->s_plane;
		sp->s_csr.csr_p.y = y_ofst;
		for (i = TOP_M; i <= btm_m; i++) {
			sp->s_csr.csr_p.x = x_ofst;
			sp->s_csr.csr_y = i;
			for (j = LFT_M; j <= rit_m; j++) {
				sp->s_csr.csr_x = j;
				copy_char(sp, 'E', 0);
				sp->s_csr.csr_p.x += char_w;
			}
			sp->s_csr.csr_p.y += char_h;
		}
		sp->s_csr.csr_x = LFT_M;
		sp->s_csr.csr_y = TOP_M;
		sp->s_csr.csr_p.x = x_ofst;
		sp->s_csr.csr_p.y = y_ofst;
		fcolor = save_f;
		bcolor = save_b;
		break;
	default:
		break;
	}
	cursor_on(&sp->s_csr.csr_p);
	sp->s_current_stat &= ~ESCAPE;
}

/*
 *  character set
 *  esc_char_setr sets which character set you use in right graphic set.
 *  currently not supported
 */
esc_char_setr(sp, c)
	register SCREEN *sp;
	int c;
{
#if defined(IPC_MRX) || defined(CPU_SINGLE)
	switch (c) {
	case 'J':
	case 'H':
		font_jisroman();
#ifdef CPU_SINGLE
		font_jisroman24();
#endif
		sp->s_current_stat &= ~JKANJI;
		break;
	case 'B':
		font_ascii();
#ifdef CPU_SINGLE
		font_ascii24();
#endif
		sp->s_current_stat &= ~JKANJI;
		break;
	}
#else /* IPC_MRX || CPU_SINGLE */
	if (c == 'B' || c == 'J' || c == 'H') {
		sp->s_current_stat &= ~JKANJI;
	}
#endif /* IPC_MRX || CPU_SINGLE */
	sp->s_current_stat &= ~ESCAPE;
}

/*
 *  character set to left graphic set
 *  esc_char_setl sets which character set you use in left graphic set.
 *  currently not supported
 */
esc_char_setl(sp, c)
	register SCREEN *sp;
	int c;
{
	sp->s_current_stat &= ~ESCAPE;
}

extern tmode;
extern  unsigned  int	first_jcode;
/*
 *  character set to kanji
 *  esc_kanji_set sets kanji
 */
esc_kanji_set(sp, c)
	register SCREEN *sp;
	int c;
{

#ifdef KM_JIS
	if (tmode == KM_JIS && (c == 'B' || c == '@')) {
		sp->s_current_stat |= JKANJI;
		first_jcode = 0;
	}
#endif
	sp->s_current_stat &= ~ESCAPE;
}

static short parm_buf[PARM_BUF_SIZ];
static short *parm_bp = parm_buf;
static int sensitive = 0;
static int pval = 0;
/*
 *  terminal parameter set command
 *  esc_parm_set(sp, c)  sets terminal parameters such as font-width, 
 *  font-height, character-width, character-height, character-position,
 *  underlind-position, screen-width, screen-height, x-offset, y-offset,
 *  right-mergin, bottom-mergin, dimmer-count, bell-length.
 */
esc_parm_set(sp, c)
	register SCREEN *sp;
	register unsigned int c;
{
	static int bufc = 0;

	if (in_str(c, sp->s_estp->terminators)) {
		if (sensitive) {
			*parm_bp++ = pval;
		} else {
			*parm_bp++ = -1;
		}
		*parm_bp++ = -1;
		parm_set(sp, parm_buf, c);
		sp->s_current_stat &= ~ESCAPE;
		sensitive = pval = 0;
		parm_bp = parm_buf;
		bufc = 0;
		return;
	}
	/*  buffering arguments  */
	if (bufc < PARM_BUF_SIZ) {
		if (c >= '0' && c <= '9') {
			pval = pval *10 + (c - '0');
			sensitive = 1;
		} else if (c == ';') {
			if (sensitive) {
				*parm_bp++ = pval;
			} else {
				*parm_bp++ = -1;
			}
			sensitive = pval = 0;
			bufc++;
		} else {
			sp->s_current_stat &= ~ESCAPE;
			sensitive = pval = 0;
			parm_bp = parm_buf;
			bufc = 0;
		}
	}
}

static	char	an_buf[AN_BUF_SIZ];

parm_set(sp, parm, terminator)
	SCREEN *sp;
	short *parm;
	unsigned int terminator;
{
	register char *bp = an_buf;
	register char *p;

	switch (terminator) {
	case 'f':
		if (parm[0] >= FONT_W_MIN && parm[0] <= consfb->font_w &&
							parm[0] < char_w)
			font_w =  parm[0];

		if (parm[1] >= FONT_H_MIN && parm[1] <= consfb->font_h &&
					parm[1] <= (char_h - ch_pos))
			font_h = parm[1];
		break;
	case 'c':
		if (parm[0] >= CHAR_W_MIN && parm[0] > font_w &&
						parm[0] <= CHAR_W_MAX)
			char_w = parm[0];

		if (parm[1] >= CHAR_H_MIN && parm[1] >= (font_h + ch_pos) &&
				parm[1] > ul_pos && parm[1] <= CHAR_H_MAX)
			char_h = parm[1];

		break;
	case 'p':
		if (parm[0] >= UL_POS_MIN && parm[0] <= UL_POS_MAX &&
						parm[0] < char_h) {
			ul_pos = parm[0];
		}
		if (parm[1] >= CH_POS_MIN && parm[1] <= CH_POS_MAX &&
					parm[1] < (char_h - font_h)) {
			ch_pos = parm[1];
		}
		break;
	case 's':
		if (parm[0] > SCR_W_MIN && parm[0] <= consfb->scr_w)
			scr_w = (parm[0] < char_w) ? char_w: parm[0];
		if (parm[1] > SCR_H_MIN && parm[1] <= consfb->scr_h)
			scr_h = (parm[1] < char_h) ? char_h: parm[1];
		break;
	case 'o':
		if (parm[0] >= X_OFST_MIN && parm[0] <= X_OFST_MAX)
			x_ofst = (parm[0] > scr_w - char_w) ?
				(scr_w - char_w): parm[0];
		if (parm[1] >= Y_OFST_MIN && parm[1] <= Y_OFST_MAX)
			y_ofst = (parm[1] > scr_h - char_h) ?
				(scr_h - char_h): parm[1];
		break;
	case 'm':
		if (parm[0] >= RIT_M_MIN) {
			if (parm[0] > RIT_M_MAX /* consfb->rit_m */) {
				parm[0] = consfb->rit_m;
			}
			rit_m = (parm[0] > (scr_w - x_ofst)/char_w) ?
				(scr_w - x_ofst)/char_w: parm[0];
		}
		if (parm[1] >= BTM_M_MIN) {
			if (parm[1] > BTM_M_MAX /* consfb->btm_m */) {
				parm[1] = consfb->btm_m;
			}
			btm_m = (parm[1] > (scr_h - y_ofst)/char_h) ?
				(scr_h - y_ofst)/char_h: parm[1];
		}
		break;
	case 'd':
		if (parm[0] >= DIM_CNT_MIN && parm[0] <= DIM_CNT_MAX)
			dim_cnt = a_dim_on = parm[0];
		else
			a_dim_on = 0;
		break;
	case 'b':
		if (parm[0] >= BELL_LEN_MIN && parm[0] <= BELL_LEN_MAX)
			bell_len = parm[0];
		break;
	case 'D':
		set_default_param();
		vt100init();
		bitmapinit();
		break;
	case 'i':
		cursor_off();
		csr_pos(sp, LFT_M, TOP_M);
		key_str.key_string = c_pos_mess;
		key_str.key_length = spr(c_pos_mess, "f=(%d,%d), ",
							font_w, font_h);
		kbd_ioctl(SCC_KEYBOARD, KIOCBACK, &key_str);

		key_str.key_length = spr(c_pos_mess, "c=(%d,%d), ",
							char_w, char_h);
		kbd_ioctl(SCC_KEYBOARD, KIOCBACK, &key_str);

		csr_pos(sp, LFT_M, (TOP_M - 1));
		key_str.key_string = c_pos_mess;
		key_str.key_length = spr(c_pos_mess, "p=(%d,%d), ",
							ul_pos, ch_pos);
		kbd_ioctl(SCC_KEYBOARD, KIOCBACK, &key_str);
		key_str.key_length = spr(c_pos_mess, "s=(%d,%d), ",
							scr_w, scr_h);
		kbd_ioctl(SCC_KEYBOARD, KIOCBACK, &key_str);

		csr_pos(sp, LFT_M, (TOP_M - 2));
		key_str.key_string = c_pos_mess;
		key_str.key_length = spr(c_pos_mess, "o=(%d,%d), ",
							x_ofst, y_ofst);
		kbd_ioctl(SCC_KEYBOARD, KIOCBACK, &key_str);
		key_str.key_length = spr(c_pos_mess, "m=(%d,%d)",
							rit_m, btm_m);
		kbd_ioctl(SCC_KEYBOARD, KIOCBACK, &key_str);

		cursor_on(&sp->s_csr.csr_p);
		return;
	case 'G':
		line(parm);
		return;
	case 'C':
		if (parm[0] >= 0) {
			sp->s_plane = fbbm_get_pixel(consfb, parm[0]);
		}
		if (parm[1] >= 0) {
			sp->s_bgcol = fbbm_get_pixel(consfb, parm[1]);
		}
		cursor_off();
		if ((sp->s_csr.csr_attributes & REVERSE) ^
			(sp->s_term_mode & DECSCNM)) {
			fcolor = sp->s_bgcol;
			bcolor = sp->s_plane;
		}
		else {
			fcolor = sp->s_plane;
			bcolor = sp->s_bgcol;
		}
		cursor_on(&sp->s_csr.csr_p);
		return;
	case 'T':
		if (parm[0] < 0 || consfb->Mono)
			return;
		/*
		 *  what value is defined on pallet N?
		 *    put string in an_buf
		 */
		*bp++ = '\033';
		*bp++ = '~';
		bp += itoa(bm_pallet_read(parm[0]), 10, bp);
		*bp++ = 'a';
		key_str.key_length = bp - an_buf;
		key_str.key_string = an_buf;
		kbd_ioctl(SCC_KEYBOARD, KIOCBACK, &key_str);
		return;
	case 't':
		if (parm[0] >= 0 && !consfb->Mono) {
			bm_pallet_write(parm[0],
					(unsigned) parm[1] << 16
					| (unsigned) parm[2] << 8
					| (unsigned) parm[3]
			);
		}
		return;
	default:
		return;
	}
	if (char_w < font_w) char_w = font_w;
	if (char_h < font_h) char_h = font_h;
	if (ch_pos > char_h - font_h) {
		ch_pos = char_h - font_h;
		ul_pos = char_h - 1;
	}
	if (rit_m > (scr_w - x_ofst)/char_w)
		rit_m = (scr_w - x_ofst)/char_w;
	if (btm_m > (scr_h - y_ofst)/char_h)
		btm_m = (scr_h - y_ofst)/char_h;
	sp->s_region.top_margin = TOP_M;
	sp->s_region.btm_margin = btm_m;
	font_r1.extent.x = font_w;
	font_r1.extent.y = font_h;
	font_r2.extent.x = font_w * 2;
	font_r2.extent.y = font_h;
	font_len1 = (font_w + 0x0f)>>4;
	font_len2 = (font_w*2 + 0x0f)>>4;
	cursor_off();
	char_r1.extent.x = char_w;
	char_r1.extent.y = char_h;
	char_r2.extent.x = char_w * 2;
	char_r2.extent.y = char_h;
	csr_pos(sp, sp->s_csr.csr_x, sp->s_csr.csr_y);
	sp->s_csr.csr_p.x = (sp->s_csr.csr_x - 1) * char_w + x_ofst;
	sp->s_csr.csr_p.y = (sp->s_csr.csr_y - 1) * char_h + y_ofst;
	cursor_on(&sp->s_csr.csr_p);
}

/* VARARGS */
spr(s, fmt, ad, dummy)
	register char *s, *fmt;
	u_int ad;
{
	register int b, c;
	register u_int *adx = &ad;
	char *base = s;

	for (;;) {
		while ((c = *fmt++) != '%') {
			*s++ = c;
			if (c == '\0')
				return (s - base - 1);
		}

		c = *fmt++;
		switch (c) {

		case 'x': case 'X':
			b = 16;
			goto number;
		case 'd': case 'D':
			b = 10;
			goto number;
		case 'o': case 'O':
			b = 8;
number:
			s += itoa(*adx, b, s);
			break;

		case 'c':
			*s++ = *adx;
			break;

		case '%':
			*s++ = c;
			break;
		}
		adx++;
	}
}

static int pfn = -1;
static int active_buf = 0;
/*
 *  define the programable function keys and answer back message.
 *  the vt100 facilities do not contain this command!
 *  command sequence is as follows:
 *       "^[Pn|n1;n2;...;nmp"		(normal mode)
 *  or
 *       "^[Pn|n1;n2;...;nmP"		(shift mode)
 *  or
 *       "^[Pn|n1;n2;...;nmZ"		(answer backe message)
 *  where, `n' denotes the decimal number asigned to function key,
 *          from `n1' to `nm' denote hexa number, finally,
 *	    `p' , `E' or `Z' tells that the sequence has terminated.
 *  remark:
 *	  when the terminator is `Z', the function number `n' can be omitted,
 *	  and even though the number is specified, there is no affection to 
 *	  the result.
 *
 *
 *  ADDITION:
 *	  there is a question: what strings are defined in programable function
 *	  key of key-number n?
 *	  in order to anwer this question, another escape sequence has appended.
 *	  command sequence is as follows:
 *
 *	   "^[Pn|i"			(normal mode)
 *  or
 *	   "^[Pn|I"			(shift	mode)
 *
 *	  then the answer is 
 *
 *	   "^[Pn|n1;n2;...;nmr"		(normal	mode)
 *  or
 *	   "^[Pn|n1;n2;...;nmR"		(shift	mode)
 *
 */
esc_pf_define(sp, c)
	SCREEN *sp;
	unsigned int c;
{
	static bufc = 0;

	if (in_str(c, sp->s_estp->terminators)) {
		pf_define(pfn, esc_bp - esc_buf + active_buf, c);
		sp->s_current_stat &= ~ESCAPE;
		active_buf = 0;
		pfn = -1;
		bufc = 0;
		return;
	}
	/*  buffering arguments  */
	if (bufc < ESC_BUF_SIZ) {
		if (pfn < 0) {
			if (c >= '0' && c <= '9') {
				*esc_bp = *esc_bp *10 + (c - '0');
			} else if (c == '|') {
				pfn = *esc_bp;
				*esc_bp = 0;
			} else {
				sp->s_current_stat &= ~ESCAPE;
				active_buf = 0;
				pfn = -1;
			}
		} else {
			active_buf = 1;
			if (c >= '0' && c <= '9') {
				*esc_bp = *esc_bp * 16 + (c - '0');
			} else if (c >= 'a' && c <= 'f') {
				*esc_bp = *esc_bp * 16 + (c - 'a' + 10);
			} else if (c >= 'A' && c <= 'F') {
				*esc_bp = *esc_bp * 16 + (c - 'A' + 10);
			} else if (c == ';') {
				esc_bp++;
				bufc++;
			} else {
				sp->s_current_stat &= ~ESCAPE;
				pfn = -1;
				active_buf = 0;
				bufc = 0;
			}
		}
	} else {
		active_buf = 0;
	}
}

pf_define(pfn, length, terminator)
	int pfn;
	int length;
	unsigned int terminator;
{
	register Pfk_string *pfk = &pfk_str;
	register Key_string *kys = &pfk_str.pfk_string;

	if (terminator == 'Z')
		return;

	if (pfn < 0 || pfn > N_PFK)
		return;
	if (terminator == 'i' || terminator == 'I') {
		pf_answer(pfn, terminator);
		return;
	}
	pfk->pfk_num = pfn ? pfn: 1;
	pfk->pfk_shift = (terminator == 'p') ? PF_NORMAL: PF_SHIFT;
	kys->key_length = length;
	kys->key_string = esc_buf;
 	kbd_ioctl(SCC_KEYBOARD, KIOCSETS, pfk); 
}

/*
 *  pf_answer(pfn, terminator)
 *  this routine answers what strings defined on pfn.
 */

char def_seq[ESC_BUF_SIZ];

pf_answer(pfn, terminator)
	int pfn;
	unsigned int terminator;
{
	register Pfk_string *pfk = &pfk_str;
	register Key_string *kys = &pfk_str.pfk_string;
	register char *bp = an_buf;
	register char *p = def_seq;
	register int length;
	register int j;
	
	/*
	 *  function key inquiry
	 *    get string in def_seq
	 */
	pfk->pfk_num = pfn ? pfn: 1;
	pfk->pfk_shift = (terminator == 'i') ? PF_NORMAL: PF_SHIFT;
	kys->key_length = ESC_BUF_SIZ;
	kys->key_string = def_seq;
	kbd_ioctl(SCC_KEYBOARD, KIOCGETS, pfk);
	length = kys->key_length;

	/*
	 *  function key answer
	 *    put string in an_buf
	 */
	*bp++ = '\033';
	*bp++ = 'P';
	bp += itoa(pfn, 10, bp);
	*bp++ = '|';
	key_str.key_length = bp - an_buf;
	key_str.key_string = an_buf;
	kbd_ioctl(SCC_KEYBOARD, KIOCBACK, &key_str);

	bp = an_buf;
	if (length--) {
		bp += itoa(*p++ & 0xff, 16, bp);
	}
	while (length > 0) {
		for (j = 0; (j < 10) && (length-- > 0); j++) {
			*bp++ = ';';
			bp += itoa(*p++ & 0xff, 16, bp);
		}
		key_str.key_length = bp - an_buf;
		kbd_ioctl(SCC_KEYBOARD, KIOCBACK, (int *)&key_str);
		bp = an_buf;
	}
	*bp++ = (terminator == 'i') ? 'r': 'R';
	key_str.key_length = bp - an_buf;
	kbd_ioctl(SCC_KEYBOARD, KIOCBACK, (int *)&key_str);
}

/*
 *  ignore
 *  esc_ignore(sp) is not called ordinally work.
 */
esc_ignore(sp)
	register SCREEN *sp;
{
	sp->s_current_stat &= ~ESCAPE;
}

static  char	*nmr = "0123456789abcdef";
/*
 *  itoa
 *  this routine converts binary to ascii decimal or hexa number
 *  according to mod.
 */
static
itoa(n, mod, buf)
	register int n;
	register int mod;
	register char *buf;
{
	register  int	i = 0;
	register  int	cnt;
	int	first = 1;
	int	k;

	n &= 0xffff;
	for (cnt = mod*mod*mod*mod*mod*mod*mod; cnt > 0; cnt /= mod) {
		k = n / cnt;
		n -= k * cnt;
		if (k == 0) {
			if (first == 0) {
				*buf++ = nmr[k];
				i++;
			}
		} else {
			*buf++ = nmr[k];
			i++;
			first = 0;
		}
	}
	if (first == 1) {
		*buf++ = '0';
		i++;
	}
	return(i);
}
