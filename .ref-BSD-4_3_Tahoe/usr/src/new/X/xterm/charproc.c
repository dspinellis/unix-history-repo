/*
 *	$Source: /u1/X/xterm/RCS/charproc.c,v $
 *	$Header: charproc.c,v 10.102 86/12/02 11:37:25 swick Exp $
 */

#include <X/mit-copyright.h>

/* Copyright (c) 1985 Massachusetts Institute of Technology		*/
/* Copyright (c) 1985	Digital Equipment Corporation			*/

/* charproc.c */

#include <stdio.h>
#include <sgtty.h>
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <X/Xlib.h>
#include "scrollbar.h"
#include "ptyx.h"
#include "VTparse.h"
#include "data.h"
#include "error.h"
#ifdef MODEMENU
#include "menu.h"
#endif MODEMENU

#define	DEFAULT		-1
#define	TEXT_BUF_SIZE	256

#define	input()		(bcnt-- > 0 ? *bptr++ : in_put())

#ifndef lint
static char csrg_id[] = "@(#)charproc.c	1.6\t(Berkeley/CSRG)\t9/21/87";
static char sccs_id[] = "@(#)charproc.c\tX10/6.6B\t1/9/87";
#endif lint

static long arg;
static int ch;
static int nparam;
static ANSI reply;
static int param[NPARAM];

static unsigned long ctotal;
static unsigned long ntotal;
static jmp_buf vtjmpbuf;

extern int groundtable[];
extern int csitable[];
extern int dectable[];
extern int eigtable[];
extern int esctable[];
extern int iestable[];
extern int igntable[];
extern int scrtable[];
extern int scstable[];

VTparse()
{
	register Screen *screen = &term.screen;
	register int *parsestate = groundtable;
	register int c;
	register char *cp;
	register int row, col, top, bot, scstype;
	WindowInfo wininfo;
	extern int bitset(), bitclr(), finput();

	if(setjmp(vtjmpbuf))
		parsestate = groundtable;
	for( ; ; )
		switch(parsestate[c = input()]) {
		 case CASE_GROUND_STATE:
			/* exit ignore mode */
			parsestate = groundtable;
			break;

		 case CASE_IGNORE_STATE:
			/* Ies: ignore anything else */
			parsestate = igntable;
			break;

		 case CASE_IGNORE_ESC:
			/* Ign: escape */
			parsestate = iestable;
			break;

		 case CASE_IGNORE:
			/* Ignore character */
			break;

		 case CASE_BELL:
			/* bell */
			Bell();
			break;

		 case CASE_BS:
			/* backspace */
			CursorBack(screen, 1);
			break;

		 case CASE_CR:
			/* carriage return */
			CarriageReturn(screen);
			break;

		 case CASE_ESC:
			/* escape */
			parsestate = esctable;
			break;

		 case CASE_VMOT:
			/*
			 * form feed, line feed, vertical tab, but not in
			 * status line
			 */
			if(!screen->instatus)
				Index(screen, 1);
			if (term.flags & LINEFEED)
				CarriageReturn(screen);
			if(screen->display->qlen > 0 ||
			 (ioctl(screen->display->fd, FIONREAD, &arg), arg) > 0)
				xevents();
			break;

		 case CASE_TAB:
			/* tab */
			screen->cur_col = TabNext(term.tabs, screen->cur_col);
			if (screen->cur_col > screen->max_col)
				screen->cur_col = screen->max_col;
			break;

		 case CASE_SI:
			screen->curgl = 0;
			break;

		 case CASE_SO:
			screen->curgl = 1;
			break;

		 case CASE_SCR_STATE:
			/* enter scr state */
			parsestate = scrtable;
			break;

		 case CASE_SCS0_STATE:
			/* enter scs state 0 */
			scstype = 0;
			parsestate = scstable;
			break;

		 case CASE_SCS1_STATE:
			/* enter scs state 1 */
			scstype = 1;
			parsestate = scstable;
			break;

		 case CASE_SCS2_STATE:
			/* enter scs state 2 */
			scstype = 2;
			parsestate = scstable;
			break;

		 case CASE_SCS3_STATE:
			/* enter scs state 3 */
			scstype = 3;
			parsestate = scstable;
			break;

		 case CASE_ESC_IGNORE:
			/* unknown escape sequence */
			parsestate = eigtable;
			break;

		 case CASE_ESC_DIGIT:
			/* digit in csi or dec mode */
			if((row = param[nparam - 1]) == DEFAULT)
				row = 0;
			param[nparam - 1] = 10 * row + (c - '0');
			break;

		 case CASE_ESC_SEMI:
			/* semicolon in csi or dec mode */
			param[nparam++] = DEFAULT;
			break;

		 case CASE_DEC_STATE:
			/* enter dec mode */
			parsestate = dectable;
			break;

		 case CASE_ICH:
			/* ICH */
			if((c = param[0]) < 1)
				c = 1;
			InsertChar(screen, c);
			parsestate = groundtable;
			break;

		 case CASE_CUU:
			/* CUU */
			/* only if not in status line */
			if(!screen->instatus) {
				if((c = param[0]) < 1)
					c = 1;
				CursorUp(screen, c);
			}
			parsestate = groundtable;
			break;

		 case CASE_CUD:
			/* CUD */
			/* only if not in status line */
			if(!screen->instatus) {
				if((c = param[0]) < 1)
					c = 1;
				CursorDown(screen, c);
			}
			parsestate = groundtable;
			break;

		 case CASE_CUF:
			/* CUF */
			if((c = param[0]) < 1)
				c = 1;
			CursorForward(screen, c);
			parsestate = groundtable;
			break;

		 case CASE_CUB:
			/* CUB */
			if((c = param[0]) < 1)
				c = 1;
			CursorBack(screen, c);
			parsestate = groundtable;
			break;

		 case CASE_CUP:
			/* CUP | HVP */
			/* only if not in status line */
			if(!screen->instatus) {
				if((row = param[0]) < 1)
					row = 1;
				if(nparam < 2 || (col = param[1]) < 1)
					col = 1;
				CursorSet(screen, row-1, col-1, term.flags);
			}
			parsestate = groundtable;
			break;

		 case CASE_ED:
			/* ED */
			switch (param[0]) {
			 case DEFAULT:
			 case 0:
				if(screen->instatus)
					ClearRight(screen);
				else
					ClearBelow(screen);
				break;

			 case 1:
				if(screen->instatus)
					ClearLeft(screen);
				else
					ClearAbove(screen);
				break;

			 case 2:
				if(screen->instatus)
					ClearLine(screen);
				else
					ClearScreen(screen);
				break;
			}
			parsestate = groundtable;
			break;

		 case CASE_EL:
			/* EL */
			switch (param[0]) {
			 case DEFAULT:
			 case 0:
				ClearRight(screen);
				break;
			 case 1:
				ClearLeft(screen);
				break;
			 case 2:
				ClearLine(screen);
				break;
			}
			parsestate = groundtable;
			break;

		 case CASE_IL:
			/* IL */
			/* only if not in status line */
			if(!screen->instatus) {
				if((c = param[0]) < 1)
					c = 1;
				InsertLine(screen, c);
			}
			parsestate = groundtable;
			break;

		 case CASE_DL:
			/* DL */
			/* only if not in status line */
			if(!screen->instatus) {
				if((c = param[0]) < 1)
					c = 1;
				DeleteLine(screen, c);
			}
			parsestate = groundtable;
			break;

		 case CASE_DCH:
			/* DCH */
			if((c = param[0]) < 1)
				c = 1;
			DeleteChar(screen, c);
			parsestate = groundtable;
			break;

		 case CASE_DA1:
			/* DA1 */
			if (param[0] <= 0) {	/* less than means DEFAULT */
				reply.a_type   = CSI;
				reply.a_pintro = '?';
				reply.a_nparam = 2;
				reply.a_param[0] = 1;		/* VT102 */
				reply.a_param[1] = 2;		/* VT102 */
				reply.a_inters = 0;
				reply.a_final  = 'c';
				unparseseq(&reply, screen->respond);
			}
			parsestate = groundtable;
			break;

		 case CASE_TBC:
			/* TBC */
			if ((c = param[0]) <= 0) /* less than means default */
				TabClear(term.tabs, screen->cur_col);
			else if (c == 3)
				TabZonk(term.tabs);
			parsestate = groundtable;
			break;

		 case CASE_SET:
			/* SET */
			modes(&term, bitset);
			parsestate = groundtable;
			break;

		 case CASE_RST:
			/* RST */
			modes(&term, bitclr);
			parsestate = groundtable;
			break;

		 case CASE_SGR:
			/* SGR */
			for (c=0; c<nparam; ++c) {
				switch (param[c]) {
				 case DEFAULT:
				 case 0:
					term.flags &= ~(INVERSE|BOLD|UNDERLINE);
					break;
				 case 1:
				 case 5:	/* Blink, really.	*/
					term.flags |= BOLD;
					break;
				 case 4:	/* Underscore		*/
					term.flags |= UNDERLINE;
					break;
				 case 7:
					term.flags |= INVERSE;
				}
			}
			parsestate = groundtable;
			break;

		 case CASE_CPR:
			/* CPR */
			if ((c = param[0]) == 5) {
				reply.a_type = CSI;
				reply.a_pintro = 0;
				reply.a_nparam = 1;
				reply.a_param[0] = 0;
				reply.a_inters = 0;
				reply.a_final  = 'n';
				unparseseq(&reply, screen->respond);
			} else if (c == 6) {
				reply.a_type = CSI;
				reply.a_pintro = 0;
				reply.a_nparam = 2;
				reply.a_param[0] = screen->cur_row+1;
				reply.a_param[1] = screen->cur_col+1;
				reply.a_inters = 0;
				reply.a_final  = 'R';
				unparseseq(&reply, screen->respond);
			}
			parsestate = groundtable;
			break;

		 case CASE_DECSTBM:
			/* DECSTBM */
			/* only if not in status line */
			if(!screen->instatus) {
				if((top = param[0]) < 1)
					top = 1;
				if(nparam < 2 || (bot = param[1]) == DEFAULT
				   || bot > screen->max_row + 1
				   || bot == 0)
					bot = screen->max_row+1;
				if (bot > top) {
					if(screen->scroll_amt)
						FlushScroll(screen);
					screen->top_marg = top-1;
					screen->bot_marg = bot-1;
					CursorSet(screen, 0, 0, term.flags);
				}
			}
			parsestate = groundtable;
			break;

		 case CASE_SUN_EMU:
			/* sub-set of sun tty emulation */
			switch(param[0]) {
			 case 3:	/* move window */
				if(nparam == 3) {
					XMoveWindow(VWindow(screen), param[2],
					 param[1]);
					XSync(FALSE);	/* synchronize */
					if(QLength() > 0)
						xevents();
				}
				break;
			 case 4:	/* resize window (pixels) */
				if(nparam == 3) {
					XChangeWindow (VWindow(screen), param[1],
					 param[2]);
					XSync(FALSE);	/* synchronize */
					if(QLength() > 0)
						xevents();
				}
				break;
			 case 5:	/* raise window */
				XRaiseWindow(VWindow(screen));
				break;
			 case 6:	/* lower window */
				XLowerWindow(VWindow(screen));
				break;
			 case 7:	/* redisplay window */
				Redraw();
				break;
			 case 8:	/* resize window (rows and columns) */
				if(nparam == 3) {
					XChangeWindow (VWindow(screen),
					 FontWidth(screen) * param[2] +
					 2 * screen->border + screen->scrollbar,
					 FontHeight(screen) * param[1] +
					 screen->statusheight + Titlebar(screen)
					 + 2 * screen->border);
					XSync(FALSE);	/* synchronize */
					if(QLength() > 0)
						xevents();
				}
				break;
			 case 13:	/* send window position */
				XQueryWindow(VWindow(screen), &wininfo);
				reply.a_type = CSI;
				reply.a_pintro = 0;
				reply.a_nparam = 3;
				reply.a_param[0] = 3;
				reply.a_param[1] = wininfo.y;
				reply.a_param[2] = wininfo.x;
				reply.a_inters = 0;
				reply.a_final  = 't';
				unparseseq(&reply, screen->respond);
				break;
			 case 14:	/* send window size (pixels) */
				reply.a_type = CSI;
				reply.a_pintro = 0;
				reply.a_nparam = 3;
				reply.a_param[0] = 4;
				reply.a_param[1] = (screen->max_col + 1) *
				 FontWidth(screen) + 2 * screen->border +
				 screen->scrollbar;
				reply.a_param[2] = (screen->max_row + 1) *
				 FontHeight(screen) + screen->statusheight +
				 Titlebar(screen) + 2 * screen->border;
				reply.a_inters = 0;
				reply.a_final  = 't';
				unparseseq(&reply, screen->respond);
				break;
			 case 18:	/* send window size (rows and cols) */
				reply.a_type = CSI;
				reply.a_pintro = 0;
				reply.a_nparam = 3;
				reply.a_param[0] = 8;
				reply.a_param[1] = screen->max_row + 1;
				reply.a_param[2] = screen->max_col + 1;
				reply.a_inters = 0;
				reply.a_final  = 't';
				unparseseq(&reply, screen->respond);
				break;
			}
			parsestate = groundtable;
			break;

		 case CASE_DECREQTPARM:
			/* DECREQTPARM */
			if ((c = param[0]) == DEFAULT)
				c = 0;
			if (c == 0 || c == 1) {
				reply.a_type = CSI;
				reply.a_pintro = 0;
				reply.a_nparam = 7;
				reply.a_param[0] = c + 2;
				reply.a_param[1] = 1;	/* no parity */
				reply.a_param[2] = 1;	/* eight bits */
				reply.a_param[3] = 112;	/* transmit 9600 baud */
				reply.a_param[4] = 112;	/* receive 9600 baud */
				reply.a_param[5] = 1;	/* clock multiplier ? */
				reply.a_param[6] = 0;	/* STP flags ? */
				reply.a_inters = 0;
				reply.a_final  = 'x';
				unparseseq(&reply, screen->respond);
			}
			parsestate = groundtable;
			break;

		 case CASE_DECSET:
			/* DECSET */
			dpmodes(&term, bitset);
			parsestate = groundtable;
			if(screen->TekEmu)
				return;
			break;

		 case CASE_DECRST:
			/* DECRST */
			dpmodes(&term, bitclr);
			parsestate = groundtable;
			break;

		 case CASE_HIDDEN:
			/* special "hidden" sequence */
			fprintf(stderr, "avg call = %ld char\n", ctotal/ntotal);
			parsestate = groundtable;
			break;

		 case CASE_DECALN:
			/* DECALN */
			if(screen->cursor_state)
				HideCursor();
			for(row = screen->max_row ; row >= 0 ; row--) {
				bzero(screen->buf[2 * row + 1],
				 col = screen->max_col + 1);
				for(cp = screen->buf[2 * row] ; col > 0 ; col--)
					*cp++ = 'E';
			}
			ScrnRefresh(screen, 0, 0, screen->max_row + 1,
			 screen->max_col + 1);
			parsestate = groundtable;
			break;

		 case CASE_GSETS:
			screen->gsets[scstype] = c;
			parsestate = groundtable;
			break;

		 case CASE_DECSC:
			/* DECSC */
			CursorSave(&term, &screen->sc);
			parsestate = groundtable;
			break;

		 case CASE_DECRC:
			/* DECRC */
			CursorRestore(&term, &screen->sc);
			parsestate = groundtable;
			break;

		 case CASE_DECKPAM:
			/* DECKPAM */
			term.keyboard.flags |= KYPD_APL;
			parsestate = groundtable;
			break;

		 case CASE_DECKPNM:
			/* DECKPNM */
			term.keyboard.flags &= ~KYPD_APL;
			parsestate = groundtable;
			break;

		 case CASE_IND:
			/* IND */
			/* only if not in status line */
			if(!screen->instatus)
				Index(screen, 1);
			if(screen->display->qlen > 0 ||
			 (ioctl(screen->display->fd, FIONREAD, &arg), arg) > 0)
				xevents();
			parsestate = groundtable;
			break;

		 case CASE_NEL:
			/* NEL */
			/* only if not in status line */
			if(!screen->instatus)
				Index(screen, 1);
			CarriageReturn(screen);
			if(screen->display->qlen > 0 ||
			 (ioctl(screen->display->fd, FIONREAD, &arg), arg) > 0)
				xevents();
			parsestate = groundtable;
			break;

		 case CASE_HTS:
			/* HTS */
			TabSet(term.tabs, screen->cur_col);
			parsestate = groundtable;
			break;

		 case CASE_RI:
			/* RI */
			/* only if not in status line */
			if(!screen->instatus)
				RevIndex(screen, 1);
			parsestate = groundtable;
			break;

		 case CASE_SS2:
			/* SS2 */
			screen->curss = 2;
			parsestate = groundtable;
			break;

		 case CASE_SS3:
			/* SS3 */
			screen->curss = 3;
			parsestate = groundtable;
			break;

		 case CASE_CSI_STATE:
			/* enter csi state */
			nparam = 1;
			param[0] = DEFAULT;
			parsestate = csitable;
			break;

		 case CASE_OSC:
			/* do osc escapes */
			do_osc(finput);
			parsestate = groundtable;
			break;

		 case CASE_RIS:
			/* RIS */
			VTReset(TRUE);
			parsestate = groundtable;
			break;

		 case CASE_LS2:
			/* LS2 */
			screen->curgl = 2;
			parsestate = groundtable;
			break;

		 case CASE_LS3:
			/* LS3 */
			screen->curgl = 3;
			parsestate = groundtable;
			break;

		 case CASE_LS3R:
			/* LS3R */
			screen->curgr = 3;
			parsestate = groundtable;
			break;

		 case CASE_LS2R:
			/* LS2R */
			screen->curgr = 2;
			parsestate = groundtable;
			break;

		 case CASE_LS1R:
			/* LS1R */
			screen->curgr = 1;
			parsestate = groundtable;
			break;

		 case CASE_TO_STATUS:
			if((c = param[0]) < 1)
				c = 1;
			ToStatus(c - 1);
			parsestate = groundtable;
			break;

		 case CASE_FROM_STATUS:
			FromStatus();
			parsestate = groundtable;
			break;

		 case CASE_SHOW_STATUS:
			ShowStatus();
			parsestate = groundtable;
			break;

		 case CASE_HIDE_STATUS:
			HideStatus();
			parsestate = groundtable;
			break;

		 case CASE_ERASE_STATUS:
			EraseStatus();
			parsestate = groundtable;
			break;

		 case CASE_XTERM_SAVE:
			savemodes(&term);
			parsestate = groundtable;
			break;

		 case CASE_XTERM_RESTORE:
			restoremodes(&term);
			parsestate = groundtable;
			break;

		 case CASE_PRINT:
			/* printable characters */
			top = bcnt > TEXT_BUF_SIZE ? TEXT_BUF_SIZE : bcnt;
			cp = bptr;
			*--bptr = c;
			while(top > 0 && isprint(*cp)) {
				top--;
				bcnt--;
				cp++;
			}
			if(screen->curss) {
				dotext(screen, term.flags,
				 screen->gsets[screen->curss], bptr, bptr + 1);
				screen->curss = 0;
				bptr++;
			}
			if(bptr < cp)
				dotext(screen, term.flags,
				 screen->gsets[screen->curgl], bptr, cp);
			bptr = cp;
			break;
		}
}

finput()
{
	return(input());
}

static int select_mask;

in_put()
{
	register Screen *screen = &term.screen;
	register char *cp;
	register int i;

	select_mask = pty_mask;	/* force initial read */
	for( ; ; ) {
		if(select_mask & pty_mask) {
			if(screen->logging)
				FlushLog(screen);
			if((bcnt = read(screen->respond, bptr = buffer,
			 BUF_SIZE)) < 0) {
				if(errno == EIO && am_slave)
					exit(0);
				else if(errno != EWOULDBLOCK)
					Panic(
				 "input: read returned unexpected error (%d)\n",
					 errno);
			} else if(bcnt == 0)
				Panic("input: read returned zero\n");
			else {
				/* strip parity bit */
				for(i = bcnt, cp = bptr ; i > 0 ; i--)
					*cp++ &= CHAR;
				if(screen->sb && screen->scrollinput &&
				 screen->topline < 0)
					ScrollToBottom(screen->sb);
				if(screen->icon_show && !screen->iconinput) {
					screen->iconinput = TRUE;
					IconBox(screen);
				}
				break;
			}
		}
		if(screen->scroll_amt)
			FlushScroll(screen);
		if(screen->cursor_set && (screen->cursor_col != screen->cur_col
		 || screen->cursor_row != screen->cur_row)) {
			if(screen->cursor_state)
				HideCursor();
			ShowCursor();
		} else if(screen->cursor_set != screen->cursor_state) {
			if(screen->cursor_set)
				ShowCursor();
			else
				HideCursor();
		}

		/* Window select/unselect */
		if (doonalarmcode)
			onalarmcode();

		if(QLength())
			select_mask = X_mask;
		else {
			XFlush();
			select_mask = Select_mask;
			if((i = select(max_plus1, &select_mask, NULL, NULL,
			 screen->timeout)) < 0){
				if (errno != EINTR)
					SysError(ERROR_SELECT);
				continue;
			} else if(i == 0) {
				if(GetButtonState(screen->sb) & HILITED)
					WindowScroll(screen,
					 ButtonRegion(screen->sb));
				screen->timeout->tv_usec = STEPTIME;
				continue;
			}
		}
		if(select_mask & X_mask)
			xevents();
	}
	bcnt--;
	return(*bptr++);
}

/*
 * process a string of characters according to the character set indicated
 * by charset.  worry about end of line conditions (wraparound if selected).
 */
dotext(screen, flags, charset, buf, ptr)
register Screen	*screen;
unsigned	flags;
char		charset;
char	*buf;
char	*ptr;
{
	register char	*s;
	register int	len;
	register int	n;
	register int	next_col;

	switch (charset) {
	case 'A':	/* United Kingdom set				*/
		for (s=buf; s<ptr; ++s)
			if (*s == '#')
				*s = '\036';	/* UK pound sign	*/
		break;

	case 'B':	/* ASCII set					*/
		break;

	case '0':	/* special graphics (line drawing)		*/
		for (s=buf; s<ptr; ++s)
			if (*s>=0x5f && *s<=0x7e)
				*s = *s == 0x5f ? 0x7f : *s - 0x5f;
		break;

	default:	/* any character sets we don't recognize	*/
		return;
	}

	len = ptr - buf; 
	ptr = buf;
	while (len > 0) {
		n = screen->max_col-screen->cur_col+1;
		if (n <= 1) {
			if (screen->do_wrap && (flags&WRAPAROUND) &&
			 !screen->instatus) {
				Index(screen, 1);
				screen->cur_col = 0;
				screen->do_wrap = 0;
				n = screen->max_col+1;
			} else
				n = 1;
		}
		if (len < n)
			n = len;
		next_col = screen->cur_col + n;
		WriteText(screen, ptr, n, flags);
		/*
		 * the call to WriteText updates screen->cur_col.
		 * If screen->cur_col != next_col, we must have
		 * hit the right margin, so set the do_wrap flag.
		 */
		screen->do_wrap = (screen->cur_col < next_col);
		len -= n;
		ptr += n;
	}
}
 
/*
 * write a string str of length len onto the screen at
 * the current cursor position.  update cursor position.
 */
WriteText(screen, str, len, flags)
register Screen	*screen;
register char	*str;
register int	len;
unsigned	flags;
{
	register int pix, cx, cy;
	register unsigned fgs = flags;
	Font	fnt;
 
   if(screen->instatus && screen->reversestatus)
	fgs ^= INVERSE;
   if(screen->cur_row - screen->topline <= screen->max_row ||
     screen->instatus) {
	/*
	if(screen->cur_row == screen->cursor_row && screen->cur_col <=
	 screen->cursor_col && screen->cursor_col <= screen->cur_col + len - 1)
		screen->cursor_state = OFF;
	 */
	if(screen->cursor_state)
		HideCursor();
 	fnt = ActiveIcon(screen) ? screen->fnt_icon
	      : (fgs & BOLD) ? screen->fnt_bold : screen->fnt_norm;
	if (fgs & INSERT)
		InsertChar(screen, len);
      if (!(AddToRefresh(screen))) {
		if(screen->scroll_amt)
			FlushScroll(screen);
	cx = CursorX(screen, screen->cur_col);
	cy = CursorY(screen, screen->cur_row);
	if (screen->show || ActiveIcon(screen)) {
	    if (fgs & INVERSE)
		XText(VWindow(screen), cx, cy, str, len, fnt,
		 pix = screen->background, screen->foreground);
	    else
	 	XText(VWindow(screen), cx, cy, str, len, fnt,
		 pix = screen->foreground, screen->background);
	    if((fgs & BOLD) && screen->enbolden)
		XTextMask(VWindow(screen), cx + 1, cy, str, len, fnt, pix);
	    if(fgs & UNDERLINE) {
		cy += FontHeight(screen) - 2;
		XLine(VWindow(screen), cx, cy, cx + len * FontWidth(screen),
		 cy, 1, 1, pix, GXcopy, AllPlanes);
	    }
	}
	/*
	 * the following statements compile data to compute the average 
	 * number of characters written on each call to XText.  The data
	 * may be examined via the use of a "hidden" escape sequence.
	 */
	ctotal += len;
	++ntotal;
      }
    }
	ScreenWrite(screen, str, flags, len);
	CursorForward(screen, len);
}
 
/*
 * process ANSI modes set, reset
 */
modes(term, func)
Terminal	*term;
int		(*func)();
{
	register Screen	*screen	= &term->screen;
	register int	i;

	for (i=0; i<nparam; ++i) {
		switch (param[i]) {
		case 4:			/* IRM				*/
			(*func)(&term->flags, INSERT);
			break;

		case 20:		/* LNM				*/
			(*func)(&term->flags, LINEFEED);
			break;
		}
	}
}

/*
 * process DEC private modes set, reset
 */
dpmodes(term, func)
Terminal	*term;
int		(*func)();
{
	register Screen	*screen	= &term->screen;
	register int	i, j;
	extern int bitset();

	for (i=0; i<nparam; ++i) {
		switch (param[i]) {
		case 1:			/* DECCKM			*/
			(*func)(&term->keyboard.flags, CURSOR_APL);
			break;
		case 3:			/* DECCOLM			*/
			if(screen->c132) {
				ClearScreen(screen);
				CursorSet(screen, 0, 0, term->flags);
				if((j = func == bitset ? 132 : 80) !=
				 ((term->flags & IN132COLUMNS) ? 132 : 80) ||
				 j != screen->max_col + 1) {
					XChangeWindow (VWindow(screen),
					 FontWidth(screen) * j + 2*screen->border
					 + screen->scrollbar,
					 FontHeight(screen) * (screen->max_row
					 + 1) + screen->statusheight +
					 Titlebar(screen) + 2 * screen->border);
					XSync(FALSE);	/* synchronize */
					if(QLength() > 0)
						xevents();
				}
				(*func)(&term->flags, IN132COLUMNS);
			}
			break;
		case 4:			/* DECSCLM (slow scroll)	*/
			if (func == bitset) {
				screen->jumpscroll = 0;
				if (screen->scroll_amt)
					FlushScroll(screen);
			} else
				screen->jumpscroll = 1;
			(*func)(&term->flags, SMOOTHSCROLL);
			break;
		case 5:			/* DECSCNM			*/
			j = term->flags;
			(*func)(&term->flags, REVERSE_VIDEO);
			if ((term->flags ^ j) & REVERSE_VIDEO)
				ReverseVideo(term);
			break;

		case 6:			/* DECOM			*/
			(*func)(&term->flags, ORIGIN);
			CursorSet(screen, 0, 0, term->flags);
			break;

		case 7:			/* DECAWM			*/
			(*func)(&term->flags, WRAPAROUND);
			break;
		case 8:			/* DECARM			*/
			j = term->flags;
			(*func)(&term->flags, AUTOREPEAT);
			if ((term->flags ^ j) & AUTOREPEAT)
				if(term->flags & AUTOREPEAT)
					XAutoRepeatOn();
				else
					XAutoRepeatOff();
			break;
		case 9:			/* MIT bogus sequence		*/
			(*func)(&screen->send_mouse_pos, 1);
			break;
		case 38:		/* DECTEK			*/
			if(func == bitset & !(screen->inhibit & I_TEK)) {
				if(screen->logging) {
					FlushLog(screen);
					screen->logstart = Tbuffer;
				}
				screen->TekEmu = TRUE;
			}
			break;
		case 40:		/* 132 column mode		*/
			(*func)(&screen->c132, 1);
			break;
		case 41:		/* curses hack			*/
			(*func)(&screen->curses, 1);
			break;
		case 42:		/* scrollbar			*/
			if(func == bitset)
				ScrollBarOn(screen, TRUE, FALSE);
			else
				ScrollBarOff(screen);
			break;
		case 43:		/* lines off top		*/
			if(screen->sb)
				SetSaveState(screen->sb, (func == bitset));
			break;
		case 44:		/* margin bell			*/
			(*func)(&screen->marginbell, 1);
			if(!screen->marginbell)
				screen->bellarmed = -1;
			break;
		case 45:		/* reverse wraparound	*/
			(*func)(&term->flags, REVERSEWRAP);
			break;
		case 46:		/* logging		*/
			if(func == bitset)
				StartLog(screen);
			else
				CloseLog(screen);
			break;
		case 47:		/* alternate buffer		*/
			if(func == bitset)
				ToAlternate(screen);
			else
				FromAlternate(screen);
			break;
		case 48:		/* reverse status line	*/
			j = screen->reversestatus;
			(*func)(&screen->reversestatus, 1);
			if(j != screen->reversestatus)
				ScrnRefresh(screen, screen->max_row + 1, 0, 1,
				 screen->max_col + 1);
			break;
		case 49:		/* page mode		*/
			j = screen->pagemode;
			(*func)(&screen->pagemode, 1);
			if(!j && screen->pagemode)
				screen->pagecnt = 0;
			break;
		}
	}
}

/*
 * process xterm private modes save
 */
savemodes(term)
Terminal *term;
{
	register Screen	*screen	= &term->screen;
	register int i;

	for (i = 0; i < nparam; i++) {
		switch (param[i]) {
		case 1:			/* DECCKM			*/
			screen->save_modes[0] = term->keyboard.flags &
			 CURSOR_APL;
			break;
		case 3:			/* DECCOLM			*/
			if(screen->c132)
				screen->save_modes[1] = term->flags &
				 IN132COLUMNS;
			break;
		case 4:			/* DECSCLM (slow scroll)	*/
			screen->save_modes[2] = term->flags & SMOOTHSCROLL;
			break;
		case 5:			/* DECSCNM			*/
			screen->save_modes[3] = term->flags & REVERSE_VIDEO;
			break;
		case 6:			/* DECOM			*/
			screen->save_modes[4] = term->flags & ORIGIN;
			break;

		case 7:			/* DECAWM			*/
			screen->save_modes[5] = term->flags & WRAPAROUND;
			break;
		case 8:			/* DECARM			*/
			screen->save_modes[6] = term->flags & AUTOREPEAT;
			break;
		case 9:			/* MIT bogus sequence		*/
			screen->save_modes[7] = screen->send_mouse_pos;
			break;
		case 40:		/* 132 column mode		*/
			screen->save_modes[8] = screen->c132;
			break;
		case 41:		/* curses hack			*/
			screen->save_modes[9] = screen->curses;
			break;
		case 42:		/* scrollbar			*/
			screen->save_modes[10] = screen->scrollbar;
			break;
		case 43:		/* lines off top		*/
			if(screen->sb)
				screen->save_modes[11] =
				 GetSaveState(screen->sb);
			break;
		case 44:		/* margin bell			*/
			screen->save_modes[12] = screen->marginbell;
			break;
		case 45:		/* reverse wraparound	*/
			screen->save_modes[13] = term->flags & REVERSEWRAP;
			break;
		case 46:		/* logging		*/
			screen->save_modes[14] = screen->logging;
			break;
		case 47:		/* alternate buffer		*/
			screen->save_modes[15] = screen->alternate;
			break;
		case 48:		/* reverse status line		*/
			screen->save_modes[16] = screen->reversestatus;
			break;
		case 49:		/* page mode			*/
			screen->save_modes[17] = screen->pagemode;
			screen->save_modes[18] = screen->pagecnt;
			break;
		}
	}
}

/*
 * process xterm private modes restore
 */
restoremodes(term)
Terminal *term;
{
	register Screen	*screen	= &term->screen;
	register int i, j;

	for (i = 0; i < nparam; i++) {
		switch (param[i]) {
		case 1:			/* DECCKM			*/
			term->keyboard.flags &= ~CURSOR_APL;
			term->keyboard.flags |= screen->save_modes[0] &
			 CURSOR_APL;
			break;
		case 3:			/* DECCOLM			*/
			if(screen->c132) {
				ClearScreen(screen);
				CursorSet(screen, 0, 0, term->flags);
				if((j = (screen->save_modes[1] & IN132COLUMNS)
				 ? 132 : 80) != ((term->flags & IN132COLUMNS)
				 ? 132 : 80) || j != screen->max_col + 1) {
					XChangeWindow (VWindow(screen),
					 FontWidth(screen) * j + 2*screen->border
					 + screen->scrollbar,
					 FontHeight(screen) * (screen->max_row
					 + 1) + screen->statusheight +
					 Titlebar(screen) + 2 * screen->border);
					XSync(FALSE);	/* synchronize */
					if(QLength() > 0)
						xevents();
				}
				term->flags &= ~IN132COLUMNS;
				term->flags |= screen->save_modes[1] &
				 IN132COLUMNS;
			}
			break;
		case 4:			/* DECSCLM (slow scroll)	*/
			if (screen->save_modes[2] & SMOOTHSCROLL) {
				screen->jumpscroll = 0;
				if (screen->scroll_amt)
					FlushScroll(screen);
			} else
				screen->jumpscroll = 1;
			term->flags &= ~SMOOTHSCROLL;
			term->flags |= screen->save_modes[2] & SMOOTHSCROLL;
			break;
		case 5:			/* DECSCNM			*/
			if((screen->save_modes[3] ^ term->flags) &
			 REVERSE_VIDEO) {
				term->flags &= ~REVERSE_VIDEO;
				term->flags |= screen->save_modes[3] &
				 REVERSE_VIDEO;
				ReverseVideo(term);
			}
			break;
		case 6:			/* DECOM			*/
			term->flags &= ~ORIGIN;
			term->flags |= screen->save_modes[4] & ORIGIN;
			CursorSet(screen, 0, 0, term->flags);
			break;

		case 7:			/* DECAWM			*/
			term->flags &= ~WRAPAROUND;
			term->flags |= screen->save_modes[5] & WRAPAROUND;
			break;
		case 8:			/* DECARM			*/
			if((screen->save_modes[6] ^ term->flags) & AUTOREPEAT) {
				term->flags &= ~REVERSE_VIDEO;
				term->flags |= screen->save_modes[6] &
				 REVERSE_VIDEO;
				if(term->flags & AUTOREPEAT)
					XAutoRepeatOn();
				else
					XAutoRepeatOff();
			}
			break;
		case 9:			/* MIT bogus sequence		*/
			screen->send_mouse_pos = screen->save_modes[7];
			break;
		case 40:		/* 132 column mode		*/
			screen->c132 = screen->save_modes[8];
			break;
		case 41:		/* curses hack			*/
			screen->curses = screen->save_modes[9];
			break;
		case 42:		/* scrollbar			*/
			if(screen->save_modes[10])
				ScrollBarOn(screen, TRUE, FALSE);
			else
				ScrollBarOff(screen);
			break;
		case 43:		/* lines off top		*/
			if(screen->sb)
				SetSaveState(screen->sb,screen->save_modes[11]);
			break;
		case 44:		/* margin bell			*/
			if(!(screen->marginbell = screen->save_modes[12]))
				screen->bellarmed = -1;
			break;
		case 45:		/* reverse wraparound	*/
			term->flags &= ~REVERSEWRAP;
			term->flags |= screen->save_modes[13] & REVERSEWRAP;
			break;
		case 46:		/* logging		*/
			if(screen->save_modes[14])
				StartLog(screen);
			else
				CloseLog(screen);
			break;
		case 47:		/* alternate buffer		*/
			if(screen->save_modes[15])
				ToAlternate(screen);
			else
				FromAlternate(screen);
			break;
		case 48:		/* reverse status line		*/
			if(screen->save_modes[16] != screen->reversestatus) {
				screen->reversestatus = 
				 screen->save_modes[16];
				ScrnRefresh(screen, screen->max_row + 1, 0, 1,
				 screen->max_col + 1);
			}
			break;
		case 49:		/* page mode			*/
			screen->pagemode = screen->save_modes[17];
			screen->pagecnt = screen->save_modes[18];
			break;
		}
	}
}

/*
 * set a bit in a word given a pointer to the word and a mask.
 */
bitset(p, mask)
int	*p;
{
	*p |= mask;
}

/*
 * clear a bit in a word given a pointer to the word and a mask.
 */
bitclr(p, mask)
int	*p;
{
	*p &= ~mask;
}

unparseseq(ap, fd)
register ANSI	*ap;
{
	register int	c;
	register int	i;
	register int	inters;

	c = ap->a_type;
	if (c>=0x80 && c<=0x9F) {
		unparseputc(ESC, fd);
		c -= 0x40;
	}
	unparseputc(c, fd);
	c = ap->a_type;
	if (c==ESC || c==DCS || c==CSI || c==OSC || c==PM || c==APC) {
		if (ap->a_pintro != 0)
			unparseputc(ap->a_pintro, fd);
		for (i=0; i<ap->a_nparam; ++i) {
			if (i != 0)
				unparseputc(';', fd);
			unparseputn(ap->a_param[i], fd);
		}
		inters = ap->a_inters;
		for (i=3; i>=0; --i)
			c = (inters >> (8*i)) & 0xff;
			if (c != 0)
				unparseputc(c, fd);
		unparseputc(ap->a_final, fd);
	}
}

unparseputn(n, fd)
unsigned int	n;
{
	unsigned int	q;

	q = n/10;
	if (q != 0)
		unparseputn(q, fd);
	unparseputc((n%10) + '0', fd);
}

unparseputc(c, fd)
{
	char	buf[2];
	register i = 1;
	extern Terminal term;

	if((buf[0] = c) == '\r' && (term.flags & LINEFEED)) {
		buf[1] = '\n';
		i++;
	}
	if (write(fd, buf, i) != i)
		Panic("unparseputc: error writing character\n", 0);
}

static int alt_pagecnt;
static int alt_pagemode;
static int alt_saving;

ToAlternate(screen)
register Screen *screen;
{
	extern ScrnBuf Allocate();

	if(screen->alternate)
		return;
	if(!screen->altbuf)
		screen->altbuf = Allocate(screen->max_row + 1, screen->max_col
		 + 1);
	if(screen->sb) {
		alt_saving = GetSaveState(screen->sb);
		SetSaveState(screen->sb, FALSE);
	} else
		alt_saving = TRUE;
	if(alt_pagemode = screen->pagemode)
		alt_pagecnt = screen->pagecnt;
	screen->pagemode = FALSE;
	SwitchBufs(screen);
	screen->alternate = TRUE;
}

FromAlternate(screen)
register Screen *screen;
{
	if(!screen->alternate)
		return;
	screen->alternate = FALSE;
	if(screen->sb)
		SetSaveState(screen->sb, alt_saving);
	if(screen->pagemode = alt_pagemode)
		screen->pagecnt = alt_pagecnt;
	SwitchBufs(screen);
}

SwitchBufs(screen)
register Screen *screen;
{
	register int rows, top;
	char *save [2 * MAX_ROWS];

	if(screen->cursor_state)
		HideCursor();
	rows = screen->max_row + 1;
	bcopy((char *)screen->buf, (char *)save, 2 * sizeof(char *) * rows);
	bcopy((char *)screen->altbuf, (char *)screen->buf, 2 * sizeof(char *) *
	 rows);
	bcopy((char *)save, (char *)screen->altbuf, 2 * sizeof(char *) * rows);

	if((top = -screen->topline) <= screen->max_row) {
		if(screen->scroll_amt)
			FlushScroll(screen);
		if(top == 0 && !screen->statusline)
			XClear(VWindow(screen));
		else
			XTileSet(VWindow(screen), screen->border, top *
			 FontHeight(screen) + screen->border + Titlebar(screen),
			 Width(screen), (screen->max_row - top + 1) *
			 FontHeight(screen), screen->bgndtile);
	}
	ScrnRefresh(screen, 0, 0, rows, screen->max_col + 1);
}

VTRun()
{
	register Screen *screen = &term.screen;
	register int i;
	
	if(!VWindow(screen) && !VTInit()) {
		if(TWindow(screen)) {
			screen->TekEmu = TRUE;
			return;
		}
		Exit(ERROR_VINIT);
	}
	screen->cursor_state = OFF;
	screen->cursor_set = ON;
	if(screen->icon_show) {
		if(screen->icon_show < 0) {
			screen->icon_show = TRUE;
			screen->mappedVwin = &screen->iconVwin;
			XMapWindow(screen->iconVwin.window);
		}
	} else if(!screen->show) {
		screen->show = TRUE;
		screen->mappedVwin = &screen->fullVwin;
		XMapWindow(VWindow(screen));
	} else
		XRaiseWindow(VWindow(screen));
	if(screen->select)
		VTSelect();
	if (L_flag > 0) {
		XWarpMouse (VWindow(screen),
			    FullWidth(screen) >> 1, FullHeight(screen) >>1);
		L_flag = -1;
	}
	bcnt = 0;
	bptr = buffer;
	while(Tpushb > Tpushback) {
		*bptr++ = *--Tpushb;
		bcnt++;
	}
	bcnt += (i = Tbcnt);
	for( ; i > 0 ; i--)
		*bptr++ = *Tbptr++;
	bptr = buffer;
	if(!setjmp(VTend))
		VTparse();
	HideCursor();
	screen->cursor_set = OFF;
	VTUnselect();
}

VTInit()
{
	int width, height;
	FontInfo *fInfo, *ifInfo;
	register Screen *screen = &term.screen;
	register Vertex *vp;
	register int i, j;
	char *def = "=80x24+1+1";
	char iconname[128];
	static short failed;
	Color cdef;
	OpaqueFrame twindow;
	WindowInfo wininfo;
	int x, y;
	Window win;
	extern char *malloc();

	if(failed)
		return(FALSE);
	
	screen->mappedVwin = &screen->fullVwin;

	TabReset (term.tabs);

	screen->fnt_norm = screen->fnt_bold = screen->fnt_icon = NULL;
	   
	if ((fInfo = XOpenFont(f_n)) == NULL) {
		fprintf(stderr, "%s: Could not open font %s!\n",
			xterm_name, f_n);
		failed = TRUE;
		return(FALSE);
	}
	screen->fnt_norm = fInfo->id;
	if (!f_b || !(screen->fnt_bold = XGetFont(f_b))) {
		screen->fnt_bold = screen->fnt_norm;
		screen->enbolden = TRUE;
	}
	screen->fullVwin.f_width = fInfo->width;
	screen->fullVwin.f_height = fInfo->height;
	
	if (screen->active_icon) {
	    if (!screen->fnt_icon) {
		if ((ifInfo = XOpenFont(f_i)) == NULL) {
		    fprintf( stderr, "%s: Could not open font %s!\n",
			     xterm_name, f_i);
		    failed = TRUE;
		    return( FALSE );
		}
		screen->fnt_icon = ifInfo->id;
	    } else
	    	XQueryFont( screen->fnt_icon, &ifInfo );

	    screen->iconVwin.f_width = ifInfo->width;
	    screen->iconVwin.f_height = ifInfo->height;
	}

	/* X11 arrow cursor feature */
	if (curs_shape && strcmp(curs_shape, "arrow") == 0)
		screen->curs = make_arrow(screen->mousecolor,
		    screen->background, GXcopy);
	else
		screen->curs = make_xterm(screen->mousecolor,
		    screen->background, GXcopy);

	twindow.bdrwidth = screen->borderwidth;
	if(grayborder)
		twindow.border = screen->graybordertile;
	else
		twindow.border = screen->bordertile;
	twindow.background = screen->bgndtile;
	if((screen->minrows = (MINSCROLLBARHEIGHT - 2 * screen->border +
	 fInfo->height - 1) / fInfo->height) < 0)
		screen->minrows = 0;

	i = 2 * screen->border + screen->scrollbar;
	j = 2 * screen->border + Titlebar(screen);
	if(screen->statusline)
		j += (screen->statusheight = fInfo->height + 2);

	if((screen->fullVwin.window = XCreateTerm ("Terminal Emulator", xterm_name,
	 geo_metry, def, &twindow, 12, screen->minrows, i, j, &width, &height,
	 fInfo, fInfo->width, fInfo->height)) == NULL) {
		fprintf(stderr, "%s: Can't create VT window\n");
		XCloseFont(fInfo);
		return(FALSE);
	}
	XSelectInput(VWindow(screen), WINDOWEVENTS);
	/*
	 * XCreateTerm flushes all events, which might include an EnterWindow
	 * or LeaveWindow.  So if the cursor is not where it is supposed to
	 * be, we set select to the appropriate thing.
	 */
	if(TWindow(screen) && XQueryMouse(RootWindow, &x, &y, &win)) {
		if(screen->timer) {
			Timer(0L);
			screen->timer = 0;
		}
		if(win == TWindow(screen))
			screen->select |= INWINDOW;
		else
			screen->select &= ~INWINDOW;
	}

	screen->fullVwin.fullwidth = twindow.width;
	screen->fullVwin.fullheight = twindow.height;
	screen->fullVwin.width = twindow.width - i;
	screen->fullVwin.height = twindow.height - j;

	/* Reset variables used by ANSI emulation. */

	screen->gsets[0] = 'B';			/* ASCII_G		*/
	screen->gsets[1] = 'B';
	screen->gsets[2] = 'B';			/* DEC supplemental.	*/
	screen->gsets[3] = 'B';
	screen->curgl = 0;			/* G0 => GL.		*/
	screen->curgr = 2;			/* G2 => GR.		*/
	screen->curss = 0;			/* No single shift.	*/

	if(screen->iconTwin.window) {
		XQueryWindow(screen->iconTwin.window, &wininfo);
		x = wininfo.x;
		y = wininfo.y;
	} else {
		x = twindow.x + (twindow.width - screen->iconVwin.width) / 2;
		y = twindow.y + (twindow.height - screen->iconVwin.height) / 2;
		IconGeometry(screen, &x, &y);
	}
	screen->iconVwin.window =
		XCreateWindow( RootWindow, x, y, 1, 1, screen->borderwidth,
			       screen->bordertile, screen->bgndtile );

	XSetIconWindow( screen->fullVwin.window, screen->iconVwin.window );

	XDefineCursor( screen->iconVwin.window, screen->arrow );
	XSelectInput( screen->iconVwin.window,
		      screen->active_icon && (term.flags & ICONINPUT)
			? ICONWINDOWEVENTS | ICONINPUTEVENTS
		        : ICONWINDOWEVENTS );

	XDefineCursor( VWindow(screen), screen->curs );
	XStoreName (VWindow(screen), screen->winname);
	strcpy(iconname, screen->winname);
	strcat(iconname, " (icon)");
	XStoreName (screen->iconVwin.window, iconname);
	XSetResizeHint (VWindow(screen), 2 * screen->border + screen->scrollbar,
	 2 * screen->border + Titlebar(screen) + screen->statusheight,
	 fInfo->width, fInfo->height);

	screen->cur_col = screen->cur_row = 0;
	screen->max_col = Width(screen)  / fInfo->width - 1;
	screen->top_marg = 0;
	screen->bot_marg = screen->max_row = Height(screen) / fInfo->height - 1;

	screen->sc.row = screen->sc.col = screen->sc.flags = NULL;

	SetIconSize( screen );		/* requires max_col, max_row */

	/* allocate memory for screen buffer (including one for status line */
	screen->buf = screen->allbuf = (ScrnBuf) Allocate (screen->max_row + 2,
					  screen->max_col +1);

	screen->do_wrap = NULL;
	screen->scrolls = screen->incopy = 0;
	free((char *)fInfo);
	vp = &VTbox[1];
	(vp++)->x = FontWidth(screen) - 1;
	(vp++)->y = FontHeight(screen) - 1;
	(vp++)->x = -(FontWidth(screen) - 1);
	vp->y = -(FontHeight(screen) - 1);
	screen->box = VTbox;
	status_box[0].x = screen->border - 1;
	screen->savelines = save_lines;
	if(screen->scrollbar) {
		screen->scrollbar = 0;
		ScrollBarOn(screen, TRUE, TRUE);
		screen->sb->action = NONE;
	}
	screen->nmarginbell = n_marginbell;
	if(Titlebar(screen))
		VTTitleShow(TRUE);
	CursorSave(&term, &screen->sc);
	return(TRUE);
}

#ifdef CHANGEFONT
VTChangeFont(screen, newfname)
	register Screen *screen;
	char *newfname;
{
	register Vertex *vp;
	FontInfo *fInfo;
	register width, height, border, extra;

	/* Make sure we can get the new font */
	if ((fInfo = XOpenFont(newfname)) == NULL)
		return(FALSE);

	/* Disallow variable fonts with no average sizes for now */
	if (fInfo->width == 0 || fInfo->height == 0) {
		XCloseFont(fInfo);
		return(FALSE);
	}

	/* Destroy old font */
	XFreeFont(screen->fnt_norm);

	screen->fnt_norm = fInfo->id;

	if (screen->enbolden)
		screen->fnt_bold = screen->fnt_norm;

	XSetResizeHint (VWindow(screen), 2 * screen->border + screen->scrollbar,
	 2 * screen->border + Titlebar(screen) + screen->statusheight,
	 fInfo->width, fInfo->height);

	screen->fullVwin.f_width = fInfo->width;
	screen->fullVwin.f_height = fInfo->height;

	free((char *)fInfo);

	width = FontWidth(screen) * (screen->max_col + 1);
	height = FontHeight(screen) * (screen->max_row + 1);
	border = 2 * screen->border;
	extra = Titlebar(screen) + screen->statusheight;

	screen->fullVwin.fullwidth = width;
	screen->fullVwin.fullheight = height;
	screen->fullVwin.height = height - extra;
	screen->fullVwin.width = width;

	vp = &VTbox[1];
	(vp++)->x = FontWidth(screen) - 1;
	(vp++)->y = FontHeight(screen) - 1;
	(vp++)->x = -(FontWidth(screen) - 1);
	vp->y = -(FontHeight(screen) - 1);

	if(screen->sb)
		ResizeScrollBar(screen->sb, width - SCROLLBARWIDTH,
		    Titlebar(screen) - 1, height - Titlebar(screen),
		    screen->max_row + 1);
	if(screen->title.tbar)
		VTTitleResize(width);

	return(TRUE);
}
#endif

VTExpose(rep)
register XExposeWindowEvent *rep;
{
	register Screen *screen = &term.screen;

	if (rep && ScreenResize (screen, rep->width, rep->height, &term.flags)
	 == -1)
		return;
	XClear (VWindow(screen));
	ScrnRefresh(screen, 0, 0, screen->max_row + 1 + screen->statusline,
	 screen->max_col + 1);
}

/*
 * Shows cursor at new cursor position in screen.
 */
ShowCursor()
{
	register Screen *screen = &term.screen;
	register int fg;
	register int bg;
	register int x, y, flags;
	register Font fnt;
	char c;

	if (screen->icon_show && !screen->active_icon) return;

	if(!screen->instatus && screen->cur_row - screen->topline >
	 screen->max_row)
		return;
	c = screen->buf[y = 2 * (screen->cursor_row = screen->cur_row)]
	 [x = screen->cursor_col = screen->cur_col];
	flags = screen->buf[y + 1][x];
	if (c == 0)
		c = ' ';
	if(screen->instatus)
		flags ^= INVERSE;
	if(screen->select) {
		if(flags & INVERSE) {
			if(screen->cursorcolor != screen->foreground) {
				fg = screen->foreground;
				bg = screen->cursorcolor;
			} else {
				fg = screen->foreground;
				bg = screen->background;
			}
		} else {
			fg = screen->background;
			bg = screen->cursorcolor;
		}
	} else {
		if(flags & INVERSE) {
			fg = screen->background;
			bg = screen->foreground;
		} else {
			fg = screen->foreground;
			bg = screen->background;
		}
	}
	fnt = ActiveIcon(screen) ? screen->fnt_icon
	      : (flags & BOLD) ? screen->fnt_bold : screen->fnt_norm;
	XText(VWindow(screen), x = CursorX (screen, screen->cur_col),
	 y = CursorY(screen, screen->cur_row), &c, 1, fnt, fg, bg);
	if((flags & BOLD) && screen->enbolden)
		XTextMask(VWindow(screen), x + 1, y, &c, 1, fnt, fg);
	if(flags & UNDERLINE) {
		bg = y + FontHeight(screen) - 2;
		XLine(VWindow(screen), x, bg, x + FontWidth(screen), bg,
		 1, 1, fg, GXcopy, AllPlanes);
	}
	if(!screen->select && !ActiveIcon(screen)) {
		screen->box->x = x;
		screen->box->y = y;
		XDraw(VWindow(screen), screen->box, NBOX, 1, 1, fg, GXcopy,
		 AllPlanes);
	}
	screen->cursor_state = ON;
}

/*
 * hide cursor at previous cursor position in screen.
 */
HideCursor()
{
	register Screen *screen = &term.screen;
	register int fg;
	register int bg;
	register int x, y, flags, instatus;
	register Font fnt;
	char c;

	if (screen->icon_show && !screen->active_icon) return;

	if(!(instatus = screen->cursor_row > screen->max_row) &&
	 screen->cursor_row - screen->topline > screen->max_row)
		return;
	c = screen->buf[y = 2 * screen->cursor_row][x = screen->cursor_col];
	flags = screen->buf[y + 1][x];
	if(instatus)
		flags ^= INVERSE;
	if(flags & INVERSE) {
		fg = screen->background;
		bg = screen->foreground;
	} else {
		fg = screen->foreground;
		bg = screen->background;
	}
	if (c == 0)
		c = ' ';
	y = (instatus ? (screen->cursor_row * FontHeight(screen) + 1) :
	 ((screen->cursor_row - screen->topline) * FontHeight(screen))) +
	 Titlebar(screen) + screen->border;
	fnt = ActiveIcon(screen) ? screen->fnt_icon
	      : (flags & BOLD) ? screen->fnt_bold : screen->fnt_norm;
	XText(VWindow(screen), x = CursorX (screen, screen->cursor_col),
	 y, &c, 1, fnt, fg, bg);
	if((flags & BOLD) && screen->enbolden)
		XTextMask(VWindow(screen), x + 1, y, &c, 1, fnt, fg);
	if(flags & UNDERLINE) {
		y += FontHeight(screen) - 2;
		XLine(VWindow(screen), x, y, x + FontWidth(screen), y,
		 1, 1, fg, GXcopy, AllPlanes);
	}
	screen->cursor_state = OFF;
}

VTSelect()
{
	register Screen *screen = &term.screen;

	if(grayborder && screen->borderwidth > 0)
		XChangeBorder(VWindow(screen), screen->bordertile);
	if(Titlebar(screen))
		VTTitleHilite();
}

VTUnselect()
{
	register Screen *screen = &term.screen;

	if(grayborder && screen->borderwidth > 0)
		XChangeBorder(VWindow(screen), screen->graybordertile);
	if(Titlebar(screen))
		VTTitleUnhilite();
}

VTReset(full)
int full;
{
	register Screen *screen = &term.screen;

	/* reset scrolling region */
	screen->top_marg = 0;
	screen->bot_marg = screen->max_row;
	term.flags &= ~ORIGIN;
	if(full) {
		TabReset (term.tabs);
		term.keyboard.flags = NULL;
		screen->gsets[0] = 'B';
		screen->gsets[1] = 'B';
		screen->gsets[2] = 'B';
		screen->gsets[3] = 'B';
		screen->curgl = 0;
		screen->curgr = 2;
		screen->curss = 0;
		ClearScreen(screen);
		screen->cursor_state = OFF;
		if(!(term.flags & AUTOREPEAT))
			XAutoRepeatOn();
		if (term.flags & REVERSE_VIDEO)
			ReverseVideo(&term);

		term.flags = term.initflags;
		if(screen->c132 && (term.flags & IN132COLUMNS)) {
			XChangeWindow (VWindow(screen), 80 * FontWidth(screen) +
			 2 * screen->border + screen->scrollbar,
			 FontHeight(screen) * (screen->max_row + 1) +
			 screen->statusheight + Titlebar(screen) +
			 2 * screen->border);
			XSync(FALSE);	/* synchronize */
			if(QLength() > 0)
				xevents();
		}
		CursorSet(screen, 0, 0, term.flags);
	}
	longjmp(vtjmpbuf, 1);	/* force ground state in parser */
}

ToStatus(col)
int col;
{
	register Screen *screen = &term.screen;

	if(col > screen->max_col)
		col = screen->max_col;
	if(!screen->instatus) {
		if(!screen->statusline)
			ShowStatus();
		CursorSave(&term, &screen->statussc);
		screen->instatus = TRUE;
		screen->cur_row = screen->max_row + 1;
	}
	screen->cur_col = col;
}

FromStatus()
{
	register Screen *screen = &term.screen;

	if(!screen->instatus)
		return;
	screen->instatus = FALSE;
	CursorRestore(&term, &screen->statussc);
}

ShowStatus()
{
	register Screen *screen = &term.screen;
	register int border = 2 * screen->border;

	if(screen->statusline)
		return;
	screen->statusline = 1;
	screen->statusheight = FontHeight(screen) + 2;
	XSetResizeHint(VWindow(screen), border + screen->scrollbar, border +
	 Titlebar(screen) + screen->statusheight, FontWidth(screen),
	 FontHeight(screen));
	XChangeWindow (VWindow(screen), FontWidth(screen) * (screen->max_col + 1)
	 + border + screen->scrollbar, FontHeight(screen) *
	 (screen->max_row + 1) + screen->statusheight + Titlebar(screen) +
	 border);
}

HideStatus()
{
	register Screen *screen = &term.screen;
	register int border = 2 * screen->border;
	register int i, j;

	if(!screen->statusline)
		return;
	if(screen->instatus)
		FromStatus();
	screen->statusline = 0;
	screen->statusheight = 0;
	bzero(screen->buf[i = 2 * (screen->max_row + 1)], j = screen->max_col +
	 1);
	bzero(screen->buf[i + 1], j);
	XSetResizeHint(VWindow(screen), border + screen->scrollbar, border +
	 Titlebar(screen), FontWidth(screen), FontHeight(screen));
	XChangeWindow (VWindow(screen), FontWidth(screen) * j + border +
	 screen->scrollbar, FontHeight(screen) * (screen->max_row + 1) +
	 border + Titlebar(screen));
}

EraseStatus()
{
	register Screen *screen = &term.screen;
	register int i, j, pix;

	if(!screen->statusline)
		return;
	bzero(screen->buf[i = 2 * (screen->max_row + 1)], j = screen->max_col +
	 1);
	bzero(screen->buf[i + 1], j);
	XPixSet(VWindow(screen), screen->border - 1, (screen->max_row + 1) *
	 FontHeight(screen) + screen->border + Titlebar(screen), j *
	 FontWidth(screen) + 2, screen->statusheight, screen->reversestatus ?
	 screen->foreground : screen->background);
	if(!screen->reversestatus)
		StatusBox(screen);
}

StatusBox(screen)
register Screen *screen;
{
	status_box[0].y = (screen->max_row + 1) * FontHeight(screen) +
	 screen->border + Titlebar(screen);
	status_box[3].x = -(status_box[1].x = (screen->max_col + 1) *
	 FontWidth(screen) + 1);
	status_box[4].y = -(status_box[2].y = FontHeight(screen) + 1);
	XDraw(VWindow(screen), status_box, NBOX, 1, 1, screen->foreground,
	 GXcopy, AllPlanes);
}

VTTitleShow(init)
int init;
{
	register Screen *screen = &term.screen;
	register int border = 2 * screen->border;

	if(!screen->title.tbar)
		VTTitleInit();
	if(!init) {
		XSetResizeHint(VWindow(screen), border + screen->scrollbar,
		 border + Titlebar(screen) + screen->statusheight,
		 FontWidth(screen), FontHeight(screen));
		XChangeWindow (VWindow(screen), FontWidth(screen) *
		 (screen->max_col + 1) + border + screen->scrollbar,
		 FontHeight(screen) * (screen->max_row + 1) + screen->statusheight
		 + Titlebar(screen) + border);
	}
	if(screen->select && !screen->TekEmu)
		VTTitleHilite();
	else
		VTTitleUnhilite();
	XMapWindow(screen->title.tbar);
}

VTTitleHide()
{
	register Screen *screen = &term.screen;
	register int border = 2 * screen->border;

	XUnmapWindow(screen->title.tbar);
	XSetResizeHint(VWindow(screen), border + screen->scrollbar, border +
	 screen->statusheight, FontWidth(screen), FontHeight(screen));
	XChangeWindow (VWindow(screen), FontWidth(screen) * (screen->max_col + 1)
	 + border + screen->scrollbar, FontHeight(screen) *
	 (screen->max_row + 1) + screen->statusheight + border);
}

VTTitleHilite()
{
	register Screen *screen = &term.screen;

	if(screen->title.hilited)
		return;
	XMapWindow(screen->title.left);
	XMapWindow(screen->title.right);
	screen->title.hilited = TRUE;
}

VTTitleUnhilite()
{
	register Screen *screen = &term.screen;

	if(!screen->title.hilited)
		return;
	XUnmapWindow(screen->title.left);
	XUnmapWindow(screen->title.right);
	screen->title.hilited = FALSE;
}

VTTitleResize(width)
register int width;
{
	register Screen *screen = &term.screen;
	register int i, j;

	if((screen->title.width = i = screen->title.fullwidth) >
	 (j = width - 2 * (MINHILITE + screen->title_n_size + 1)))
		screen->title.width = (i = j) + screen->title_n_size;
	j = width - i - 2 * (screen->title_n_size + 1);
	i = j / 2;
	j -= i;
	screen->title.x = i + 1 + screen->title_n_size;
	XChangeWindow(screen->title.tbar, width, screen->titleheight - 1);
	XChangeWindow(screen->title.left, i, screen->titlefont->height);
	XConfigureWindow(screen->title.right, width - j - 1, TITLEPAD, j,
	 screen->titlefont->height);
}

VTTitleExpose(rep)
register XExposeWindowEvent *rep;
{
	register Screen *screen = &term.screen;

	if(rep && (rep->x > (screen->title.x + screen->title.width) ||
	 (rep->x + rep->width) < screen->title.x ||
	 rep->y > (screen->title.y + screen->titlefont->height) ||
	 (rep->y + rep->height) < screen->title.y))
		return;
	XText(screen->title.tbar, screen->title.x, screen->title.y,
	 screen->winname, screen->winnamelen, screen->titlefont->id,
	 screen->foreground, screen->background);
}

VTTitleInit()
{
	register Screen *screen = &term.screen;
	register int w, i, j;
	OpaqueFrame hilite[2];
	extern Pixmap make_hilite();

	if((screen->title.tbar = XCreateWindow(VWindow(screen), -1, -1,
	 w = FullWidth(screen), screen->titleheight - 1, 1, screen->bordertile,
	 screen->bgndtile)) == NULL)
		Error(ERROR_CRTITLE);
	XSelectInput(screen->title.tbar, ButtonPressed | ButtonReleased |
	 ExposeWindow | EnterWindow | LeaveWindow | UnmapWindow);
	XDefineCursor(screen->title.tbar, screen->arrow);
	if(!screen->hilitetile && (screen->hilitetile =
	 make_hilite(screen->foreground, screen->background)) == NULL)
		Error(ERROR_HILITE);
	screen->title.fullwidth = XQueryWidth(screen->winname,
	 screen->titlefont->id);
	if((screen->title.width = i = screen->title.fullwidth) >
	 (j = w - 2 * (MINHILITE + screen->title_n_size + 1)))
		screen->title.width = (i = j) + screen->title_n_size;
	j = w - i - 2 * (screen->title_n_size + 1);
	i = j / 2;
	j -= i;
	screen->title.x = i + 1 + screen->title_n_size;
	screen->title.y = TITLEPAD;
	hilite[0].x = 1;
	hilite[1].x = w - j - 1;
	hilite[0].y = hilite[1].y = TITLEPAD;
	hilite[0].width = i;
	hilite[1].width = j;
	hilite[0].height = hilite[1].height = screen->titlefont->height;
	hilite[0].bdrwidth = hilite[1].bdrwidth = 0;
	hilite[0].border = hilite[1].border = NULL;
	hilite[0].background = hilite[1].background = screen->hilitetile;
	if(XCreateWindows(screen->title.tbar, hilite, 2) != 2)
		Error(ERROR_CRLFRG);
	screen->title.left = hilite[0].self;
	screen->title.right = hilite[1].self;
}

#ifdef MODEMENU
#define	MMENU_SCROLL	0
#define	MMENU_VIDEO	(MMENU_SCROLL+1)
#define	MMENU_WRAP	(MMENU_VIDEO+1)
#define	MMENU_REVERSEWRAP (MMENU_WRAP+1)
#define	MMENU_NLM	(MMENU_REVERSEWRAP+1)
#define	MMENU_CURSOR	(MMENU_NLM+1)
#define	MMENU_PAD	(MMENU_CURSOR+1)
#define	MMENU_REPEAT	(MMENU_PAD+1)
#define	MMENU_SCROLLBAR	(MMENU_REPEAT+1)
#define	MMENU_PAGEMODE	(MMENU_SCROLLBAR+1)
#define	MMENU_STATUS	(MMENU_PAGEMODE+1)
#define	MMENU_REVSTATUS	(MMENU_STATUS+1)
#define	MMENU_C132	(MMENU_REVSTATUS+1)
#define	MMENU_CURSES	(MMENU_C132+1)
#define	MMENU_MARGBELL	(MMENU_CURSES+1)
#define	MMENU_TEKWIN	(MMENU_MARGBELL+1)
#define	MMENU_ALTERN	(MMENU_TEKWIN+1)
#define	MMENU_LINE	(MMENU_ALTERN+1)
#define	MMENU_RESET	(MMENU_LINE+1)
#define	MMENU_FULLRESET	(MMENU_RESET+1)
#define	MMENU_TEKMODE	(MMENU_FULLRESET+1)
#define	MMENU_HIDEVT	(MMENU_TEKMODE+1)

static char *vtext[] = {
	"Jump Scroll",
	"Reverse Video",
	"Auto Wraparound",
	"Reverse Wraparound",
	"Auto Linefeed",
	"Application Cursors",
	"Application Pad",
	"Auto Repeat",
	"Scrollbar",
	"Page Scroll",
	"Status Line",
	"Reverse Status Line",
	"80 <-> 132 Columns",
	"Curses Emulation",
	"Margin Bell",
	"Tek Window Showing",
	"Alternate Screen",
	"-",
	"Soft Reset",
	"Full Reset",
	"Select Tek Mode",
	"Hide VT Window",
	0,
};


static int menutermflags;
static int menukbdflags;
static int t132;
static int taltern;
static int tcurses;
static int tmarginbell;
static int tpagemode;
static int trevstatus;
static int tscrollbar;
static int tshow;
static int tstatusline;

Menu *setupmenu(menu)
register Menu **menu;
{
	register Screen *screen = &term.screen;
	register char **cp;
	register int flags = term.flags;
	register int kflags = term.keyboard.flags;

	if (*menu == NULL) {
		if ((*menu = NewMenu("Modes", re_verse)) == NULL)
			return(NULL);
		for(cp = vtext ; *cp ; cp++)
			AddMenuItem(*menu, *cp);
		if(!(flags & SMOOTHSCROLL))
			CheckItem(*menu, MMENU_SCROLL);
		if(flags & REVERSE_VIDEO)
			CheckItem(*menu, MMENU_VIDEO);
		if(flags & WRAPAROUND)
			CheckItem(*menu, MMENU_WRAP);
		if(flags & REVERSEWRAP)
			CheckItem(*menu, MMENU_REVERSEWRAP);
		if(flags & LINEFEED)
			CheckItem(*menu, MMENU_NLM);
		if(kflags & CURSOR_APL)
			CheckItem(*menu, MMENU_CURSOR);
		if(kflags & KYPD_APL)
			CheckItem(*menu, MMENU_PAD);
		if(flags & AUTOREPEAT)
			CheckItem(*menu, MMENU_REPEAT);
		if(tscrollbar = (screen->scrollbar > 0))
			CheckItem(*menu, MMENU_SCROLLBAR);
		if(tpagemode = screen->pagemode)
			CheckItem(*menu, MMENU_PAGEMODE);
		if(tstatusline = screen->statusline)
			CheckItem(*menu, MMENU_STATUS);
		if(trevstatus = screen->reversestatus)
			CheckItem(*menu, MMENU_REVSTATUS);
		if(t132 = screen->c132)
			CheckItem(*menu, MMENU_C132);
		if(tcurses = screen->curses)
			CheckItem(*menu, MMENU_CURSES);
		if(tmarginbell = screen->marginbell)
			CheckItem(*menu, MMENU_MARGBELL);
		if(tshow = screen->Tshow)
			CheckItem(*menu, MMENU_TEKWIN);
		else
			DisableItem(*menu, MMENU_HIDEVT);
		DisableItem(*menu, MMENU_ALTERN);
		if(taltern = screen->alternate) {
			CheckItem(*menu, MMENU_ALTERN);
			DisableItem(*menu, MMENU_PAGEMODE);
		}
		DisableItem(*menu, MMENU_LINE);
		if(screen->inhibit & I_TEK) {
			DisableItem(*menu, MMENU_TEKWIN);
			DisableItem(*menu, MMENU_TEKMODE);
		}
		menutermflags = flags;
		menukbdflags = kflags;
		return(*menu);
	}
	if ((menutermflags ^= flags) & SMOOTHSCROLL)
		SetItemCheck(*menu, MMENU_SCROLL, !(flags & SMOOTHSCROLL));
	if (menutermflags & REVERSE_VIDEO)
		SetItemCheck(*menu, MMENU_VIDEO, flags & REVERSE_VIDEO);
	if (menutermflags & WRAPAROUND)
		SetItemCheck(*menu, MMENU_WRAP, flags & WRAPAROUND);
	if (menutermflags & REVERSEWRAP)
		SetItemCheck(*menu, MMENU_REVERSEWRAP, flags & REVERSEWRAP);
	if (menutermflags & LINEFEED)
		SetItemCheck(*menu, MMENU_NLM, flags & LINEFEED);
	if ((menukbdflags ^= kflags) & CURSOR_APL)
		SetItemCheck(*menu, MMENU_CURSOR, kflags & CURSOR_APL);
	if (menukbdflags & KYPD_APL)
		SetItemCheck(*menu, MMENU_PAD, NULL, kflags & KYPD_APL);
	if(tscrollbar != (screen->scrollbar > 0))
		SetItemCheck(*menu, MMENU_SCROLLBAR, (tscrollbar =
		 (screen->scrollbar > 0)));
	if(tpagemode != screen->pagemode)
		SetItemCheck(*menu, MMENU_PAGEMODE, (tpagemode =
		 screen->pagemode));
	if(tstatusline != screen->statusline)
		SetItemCheck(*menu, MMENU_STATUS, (tstatusline =
		 screen->statusline));
	if(trevstatus != screen->reversestatus)
		SetItemCheck(*menu, MMENU_REVSTATUS, (trevstatus =
		 screen->reversestatus));
	if(t132 != screen->c132)
		SetItemCheck(*menu, MMENU_C132, (t132 = screen->c132));
	if(tcurses != screen->curses)
		SetItemCheck(*menu, MMENU_CURSES, (tcurses = screen->curses));
	if(tmarginbell != screen->marginbell)
		SetItemCheck(*menu, MMENU_MARGBELL, (tmarginbell =
		screen->marginbell));
	if(tshow != screen->Tshow) {
		SetItemCheck(*menu, MMENU_TEKWIN, (tshow = screen->Tshow));
		SetItemDisable(*menu, MMENU_HIDEVT, !tshow);
	}
	if(taltern != screen->alternate) {
		SetItemCheck(*menu, MMENU_ALTERN, (taltern =
		 screen->alternate));
		SetItemDisable(*menu, MMENU_PAGEMODE, taltern);
	}
	menutermflags = flags;
	menukbdflags = kflags;
	return(*menu);
}

domenufunc(item)
int item;
{
	register Screen *screen = &term.screen;

	switch (item) {
	case MMENU_SCROLL:
		term.flags ^= SMOOTHSCROLL;
		if (term.flags & SMOOTHSCROLL) {
			screen->jumpscroll = FALSE;
			if (screen->scroll_amt)
				FlushScroll(screen);
		} else
			screen->jumpscroll = TRUE;
		break;

	case MMENU_VIDEO:
		term.flags ^= REVERSE_VIDEO;
		ReverseVideo(&term);
		break;

	case MMENU_WRAP:
		term.flags ^= WRAPAROUND;
		break;

	case MMENU_REVERSEWRAP:
		term.flags ^= REVERSEWRAP;
		break;

	case MMENU_NLM:
		term.flags ^= LINEFEED;
		break;

	case MMENU_CURSOR:
		term.keyboard.flags ^= CURSOR_APL;
		break;

	case MMENU_PAD:
		term.keyboard.flags ^= KYPD_APL;
		break;

	case MMENU_REPEAT:
		term.flags ^= AUTOREPEAT;
		if (term.flags & AUTOREPEAT)
			XAutoRepeatOn();
		else
			XAutoRepeatOff();
		break;

	case MMENU_SCROLLBAR:
		if(screen->scrollbar)
			ScrollBarOff(screen);
		else
			ScrollBarOn(screen, TRUE, FALSE);
		break;

	case MMENU_PAGEMODE:
		if(screen->pagemode = !screen->pagemode)
			screen->pagecnt = 0;
		break;

	case MMENU_STATUS:
		if(screen->statusline)
			HideStatus();
		else
			ShowStatus();
		break;

	case MMENU_REVSTATUS:
		screen->reversestatus = !screen->reversestatus;
		ScrnRefresh(screen, screen->max_row + 1, 0, 1, screen->max_col
		 + 1);
		break;

	case MMENU_C132:
		screen->c132 = !screen->c132;
		break;

	case MMENU_MARGBELL:
		if(!(screen->marginbell = !screen->marginbell))
			screen->bellarmed = -1;
		break;

	case MMENU_CURSES:
		screen->curses = !screen->curses;
		break;

	case MMENU_FULLRESET:
		VTReset(TRUE);
		break;

	case MMENU_RESET:
		VTReset(FALSE);
		break;

	case MMENU_HIDEVT:
		screen->show = FALSE;
		XUnmapWindow(VWindow(screen));
		reselectwindow(screen);
		SyncUnmap(VWindow(screen), WINDOWEVENTS);
			/* drop through */
	case MMENU_TEKMODE:
		if(!screen->TekEmu) {
			if(screen->logging) {
				FlushLog(screen);
				screen->logstart = Tbuffer;
			}
			screen->TekEmu = TRUE;
			if(screen->pagemode) {
				Scroll(screen, screen->pagecnt);
				screen->pagecnt = 0;
				ioctl(screen->respond, TIOCSTART, NULL);
			}
			longjmp(VTend, 1);
		} else
			XRaiseWindow(TWindow(screen));
		break;

	case MMENU_TEKWIN:
		if(screen->Tshow = !screen->Tshow) {
			if(TWindow(screen) || TekInit()) {
				XMapWindow(TWindow(screen));
				screen->Tshow = TRUE;
			}
		} else {
			screen->Tshow = FALSE;
			XUnmapWindow(TWindow(screen));
			SyncUnmap(TWindow(screen), TWINDOWEVENTS);
			if(screen->TekEmu) {
				if(screen->logging) {
					FlushLog(screen);
					screen->logstart = buffer;
				}
				longjmp(Tekend, 1);
			}
		}
		reselectwindow(screen);
		break;
	}
}
#endif MODEMENU
