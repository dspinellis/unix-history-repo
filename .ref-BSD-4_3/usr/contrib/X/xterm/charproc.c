#include <X/mit-copyright.h>

/* Copyright (c) 1985 Massachusetts Institute of Technology		*/
/* Copyright (c) 1985	Digital Equipment Corporation			*/
 
/* charproc.c */
 
#include <X/Xlib.h>
#include "ptyx.h"
#include <stdio.h>
#include <sgtty.h>
#include "chartable.h"
#include "esctable.h"
 
#ifndef lint
static char *rcsid_charproc_c = "$Header: charproc.c,v 10.10 86/02/01 16:05:52 tony Rel $";
#endif lint

/* CAUTION: getb "knows" that the variable "c" is receiving the result	*/

#define	getb(buf) (--buf_cnt >= 0 ? *buf_ptr++:				\
	((c=fill(buf)), buf_cnt=buf->cnt, buf_ptr=buf->ptr, c))

#define	peekb(buf)  (buf_cnt >  0 ? *buf_ptr  : -1)

#define ANSIflush()   { arg = flushbuf(screen, term->flags, text_ptr);	\
			buffermode = 0;					\
			text_ptr = text_buf;				\
			text_cnt = TEXT_BUF_SIZE; }

#define TEKflush()	{	arg = TekFlushBuf(term, text_ptr);	\
				buffermode = 0;				\
				text_ptr = text_buf;			\
				text_cnt = TEXT_BUF_SIZE;		\
			}

#define	TEXT_BUF_SIZE	256

extern TekLink *tb_end_link;
extern int tb_end;

unsigned char	text_buf[TEXT_BUF_SIZE];
 
unsigned	nri	= 0;
unsigned	nlf	= 0;
unsigned	ctotal	= 0;
unsigned	ntotal	= 0;
 
/*
 * in normal mode, buffer up as large a string of printing characters
 * as possible before flushing the buffer.  buffer up LF's and RI's
 * so these can be processed as a block.
 */
ANSInormal(term)
Terminal	*term;
{
	register int		c;
	register unsigned char	*text_ptr;
	register int		text_cnt;
	register unsigned char	*buf_ptr;
	register int		buf_cnt;
	register unsigned	buffermode;
	register Screen *screen = &term->screen;
	register Buffer *trmbuf = &term->buf;
	int			arg;
 
	buffermode = 0;
	arg = 0;
	c = 0;
	text_ptr = text_buf;
	text_cnt = TEXT_BUF_SIZE;
	buf_cnt = trmbuf->cnt;
	buf_ptr = trmbuf->ptr;
 
	if (screen->TekGMode || screen->TekAMode)
		return (TekMode(term));

	for (;;) {
		/*
		 * if special condition has occurred, c will already
		 * be set to EOF.  otherwise, pick up a new char.
		 */
		if (c >= 0)
			c = getb(trmbuf);

		if (ctable[c] == CPRINTING) {
			buffermode = BUFFER_MODE;
			if (--text_cnt >= 0)
				*text_ptr++ = c;
			else {
				ANSIflush();
				--text_cnt, *text_ptr++ = c;
				if (arg)
					c = EOF;
			}
			continue;

		}

		switch (ctable[c]) {
		case CIGNORE:
			break;


		case CLINEFEED:
			if (buffermode || nri) {
				ANSIflush();
				if (arg)
					c = EOF;
			}
			++nlf;
			break;

		case CRETURN:
			if (buffermode) {
				ANSIflush();
				if (arg)
					c = EOF;
			}
			screen->cur_col = 0;
			screen->do_wrap = 0;
			break;

		case CRI:
RI_kluge:		if (buffermode || nlf) {
				ANSIflush();
				if (arg)
					c = EOF;
			}
			++nri;
			break;

		case FLUSH:
			ANSIflush();
			trmbuf->cnt = buf_cnt;
			trmbuf->ptr = buf_ptr;
			return (arg);

		case CESC:
			/* try to intercept RI's so we can buffer them	*/
			if (peekb(trmbuf) == 'M') {
				c = getb(trmbuf);
				goto RI_kluge;
			}
			/* no RI detected; fall into default case	*/

		default:
			ANSIflush();
			docontrol(term, c);
	
			/*
			 * if mode changes or event occurs, force a
			 * cleanup and return by setting char to EOF
			 */
			if (arg 
			|| screen->mode!=ANSInormal 
			|| screen->TekGMode 
			|| screen->TekAMode)
				c = EOF;
		}
	}
}
 
/*
 * flush the buffer of printing characters.  first, process any LF's or
 * RI's that have been buffered.
 */
flushbuf(screen, flags, text_ptr)
register Screen	*screen;
unsigned	flags;
register unsigned char	*text_ptr;
{
	register unsigned char	*s;
	register unsigned char	*p;
	int			arg = 0;
 
	if (nri != 0) {
		ioctl(screen->display->fd, FIONREAD, &arg);
		RevIndex(screen, nri);
		nri = 0;
	}
	if (nlf != 0) {
		ioctl(screen->display->fd, FIONREAD, &arg);
		Index(screen, nlf);
		if (flags & LINEFEED) {		/* do a carriage return	*/
			screen->cur_col = 0;
			screen->do_wrap = 0;
		}
		nlf = 0;
	}

	s = text_buf;
	/*
	 * if a single shift is in effect, process 
	 * the one character it affects.
	 */
	if (screen->curss!=0 && s<text_ptr) {
		dotext(screen, flags, screen->gsets[screen->curss], s, s+1);
		++s;
		screen->curss = 0;
	}

	/*
	 * characters with and without the high bit set must be processed
	 * separately (they select different character sets).  collect them
	 * into groups before processing for as much efficiency as possible.
	 */
	while (s < text_ptr) {
		p = s;
		if (*s < 0x80) {
			/* character doesn't have high bit set	*/
			while (*s<0x80 && s<text_ptr)
				++s;
			dotext(screen, flags,
				screen->gsets[screen->curgl], p, s);
		}
		else {	/* character has high bit set		*/
			while (*s>=0x80 && s<text_ptr) {
				*s &= 0x7f;
				++s;
			}
			dotext(screen, flags,
				screen->gsets[screen->curgr], p, s);
		}
	}

	return (arg || screen->display->qlen!=0);
}

/*
 * process a string of characters according to the character set indicated
 * by charset.  worry about end of line conditions (wraparound if selected).
 */
dotext(screen, flags, charset, buf, ptr)
register Screen	*screen;
unsigned	flags;
char		charset;
unsigned char	*buf;
unsigned char	*ptr;
{
	register unsigned char	*s;
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
				*s = *s - 0x5f;
		break;

	default:	/* any character sets we don't recognize	*/
		return;
	}

	len = ptr - buf; 
	ptr = buf;
	while (len > 0) {
		n = screen->max_col-screen->cur_col+1;
		if (n <= 1) {
			if (screen->do_wrap && (flags&WRAPAROUND)) {
				Index(screen, 1);
				screen->cur_col = 0;
				screen->do_wrap = 0;
				n = screen->max_col+1;
			}
			else
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
register unsigned char	*str;
register int	len;
unsigned	flags;
{
	Font	fnt;
 
 	fnt = (flags & BOLD ? screen->fnt_bold : screen->fnt_norm);
	if (flags & INSERT)
		InsertChar(screen, len);
#ifdef JUMPSCROLL
	if (!(AddToRefresh(screen))) {
		if(screen->scroll_amt)
			FlushScroll(screen);
#endif JUMPSCROLL
	if (flags & INVERSE)
		XText(screen->window, CursorX(screen), CursorY(screen),
			str, len, fnt, screen->background, screen->foreground);
	else
	 	XText(screen->window, CursorX(screen), CursorY(screen),
			str, len, fnt, screen->foreground, screen->background);
	/*
	 * the following statements compile data to compute the average 
	 * number of characters written on each call to XText.  The data
	 * may be examined via the use of a "hidden" escape sequence.
	 */
	ctotal += len;
	++ntotal;
#ifdef JUMPSCROLL
	}
#endif JUMPSCROLL
	ScreenWrite(screen, str, flags, len);
	CursorForward(screen, len);
}
 
/*
 * in Tek mode (graphics or alpha), buffer up as large a string of printing
 * characters as possible before flushing buffer.  in graphics mode the
 * characters are plotted, in alpha mode they are printed.
 */
TekMode(term)
Terminal	*term;
{
	register int		c;
	register unsigned char	*text_ptr;
	register int		text_cnt;
	register unsigned char	*buf_ptr;
	register int		buf_cnt;
	register unsigned	buffermode;
	register Screen *screen = &term->screen;
	register Buffer *trmbuf = &term->buf;
	int			arg;
 
	c = 0;
	buffermode = 0;
	text_ptr = text_buf;
	text_cnt = TEXT_BUF_SIZE;
	buf_cnt = trmbuf->cnt;
	buf_ptr = trmbuf->ptr;

	for (;;) {
		/*
		 * if special condition has occurred, c will already
		 * be set to EOF.  otherwise, pick up a new char.
		 */
		if (c >= 0)
			c = getb(trmbuf);

		if (c > 0)
			c &= 0x7f;

		if (ctable[c]==CPRINTING || (c==DEL && screen->TekGMode)) {
			buffermode = BUFFER_MODE;
			if (--text_cnt >= 0)
				*text_ptr++ = c;
			else {
				TEKflush();
				--text_cnt, *text_ptr++ = c;
				if (arg)
					c = EOF;
			}
			continue;

		}

		switch (ctable[c]) {
		case CIGNORE:
			break;

		case CTEKINIT:
			if (buffermode)
				TEKflush();
			TekInit(term, c);
			if (arg)
				c = EOF;
			break;

		case FLUSH:
			TEKflush();
			trmbuf->cnt = buf_cnt;
			trmbuf->ptr = buf_ptr;
			return (arg);

		default:	/* BEL, BS, HT, LF, VT, FF, CR, ESC, US	*/
				/* and also SI, SO, CAN, SUB		*/
			TEKflush();
			if (screen->TekGMode)
				TekAlph(term, c);
			docontrol(term, c);
	
			/*
			 * if mode changes or event occurs, force a
			 * cleanup and return by setting char to EOF
			 */
			if (arg || screen->mode!=ANSInormal 
			|| (screen->TekGMode==0 && screen->TekAMode==0))
				c = EOF;
		}
	}
}
 
/*
 * flush the buffer of printing characters.
 * use TekString, TekPoint, or TekPlot according to current mode.
 */
TekFlushBuf(term, text_ptr)
Terminal	*term;
register unsigned char	*text_ptr;
{
	register Screen *screen = &term->screen;
	register unsigned char	*s;
 
	s = text_buf;
	if (text_ptr != text_buf) {
		if (screen->TekGMode) {
			if (screen->TekIMode)
				while (s < text_ptr) {
					TekBufPut(*s);
					TekPoint(term, *s++);
				}
			else
				while (s < text_ptr) {
					TekBufPut(*s);
					TekPlot(term, *s++);
				}
		}
		else
			TekString(term, text_buf, text_ptr-text_buf);
	}

	return (screen->display->qlen != 0);
}

/*
 * a string (DCS, OSC, PM, APC) has been encountered in ANSI mode.
 * all printing characters are ignored until a string terminator
 * is seen (other possible exits are CAN, SUB, ESC, and C1 controls).
 */
ANSIstring(term)
Terminal	*term;
{
	register int	c;
	register Screen *screen = &term->screen;
	register Buffer *trmbuf = &term->buf;
	register unsigned char	*buf_ptr;
	register int		buf_cnt;
 
	buf_cnt = trmbuf->cnt;
	buf_ptr = trmbuf->ptr;
	while (screen->mode==ANSIstring && screen->display->qlen==0) {
		if ((c=getb(trmbuf)) == -1)
			break;
		if ((c=parseseq(c, &screen->ansi, screen->TekEmu)) != -1) {
			switch (ctable[c]) {
			case CIGNORE:
			case CPRINTING:
				break;
			case CESC:
			case CCSI:
				screen->mode = ANSInormal;
				procseq(term);
				break;
			case CDCS:
				screen->mode = ANSIstring;
				procseq(term);
				break;
			case CCANCEL:
				screen->mode = ANSInormal;
				break;
			default:
				break;
			}
		}
	}
	trmbuf->cnt = buf_cnt;
	trmbuf->ptr = buf_ptr;
	return (screen->display->qlen != 0);
}
 
/*
 * call parseseq with each new character until the sequence has been
 * parsed.  once parsed, call procseq to process the sequence.  DEC
 * standard says C0 controls in the middle of a sequence are acted
 * on just as if they weren't in the middle of a sequence (ANSI says
 * this is an error condition, but as always doesn't specify how to
 * handle the error).
 */
ANSIparse(term)
Terminal	*term;
{
	register int	c, ch;
	register Screen *screen = &term->screen;
	register Buffer *trmbuf = &term->buf;
	register unsigned char	*buf_ptr;
	register int		buf_cnt;
 
	c = 0;
	buf_cnt = trmbuf->cnt;
	buf_ptr = trmbuf->ptr;

	for (;;) {
		/*
		 * if special condition has occurred, c will already
		 * be set to EOF.  otherwise, pick up a new char.
		 */
		if (c >= 0)
			c = getb(trmbuf);

		if (c == EOF) {
			trmbuf->cnt = buf_cnt;
			trmbuf->ptr = buf_ptr;
			return (screen->display->qlen != 0);
		}

		if ((ch=parseseq(c, &screen->ansi, screen->TekEmu)) != -1) {
			switch (ctable[ch]) {
			case CIGNORE:
				break;

			case CESC:
			case CCSI:
				screen->mode = ANSInormal;
				procseq(term);
				break;

			case CDCS:
				screen->mode = ANSIstring;
				procseq(term);
				break;

			default:
				docontrol(term, ch);
				break;
			}
	
			/*
			 * if mode changes or event occurs, force a
			 * cleanup and return by setting char to EOF
			 */
			if (screen->mode!=ANSIparse || screen->ansi.a_type==0) {
				c = EOF;
				if (screen->mode==ANSIparse)
					screen->mode = ANSInormal;
				screen->ansi.a_type = 0;
				screen->ansi.a_pintro = 0;
				screen->ansi.a_final = 0;
			}
			if (screen->display->qlen != 0);
				c = EOF;

		}
	}
}
 
/*
 * some sort of ANSI sequence has been parsed.  look through the 
 * esctable to look for a match.  if found, apply default parameters
 * and dispatch.
 */
procseq(term)
Terminal	*term;
{
	register ANSI	*ap = &term->screen.ansi;
	register int	i;
	register long	*p;
	register long	a_funct;
#define	SEQKEY		p[0]	/* type, private, & final in a long	*/
#define INTERS		p[1]	/* intermediate(s) in a long		*/
#define	SEQTYPE		p[2]	/* sequence type (dispatch value)	*/
#define	NDEFLT		p[3]	/* number of default parameters		*/
#define DEFAULT(n)	p[4+n]	/* the n'th default parameter (0 origin)*/
#define FIXEDLEN	4	/* fixed (min) length of table entries	*/
 
	/*
	 * just return if any error encountered
	 */
	if (ap->a_nastyf != 0)
		return;

	a_funct = (ap->a_final << 16) | (ap->a_pintro << 8) | ap->a_type;

	/*
	 * step through table, looking for an
	 * entry which matches the current sequence.
	 */
	for (p=esctable; *p!=0; p+=NDEFLT+FIXEDLEN) {
		if (SEQKEY==a_funct
		&& (INTERS==ap->a_inters || INTERS==WILD)) {
			/*
			 * we have a match.
			 * get default values from table.
			 */
			for (i=0; i<NDEFLT; ++i) {
				if (ap->a_dflt[i] || i>=ap->a_nparam)
					ap->a_param[i] = DEFAULT(i);
			}
			if (ap->a_nparam < NDEFLT)
				ap->a_nparam = NDEFLT;
			/*
			 * ready to process the sequence.
			 */
			doescape(term, SEQTYPE, ap);
			break;
		}
	}
}
 
/*
 * this is a big switch of all the ANSI (and other) sequences
 * we know how to handle.  the cases of the switch must
 * have a corresponding entry in the esctable.
 */
doescape(term, kind, ap)
Terminal	*term;
long		kind;
ANSI		*ap;
{
	register Screen *screen = &term->screen;
	register	row, col;
	register	i, top, bot;
	ANSI		reply;
	int		bitset(), bitclr();
	register unsigned char	*text_ptr;
	register int		text_cnt;

	switch (kind) {
	case DECKPAM:
		term->keyboard.flags |= KYPD_APL;
		break;
 
	case DECKPNM:
		term->keyboard.flags &= ~KYPD_APL;
		break;
 
	case DECTC1:
		screen->rx8bit = 0;
		break;
 
	case DECSC:
		screen->sc.row = screen->cur_row;
		screen->sc.col = screen->cur_col;
		screen->sc.flags = term->flags;
		break;
 
	case DECAC1:
		screen->rx8bit = 1;
		break;
 
	case S7C1T:
		screen->tx8bit = 0;
		break;
 
	case S8C1T:
		screen->tx8bit = 1;
		break;
 
	case DECRC:
		term->flags &= ~(BOLD|INVERSE);
		term->flags |= screen->sc.flags&(BOLD|INVERSE);
		CursorSet(screen, screen->sc.row,
			screen->sc.col, term->flags);
		break;
 
	case LS2:
		screen->curgl = 2;
		break;
 
	case LS3:
		screen->curgl = 3;
		break;
 
	case LS3R:
		screen->curgr = 3;
		break;
 
	case LS2R:
		screen->curgr = 2;
		break;
 
	case LS1R:
		screen->curgr = 1;
		break;
 
	case DESIGNATE:
		switch (ap->a_inters) {
		case '(':
			screen->gsets[0] = ap->a_final;
			break;
 
		case ')':
			screen->gsets[1] = ap->a_final;
			break;
 
		case '*':
			screen->gsets[2] = ap->a_final;
			break;
 
		case '+':
			screen->gsets[3] = ap->a_final;
			break;

		case '#':		/* special "hidden" sequence	*/
			if (ap->a_final == '1')
				fprintf(stderr, "avg call = %d char\n", 
					ctotal/ntotal);

		default:
			break;
		}
		break;
 
	case DA1:
		if (ap->a_param[0]==0) {
			reply.a_type   = CSI;
			reply.a_pintro = '?';
			reply.a_nparam = 1;
			reply.a_param[0] = 6;		/* VT102	*/
			reply.a_inters = 0;
			reply.a_final  = 'c';
			unparseseq(&reply, screen->tx8bit, screen->respond);
		}
		break;
 
	case TBC:
		if (ap->a_param[0]==0)
			TabClear(term->tabs, screen->cur_col);
		else if (ap->a_param[0] == 3)
			TabZonk(term->tabs);
		break;
 
	case SET:
		modes(term, bitset);
		break;
 
	case DECSET:
		dpmodes(term, bitset);
		break;
 
	case RST:
		modes(term, bitclr);
		break;
 
	case DECRST:
		dpmodes(term, bitclr);
		break;
 
	case SGR:
		for (i=0; i<ap->a_nparam; ++i) {
			switch (ap->a_param[i]) {
			case 0:
				term->flags &= ~(INVERSE|BOLD);
				break;
 
			case 1:
			case 4:		/* Underscore, really	*/
			case 5:		/* Blink, really.	*/
				term->flags |= BOLD;
				break;
 
			case 7:
				term->flags |= INVERSE;
			}
		}
		break;
 
	case CPR:
		if (ap->a_param[0]==5) {
			reply.a_type = CSI;
			reply.a_pintro = 0;
			reply.a_nparam = 1;
			reply.a_param[0] = 0;
			reply.a_inters = 0;
			reply.a_final  = 'n';
			unparseseq(&reply, screen->tx8bit, screen->respond);
			break;
		}
		if (ap->a_param[0]==6) {
			reply.a_type = CSI;
			reply.a_pintro = 0;
			reply.a_nparam = 2;
			reply.a_param[0] = screen->cur_row+1;
			reply.a_param[1] = screen->cur_col+1;
			reply.a_inters = 0;
			reply.a_final  = 'R';
			unparseseq(&reply, screen->tx8bit, screen->respond);
			break;
		}
		break;
 
	case DECSTBM:
		top = ap->a_param[0];
		bot = ap->a_param[1];
		if (top < 1)
			top = 1;
		if (bot > screen->max_row+1)
			bot = screen->max_row+1;
		if (bot > top) {
#ifdef JUMPSCROLL
			if(screen->scroll_amt)
				FlushScroll(screen);
#endif JUMPSCROLL
			screen->top_marg = top-1;
			screen->bot_marg = bot-1;
			CursorSet(screen, 0, 0, term->flags);
		}
		break;
 
	case ICH:
		InsertChar(screen, ap->a_param[0]);
		break;
 
	case CUU:
		CursorUp(screen, ap->a_param[0]);
		break;
	
	case CUD:
		CursorDown(screen, ap->a_param[0]);
		break;
	
	case CUF:
		CursorForward(screen, ap->a_param[0]);
		break;
	
	case CUB:
		CursorBack(screen, ap->a_param[0]);
		break;
	
	case HVP:
	case CUP:
		row = ap->a_param[0];
		col = ap->a_param[1];
 
		screen->cur_x = screen->cur_y = 0;
		screen->TekAMode = 0;
		CursorSet(screen, row-1, col-1, term->flags);
		break;
 
	case ED:
		switch (ap->a_param[0]) {
		case 0:	ClearBelow(screen);
			break;

		case 1:	ClearAbove(screen);
			break;

		case 2:	ClearScreen(screen);
			if (screen->TekEmu)
				TekErase(term);
			break;
		}
		break;
 
	case EL:
		switch (ap->a_param[0]) {
		case 0:	ClearRight(screen);
			break;

		case 1:	ClearLeft(screen);
			break;

		case 2:	ClearLine(screen);
			break;
		}
		break;
 
	case IL:
		InsertLine(screen, ap->a_param[0]);
		break;
 
	case DL:
		DeleteLine(screen, ap->a_param[0]);
		break;
 
	case DCH:
		DeleteChar(screen, ap->a_param[0]);
		break;
 
	case DECALN:
		text_ptr = text_buf;
		text_cnt = TEXT_BUF_SIZE;
		for (i=0; i<256; ++i) {
			if (--text_cnt >= 0)
				*text_ptr++ = i;
			else {
				flushbuf(screen, term->flags, text_ptr);
				text_ptr = text_buf;
				text_cnt = TEXT_BUF_SIZE-1;
				*text_ptr++ = i;
			}
		}
		flushbuf(screen, term->flags, text_ptr);
		break;

	case TEKESCFF:
	case TEKCSIUS:
		CursorSet(screen, 0, 0, term->flags);
		ClearScreen(screen);
		TekErase(term);
		break;

	case TEKESCSUB:
		TekCursor(term);
		break;

	case TEKESCINQ:
		TekInq(term);
		break;

	default:
		Panic("unexpected dispatch (%d) encountered in doescape", kind);
		break;
	}
}
 
/*
 * this is a big switch of all the control characters we know how to handle.
 * the cases of the switch have a corresponding entry in the ctable.
 */
docontrol(term, c)
Terminal	*term;
{
	register Screen *screen = &term->screen;
 
	switch (ctable[c]) {
	case CIGNORE:
		break;
 
	case CBELL:
		XFeep(0);
		break;
 
	case CBACKSPACE:
		CursorBack(screen, 1);
		break;
 
	case CTAB:
		screen->cur_col = TabNext(term->tabs, screen->cur_col);
		if (screen->cur_col > screen->max_col)
			screen->cur_col = screen->max_col;
		break;
 
	case CLINEFEED:		/* actually both VT and LF map here	*/
	case CFORMFEED:
		if (screen->TekAMode)
			TekReset(term);
		Index(screen, 1);
		if (term->flags & LINEFEED)
			CarriageReturn(screen);
		break;
 
	case CRETURN:
		if (screen->TekAMode)
			TekReset(term);
		CarriageReturn(screen);
		break;
 
	case CLS1:
		screen->curgl = 1;
		break;
 
	case CLS0:
		screen->curgl = 0;
		break;
 
	case CCANCEL:
		screen->mode = ANSInormal;
		break;
 
	case CESC:
	case CDCS:
	case CCSI:
		screen->ansi.a_type  = c;
		screen->ansi.a_pintro = 0;
		screen->ansi.a_final = 0;
		screen->ansi.a_inters = 0;
		screen->ansi.a_nparam = 0;
		screen->ansi.a_nastyf = 0;
		screen->mode = ANSIparse;
		break;
 
	case CTEKINIT:		/* come here from FS, GS, or RS		*/
		if (screen->TekEmu)
			TekInit(term, c);
		break;
 
	case CTEKALPH:		/* US					*/
		break;		/* just ignore; can only get to alpha	*/
				/* mode from Tek graphics mode		*/

	case CIND:
		Index(screen, 1);
		break;
 
	case CNEL:
		Index(screen, 1);
		CarriageReturn(screen);
		break;
 
	case CRI:
		RevIndex(screen, 1);
		break;
 
	case CSS2:
		screen->curss = 2;
		break;
 
	case CSS3:
		screen->curss = 3;
		break;
 
	default:
		Panic("unexpected char (0x%x) encountered in docontrol", c);
		break;
	}
}

/*
 * process ANSI modes set, reset
 */
modes(term, func)
Terminal	*term;
int		(*func)();
{
	register Screen	*screen	= &term->screen;
	register ANSI	*ansi;
	register int	i;

	ansi = &screen->ansi;
	for (i=0; i<ansi->a_nparam; ++i) {
		switch (ansi->a_param[i]) {
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
	register ANSI	*ap;
	register int	i;

	ap = &screen->ansi;
	for (i=0; i<ap->a_nparam; ++i) {
		switch (ap->a_param[i]) {
		case 1:			/* DECCKM			*/
			(*func)(&term->keyboard.flags, CURSOR_APL);
			break;

#ifdef JUMPSCROLL
		case 4:			/* DECSCLM (slow scroll)	*/
			if (func == bitset) {
				screen->jumpscroll = 0;
				if (screen->scroll_amt)
					FlushScroll(screen);
			} else if (!screen->TekEmu)
				screen->jumpscroll = 1;
			(*func)(&term->flags, SMOOTHSCROLL);
			break;
#endif JUMPSCROLL
		case 5:			/* DECSCNM			*/
			i = term->flags;
			(*func)(&term->flags, REVERSE_VIDEO);
			if ((term->flags ^ i) & REVERSE_VIDEO)
				ReverseVideo(term);
			break;

		case 6:			/* DECOM			*/
			(*func)(&term->flags, ORIGIN);
			CursorSet(screen, 0, 0, term->flags);
			break;

		case 7:			/* DECAWM			*/
			(*func)(&term->flags, WRAPAROUND);
			break;
		case 9:			/* MIT bogus sequence		*/
			(*func)(&screen->send_mouse_pos, 1);
			break;
		case 38:		/* DECTEK			*/
			(*func)(&screen->TekEmu, 1);
			/*
			 * probably need to do some work here to get
			 * into or out of Tek emulation cleanly
			 */
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

ReverseVideo (term)
	Terminal *term;
{
	register Screen *screen = &term->screen;
	int tmp;

	XDefineCursor(screen->window,
		      (term->flags & REVERSE_VIDEO) ? screen->rcurs : screen->curs);
	tmp = screen->background;
	if (screen->cursorcolor == screen->foreground)
	    screen->cursorcolor = tmp;
	screen->background = screen->foreground;
	screen->foreground = tmp;
	XFreePixmap(screen->bgndtile);
	screen->bgndtile = XMakeTile(screen->background);
	if (screen->borderwidth &&
	    screen->background < 2 &&
	    screen->foreground < 2) {
		if (screen->bgndtile == BlackPixmap)
		    screen->bordertile = WhitePixmap;
		else if (screen->bgndtile == WhitePixmap)
		    screen->bordertile = BlackPixmap;
	    XChangeBorder (screen->window, screen->bordertile);
	}
	XChangeBackground (screen->window, screen->bgndtile);
	XClear (screen->window);
	ScrnRefresh (screen, 0, 0, screen->max_row +1, screen->max_col + 1);
	if (screen->TekEmu) TekRefresh (term);
}
