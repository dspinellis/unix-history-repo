/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
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
 *	@(#)bmd.c	8.1 (Berkeley) 6/10/93
 */

/*

 * bmd.c --- Bitmap-Display raw-level driver routines
 *
 *	by A.Fujita, SEP-09-1992
 */

#undef	BMD_PRINTF

#include <sys/param.h>
#include <sys/systm.h>

extern u_short bmdfont[][20];

#define isprint(c)	( c < 0x20 ? 0 : 1)

/*
 *  Width & Hight
 */

#define	PB_WIDTH	2048				/* Plane Width   (Bit) */
#define	PB_HIGHT	1024				/* Plane Hight   (Bit) */
#define PL_WIDTH	64				/* Plane Width  (long) */
#define PS_WIDTH	128				/* Plane Width  (long) */
#define P_WIDTH		256				/* Plane Width  (Byte) */

#define SB_WIDTH	1280				/* Screen Width  (Bit) */
#define	SB_HIGHT	1024				/* Screen Hight  (Bit) */
#define SL_WIDTH	40				/* Screen Width (Long) */
#define S_WIDTH		160				/* Screen Width (Byte) */

#define FB_WIDTH	12				/* Font Width    (Bit) */
#define FB_HIGHT	20				/* Font Hight    (Bit) */


#define NEXT_LINE(addr)				( addr +  (PL_WIDTH * FB_HIGHT) )
#define SKIP_NEXT_LINE(addr)			( addr += (PL_WIDTH - SL_WIDTH) )


void	bmd_add_new_line();

void	bmd_draw_char();
void	bmd_reverse_char();
void	bmd_erase_char();
void	bmd_erase_screen();
void	bmd_scroll_screen();

void	bmd_escape();


struct bmd_linec {
	struct bmd_linec *bl_next;
	struct bmd_linec *bl_prev;
	int	bl_col;
	int	bl_end;
	u_char	bl_line[128];
};

struct bmd_softc {
	int	bc_stat;
	char   *bc_raddr;
	char   *bc_waddr;
	int	bc_xmin;
	int	bc_xmax;
	int	bc_ymin;
	int	bc_ymax;
	int	bc_col;
	int	bc_row;
	struct bmd_linec *bc_bl;
	char	bc_escseq[8];
	char   *bc_esc;
	void  (*bc_escape)();
};

#define	STAT_NORMAL	0x0000
#define	STAT_ESCAPE	0x0001
#define	STAT_STOP	0x0002

struct	bmd_softc bmd_softc;
struct	bmd_linec bmd_linec[52];

int	bmd_initflag = 0;

/*
 * Escape-Sequence
 */

#define push_ESC(p, c)		*(p)->bc_esc++ = c; *(p)->bc_esc = '\0'

void
bmd_escape(c)
	int c;
{
	register struct bmd_softc *bp = &bmd_softc;

	bp->bc_stat &= ~STAT_ESCAPE;
	bp->bc_esc = &bp->bc_escseq[0];
/*	bp->bc_escape = bmd_escape;	*/
}

/*
 * Entry Routine
 */

bmdinit()
{
	register struct bmd_softc *bp = &bmd_softc;
	register struct bmd_linec *bq;
	register int i;

	bp->bc_raddr = (char *) 0xB10C0008;		/* plane-0 hardware address */
	bp->bc_waddr = (char *) 0xB1080008;		/* common bitmap hardware address */

	/*
	 *  adjust plane position
	 */

	fb_adjust(7, -27);

	bp->bc_stat  = STAT_NORMAL;

	bp->bc_xmin  = 8;
	bp->bc_xmax  = 96;
	bp->bc_ymin  = 2;
	bp->bc_ymax  = 48;

	bp->bc_row = bp->bc_ymin;

	for (i = bp->bc_ymin; i < bp->bc_ymax; i++) {
		bmd_linec[i].bl_next = &bmd_linec[i+1];
		bmd_linec[i].bl_prev = &bmd_linec[i-1];
	}
	bmd_linec[bp->bc_ymax-1].bl_next = &bmd_linec[bp->bc_ymin];
	bmd_linec[bp->bc_ymin].bl_prev = &bmd_linec[bp->bc_ymax-1];

	bq = bp->bc_bl = &bmd_linec[bp->bc_ymin];
	bq->bl_col = bq->bl_end = bp->bc_xmin;

	bp->bc_col = bp->bc_xmin;

	bp->bc_esc = &bp->bc_escseq[0];
	bp->bc_escape = bmd_escape;

	bmd_erase_screen((u_long *) bp->bc_waddr);	/* clear screen */

							/* turn on  cursole */
	bmd_reverse_char(bp->bc_raddr,
			 bp->bc_waddr,
			 bq->bl_col, bp->bc_row);

	bmd_initflag = 1;
}

bmdputc(c)
	register int c;
{
	register struct bmd_softc *bp;
	register struct bmd_linec *bq;
	register int i;

	if (!bmd_initflag)
		bmdinit();

	bp = &bmd_softc;
	bq = bp->bc_bl;

							/* skip out, if STAT_STOP */
	if (bp->bc_stat & STAT_STOP)
		return(c);

	c &= 0x7F;
							/* turn off cursole */
	bmd_reverse_char(bp->bc_raddr,
			 bp->bc_waddr,
			 bq->bl_col, bp->bc_row);
							/* do escape-sequence */

	if (bp->bc_stat & STAT_ESCAPE) {
		*bp->bc_esc++ = c;
		(*bp->bc_escape)(c);
		goto done;
	}

	if (isprint(c)) {
		bmd_draw_char(bp->bc_raddr, bp->bc_waddr,
			      bq->bl_col, bp->bc_row, c);
		bq->bl_col++;
		bq->bl_end++;
		if (bq->bl_col >= bp->bc_xmax) {
			bq->bl_col = bq->bl_end = bp->bc_xmin;
			bp->bc_row++;
			if (bp->bc_row >= bp->bc_ymax) {
				bmd_scroll_screen((u_long *) bp->bc_raddr,
						  (u_long *) bp->bc_waddr,
						  bp->bc_xmin, bp->bc_xmax,
						  bp->bc_ymin, bp->bc_ymax);

				bp->bc_row = bp->bc_ymax - 1;
			}
		}
	} else {
		switch (c) {
		case 0x08:				/* BS */
			if (bq->bl_col > bp->bc_xmin) {
				bq->bl_col--;
			}
			break;

		case 0x09:				/* HT */
		case 0x0B:				/* VT */
			i = ((bq->bl_col / 8) + 1) * 8;
			if (i < bp->bc_xmax) {
				bq->bl_col = bq->bl_end = i;
			}
			break;

		case 0x0A:				/* NL */
			bp->bc_row++;
			if (bp->bc_row >= bp->bc_ymax) {
				bmd_scroll_screen((u_long *) bp->bc_raddr,
						  (u_long *) bp->bc_waddr,
						  bp->bc_xmin, bp->bc_xmax,
						  bp->bc_ymin, bp->bc_ymax);

				bp->bc_row = bp->bc_ymax - 1;
			}
			break;

		case 0x0D:				/* CR */
			bq->bl_col = bp->bc_xmin;
			break;

		case 0x1b:				/* ESC */
			bmdputc('<');
			bmdputc('E');
			bmdputc('S');
			bmdputc('C');
			bmdputc('>');
/*
			bp->bc_stat |= STAT_ESCAPE;
			*bp->bc_esc++ = 0x1b;
 */
			break;

		case 0x7F:				/* DEL */
			if (bq->bl_col > bp->bc_xmin) {
				bq->bl_col--;
				bmd_erase_char(bp->bc_raddr,
					       bp->bc_waddr,
					       bq->bl_col, bp->bc_row);
			}
			break;

		default:
			break;
		}
	}

 done:
							/* turn on  cursole */
	bmd_reverse_char(bp->bc_raddr,
			 bp->bc_waddr,
			 bq->bl_col, bp->bc_row);

	return(c);
}

/*
 *
 */

bmd_on()
{
	bmdinit();
}

bmd_off()
{
	register struct bmd_softc *bp = &bmd_softc;

	bp->bc_stat |= STAT_STOP;
	bmd_erase_screen((u_long *) bp->bc_waddr);	/* clear screen */
}

bmd_clear()
{
	register struct bmd_softc *bp = &bmd_softc;
	register struct bmd_linec *bq = bp->bc_bl;

	bmd_erase_screen((u_long *) bp->bc_waddr);	/* clear screen */

	bmd_reverse_char(bp->bc_raddr,
			 bp->bc_waddr,
			 bq->bl_col, bp->bc_row);	/* turn on  cursole */
}

bmd_home()
{
	register struct bmd_softc *bp = &bmd_softc;
	register struct bmd_linec *bq = bp->bc_bl;

	bmd_reverse_char(bp->bc_raddr,
			 bp->bc_waddr,
			 bq->bl_col, bp->bc_row);	/* turn off cursole */

	bq->bl_col = bq->bl_end = bp->bc_xmin;
	bp->bc_row = bp->bc_ymin;

	bmd_reverse_char(bp->bc_raddr,
			 bp->bc_waddr,
			 bq->bl_col, bp->bc_row);	/* turn on  cursole */
}

/*
 *  charactor operation routines
 */

void
bmd_draw_char(raddr, waddr, col, row, c)
	char *raddr;
	char *waddr;
	int col;
	int row;
	int c;
{
	volatile register u_short  *p,  *q, *fp;
	volatile register u_long  *lp, *lq;
	register int i;

	fp = &bmdfont[c][0];

	switch (col % 4) {

	case 0:
		p = (u_short *) ( raddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ));
		q = (u_short *) ( waddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ));
		for (i = 0; i < FB_HIGHT; i++) {
			*q = (*p & 0x000F) | (*fp & 0xFFF0);
			p += 128;
			q += 128;
			fp++;
		}
		break;

	case 1:
		lp = (u_long *) ( raddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ));
		lq = (u_long *) ( waddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ));
		for (i = 0; i < FB_HIGHT; i++) {
			*lq = (*lp & 0xFFF000FF) | ((u_long)(*fp & 0xFFF0) << 4);
			lp += 64;
			lq += 64;
			fp++;
		}
		break;

	case 2:
		lp = (u_long *) ( raddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ) + 2 );
		lq = (u_long *) ( waddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ) + 2 );
		for (i = 0; i < FB_HIGHT; i++) {
			*lq = (*lp & 0xFF000FFF) | ((u_long)(*fp & 0xFFF0) << 8);
			lp += 64;
			lq += 64;
			fp++;
		}
		break;

	case 3:
		p = (u_short *) ( raddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ) + 4 );
		q = (u_short *) ( waddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ) + 4 );
		for (i = 0; i < FB_HIGHT; i++) {
			*q = (*p & 0xF000) | ((*fp & 0xFFF0) >> 4);
			p += 128;
			q += 128;
			fp++;
		}
		break;

	default:
		break;
	}
}

void
bmd_reverse_char(raddr, waddr, col, row)
	char *raddr;
	char *waddr;
	int col;
	int row;
{
	volatile register u_short  *p,  *q,  us;
	volatile register u_long  *lp, *lq,  ul;
	register int i;

	switch (col%4) {

	case 0:
		p = (u_short *) ( raddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ));
		q = (u_short *) ( waddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ));
		for (i = 0; i < FB_HIGHT; i++) {
			*q = (*p & 0x000F) | (~(*p) & 0xFFF0);
			p += 128;
			q += 128;
		}
		break;

	case 1:
		lp = (u_long *) ( raddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ));
		lq = (u_long *) ( waddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ));
		for (i = 0; i < FB_HIGHT; i++) {
			*lq = (*lp & 0xFFF000FF) | (~(*lp) & 0x000FFF00);
			lp += 64;
			lq += 64;
		}
		break;

	case 2:
		lp = (u_long *) ( raddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ) + 2 );
		lq = (u_long *) ( waddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ) + 2 );
		for (i = 0; i < FB_HIGHT; i++) {
			*lq = (*lp & 0xFF000FFF) | (~(*lp) & 0x00FFF000);
			lp += 64;
			lq += 64;
		}
		break;

	case 3:
		p = (u_short *) ( raddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ) + 4 );
		q = (u_short *) ( waddr + (( row * FB_HIGHT ) << 8 ) + (( col / 4 ) * 6 ) + 4 );
		for (i = 0; i < FB_HIGHT; i++) {
			*q = (*p & 0xF000) | (~(*p) & 0x0FFF);
			p += 128;
			q += 128;
		}
		break;

	default:
		break;
	}
}

void
bmd_erase_char(raddr, waddr, col, row)
	char *raddr;
	char *waddr;
	int col;
	int row;
{
	bmd_draw_char(raddr, waddr, col, row, 0);

	return;
}


/*
 * screen operation routines
 */

void
bmd_erase_screen(lp)
	volatile register u_long *lp;
{
	register int i, j;

	for (i = 0; i < SB_HIGHT; i++) {
		for (j = 0; j < SL_WIDTH; j++)
			*lp++ = 0;
		SKIP_NEXT_LINE(lp);
	}

	return;
}

void
bmd_scroll_screen(lp, lq, xmin, xmax, ymin, ymax)
	volatile register u_long *lp;
	volatile register u_long *lq;
	int xmin, xmax, ymin, ymax;
{
	register int i, j;

	lp += ((PL_WIDTH * FB_HIGHT) * (ymin + 1));
	lq += ((PL_WIDTH * FB_HIGHT) *  ymin);

	for (i = 0; i < ((ymax - ymin -1) * FB_HIGHT); i++) {
		for (j = 0; j < SL_WIDTH; j++) {
			*lq++ = *lp++;
		}
		lp += (PL_WIDTH - SL_WIDTH);
		lq += (PL_WIDTH - SL_WIDTH);
	}

	for (i = 0; i < FB_HIGHT; i++) {
		for (j = 0; j < SL_WIDTH; j++) {
			*lq++ = 0;
		}
		lq += (PL_WIDTH - SL_WIDTH);
	}

}


#ifdef	BMD_PRINTF

#include <machine/stdarg.h>

void		bmd_kprintf();
static char *	bmd_sprintn();

void
#ifdef __STDC__
bmd_printf(const char *fmt, ...)
#else
bmd_printf(fmt, va_alist)
	char *fmt;
#endif
{
	va_list ap;

	va_start(ap, fmt);
	bmd_kprintf(fmt, ap);
	va_end(ap);
}

/*
 * Scaled down version of printf(3).
 *
 * Two additional formats:
 *
 * The format %b is supported to decode error registers.
 * Its usage is:
 *
 *	printf("reg=%b\n", regval, "<base><arg>*");
 *
 * where <base> is the output base expressed as a control character, e.g.
 * \10 gives octal; \20 gives hex.  Each arg is a sequence of characters,
 * the first of which gives the bit number to be inspected (origin 1), and
 * the next characters (up to a control character, i.e. a character <= 32),
 * give the name of the register.  Thus:
 *
 *	kprintf("reg=%b\n", 3, "\10\2BITTWO\1BITONE\n");
 *
 * would produce output:
 *
 *	reg=3<BITTWO,BITONE>
 *
 * The format %r passes an additional format string and argument list
 * recursively.  Its usage is:
 *
 * fn(char *fmt, ...)
 * {
 *	va_list ap;
 *	va_start(ap, fmt);
 *	printf("prefix: %r: suffix\n", fmt, ap);
 *	va_end(ap);
 * }
 *
 * Space or zero padding and a field width are supported for the numeric
 * formats only.
 */
void
bmd_kprintf(fmt, ap)
	register const char *fmt;
	va_list ap;
{
	register char *p, *q;
	register int ch, n;
	u_long ul;
	int base, lflag, tmp, width;
	char padc;

	for (;;) {
		padc = ' ';
		width = 0;
		while ((ch = *(u_char *)fmt++) != '%') {
			if (ch == '\0')
				return;
			if (ch == '\n')
				bmdputc('\r');
			bmdputc(ch);
		}
		lflag = 0;
reswitch:	switch (ch = *(u_char *)fmt++) {
		case '0':
			padc = '0';
			goto reswitch;
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			for (width = 0;; ++fmt) {
				width = width * 10 + ch - '0';
				ch = *fmt;
				if (ch < '0' || ch > '9')
					break;
			}
			goto reswitch;
		case 'l':
			lflag = 1;
			goto reswitch;
		case 'b':
			ul = va_arg(ap, int);
			p = va_arg(ap, char *);
			for (q = bmd_sprintn(ul, *p++, NULL); ch = *q--;)
				bmdputc(ch);

			if (!ul)
				break;

			for (tmp = 0; n = *p++;) {
				if (ul & (1 << (n - 1))) {
					bmdputc(tmp ? ',' : '<');
					for (; (n = *p) > ' '; ++p)
						bmdputc(n);
					tmp = 1;
				} else
					for (; *p > ' '; ++p)
						continue;
			}
			if (tmp)
				bmdputc('>');
			break;
		case 'c':
			bmdputc(va_arg(ap, int));
			break;
		case 'r':
			p = va_arg(ap, char *);
			bmd_kprintf(p, va_arg(ap, va_list));
			break;
		case 's':
			p = va_arg(ap, char *);
			while (ch = *p++)
				bmdputc(ch);
			break;
		case 'd':
			ul = lflag ? va_arg(ap, long) : va_arg(ap, int);
			if ((long)ul < 0) {
				bmdputc('-');
				ul = -(long)ul;
			}
			base = 10;
			goto number;
		case 'o':
			ul = lflag ? va_arg(ap, u_long) : va_arg(ap, u_int);
			base = 8;
			goto number;
		case 'u':
			ul = lflag ? va_arg(ap, u_long) : va_arg(ap, u_int);
			base = 10;
			goto number;
		case 'x':
			ul = lflag ? va_arg(ap, u_long) : va_arg(ap, u_int);
			base = 16;
number:			p = bmd_sprintn(ul, base, &tmp);
			if (width && (width -= tmp) > 0)
				while (width--)
					bmdputc(padc);
			while (ch = *p--)
				bmdputc(ch);
			break;
		default:
			bmdputc('%');
			if (lflag)
				bmdputc('l');
			/* FALLTHROUGH */
		case '%':
			bmdputc(ch);
		}
	}
}

/*
 * Put a number (base <= 16) in a buffer in reverse order; return an
 * optional length and a pointer to the NULL terminated (preceded?)
 * buffer.
 */
static char *
bmd_sprintn(ul, base, lenp)
	register u_long ul;
	register int base, *lenp;
{					/* A long in base 8, plus NULL. */
	static char buf[sizeof(long) * NBBY / 3 + 2];
	register char *p;

	p = buf;
	do {
		*++p = "0123456789abcdef"[ul % base];
	} while (ul /= base);
	if (lenp)
		*lenp = p - buf;
	return (p);
}
#endif
