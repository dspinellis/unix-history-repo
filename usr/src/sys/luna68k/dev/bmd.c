/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)bmd.c	7.3 (Berkeley) %G%
 */
/*

 * bmd.c --- Bitmap-Display raw-level driver routines
 *
 *	by A.Fujita, SEP-09-1992
 */


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
#define	STAT_INSERT	0x0100

struct	bmd_softc bmd_softc;
struct	bmd_linec bmd_linec[52];

void	bmd_escape();
void	bmd_escape_0();
void	bmd_escape_1();


/*
 * Escape-Sequence
 */

void
bmd_escape(c)
	int c;
{
	register struct bmd_softc *bp = &bmd_softc;

	switch (c) {

	case '[':
		bp->bc_escape = bmd_escape_0;
		break;

	default:
		bp->bc_stat &= ~STAT_ESCAPE;
		bp->bc_esc = &bp->bc_escseq[0];
		bp->bc_escape = bmd_escape;
		break;
	}
}

void
bmd_escape_0(c)
	int c;
{
	register struct bmd_softc *bp = &bmd_softc;
	register struct	bmd_linec *bq = bp->bc_bl;

	switch (c) {

	case 'A':
		if (bp->bc_row > bp->bc_ymin) {
			bp->bc_row--;
		}
		break;

	case 'C':
		if (bq->bl_col < bp->bc_xmax - 1) {
			bq->bl_col++;
		}
		break;

	case 'K':
		if (bq->bl_col < bp->bc_xmax) {
			register int col;
			for (col = bq->bl_col; col < bp->bc_xmax; col++)
				bmd_erase_char(bp->bc_raddr,
					       bp->bc_waddr,
					       col, bp->bc_row);
		}
		bq->bl_end = bq->bl_col;
		break;

	case 'H':
		bq->bl_col = bq->bl_end = bp->bc_xmin;
		bp->bc_row = bp->bc_ymin;
		break;

	default:
/*
		*bp->bc_esc++ = c;
		bp->bc_escape = bmd_escape_1;
		return;
 */
		break;
	}

	bp->bc_stat &= ~STAT_ESCAPE;
	bp->bc_esc = &bp->bc_escseq[0];
	bp->bc_escape = bmd_escape;
}

void
bmd_escape_1(c)
	int c;
{
	register struct bmd_softc *bp = &bmd_softc;
	register struct	bmd_linec *bq = bp->bc_bl;
	register int col = 0, row = 0;
	register char *p;

	switch (c) {

	case 'J':
		bp->bc_stat &= ~STAT_ESCAPE;
		bp->bc_esc = &bp->bc_escseq[0];
		bp->bc_escape = bmd_escape;
		break;

	case 'H':
		for (p = &bp->bc_escseq[0]; *p != ';'; p++)
			row = (row * 10) + (*p - 0x30);
		p++;
		for (p = &bp->bc_escseq[0]; p != bp->bc_esc; p++)
			col = (col * 10) + (*p - 0x30);

		bq->bl_col = col + bp->bc_xmin;
		bp->bc_row = row + bp->bc_ymin;

		bp->bc_stat &= ~STAT_ESCAPE;
		bp->bc_esc = &bp->bc_escseq[0];
		bp->bc_escape = bmd_escape;
		break;

	default:
		*bp->bc_esc++ = c;
		break;
	}
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
}

bmdputc(c)
	register int c;
{
	register struct bmd_softc *bp = &bmd_softc;
	register struct bmd_linec *bq = bp->bc_bl;
	register int i;

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
			bp->bc_stat |= STAT_ESCAPE;
			*bp->bc_esc++ = 0x1b;
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

bmdclear()
{
	register struct bmd_softc *bp = &bmd_softc;
	register struct bmd_linec *bq = bp->bc_bl;

	bmd_erase_screen((u_long *) bp->bc_waddr);	/* clear screen */

	bq->bl_col = bq->bl_end = bp->bc_xmin;
	bp->bc_row = bp->bc_ymin;

	bmd_reverse_char(bp->bc_raddr,
			 bp->bc_waddr,
			 bq->bl_col, bp->bc_row);	/* turn on  cursole */
}


/*
 *
 */

void
bmd_add_new_line()
{
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
