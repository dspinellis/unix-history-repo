/*
 * text.c 
 *
 * Copyright (c) 1985 Massachusetts Institue of Technology
 * Copyright (c) 1986 Sun Microsystems, Inc.
 * Copyright (c) 1986 David C. Martin, UC Berkeley
 *
 * David C. Martin 
 * ARPA: dcmartin@ingres.Berkeley.EDU
 * UUCP: ..!ucbvax!dcmartin
 *
 * $Log:	text.c,v $
 * Revision 10.5  86/11/29  13:48:49  jg
 * fixes from Berkeley
 * 
 * Revision 1.10  86/07/27  13:49:58  dcmartin
 * removed debugging statements
 * 
 * Revision 1.9  86/07/25  14:45:48  dcmartin
 * modified PrintTextMask() to handle variable width fonts
 * 
 * Revision 1.8  86/07/17  10:37:29  dcmartin
 * release version w/ support for variable width text
 * 
 * Revision 1.7  86/07/17  10:32:19  dcmartin
 * 
 */

#ifndef lint
static char rcs_id[] = "$Header: text.c,v 10.5 86/11/29 13:48:49 jg Rel $";
#endif lint

#include <X/mit-copyright.h>

/*
 * The Sun X drivers are a product of Sun Microsystems, Inc. and are provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify these drivers without charge, but are not authorized
 * to license or distribute them to anyone else except as part of a product or
 * program developed by the user.
 * 
 * THE SUN X DRIVERS ARE PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND
 * INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE
 * PRACTICE.
 *
 * The Sun X Drivers are provided with no support and without any obligation
 * on the part of Sun Microsystems, Inc. to assist in their use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THE SUN X
 * DRIVERS OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifdef sun

/*
 *	ToDo:
 *		Color
 */

#include "Xsun.h"
#ifndef stdin
#include <stdio.h>
#endif
#include <pixrect/memreg.h>
#include <pixrect/cg2reg.h>

/* I've put in some rather ugly hacks, in the name of performance.  The
   global variables private_* are really extra parameters to the batchrop
   routines.  I did this, rather than adding parameters, because I wanted to
   do the least violence to the "official" specs of batchrop -- this way X
   will vaguely work on displays that don't use one of the tuned batchrops.
	JAG */

int         private_fgcolor, private_bgcolor, private_czmask;

extern struct pixrect *PixRect;

#define	MAXCHARS	400

extern int
PrintText(text, textlen, font, fore, back, charpad, spacepad, dstx, dsty, 
	clips, clipcount, func, zmask)
register unsigned char	*text;
FONT			*font;
int			textlen, fore, back, charpad, spacepad, dstx, dsty;
CLIP			*clips;
int			clipcount, zmask;
int			func;
{
	extern CURSOR		*CurrentCursor;
	extern			CursorDisplayed;
	int			cleft, ctop, cwidth, cheight;
	int			op;
	extern char		FBMap[];
	unsigned char		*limit = text + 
			   	    (textlen < MAXCHARS ? textlen : MAXCHARS);
	register int		w = 0;
	int			bsize = 0;
	int			lheight;
	int			sbot, sright;
	static struct pr_prpos	bat[MAXCHARS];

	private_czmask = zmask;
	private_fgcolor = fore;
	private_bgcolor = back;
	if (fore & 1)
		func += 0x20;
	if (back & 1)
		func += 0x10;
	func = FBMap[func];
	op = SUN_FROM_X_OP(func) | PIX_COLOR(fore);
	/* this is a gross abuse of C, but ... */
	{
		register struct pixfont		*pf;
		register struct pr_prpos	*p;
		register struct pixchar		*pc;
		register int			pwidth = 0;

		pf = (struct pixfont *) font->data;
		p = bat;
		lheight = pf->pf_defaultsize.y;
		if (charpad == 0 && spacepad == 0) {
			/* for each character in the text */
			while (text < limit) {
				pc = &(pf->pf_char[*text++]);
				if (pc == 0 || pc->pc_pr == NULL)
					continue;
				p->pr = pc->pc_pr;
				/* 
				 * pr_batchrop() is confusing... 
				 * you must give the offset for the previous
				 * pixrect 
				 */
				p->pos.x = pwidth;
				/* store the character width */
				pwidth = pc->pc_adv.x;
				/* increment the total width */
				w += pwidth;
				p++;
				bsize++;
			}
		} else {
			/* for efficiency ... */
			struct pixchar	*space = &(pf->pf_char[font->space]);

			while (text < limit) {
				pc = &(pf->pf_char[*text++]);
				if (pc == (struct pixchar *) NULL || 
				    pc->pc_pr == (struct pixrect *) NULL)
					continue;
				p->pr = pc->pc_pr;
				/* 
				 * pr_batchrop() is confusing... 
				 * you must give the offset for the previous
				 * pixrect 
				 */
				p->pos.x = pwidth;
				/* store the character width */
				pwidth = pc->pc_adv.x + charpad;
				/* add space? */
				if (pc == space)
					pwidth += spacepad;
				/* increment the total width */
				w += pwidth;
				p++;
				bsize++;
			}
		}
	} /* end gross abuse */
	/* determine the right and bottom of the region */
	sbot = dsty + lheight;
	sright = dstx + w;
	/* if the cursor is where we want to put text get rid of it */
	if (CursorDisplayed) {
		extern DEVICE		*CurrentDevice;
		register vsCursor	*ms = CurrentDevice->mouse;
		register CURSOR		*cs = CurrentCursor;

		if (ms->y < sbot && 
		    ms->x < sright && 
		    ms->y + cs->height > dsty && 
		    ms->x + cs->width > dstx)
			DisplayCursor(NULL);
	}
	do {
		GetNextClip(clips, cleft, ctop, cwidth, cheight);
		if (dsty >= ctop && 
		    sbot <= ctop + cheight && 
		    dstx >= cleft && 
		    sright <= cleft + cwidth) {
			pr_batchrop(PixRect, dstx - bat[0].pos.x, dsty,
				op | PIX_DONTCLIP, bat, bsize);
		} else {
			struct pixrect	*region;

			if (dsty > ctop + cheight)
				continue;
			if (dsty + lheight <= ctop)
				continue;
			region = pr_region(PixRect, cleft, ctop, cwidth, 
			    cheight);
			pr_batchrop(region, dstx - cleft - bat[0].pos.x,
				dsty - ctop, op, bat, bsize);
			pr_destroy(region);
		}
	} while (--clipcount > 0);
	/* redisplay the cursor if we zapped it */
	if (!CursorDisplayed)
		DisplayCursor(CurrentCursor);
} /* end PrintText() */

extern int
PrintTextMask(text, textlen, font, srcpix, charpad, spacepad, dstx, dsty,
	clips, clipcount, func, zmask)
unsigned char	*text;
FONT		*font;
int		textlen, srcpix, charpad, spacepad, dstx, dsty;
CLIP		*clips;
int		clipcount, zmask;
register int	func;
{
	extern CURSOR		*CurrentCursor;
	extern			CursorDisplayed;
	int			cleft, ctop, cwidth, cheight;
	int			op;
	extern char		SSMap[];
	unsigned char		*limit = text + 
				    (textlen < MAXCHARS ? textlen : MAXCHARS);
	register int		w = 0;
	static struct pr_prpos	bat[MAXCHARS];
	int			bsize = 0, lheight, sbot, sright;

	SetZmask(PixRect, &zmask);
	private_bgcolor = -1;
	private_fgcolor = srcpix;
	if (PixRect->pr_depth == 1) {
		if ((srcpix & 1) == 0)
			func += 0x10;
		op = SUN_FROM_X_OP(SSMap[func]) & 
		    PIX_SRC | PIX_NOT(PIX_SRC) & PIX_DST;
	} else
		op = SUN_FROM_X_OP(func);
	if (PixRect->pr_depth > 1)
		op |= PIX_COLOR(srcpix);
	/* this is a gross abuse of C, but ... */
	{
		register struct pixfont		*pf;
		register struct pr_prpos	*p;
		register struct pixchar		*pc;
		register int			pwidth = 0;

		pf = (struct pixfont *) font->data;
		p = bat;
		lheight = pf->pf_defaultsize.y;
		if (charpad == 0 && spacepad == 0) {
			/* for each character in the text */
			while (text < limit) {
				pc = &(pf->pf_char[*text++]);
				if (pc == 0 || pc->pc_pr == NULL)
					continue;
				p->pr = pc->pc_pr;
				/* 
				 * pr_batchrop() is confusing... 
				 * you must give the offset for the previous
				 * pixrect 
				 */
				p->pos.x = pwidth;
				/* store the character width */
				pwidth = pc->pc_adv.x;
				/* increment the total width */
				w += pwidth;
				p++;
				bsize++;
			}
		} else {
			/* for efficiency ... */
			struct pixchar	*space = &(pf->pf_char[font->space]);

			while (text < limit) {
				pc = &(pf->pf_char[*text++]);
				if (pc == 0 || pc->pc_pr == NULL)
					continue;
				p->pr = pc->pc_pr;
				/* 
				 * pr_batchrop() is confusing... 
				 * you must give the offset for the previous
				 * pixrect 
				 */
				p->pos.x = pwidth;
				/* store the character width */
				pwidth = pc->pc_adv.x + charpad;
				/* add space? */
				if (pc == space)
					pwidth += spacepad;
				/* increment the total width */
				w += pwidth;
				p++;
				bsize++;
			}
		}
	} /* end gross abuse */
	/* determine right/bottom corner */
	sbot = dsty + lheight;
	sright = dstx + w;
	/* if the cursor is display where we wish to output -- zap it */
	if (CursorDisplayed) {
		extern DEVICE		*CurrentDevice;
		register vsCursor	*ms = CurrentDevice->mouse;
		register CURSOR		*cs = CurrentCursor;

		if (ms->y < sbot && 
		    ms->x < sright && 
		    ms->y + cs->height > dsty && 
		    ms->x + cs->width > dstx)
			DisplayCursor(NULL);
	}
	do {
		GetNextClip(clips, cleft, ctop, cwidth, cheight);
		if (dsty >= ctop && 
		    sbot <= ctop + cheight && 
		    dstx >= cleft && 
		    sright <= cleft + cwidth)
			pr_batchrop(PixRect, dstx - bat[0].pos.x, dsty,
				op | PIX_DONTCLIP, bat, bsize);
		else {
			struct pixrect	*region;
			if (dsty > ctop + cheight)
				continue;
			if (dsty + lheight <= ctop)
				continue;
			region = pr_region(PixRect, cleft, ctop, cwidth, 
			    cheight);
			pr_batchrop(region, dstx - cleft - bat[0].pos.x, 
			    dsty - ctop, op, bat, bsize);
			pr_destroy(region);
		}
	} while (--clipcount > 0);
	/* restore cursor if we zapped it */
	if (!CursorDisplayed)
		DisplayCursor(CurrentCursor);
	/* another gross abuse of C... */
	{
		static		allmask = -1;

		SetZmask(PixRect, &allmask);
	}
} /* end PrintTextMask() */


/*
 * Copyright (c) 1983 by Sun Microsystems, Inc.
 */

/*
 * Memory batchrop
 */


extern char pr_reversedst[];
extern struct pixrectops mem_ops;



#define MEMBATCH(IfClip, IfMask, op, IfReverse)				\
    for (; --count >= 0; src++) {					\
	dst.pos.x += src->pos.x;					\
	dp = dp0 + (((dskew = xoff0 + dst.pos.x) >> 3) & ~1);		\
	dskew &= 0xF;							\
	spr = src->pr;							\
	sizex = spr->pr_size.x;						\
	sizey = spr->pr_size.y;						\
	sprd = mpr_d(spr);						\
	if (sprd->md_linebytes != 2)					\
	    goto hard;							\
	sp = (u_short *) sprd->md_image;				\
	IfClip(	if (dst.pos.x + sizex > limx)				\
		    goto hard;						\
		if (dst.pos.y + sizey > limy)				\
		sizey = limy - dst.pos.y;				\
		if (dst.pos.x < 0)					\
		    goto hard;						\
		if (dst.pos.y < 0) {					\
		    sizey += dst.pos.y;					\
		    sp -= dst.pos.y;					\
		    dp -= pr_product(dst.pos.y, vert);			\
		}							\
		if (sizex <= 0)						\
		    continue;						\
	,)								\
	if (--sizey>=0)							\
	if (dskew + sizex <= 16) {					\
	    IfMask(	register short mask;				\
			mask = 0x8000;					\
			sizex -= 1;					\
			mask >>= sizex;					\
			((unsigned short) mask) >>= dskew;		\
			IfReverse(mask = ~mask;,),)			\
	    do {							\
		IfMask(*(u_short *) dp IfReverse(&,|)= mask;,)		\
		    *(u_short *) dp op (*sp++ >> dskew);		\
		dp += vert;						\
	    } while (--sizey != -1);					\
	}								\
	else {								\
	    IfMask(	register long mask;				\
			mask = 0x80000000;				\
			sizex -= 1;					\
			mask >>= sizex;					\
			((unsigned long) mask) >>= dskew;		\
			IfReverse(mask = ~mask;,),)			\
	    dskew = 16 - dskew;						\
	    do {							\
		IfMask(*(u_int *) dp IfReverse(&,|)= mask;,)		\
		*(u_int *) dp op (*sp++ << dskew);			\
		dp += vert;						\
	    } while (--sizey != -1);					\
	}								\
    }

#define MTRUE(a,b) a
#define MFALSE(a,b) b

#define ClippedOp(mask,op,revmask) \
    if(clip) MEMBATCH(MTRUE,mask,op,revmask) \
    else MEMBATCH(MFALSE,mask,op,revmask)

mem_batchrop(dst, op, src, count)
    struct pr_prpos dst;
    int         op;
    struct pr_prpos *src;
    short       count;
{
    register u_short *sp;
    register char *dp;
    char       *dp0;
    register char *handy;
    register short sizex, sizey;
    register    vert, dskew;
    int         dskew0, xoff0;
    int         errors = 0;
    int         clip, limx, limy;
    int         oppassed = op;

    /*
     * Preliminaries: get pixrect data and image pointers; decide whether
     * clipping.  If not clipping, normalize op, else compute limits for
     * cursors for later comparisons. 
     */

    clip = 0;
    if (!(op & PIX_DONTCLIP)) {
	clip = 1;
	limx = dst.pr->pr_size.x;
	limy = dst.pr->pr_size.y;
    }
    op = (op >> 1) & 0xf;	/* Kill dontclip, just keep useful */
    /*
     * If destination is reverse video, invert function. FIXME: we dont
     * deal with a reverse video source. Admittedly its unlikely that
     * anyone will call batchrop with a device pixrect as source (since we
     * copy the whole pixrect), but this is a bug. 
     */
    if (mpr_d(dst.pr)->md_flags & MP_REVERSEVIDEO)
	op = pr_reversedst[op];

    vert = mpr_d(dst.pr)->md_linebytes;
#define dprd ((struct mpr_data *)handy)
    dprd = mpr_d(dst.pr);
    xoff0 = dprd->md_offset.x;
    dp0 = (char *) ((int) dprd->md_image
		    + pr_product(dprd->md_linebytes,
				 dst.pos.y + dprd->md_offset.y));
#undef dprd
restart:
#define spr ((struct pixrect *)handy)
#define sprd ((struct mpr_data *)handy)
    switch (op) {
    case (PIX_SRC ^ PIX_DST) >> 1:
	ClippedOp(MFALSE, ^=, MTRUE);
	break;
    case PIX_SRC >> 1:
	ClippedOp(MTRUE, |=, MTRUE);
	break;
    case PIX_NOT(PIX_SRC) >> 1:
	ClippedOp(MTRUE, ^=, MFALSE);
	break;
    case (PIX_SRC | PIX_DST) >> 1:
	ClippedOp(MFALSE, |=, MTRUE);
	break;
    case (PIX_NOT(PIX_SRC) & PIX_DST) >> 1:
	ClippedOp(MFALSE, &=~, MTRUE);
	break;
    default:
	for (; --count >= 0; src++) {
	    dst.pos.x += src->pos.x;
	    errors |= mem_rop(dst.pr, dst.pos, src->pr->pr_size,
			      oppassed, src->pr, 0, 0);
	}
    }
    return errors;
hard:
    if (dst.pos.x + sizex <= 0)
	/*
	 * Completely clipped on left... 
	 */
	;
    else {
	errors |= mem_rop(dst.pr, dst.pos, src->pr->pr_size,
			  oppassed, src->pr, 0, 0);
    }
    src++;
    goto restart;
}

/*
 * cg2batch.c: Sun2 Color batchrop
 */

extern struct pixrectops mem_ops;


extern short mrc_lmasktable[];
extern short mrc_rmasktable[];

#define resolution unused, 0

cg2_batchrop(dst, op, src, count)
    struct pr_prpos dst;
    int         op;
    struct pr_prpos *src;
    register int count;
{
    register short sizey;
    register int tem, w, prime, linebytes;
    register short *bx, *leftx, *ma;
    register struct memropc *ropregs;
    short       sizex;
    int         clip;
    struct pixrect *pr;
    short      *ma_homey;
    short       homex, homey, limx, limy, dstx, dsty, by;

    struct cg2fb *fb;
    struct mpr_data *md;
    int         oppassed = op;
    int         errors = 0;
    short       color;

    if (count <= 0)
	return (0);

    /*
     * Preliminaries: get pixrect data and frame buffer pointers; decide
     * whether clipping.  If not clipping, normalize op, else compute
     * limits for cursors for later comparisons. 
     */
#define dbd ((struct cg2pr *)leftx)
    dbd = cg2_d(dst.pr);
    homex = dbd->cgpr_offset.x;
    homey = dbd->cgpr_offset.y;
#define FB ((struct cg2fb *)leftx)
    FB = dbd->cgpr_va;
    fb = FB;
#undef dbd
    ropregs = &FB->ropcontrol[CG2_ALLROP].ropregs;
    if (op & PIX_DONTCLIP) {
	op &= ~PIX_DONTCLIP;
	clip = 0;
    }
    else {
	clip = 1;
	limx = homex + dst.pr->pr_size.x;
	limy = homey + dst.pr->pr_size.y;
    }
    dstx = homex + dst.pos.x;
    dsty = homey + dst.pos.y;
    if (private_bgcolor < 0) {
	FB->ppmask.reg = private_fgcolor;	/* set colored text */
	ropregs->mrc_pattern = -1;
	FB->ppmask.reg = ~private_fgcolor;
	ropregs->mrc_pattern = 0;
	FB->ppmask.reg = private_czmask;
	switch (op & 0x1E) {
	case PIX_SRC ^ PIX_DST:
	    ropregs->mrc_op = CG_SRC & (CG_MASK ^ CG_DEST) | ~CG_SRC & CG_DEST;
	    break;
	case PIX_NOT(PIX_DST):
	    ropregs->mrc_op = CG_SRC & (~CG_DEST) | ~CG_SRC & CG_DEST;
	    break;
	default:
	    ropregs->mrc_op = CG_SRC & CG_MASK | ~CG_SRC & CG_DEST;
	    break;
	}
    }
    else {
	FB->ppmask.reg = private_fgcolor & private_bgcolor;
	ropregs->mrc_op = ~0;
	FB->ppmask.reg = ~(private_fgcolor | private_bgcolor);
	ropregs->mrc_op = 0;
	FB->ppmask.reg = private_fgcolor & ~private_bgcolor;
	ropregs->mrc_op = CG_SRC;
	FB->ppmask.reg = ~private_fgcolor & private_bgcolor;
	ropregs->mrc_op = ~CG_SRC;
	FB->ppmask.reg = private_czmask;
    }
    FB->status.reg.ropmode = PWWWRD;
    linebytes = cg2_linebytes(FB, PWWWRD);
#undef FB

    for (; --count >= 0; src++) {
	/*
	 * Update destination x and y by pre-advance amount. If no pixrect
	 * for this, then skip to next. 
	 */
	dstx += src->pos.x;
	pr = src->pr;
	if (pr == 0)
	    continue;
	sizex = pr->pr_size.x;
	sizey = pr->pr_size.y;
	md = mpr_d(pr);
	ma = md->md_image;

	/*
	 * Grab sizes and address of image.  If clipping (rare case
	 * hopefully) compare cursors against limits. 
	 */
	by = dsty;
	tem = 0;
	if (clip) {
	    if (dstx + sizex > limx)
		sizex = limx - dstx;
	    if (dsty + sizey > limy)
		sizey = limy - dsty;
	    if (dsty < homey) {	/* works if pr_depth = 1! */
		tem = homey - dsty;
		by += tem;
		ma += pr_product(tem, (md->md_linebytes >> 1));
		sizey -= tem;
		tem = 0;
	    }
	    if (dstx < homex) {
		tem = homex - dstx;
		ma += tem >> 4;
		sizex -= tem;
	    }
	    if (sizex <= 0)
		continue;
	}

	/*
	 * Hard case: characters greater than 16 wide. 
	 */
	ma_homey = ma;

	/* set the ROP chip word width and opcount */

	w = cg2_prskew(dstx);	/* source skew is 0 */
	ropregs->mrc_shift = (w & 0xF) | (1 << 8);
	prime = !w;
	w = (sizex + w + (tem & 0xF) - 1) >> 4;
	ropregs->mrc_width = w;
	ropregs->mrc_opcount = w;

	/* set the ROP chip end masks */

	ropregs->mrc_mask1 =
	    mrc_lmasktable[(tem += dstx) & 0xf];
	ropregs->mrc_mask2 =
	    mrc_rmasktable[(sizex + tem - 1) & 0xf];

	leftx = cg2_ropwordaddr(fb, 0, tem, by);
	tem = md->md_linebytes;
	if (--sizey >= 0)
	    if (w) {
		w++;
		do {
		    register short i = w;
		    ma = ma_homey;
		    bx = leftx;
		    if (prime)
			ropregs->mrc_source1 = *ma++;
		    while (i--)
			*bx++ = *ma++;
		    (char *) ma_homey += tem;
		    (char *) leftx += linebytes;
		} while (--sizey != -1);
	    }
	    else {
		bx = leftx;
		ma = ma_homey;
		if (prime) {
		    ma++;
		    do {
			ma--;
			ropregs->mrc_source1 = *ma++;
			*bx = *ma;
			(char *) ma += tem;
			(char *) bx += linebytes;
		    } while (--sizey != -1);
		}
		else
		    do {
			*bx = *ma;
			(char *) ma += tem;
			(char *) bx += linebytes;
		    } while (--sizey != -1);
	    }
    }

    return (errors);
}

#endif	sun
