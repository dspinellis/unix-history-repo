/*
 *	Copyright 1984, 1985 by the Regents of the University of
 *	California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */


/* defines and defines to describe how to deal with the screen */

#define LINESIZE	80
#define NUMBERLINES	24
#define SCREENSIZE	(LINESIZE*NUMBERLINES)
#define LowestScreen()	0
#define HighestScreen()	(SCREENSIZE-1)

#define ScreenLineOffset(x)	((x)%LINESIZE)
#define ScreenLine(x)	((int)((x)/LINESIZE))
#define ScreenInc(x)	(((x)==HighestScreen())? LowestScreen():x+1)
#define ScreenDec(x)	(((x)==LowestScreen())? HighestScreen():x-1)
#define ScreenUp(x)	(((x)+(SCREENSIZE-LINESIZE))%SCREENSIZE)
#define ScreenDown(x)	(((x)+LINESIZE)%SCREENSIZE)
#define IsOrder(x)	((x) && ((x) < 0x40) && (\
			    ((x) == ORDER_SF) || \
			    ((x) == ORDER_SBA) || \
			    ((x) == ORDER_IC) || \
			    ((x) == ORDER_PT) || \
			    ((x) == ORDER_RA) || \
			    ((x) == ORDER_EUA) || \
			    ((x) == ORDER_YALE)))
#define BAIC(x)		((x)&0x3f)
#define CIAB(x)		(CIABuffer[(x)&0x3f])
#define BufferTo3270_0(x)	(CIABuffer[(int)((x)/0x40)])
#define BufferTo3270_1(x)	(CIABuffer[(x)&0x3f])
#define Addr3270(x,y)	(BAIC(x)*64+BAIC(y))
#define SetBufferAddress(x,y)	((x)*LINESIZE+(y))

/* These know how fields are implemented... */

#define	FieldInc(p)	FieldFind(FieldForward, p, LowestScreen())
#define	FieldDec(p)	(HighestScreen() - \
				FieldFind(FieldReverse, \
					HighestScreen()-p, HighestScreen()))
#define WhereAttrByte(p)	(IsStartField(p)? p: FieldDec(p))
#define	WhereHighByte(p)	ScreenDec(FieldInc(p))
#define WhereLowByte(p)		ScreenInc(WhereAttrByte(p))
#define FieldAttributes(x)	(IsStartField(x)? Host[x].field&0xff : \
				    Host[WhereAttrByte(x)].field&0xff)
#define TermAttributes(x)	(TermIsStartField(x)? Terminal[x].field&0xff : \
				    Terminal[WhereTermAttrByte(x)].field&0xff)
#define TurnOffMdt(x)	(Host[WhereAttrByte(x)].field &= ~ATTR_MDT)
#define TurnOnMdt(x)	(Host[WhereAttrByte(x)].field |= ATTR_MDT)
#define HasMdt(x)	(Host[x].field&ATTR_MDT)	/* modified tag */

#define IsStartField(x)	(Host[x].field&ATTR_MASK)	/* field starts here */
#define TermIsStartField(x)	(Terminal[x].field&ATTR_MASK)
#define NewField(p,a)	(Host[p].field = (a)|ATTR_MASK, \
			    FieldForward[p] = FieldReverse[SCREENSIZE-p-1] = 1)
#define TermNewField(p,a)	(Terminal[p].field = (a)|ATTR_MASK)
#define DeleteField(p)	(Host[p].field = 0, \
			    FieldForward[p] = FieldReverse[SCREENSIZE-p-1] = 0)
#define TermDeleteField(p)	(Terminal[p].field = 0)
#define	DeleteAllFields()	(bzero(FieldForward, sizeof FieldForward), \
				    bzero(FieldReverse, sizeof FieldReverse))


/* The following are independent of the implementation of fields */
#define IsProtectedAttr(p,a)	(IsStartField(p) || ((a)&ATTR_PROT))
#define IsProtected(p)	IsProtectedAttr(p,FieldAttributes(p))

#define IsUnProtected(x)	(!IsProtected(x))

#define IsAutoSkip(x)	(FieldAttributes(x)&ATTR_AUTO_SKIP)

#define IsNonDisplayAttr(c)	(((c)&ATTR_DSPD_MASK) == ATTR_DSPD_NONDISPLAY)
#define	IsNonDisplay(p)	IsNonDisplayAttr(FieldAttributes(p))

#define IsHighlightedAttr(c) \
		(((c)&ATTR_DSPD_MASK) == ATTR_DSPD_HIGH)
#define	IsHighlighted(p) \
		(IsHighlightedAttr(FieldAttributes(p)) && !IsStartField(p))

#define TermIsNonDisplay(x)	\
		    ((TermAttributes(x)&ATTR_DSPD_MASK) == ATTR_DSPD_NONDISPLAY)
#define TermIsHighlighted(x) \
		(((TermAttributes(x)&ATTR_DSPD_MASK) == ATTR_DSPD_HIGH) \
				    && !TermIsStartField(x))

#define TerminalCharacterAttr(c,p,a)	(IsNonDisplayAttr(a) ? ' ':c)
#define TerminalCharacter(c,p)	TerminalCharacterAttr(c,p,FieldAttributes(p))

#define NeedToRedisplayFields(p) ((TermIsNonDisplay(p) != IsNonDisplay(p)) || \
				(TermIsHighlighted(p) != IsHighlighted(p)))
#define NeedToRedisplayFieldsAttr(p,c) ( \
			(TermIsNonDisplay(p) != IsNonDisplayAttr(c)) || \
			(TermIsHighlighted(p) != IsHighlightedAttr(c)))

#define NotVisuallyCompatibleAttributes(p,c,d) ( \
			(IsNonDisplayAttr(c) != IsNonDisplayAttr(d)) || \
			(IsHighlightedAttr(c) != IsHighlightedAttr(d)))

#define NeedToRedisplayAttr(c,p,a) \
			((c != GetTerminal(p)) || NeedToRedisplayFieldsAttr(p,a))
#define NeedToRedisplay(c,p)	NeedToRedisplayAttr(c,p,FieldAttributes(p))

#define MAX(x,y)	((x)<(y)? (y):(x))
#define MIN(x,y)	((x)<(y)? x:(y))

#define GetHost(i)	Host[i].data
#define SetHost(i,c)	(Host[i].data = c)

#define GetTerminal(i)		Terminal[i].data
#define SetTerminal(i,c)	(Terminal[i].data = c)

struct {
    char	data,	/* data at this position */
		field;	/* field attributes of this location if ATTR_MASK */
}	Host[SCREENSIZE],		/* host view of screen */
	Terminal[SCREENSIZE];

char	FieldForward[SCREENSIZE],	/* non-zero for SF, 0..1919 */
	FieldReverse[SCREENSIZE];	/* non-zero for SF, 1919..0 */


int CursorAddress;			/* where cursor is */
int BufferAddress;			/* where writes are going */

int Lowest, Highest;

/* the Following are globals */

extern char CIABuffer[];

int UnLocked;		/* is the keyboard unlocked */
int AidByte;

int Initialized;	/* are we initialized? */
