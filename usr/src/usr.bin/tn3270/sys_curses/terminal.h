/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)terminal.h	3.2 (Berkeley) %G%
 */

#define	INCLUDED_TERMINAL

/*
 * In the situation where we have a copy of the terminal screen in front
 * of us, here are some macros to deal with them.
 */

#define TermAttributes(x)	(TermIsStartField(x)? GetTerminal(x)&0xff : \
				    GetTerminal(WhereTermAttrByte(x))&0xff)
#define TermIsStartField(x)	((GetTerminal(x)&ATTR_MASK) == ATTR_MASK)
#define TermNewField(p,a)	SetTerminal(p, (a)|ATTR_MASK)
#define TermDeleteField(p)	SetTerminal(p, 0)
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


#define GetTerminal(i)		GetGeneric(i, Terminal)
#define GetTerminalPointer(p)	GetGenericPointer(p)
#define SetTerminal(i,c)	SetGeneric(i,c,Terminal)
