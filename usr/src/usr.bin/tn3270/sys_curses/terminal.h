/*
 *	@(#)screen.h	3.1  10/29/86
 */

#define	INCLUDED_TERMINAL

#if	defined(SLOWSCREEN)

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

#endif	/* defined(SLOWSCREEN) */

#define GetTerminal(i)		GetGeneric(i, Terminal)
#define GetTerminalPointer(p)	GetGenericPointer(p)
#define SetTerminal(i,c)	SetGeneric(i,c,Terminal)
