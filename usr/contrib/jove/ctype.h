/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

/* The code in this file was snarfed from ctype.h and modified for JOVE. */

#define	_U	01
#define	_L	02
#define	_N	04
#define _P	010
#define _C	020
#define _W	040
#define _Op	0100
#define _Cl	0200

extern int	SyntaxTable;
#define iswhite(c)	(isspace(c))
#define isword(c)	((CharTable[SyntaxTable])[c]&(_W))
#define	isalpha(c)	((CharTable[SyntaxTable])[c]&(_U|_L))
#define	isupper(c)	((CharTable[SyntaxTable])[c]&_U)
#define	islower(c)	((CharTable[SyntaxTable])[c]&_L)
#define	isdigit(c)	((CharTable[SyntaxTable])[c]&_N)
#define	isspace(c)	(c == ' ' || c == '\t')
#define ispunct(c)	((CharTable[SyntaxTable])[c]&_P)
#define toupper(c)	((c)&~040)
#define tolower(c)	((c)|040)
#define toascii(c)	((c)&0177)
#define isctrl(c)	((CharTable[0][c&0177])&_C)
#define isopenp(c)	((CharTable[0][c&0177])&_Op)
#define isclosep(c)	((CharTable[0][c&0177])&_Cl)
#define has_syntax(c,s)	((CharTable[SyntaxTable][c&0177])&s)
#define WITH_TABLE(x) \
{ \
	int	push = SyntaxTable; \
	SyntaxTable = x;

#define END_TABLE() \
	SyntaxTable = push; \
}
