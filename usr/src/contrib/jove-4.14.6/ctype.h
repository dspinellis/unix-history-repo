/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define	C_UPPER	01	/* UPPER case */
#define	C_LOWER	02	/* LOWER case */
#define	C_DIGIT	04	/* DIGIT */
#define	C_PUNCT	010	/* PUNCTuation */
#define	C_CTRL	020	/* ConTRoL */
#define	C_WORD	040	/* WORD */
#define	C_BRA	0100	/* open BRAket */
#define	C_KET	0200	/* close braKET */

extern const unsigned char	*SyntaxTable;	/* CharTable[?] */
#define	jiswhite(c)	(jisspace(c))
#define	jisword(c)	(SyntaxTable[c]&C_WORD)
#define	jisalpha(c)	(SyntaxTable[c]&(C_UPPER|C_LOWER))
#define	jisupper(c)	(SyntaxTable[c]&C_UPPER)
#define	jislower(c)	(SyntaxTable[c]&C_LOWER)
#define	jisdigit(c)	(SyntaxTable[c]&C_DIGIT)
#define	jisspace(c)	((c) == ' ' || (c) == '\t')
/* #define	jispunct(c)	(SyntaxTable[c]&C_PUNCT) */

#define	has_syntax(c,s)	(SyntaxTable[(c)&CHARMASK]&(s))


/* #define	toascii(c)	((c)&CHARMASK) */
#define	jiscntrl(c)	((CharTable[0][c&CHARMASK])&C_CTRL)
#define	jisopenp(c)	((CharTable[0][c&CHARMASK])&C_BRA)
#define	jisclosep(c)	((CharTable[0][c&CHARMASK])&C_KET)

#ifdef	ASCII7
# define	jtolower(c)	((c)|040)
#else	/* !ASCII7 */
  extern char jtolower proto((int /*char*/));
#endif	/* !ASCII7 */

#define	WITH_TABLE(x) \
{ \
	const unsigned char	*push = SyntaxTable; \
	SyntaxTable = CharTable[(x)];

#define	END_TABLE() \
	SyntaxTable = push; \
}

extern const unsigned char	CharTable[NMAJORS][NCHARS];
extern const char	RaiseTable[NCHARS];
#define	CharUpcase(c)	(RaiseTable[c])
