/*
 * Macros for determining character type.
 *
 * The table chrtype (in char.c) classifies each character
 *  in one of the categories defined below.
 */

#define	_U	01		/* upper case */
#define	_L	02		/* lower case */
#define	_N	04		/* digit */
#define	_S	010		/* space */
#define	_P	020		/* punctuation */
#define	_C	040		/* control */
#define	_X	0100		/* hex digit (a-f) */

extern	char	ctype[];

#define isalpha(c)	(ctype[(c)&0377]&(_U|_L)) 
#define isupper(c)	(ctype[(c)&0377]&_U) 
#define islower(c)	(ctype[(c)&0377]&_L) 
#define isdigit(c)	(ctype[(c)&0377]&_N) 
#define isxdigit(c)	(ctype[(c)&0377]&(_N|_X)) 
#define isspace(c)	(ctype[(c)&0377]&_S) 
#define ispunct(c)	(ctype[(c)&0377]&_P) 
#define isalnum(c)	(ctype[(c)&0377]&(_U|_L|_N)) 
#define isprint(c)	(ctype[(c)&0377]&(_P|_U|_L|_N)) 
#define iscntrl(c)	(ctype[(c)&0377]&_C) 
#define isascii(c)	((unsigned)(c)<=0177) 
#define toupper(c)	(((c)&0377)-'a'+'A') 
#define tolower(c)	(((c)&0377)-'A'+'a') 
#define toascii(c)	(((c)&0177) 
#define tonum(c)	(isdigit(c)?(c)-'0':10+(((c)|(040))-'a')) 
