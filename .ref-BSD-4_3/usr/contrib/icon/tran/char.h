/*
 * Macros for determining character type.
 *
 * The table chrtype (in char.c) classifies each character
 *  in one of the categories defined below.
 */

#define _S	 01		/* space */
#define _U	 02		/* upper case */
#define _L	 04		/* lower case */
#define _A	010		/* other alphabetic (e.g. "_") */
#define _N	020		/* digit */

#define _UL	(_U | _L)	/* letter */
#define _ULA	(_UL | _A)	/* alphabetic */
#define _ULAN	(_ULA | _N)	/* alphanumeric */

#define isspace(c)	(chrtype[c] & _S) 
#define isupper(c)	(chrtype[c] & _U) 
#define islower(c)	(chrtype[c] & _L) 
#define isletter(c)	(chrtype[c] & _UL) 
#define isalpha(c)	(chrtype[c] & _ULA) 
#define isdigit(c)	(chrtype[c] & _N) 
#define isalnum(c)	(chrtype[c] & _ULAN) 

#define toupper(c)	(islower(c)? (c - ('a'-'A')) : c) 
#define tolower(c)	(isupper(c)? (c + ('a'-'A')) : c) 
#define tonum(c)	(isdigit(c)? (c - '0') : ((c & 037) + 9)) 

extern char chrtype[];
