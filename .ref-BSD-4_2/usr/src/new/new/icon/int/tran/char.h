#

/*
 * macros for determining character type
 *
 * the table _ctype (in char.c) classifies each character
 * in one of the categories defined below
 */

#define _S       01             /* space */
#define _U       02             /* upper case */
#define _L       04             /* lower case */
#define _A      010             /* other alphabetic (e.g. "_") */
#define _N      020             /* digit */

#define _UL     (_U | _L)       /* letter */
#define _ULA    (_UL | _A)      /* alphabetic */
#define _ULAN   (_ULA | _N)     /* alphanumeric */

#define isspace(c)      (_ctype[c] & _S)
#define isupper(c)      (_ctype[c] & _U)
#define islower(c)      (_ctype[c] & _L)
#define isletter(c)     (_ctype[c] & _UL)
#define isalpha(c)      (_ctype[c] & _ULA)
#define isdigit(c)      (_ctype[c] & _N)
#define isalnum(c)      (_ctype[c] & _ULAN)

#define toupper(c)      (islower(c)? (c - ('a'-'A')) : c)
#define tolower(c)      (isupper(c)? (c + ('a'-'A')) : c)
#define tonum(c)        (isdigit(c)? (c - '0') : ((c & 037) + 9))

extern char _ctype[];
