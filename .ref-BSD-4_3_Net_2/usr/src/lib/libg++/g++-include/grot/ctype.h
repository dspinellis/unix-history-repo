// Here's a ctype.h for SunOS-3 and vax 4.3BSD.  
// It will probably work on most BSD derived systems. 
// Just compare it to the C version to verify.
// No big deal, but it will save you some typing.
   
#ifndef _ctype_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _ctype_h

int isalpha(char c);
int isupper(char c);
int islower(char c);
int isdigit(char c);
int isxdigit(char c);
int isspace(char c);
int ispunct(char c);
int isalnum(char c);
int isprint(char c);
int isgraph(char c);
int iscntrl(char c);
int isascii(char c);
int toupper(char c);
int tolower(char c);
int toascii(char c);

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

#include <stdio.h>  /* sorry, but needed for USG stuff */
   
static const int _U = 01;
static const int _L = 02;
static const int _N = 04;
static const int _S = 010;
static const int _P = 020;
static const int _C = 040;


#if defined(USG) || defined(DGUX)
static const int _B = 0100;	/* different from BSD */
static const int _X = 0200;	/* different from BSD */
#else
static const int _X = 0100;
static const int _B = 0200;
#endif


#ifdef DGUX
#define CTYPE_TYPE	short
#else
#define CTYPE_TYPE	char
#endif

#if defined(DGUX) || defined(USG) || defined(hpux)
#define _ctype_ _ctype
#endif

extern "C" {
extern	CTYPE_TYPE	_ctype_[];
}
   
/*
*	The following bit of ugliness is to ensure that the __ctype__[]
*	initialization is brought in from the VAX-11 "C" runtime library
*/
#ifdef VMS
#define	_ctype_	$$PsectAttributes_NOWRT$$_ctype_
extern	char	_ctype_[];
extern "C" {
   extern c$v_ctypedefs();
   static __ctype__dummy(){c$v_ctypedefs();}
   }
#endif


#ifdef VMS
inline int isalpha(char c)  { return ((_ctype_)[c]&(_U|_L)); }
inline int isupper(char c)  { return ((_ctype_)[c]&_U); }
inline int islower(char c)  { return ((_ctype_)[c]&_L); }
inline int isdigit(char c)  { return ((_ctype_)[c]&_N); }
inline int isxdigit(char c) { return ((_ctype_)[c]&_X); }
inline int isspace(char c)  { return ((_ctype_)[c]&_S); }
inline int ispunct(char c)  { return ((_ctype_)[c]&_P); }
inline int isalnum(char c)  { return ((_ctype_)[c]&(_U|_L|_N)); }
inline int isprint(char c)  { return ((_ctype_)[c]&(_P|_U|_L|_N|_B)); }
inline int isgraph(char c)  { return ((_ctype_)[c]&(_P|_U|_L|_N)); }
inline int iscntrl(char c)  { return ((_ctype_)[c]&_C); }
inline int isascii(char c)  { return ((unsigned)(c)<=0177); }
inline int toupper(char c)  { return ((c)-'a'+'A'); }
inline int tolower(char c)  { return ((c)-'A'+'a'); }
inline int toascii(char c)  { return ((c)&0177); }
#else
inline int isalpha(char c)  { return ((_ctype_+1)[c]&(_U|_L)); }
inline int isupper(char c)  { return ((_ctype_+1)[c]&_U); }
inline int islower(char c)  { return ((_ctype_+1)[c]&_L); }
inline int isdigit(char c)  { return ((_ctype_+1)[c]&_N); }
inline int isxdigit(char c) { return ((_ctype_+1)[c]&_X); }
inline int isspace(char c)  { return ((_ctype_+1)[c]&_S); }
inline int ispunct(char c)  { return ((_ctype_+1)[c]&_P); }
inline int isalnum(char c)  { return ((_ctype_+1)[c]&(_U|_L|_N)); }
inline int isprint(char c)  { return ((_ctype_+1)[c]&(_P|_U|_L|_N|_B)); }
inline int isgraph(char c)  { return ((_ctype_+1)[c]&(_P|_U|_L|_N)); }
inline int iscntrl(char c)  { return ((_ctype_+1)[c]&_C); }
inline int isascii(char c)  { return ((unsigned)(c)<=0177); }
inline int toupper(char c)  { return islower(c)? (c-'a'+'A') : c; }
inline int tolower(char c)  { return isupper(c)? (c-'A'+'a') : c; }
inline int toascii(char c)  { return ((c)&0177); }
#endif

#ifdef _ctype_
#undef _ctype_
#endif

#ifdef CTYPE_TYPE
#undef CTYPE_TYPE
#endif

#endif /* __OPTIMIZE__ */

#endif _ctype_h
