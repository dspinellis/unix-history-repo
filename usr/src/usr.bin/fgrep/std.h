/* std.h - automagically adapt to old and new compilers.
   In the Public Domain; written by Mike Haertel. */

#if __STDC__

#include <stddef.h>
#include <limits.h>

typedef void *PTR;
typedef const void *PTRCONST;

#define AND ,
#define DEFUN(F, L, P) F(P)
#define EXFUN(F, P) F P

#else

#define const
#define volatile

/* The following approximations of <stddef.h> and <limits.h> are appropriate
   for most machines. */
typedef int ptrdiff_t;
typedef unsigned int size_t;
#define NULL 0
#define offsetof(T, M) ((size_t)&((T *) 0)->(M))

#define CHAR_BIT 8
#define SCHAR_MIN -128
#define SCHAR_MAX 127
#define UCHAR_MAX 255
#define CHAR_MIN ((char) UCHAR_MAX < 0 ? SCHAR_MIN : 0)
#define CHAR_MAX ((char) UCHAR_MAX < 0 ? SCHAR_MAX : UCHAR_MAX)
#define SHRT_MIN -32768
#define SHRT_MAX 32767
#define USHRT_MAX 65535
#define INT_MIN (sizeof (int) == 2 ? -32768 : -2147483648)
#define INT_MAX (sizeof (int) == 2 ? 32767 : 2147483647)
#define UINT_MAX (sizeof (int) == 2 ? 65535 : 4294967295)
#define LONG_MIN -2147483648L
#define LONG_MAX 2147483647L
#define ULONG_MAX 4294967295

typedef char *PTR;
typedef const char *PTRCONST;

#define AND ;
#define DEFUN(F, L, P) F L P ;
#define EXFUN(F, P) F()

#endif

/* Deal with <ctype.h> lossage. */
#include <ctype.h>

#ifndef isascii

#define ISALNUM(C) isalnum(C)
#define ISALPHA(C) isalpha(C)
#define ISCNTRL(C) iscntrl(C)
#define ISDIGIT(C) isdigit(C)
#define ISGRAPH(C) isgraph(C)
#define ISLOWER(C) islower(C)
#define ISPRINT(C) isprint(C)
#define ISPUNCT(C) ispunct(C)
#define ISSPACE(C) isspace(C)
#define ISUPPER(C) isupper(C)
#define ISXDIGIT(C) isxdigit(C)
#define TOLOWER(C) tolower(C)
#define TOUPPER(C) toupper(C)

#else

#define ISALNUM(C) (isascii(C) && isalnum(C))
#define ISALPHA(C) (isascii(C) && isalpha(C))
#define ISCNTRL(C) (isascii(C) && iscntrl(C))
#define ISDIGIT(C) (isascii(C) && isdigit(C))
#define ISGRAPH(C) (isascii(C) && isgraph(C))
#define ISLOWER(C) (isascii(C) && islower(C))
#define ISPRINT(C) (isascii(C) && isprint(C))
#define ISPUNCT(C) (isascii(C) && ispunct(C))
#define ISSPACE(C) (isascii(C) && isspace(C))
#define ISUPPER(C) (isascii(C) && isupper(C))
#define ISXDIGIT(C) (isascii(C) && isxdigit(C))
#define TOLOWER(C) (ISUPPER(C) ? tolower(C) : (C))
#define TOUPPER(C) (ISLOWER(C) ? toupper(C) : (C))

#endif

/* Declaring this here should be safe.  Some losing <errno.h>'s don't. */
extern int errno;

/* Adapt variable arguments to new implementations (with <stdarg.h>)
   or old (which are assumed to have <varargs.h>). */

#if __STDC__

#include <stdarg.h>

#define VA_ALIST ...
#define VA_DCL ...
#define VA_LIST va_list
#define VA_START(AP, LASTARG) va_start(AP, LASTARG)
#define VA_ARG(AP, TYPE) va_arg(AP, TYPE)
#define VA_END(AP) va_end(AP)

#define VA_DEFUN(F, L, P) F(P)

#else

#include <varargs.h>

#define VA_ALIST va_alist
#define VA_DCL va_dcl
#define VA_LIST va_list
#define VA_START(AP, LASTARG) va_start(AP)
#define VA_ARG(AP, TYPE) va_arg(AP, TYPE)
#define VA_END(AP) va_end(AP)

#define VA_DEFUN(F, L, P) F L P

#endif

/* Declarations of traditional library routines. */
extern double EXFUN(atof, (const char *));
extern int EXFUN(atoi, (const char *));
extern long int EXFUN(atol, (const char *));
extern int EXFUN(rand, (void));
extern void EXFUN(srand, (int));
extern PTR EXFUN(calloc, (size_t, size_t));
extern void EXFUN(free, (PTR));
extern PTR EXFUN(malloc, (size_t));
extern PTR EXFUN(realloc, (PTR, size_t));
extern void EXFUN(abort, (void));
extern void EXFUN(exit, (int));
extern char *EXFUN(getenv, (const char *));
extern int EXFUN(system, (const char *));
extern void EXFUN(qsort, (PTR, size_t, size_t,
			  int EXFUN((*), (PTRCONST, PTRCONST))));
extern int EXFUN(abs, (int));
extern long int EXFUN(labs, (long int));

#ifdef X_strerror
extern char *EXFUN(strerror, (int));
#endif
