#define USG

#include "xm-m68k.h"

#define bcopy(a,b,c) memcpy (b,a,c)
#define bzero(a,b) memset (a,0,b)
#define bcmp(a,b,c) memcmp (a,b,c)

/* If compiling with HPUX compiler, we are probably using alloca.c,
   so help it work right.  */
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif
