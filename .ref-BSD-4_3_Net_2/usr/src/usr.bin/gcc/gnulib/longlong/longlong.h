/* More subroutines needed by GCC output code on some machines.  */
/* Compile this one with gcc.  */

#include "config.h"
#include <stddef.h>

#ifndef SItype
#define SItype long int
#endif

/* long long ints are pairs of long ints in the order determined by
   WORDS_BIG_ENDIAN.  */

#ifdef WORDS_BIG_ENDIAN
  struct longlong {long high, low;};
#else
  struct longlong {long low, high;};
#endif

/* We need this union to unpack/pack longlongs, since we don't have
   any arithmetic yet.  Incoming long long parameters are stored
   into the `ll' field, and the unpacked result is read from the struct
   longlong.  */

typedef union
{
  struct longlong s;
  long long ll;
  SItype i[2];
  unsigned SItype ui[2];
} long_long;

/* Internally, long long ints are strings of unsigned shorts in the
   order determined by BYTES_BIG_ENDIAN.  */

#define B 0x10000
#define low16 (B - 1)

#ifdef BYTES_BIG_ENDIAN

/* Note that HIGH and LOW do not describe the order
   of words in a long long.  They describe the order of words
   in vectors ordered according to the byte order.  */

#define HIGH 0
#define LOW 1

#define big_end(n)	0 
#define little_end(n)	((n) - 1)
#define next_msd(i)	((i) - 1)
#define next_lsd(i)	((i) + 1)
#define is_not_msd(i,n)	((i) >= 0)
#define is_not_lsd(i,n)	((i) < (n))

#else

#define LOW 0
#define HIGH 1

#define big_end(n)	((n) - 1)
#define little_end(n)	0 
#define next_msd(i)	((i) + 1)
#define next_lsd(i)	((i) - 1)
#define is_not_msd(i,n)	((i) < (n))
#define is_not_lsd(i,n)	((i) >= 0)

#endif
