/* m- file for Sun 68000's OPERATING SYSTEM version 3
   (for either 68000 or 68020 systems).  */

#include "m-sun2.h"
#undef sun2
#ifndef sun3
#define sun3
#endif

/* Say that the text segment of a.out includes the header;
   the header actually occupies the first few bytes of the text segment
   and is counted in hdr.a_text.  */

#define A_TEXT_OFFSET(HDR) sizeof (HDR)

/* In case we are using floating point, work together with crt0.c.  */

#ifndef __GNUC__
#define C_SWITCH_MACHINE -fsoft
#endif

/* This line is needed if you are linking with X windows
   and the library xlib was compiled to use the 68881.
   For maximum cleanliness, don't edit this file;
   instead, insert this line in config.h.  */
/* #define START_FILES crt0.o /usr/lib/Mcrt1.o  */
