/* m- file for Mips running RISCOS version 4.  */

#include "m-mips.h"

/* Define MIPS2 if you have an R6000 or R4000.  */
/* #define MIPS2 */

#ifdef MIPS2
#define C_DEBUG_SWITCH -systype bsd43 -O -Olimit 791 -g3 -mips2
#else
#define C_DEBUG_SWITCH -systype bsd43 -O -Olimit 791 -g3
#endif

#ifdef TERMINFO
#undef TERMINFO
#endif

#define START_FILES pre-crt0.o /lib/crt1.o
#define LIB_STANDARD -lmld -lc /lib/crtn.o


#define COFF
#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE -systype bsd43 -g3 -D 800000
