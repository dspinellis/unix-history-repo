/* m-news-risc.h is for the "RISC News".  */

#include "m-mips.h"
#undef LIBS_MACHINE

#define LIBS_MACHINE -lmld

#define COFF
#undef LD_SWITCH_MACHINE
#define LD_SWITCH_MACHINE -x -D 800000

/* #define C_OPTIMIZE_SWITCH -O2 */
#define C_OPTIMIZE_SWITCH -O

#define C_DEBUG_SWITCH -g3

#undef TERMINFO
