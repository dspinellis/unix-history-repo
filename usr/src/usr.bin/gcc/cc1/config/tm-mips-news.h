/* like pmax except BIG ENDIAN instead of LITTLE ENDIAN  */

#define DECSTATION
#include "tm-mips.h"

#undef CPP_SPEC
				/* default RISC NEWS environment */
#define CPP_SPEC "-Dr3000 -DLANGUAGE_C -DMIPSEB -DSYSTYPE_BSD -Dsony_news -Dunix -I/usr/include2.11"

#undef MACHINE_TYPE
#define MACHINE_TYPE "Sony NEWS (RISC NEWS)"

/* Define this if most significant byte of a word is the lowest numbered.
*/
#define BYTES_BIG_ENDIAN

/* Define this if most significant word of a multiword number is numbered.
*/
#define WORDS_BIG_ENDIAN
