/* Definitions for Intel 386 running ESIX Unix System V.  */
/* Just like SCO support except don't use their special library and
   predefined symbol.  */

/* Mostly it's like AT&T Unix System V. */

#include "tm-i386v.h"

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}"

#define ENDFILE_SPEC "crtn.o%s"

/* Library spec.  */

#undef LIB_SPEC
#define LIB_SPEC \
 "%{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp} -lc"

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
"-Dunix -Di386 -DM_UNIX -DM_I386 -DM_COFF -DM_WORDSWAP"

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE}"

/* SCO's assember doesn't grok '$' in labels (for g++) */

#define NO_DOLLAR_IN_LABEL
