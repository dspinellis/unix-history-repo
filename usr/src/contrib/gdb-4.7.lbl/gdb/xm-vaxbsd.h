/* Definitions to make GDB run on a vax under 4.2bsd. */

/* We have to include these files now, so that GDB will not make
   competing definitions in defs.h.  */
#include <machine/endian.h>
#include <machine/limits.h>
#include "xm-vax.h"

/* In non-ANSI compiles, memcpy and memset are still void *, not char *.  */
#define MEM_FNS_DECLARED
