/* m- file for Integrated Solutions 386 machine.  */

#include "m-intel386.h"

#define LIBX10_MACHINE -lnsl_s
#define LIBX11_MACHINE -lnsl_s

#undef HAVE_PTYS
#undef SYSV_PTYS
#undef HAVE_SOCKETS

#define LIBS_DEBUG -lg
