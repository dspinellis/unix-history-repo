/* Definitions for ESIX, a variant of v.5.3 for the 386.  */
/* These are based on reports for ESIX 5.3.2 D.  */

#include "s-usg5-3.h"

#define ESIX

#define HAVE_TIMEVAL
#define MISSING_UTIMES 

/* Some versions of V.3 have this, but not all. ESIX does. */
#define HAVE_PTYS
#define SYSV_PTYS

#undef sigsetmask

/* ESIX has FIONREAD, but it doesn't work right on the ptys or pipes */
#define BROKEN_FIONREAD

#define LIBS_SYSTEM -lbsd

#define NO_SIOCTL_H
