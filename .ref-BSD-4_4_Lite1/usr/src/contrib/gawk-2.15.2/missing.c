/*
 * Do all necessary includes here, so that we don't have to worry about
 * overlapping includes in the files in missing.d.
 */
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#if !defined(VMS) || (!defined(VAXC) && !defined(__DECC))
#include <fcntl.h>
#include <sys/types.h>
#else	/*VMS w/ VAXC or DECC*/
#include <file.h>
#include <types.h>
#endif
#include <varargs.h>

#include "config.h"

#ifndef __STDC__
#define const
#endif /* !__STDC__ */

#ifdef STDC_HEADERS
#include <string.h>
#endif

#ifdef TZSET_MISSING
#include <sys/time.h>
#else
#include <time.h>
#endif

#ifdef atarist
/*
 * this will work with gcc compiler - for other compilers you may
 * have to replace path separators in this file into backslashes
 */
#include "atari/stack.c"
#include "atari/tmpnam.c"
/* #include "atari/textrd.c" */	/* current libraries are correct bug fix */
#endif /* atarist */

#ifdef SYSTEM_MISSING
#ifdef atarist
#include "atari/system.c"
#else
#include "missing/system.c"
#endif
#endif /* SYSTEM_MISSING */

#ifdef MEMCMP_MISSING
#include "missing/memcmp.c"
#endif	/* MEMCMP_MISSING */

#ifdef MEMCPY_MISSING
#include "missing/memcpy.c"
#endif	/* MEMCPY_MISSING */

#ifdef MEMSET_MISSING
#include "missing/memset.c"
#endif	/* MEMSET_MISSING */

#ifdef RANDOM_MISSING
#include "missing/random.c"
#endif	/* RANDOM_MISSING */

#ifdef STRCASE_MISSING
#include "missing/strncasecmp.c"
#endif	/* STRCASE_MISSING */

#ifdef STRERROR_MISSING
#include "missing/strerror.c"
#endif	/* STRERROR_MISSING */

#ifdef STRFTIME_MISSING
#include "missing/strftime.c"
#endif	/* STRFTIME_MISSING */

#ifdef STRCHR_MISSING
#include "missing/strchr.c"
#endif	/* STRCHR_MISSING */

#ifdef STRTOD_MISSING
#include "missing/strtod.c"
#endif	/* STRTOD_MISSING */

#ifdef TZSET_MISSING
#include "missing/tzset.c"
#endif /* TZSET_MISSING */
