/* maxpath.h - Find out what this system thinks MAXPATHLEN is. */

#if !defined (_MAXPATH_H)
#define _MAXPATH_H

#if defined (isc386) && !defined (BUILDING_MAKEFILE)
#  include <limits.h>
#  if !defined (MAXPATHLEN) && defined (PATH_MAX)
#    define MAXPATHLEN PATH_MAX
#endif /* !MAXPATHLEN && PATH_MAX */
#endif /* isc386 && BUILDING_MAKEFILE */

/* Yecch!  Who cares about this gross concept in the first place? */
#if !defined (MAXPATHLEN)
#  define MAXPATHLEN 1024
#endif /* MAXPATHLEN */

#endif /* _MAXPATH_H */

