/* Configuration for GCC for Intel i386 running Linux.
 *
 * Written by H.J. Lu (hlu@eecs.wsu.edu)
 */

#include "xm-i386.h"
#include "xm-svr3.h"

#undef BSTRING
#define BSTRING
#undef bcmp
#undef bcopy
#undef bzero
#undef index
#undef rindex

#if 0 /* These conflict with stdlib.h in protoize, it is said,
	 and there's no evidence they are actually needed.  */
#undef malloc(n)
#define malloc(n)	malloc ((n) ? (n) : 1)
#undef calloc(n,e)
#define calloc(n,e)	calloc (((n) ? (n) : 1), ((e) ? (e) : 1))
#endif
