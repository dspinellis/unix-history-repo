/* System-dependent stuff, for Sony NEwS, Mach, and other systems 
   in the "old BSD Unix" tradition.  */

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
extern char *alloca ();
#endif

#include <sys/types.h>
#include <sys/dir.h>
typedef struct direct dirent;
