/* System-dependent stuff, for SGI irix */

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
extern char *alloca ();
#endif

#include <sys/types.h>
#include <sys/stream.h>
#include <sys/dir.h>
typedef struct direct dirent;
