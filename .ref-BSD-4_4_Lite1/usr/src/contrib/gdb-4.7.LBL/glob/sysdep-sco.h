/* System-dependent stuff, for SCO systems */

#include <malloc.h>

#ifdef __GNUC__
#define alloca __builtin_alloca
#endif

#include <sys/types.h>
#include <sys/stream.h>
#include <sys/ptem.h>
#include <dirent.h>
typedef struct dirent dirent;
