/* System-dependent stuff, for Apollo 68k SysV-under-Domain/OS  systems */

#include <malloc.h>

#ifdef __GNUC__
#define alloca __builtin_alloca
#endif

#include <sys/types.h>
#include <dirent.h>
typedef struct dirent dirent;
