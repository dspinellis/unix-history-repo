#include <sys/param.h>
#include <sys/file.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/ktrace.h>
#include <stdio.h>

#define DEF_FACS (KTRFAC_SYSCALL | KTRFAC_SYSRET | KTRFAC_NAMEI)
#define DEF_TRACEFILE	"ktrace.data"
