#ifndef wait_h

#include <sys/resource.h>

#define wait WaitStatus

extern "C" {

#ifdef VMS
#include "GNU_CC_INCLUDE:[sys]wait.h"
#else
#include "//usr/include/sys/wait.h"
#endif

#undef wait

#ifndef wait_h
#define wait_h 1
#endif

extern int wait3(WaitStatus*, int options, struct rusage*);
extern int wait4(int, WaitStatus*, int, struct rusage*);
}

#endif
