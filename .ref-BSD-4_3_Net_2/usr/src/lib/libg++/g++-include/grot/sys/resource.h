#ifndef resource_h
#pragma once
#ifndef USG
#include <time.h>

extern "C"
{

#define KERNEL
#ifdef VMS
#include "GNU_CC_INCLUDE:[sys]resource.h"
#else
#include "//usr/include/sys/resource.h"
#endif
#undef KERNEL

#ifndef resource_h
#define resource_h 1
#endif

int getrusage(int, struct rusage*);
int getrlimit (int resource, struct rlimit *rlp);
int setrlimit (int resource, struct rlimit *rlp);

}

#endif /* USG */
#endif 
