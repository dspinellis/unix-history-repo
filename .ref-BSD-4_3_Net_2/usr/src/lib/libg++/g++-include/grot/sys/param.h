#ifndef param_h
#pragma once

#include <time.h>

extern "C"
{

#ifndef hpux
#define KERNEL
#endif
#ifdef VMS
#include "GNU_CC_INCLUDE:[sys]param.h"
#else
#include "//usr/include/sys/param.h"
#endif
#undef KERNEL

#ifndef param_h
#define param_h 1
#endif

/* kill commonly overloaded possible param.h macros */
#undef setbit
#undef clrbit
#undef isset
#undef isclr
#undef howmany
#undef roundup
#undef MIN
#undef MAX

}

#endif
