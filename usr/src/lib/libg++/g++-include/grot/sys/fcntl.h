#ifndef sys_fcntl_h
#pragma once

extern "C" {

#define KERNEL

#ifdef VMS
#include "GNU_CC_INCLUDE:[sys]fcntl.h"
#else
#include "//usr/include/sys/fcntl.h"
#endif

#ifndef sys_fcntl_h
#define sys_fcntl_h 1
#endif

#undef KERNEL

}

#include <std.h>

#endif
