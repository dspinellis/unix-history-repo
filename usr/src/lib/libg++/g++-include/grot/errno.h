#ifndef errno_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif

extern "C" {
#ifdef VMS
#include "GNU_CC_INCLUDE:[000000]errno.h"
#else
#include "//usr/include/errno.h"
#endif
}

#ifndef errno_h
#define errno_h 1
#endif

#include <std.h>

#endif
