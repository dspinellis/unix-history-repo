/*
 * Copyright (c) 1994 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 */

#include "args.h"

#ifndef STDARG_H
#define STDARG_H

#ifdef VARARGS

#include <varargs.h>

#else /*VARARG*/

#ifdef STDARG

#include <stdarg.h>

#else /*STDARG*/

/*
 * SIMULATE_STDARG
 *
 * WARNING: This type of stdarg makes assumptions about the stack
 * 	    that may not be true on your system.  You may want to
 *	    define STDARG (if using ANSI C) or VARARGS.
 */

typedef char *va_list;
#define va_start(ap,parmn) (void)((ap) = (char*)(&(parmn) + 1))
#define va_end(ap) (void)((ap) = 0)
#define va_arg(ap, type) \
    (((type*)((ap) = ((ap) + sizeof(type))))[-1])

#endif /*STDARG*/
#endif /*VARARG*/

#endif

/* END CODE */
