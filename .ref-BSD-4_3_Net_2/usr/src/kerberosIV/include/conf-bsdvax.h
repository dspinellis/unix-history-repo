/*
 * $Source: /mit/kerberos/src/include/RCS/conf-bsdvax.h,v $
 * $Author: jtkohl $
 * $Header: conf-bsdvax.h,v 4.0 89/01/23 09:58:12 jtkohl Exp $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 *
 * Machine-type definitions: VAX
 */

#include <mit-copyright.h>

#define VAX
#define BITS32
#define BIG
#define LSBFIRST
#define BSDUNIX

#ifndef __STDC__
#ifndef NOASM
#define VAXASM
#endif /* no assembly */
#endif /* standard C */
