/* local.h - fine the -lndir include file */
/* @(#)$Id: local.h,v 1.3 1992/02/06 17:37:44 jromine Exp $ */

#ifndef	BSD42
#include <sys/types.h>
#else	/* BSD42 */
#include <sys/param.h>
#endif

#ifndef	BSD42
#ifndef NDIR
#ifndef	SYS5DIR
#include <dir.h>		/* last choice */
#else	/* SYS5DIR */
#include <dirent.h>
#endif
#else	/* NDIR */
#include <ndir.h>
#endif
#else	/* BSD42 */
#include <sys/dir.h>
#endif

#include <sys/stat.h>
