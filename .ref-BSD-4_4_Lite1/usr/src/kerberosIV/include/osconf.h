/*
 * $Source: /usr/src/kerberosIV/include/RCS/osconf.h,v $
 * $Author: torek $
 * $Header: /usr/src/kerberosIV/include/RCS/osconf.h,v 4.10 93/05/25 16:49:56 torek Exp $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 *
 * Athena configuration.
 */

#include <mit-copyright.h>

/* something tells me some of these files should be renamed... */
#if defined(tahoe) || defined(hp300) || defined(luna68k)
#include "conf-bsdtahoe.h"
#endif

#ifdef vax
#include "conf-bsdvax.h"
#endif

#if defined(mips)
#include "conf-ultmips2.h"
#endif

#ifdef ibm032
#include "conf-bsdibm032.h"
#endif

#ifdef apollo
#include "conf-bsdapollo.h"
#endif

#if defined(sparc) || defined(hp800)
#include "conf-bsdsparc.h"
#endif

#ifdef i386
#include "conf-bsd386i.h"
#endif /* i386 */

#if defined(sun2) || defined(sun3)
#include "conf-bsdm68k.h"
#endif

#ifdef pyr
#include "conf-pyr.h"
#endif
