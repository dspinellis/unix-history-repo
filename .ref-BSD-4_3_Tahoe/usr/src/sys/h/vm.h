/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vm.h	7.2 (Berkeley) 5/14/88
 */

/*
 *	#include "../h/vm.h"
 * or	#include <vm.h>		 in a user program
 * is a quick way to include all the vm header files.
 */
#if defined(KERNEL) || defined(STANDALONE)
#include "vmparam.h"
#include "vmmac.h"
#include "vmmeter.h"
#include "vmsystm.h"
#else
#include <sys/vmparam.h>
#include <sys/vmmac.h>
#include <sys/vmmeter.h>
#include <sys/vmsystm.h>
#endif
