/*	vm.h	4.1	11/9/80	*/

/*
 *	#include "../h/vm.h"
 * or	#include <vm.h>		 in a user program
 * is a quick way to include all the vm header files.
 */
#ifdef KERNEL
#include "../h/vmparam.h"
#include "../h/vmclust.h"
#include "../h/vmtune.h"
#include "../h/vmmac.h"
#include "../h/vmmeter.h"
#include "../h/vmtotal.h"
#include "../h/vmsystm.h"
#include "../h/vmklust.h"
#else
#include <sys/vmparam.h>
#include <sys/vmclust.h>
#include <sys/vmtune.h>
#include <sys/vmmac.h>
#include <sys/vmmeter.h>
#include <sys/vmtotal.h>
#include <sys/vmsystm.h>
#include <sys/vmklust.h>
#endif
