/*	vm.h	4.3	81/04/23	*/

/*
 *	#include "../h/vm.h"
 * or	#include <vm.h>		 in a user program
 * is a quick way to include all the vm header files.
 */
#ifdef KERNEL
#include "../h/vmparam.h"
#include "../h/vmmac.h"
#include "../h/vmmeter.h"
#include "../h/vmsystm.h"
#else
#include <sys/vmparam.h>
#include <sys/vmmac.h>
#include <sys/vmmeter.h>
#include <sys/vmsystm.h>
#endif
