/*	%M%	%I%	%E%	*/

#ifdef vax
#ifdef KERNEL
#include "../vax/pte.h"
#else
#include <vax/pte.h>
#endif
#endif

#ifdef sun
#ifdef KERNEL
#include "../sun/pte.h"
#else
#include <sun/pte.h>
#endif
#endif
