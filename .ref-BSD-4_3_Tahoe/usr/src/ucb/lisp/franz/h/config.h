/*					-[Thu Mar  3 15:57:51 1983 by jkf]-
 * 	config.h			$Locker:  $
 * configuration dependent info
 *
 * $Header: config.h,v 1.16 87/12/14 18:33:31 sklower Exp $
 *
 * (c) copyright 1982, Regents of the University of California
 */
 
/* 
 * this file contains parameters which each site is likely to modify
 * in order to personalize the configuration of Lisp at their site.
 * The typical things to modifiy are:
 *    [optionally] turn on GCSTRINGS
 *    [optionally] provide a value for SITE 
 */

/*
 * The type of machine and os this is to run on will come from
 * the file lconf.h.  The lconf.h file is created by the shell script
 * 'lispconf' in the directory ../..
 * lconf.h will define exactly one of these symbols:
 *  vax_4_2 vax_4_1c vax_4_1a vax_4_1 vax_unix_ts vax_eunice_vms
 *  sun_4_2 sun_4_1c sun_unisoft dual_unisoft pixel_unisoft lisa_unisys3
 *  mc500_2_0
 */
#include "lconf.h"


/* GCSTRINGS - define this if you want the garbage collector to reclaim
 *  strings.  It is not normally set because in typical applications the
 *  expense of collecting strings is not worth the amount of space
 *  retrieved
 */
 
/* #define GCSTRINGS */

/*
 * set up the global defines based on the choice above
 * the global names are
 * operating system:
 *		  os_unix
 *		     os_4_1, os_4_1a, os_4_1c, os_4_2, os_unix_ts
 *		  os_vms
 */

/* OFFSET -  this is the offset to the users address space. */
/* NB: this is not necessarily tied to the hardware.  Pixel
   informs us that when they put up 4.1 the offsetis likely
   to change */

#if vax_4_1 || vax_4_1a || vax_4_1c || vax_4_2 || vax_4_3 || vax_unix_ts || vax_eunice_vms
#define m_vax 1
#endif

#if tahoe_4_3
#define m_tahoe 1
#endif

#if sun_4_2beta || sun_4_2 || sun_4_1c
#define m_68k		1
#define OFFSET		0x8000
#endif

#if sun_unisoft
#define m_68k		1
#define	OFFSET		0x40000
#endif

#if dual_unisoft
#define m_68k		1
#define m_68k_dual	1
#define OFFSET		0x800000
#endif

#if pixel_unisoft
#define m_68k		1
#define OFFSET		0x20000
#endif

#if lisa_unisys3
#define m_68k		1
#define OFFSET		0x20000
#define unisys3botch	1
#define os_unix_ts	1
#endif

#if mc500_2_0
#define OFFSET 0
#define m_68k 1
#define os_masscomp 1
#endif

/* next the operating system */
#if vax_4_1 || vax_4_1a || vax_4_1c || vax_4_2 || vax_4_3 || vax_unix_ts || m_68k || tahoe_4_3
#define os_unix		1
#endif

#if vax_4_1
#define os_4_1		1
#endif
#if vax_4_1a
#define os_4_1a		1
#endif
#if vax_4_1c || sun_4_1c
#define os_4_1c 	1
#endif
#if vax_4_2 || sun_4_2 || sun_4_2beta
#define os_4_2	 	1
#endif
#if vax_4_3 || tahoe_4_3
#define os_4_3		1
#endif
#if vax_unix_ts
#define os_unix_ts 	1
#endif
#if vax_eunice_vms
#define os_vms		1
#endif

#if sun_unisoft || dual_unisoft || pixel_unisoft
#define os_unisoft 1
#endif

/* MACHINE -  this is put on the (status features) list */
#if m_68k
#define MACHINE "68k"
#define PORTABLE
#endif

/* RTPORTS -- this O.S. allocates FILE *'s at run-time */
#if os_4_3
#define RTPORTS 1
#endif

#if m_vax
#define MACHINE "vax"
#define NILIS0	1
#endif

#if m_tahoe
#define MACHINE "tahoe"
#define NILIS0	1
#endif

/*
** NILIS0 -- for any UNIX implementation in which the users
**	address space starts at 0 (like m_vax, above). 
**
** NPINREG -- for the verison if lisp that keeps np and lbot in global
**	registers.  On the 68000, there is a special `hacked' version
**	of the C compiler that is needed to do this.
**
** #define NILIS0		1
** #define NPINREG		1
*/

/*
 * SPISFP -- this is to indicate that the stack and frame pointer
 * are the same, or at least that you can't pull the same shenanigans
 * as on the vax or sun by pushing error frames at the end of C
 * frames and using alloca.  This should make life easier for
 * a native VMS version or IBM or RIDGE or Bellmac-32.
 * #define SPISFP 1
 */

#if sun_4_2beta
#define SPISFP 1
#endif

#if m_vax || m_tahoe
#define OFFSET		0x0
#define NPINREG		1
#endif




/* OS -  this is put on the (status features) list */
#if os_unix
#define OS      "unix"
#endif
#if os_vms
#define OS 	"vms"
#endif

/* DOMAIN - this is put on the (status features) list and
 * 	is the value of (status domain)
 */
#define DOMAIN  "ucb"

/* SITE - the name of the particular machine this lisp is running on
 *    this value is available via (sys:gethostname).
 *    On 4.1a systems it is possible to determine this dynamically cheaply
 */
#if ! (os_4_1a || os_4_1c || os_4_2 || os_4_3)
#define SITE    "unknown-site"
#endif


/*  TTSIZE is the absolute limit, in pages (both text and data), of the
 * size to which the lisp system may grow.
 * If you change this, you must recompile alloc.c and data.c.
 */
#if (sun_4_2 || sun_4_2beta || HOLE)
#define TTSIZE 10216
#else
#define TTSIZE 6120
#endif

#if m_vms 
#undef TTSIZE
#define TTSIZE 10216
#define FREESIZE 512 * 10000
#endif 
