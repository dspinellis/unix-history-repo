/* vxWorks.h - VxWorks standard definitions header */

/*  Copyright 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1992 Free Software Foundation, Inc.

    This code was donated by Wind River Systems, Inc. */

/*
modification history
--------------------
01z,05oct90,shl  added copyright notice.
                 made #endif ANSI style.
01y,28sep90,del  added I960 defines.
01x,29may90,del	 moved types to vxTypes.h
01w,09apr90,jcf  added timeout definitions.
01v,24jan90,gae  moved network configuration flags here from makefile's.
01u,01sep88,mcl  definition of INSTR dependent on processor family; added SPARC.
	   +gae  added MC680X0 and defined CPU_FAMILY.
01t,08apr89,dnw  added ifdef to prevent inclusion of vxWorks.h more than once.
01s,22jun88,dnw	 moved READ, WRITE, and UPDATE back here from ioLib.h.
01r,22apr88,gae  oops! forgot some #endif's in 01q.
01q,12apr88,gae  removed QUICK & WAIT; added STD_{IN,OUT,ERR}.
		 fixed #define's of FALSE, TRUE, etc.
		 moved READ, WRITE, and UPDATE to ioLib.h.
01p,04dec87,dnw  added undefine of MC68000 to get around Green Hills bug that
		   pre-defines MC68000.
01o,12nov87,ecs  added type ULONG.
01n,08feb86,dnw  added types INSTR, UINT, USHORT.
01m,14oct85,rdc  added BUS types.
01l,16jul85,jlf  added conditional for NULL and EOF.
01k,24jun85,rdc  installed condtional compile so we can include in
		 assembly language files.  See instructions below.
		 Added System type macro and CPU type macro.
01j,13jun85,dnw  cleaned-up, removed more obsolete stuff to wrs.h
01i,11sep84,jlf  changed name from wrs.h to vxWorks.h.  removed GLOBAL.
01h,03jun84,dnw  removed IGNORE declaration.
01g,09apr84,jlf  added MEMBER_SIZE macro.
01f,14dec83,dnw  added MSB, LSB macros
01e,17nov83,jlf  added STATUS type, for routines which return a status.
01d,13jul83,dnw  added NELEMENTS macro
01c,14May83,dnw  added OFFSET macro
01b,17Feb83,dnw  added stuff from Whitesmiths std.h
01a,15Feb83,dnw  written
*/

#ifndef INCvxWorksh
#define INCvxWorksh

#if	!defined(NULL) || (NULL!=0)
#define NULL		0
#endif	

#if	!defined(EOF) || (EOF!=(-1))
#define EOF		(-1)
#endif	

#if	!defined(FALSE) || (FALSE!=0)
#define FALSE		0
#endif	

#if	!defined(TRUE) || (TRUE!=1)
#define TRUE		1
#endif	


#define NONE		(-1)	/* for times when NULL won't do */
#define EOS		'\0'	/* C string terminator */


/* return status values */

#define OK		0
#define ERROR		(-1)

/* timeout defines */

#define NO_WAIT		0
#define WAIT_FOREVER	(-1)

/* low-level I/O input, output, error fd's */

#define	STD_IN	0
#define	STD_OUT	1
#define	STD_ERR	2

/* modes - must match O_RDONLY/O_WRONLY/O_RDWR in ioLib.h! */

#define READ		0
#define WRITE		1
#define UPDATE		2

/* SYSTEM types */

#define V7		1	/* ATT version 7 */
#define SYS_V		2	/* ATT System 5 */
#define BSD_4_2		3	/* Berkeley BSD 4.2 */

/* CPU types */

/* The Green Hills compiler pre-defines "MC68000"!! */
#ifdef MC68000
#undef MC68000
#endif	

#define MC68000		1
#define MC68010		2
#define MC68020		3
#define MC68030		4
#define MC68040		5
#define MC680X0		9

#define SPARC		10

#ifndef I960
#define	I960		20
#endif	

#define	I960KB		21
#define	I960CA		22

#if	CPU==MC68000 || CPU==MC68010 || CPU==MC68020 || CPU==MC68030
#define	CPU_FAMILY	MC680X0
#endif	/* CPU==MC68000 || CPU==MC68010 || CPU==MC68020 || CPU==MC68030 */

#if	CPU==SPARC
#define	CPU_FAMILY	SPARC
#endif	/* CPU==SPARC */

#if 	CPU==I960KB
#define	CPU_FAMILY	I960
#endif	/* 	CPU==I960KB */

#if 	CPU==I960CA
#define	CPU_FAMILY	I960
#endif	/* 	CPU==I960CA */

/* BUS types */

#define VME_BUS		1
#define MULTI_BUS	2

/* network configuration parameters */

#define	INET		/* include internet protocols */
#define	BSD	43	/* BSD 4.3 -like OS */
#define	BSDDEBUG	/* turn on debug */
#define	GATEWAY		/* tables to be initialized for gateway routing */

/* common macros */

#define MSB(x)	(((x) >> 8) & 0xff)	/* most signif byte of 2-byte integer */
#define LSB(x)	((x) & 0xff)		/* least signif byte of 2-byte integer*/

#define OFFSET(structure, member)	/* byte offset of member in structure*/\
		((int) &(((structure *) 0) -> member))

#define MEMBER_SIZE(structure, member)	/* size of a member of a structure */\
		(sizeof (((structure *) 0) -> member))

#define NELEMENTS(array)		/* number of elements in an array */ \
		(sizeof (array) / sizeof ((array) [0]))

#define FOREVER	for (;;)

#define max(x, y)	(((x) < (y)) ? (y) : (x))
#define min(x, y)	(((x) < (y)) ? (x) : (y))


/* storage class specifier definitions */

#define FAST	register
#define IMPORT	extern
#define LOCAL	static


/* include typedefs - must come after CPU_FAMILY definitions above */

#include "vxTypes.h"

#endif	/* INCvxWorksh */
