/* vxTypes.h - VxWorks type definition header */

/*  Copyright 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1992 Free Software Foundation, Inc.

    This code was donated by Wind River Systems, Inc. */

/*
modification history
--------------------
01c,05oct90,shl  added copyright notice.
                 made #endif ANSI style.
01b,10aug90,dnw  added VOIDFUNCPTR
01a,29may90,del  written.
*/

#ifndef INCvxTypesh
#define INCvxTypesh

/* The following stuff must NOT be included if this include file is used
 * from assembly language.  Just #define ASMLANGUAGE before the include,
 * to get rid of it.
 */

#ifndef ASMLANGUAGE

/* vxWorks types */

typedef	char		INT8;
typedef	short		INT16;
typedef	int		INT32;

typedef	unsigned char	UINT8;
typedef	unsigned short	UINT16;
typedef	unsigned int	UINT32;

typedef	unsigned char	UCHAR;
typedef unsigned short	USHORT;
typedef	unsigned int	UINT;
typedef unsigned long	ULONG;

typedef	int		BOOL;
typedef	int		VOID;
typedef	int		STATUS;	
typedef int 		ARGINT;

typedef int 		(*FUNCPTR) ();	    /* ptr to function returning int */
typedef VOID 		(*VOIDFUNCPTR) ();  /* ptr to function returning VOID */


/* historical definitions - now obsolete */

typedef char		TINY;		/* obsolete */
typedef char		TBOOL;		/* obsolete */
typedef unsigned char	UTINY;		/* obsolete */


/* architecture dependent typedefs */

#ifdef	CPU_FAMILY

#if	CPU_FAMILY==MC680X0
typedef unsigned short INSTR;		/* word-aligned instructions */
#endif	/* CPU_FAMILY==MC680X0 */

#if	CPU_FAMILY==SPARC
typedef unsigned long INSTR;		/* 32 bit word-aligned instructions */
#endif	/* CPU_FAMILY==SPARC */

#endif	

#endif	/* ASMLANGUAGE */
#endif	/* INCvxTypesh */
