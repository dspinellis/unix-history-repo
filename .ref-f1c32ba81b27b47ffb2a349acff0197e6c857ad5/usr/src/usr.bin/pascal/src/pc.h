/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pc.h	5.3 (Berkeley) %G%
 */

#include <setjmp.h>

    /*
     *	random constants for pc
     */
    
    /*
     *	the name of the display.
     */
#define	DISPLAYNAME	"__disply"

    /*
     *	the structure below describes the locals used by the run time system.
     *	at function entry, at least this much space is allocated,
     *	and the following information is filled in:
     *	the address of a routine to close the current frame for unwinding,
     *	a pointer to the display entry for the current static level and
     *	the previous contents of the display for this static level.
     *	the curfile location is used to point to the currently active file,
     *	and is filled in as io is initiated.
     *	one of these structures is allocated on the (negatively growing) stack.
     *	at function entry, fp is set to point to the last field of the struct,
     *	thus the offsets of the fields are as indicated below.
     */
#ifdef vax
struct rtlocals {
    jmp_buf		gotoenv;
    struct iorec	*curfile;
    struct dispsave	dsave;
} rtlocs;
#endif vax

#ifdef tahoe
struct rtlocals {
    jmp_buf		gotoenv;
    struct iorec	*curfile;
    struct dispsave	dsave;
    long		savedregs[9];
} rtlocs;
#endif tahoe

#ifdef mc68000
struct rtlocals {
    jmp_buf		gotoenv;
    struct iorec	*curfile;
    struct dispsave	dsave;
} rtlocs;
#endif mc68000

#define	GOTOENVOFFSET	( -sizeof rtlocs )
#define	CURFILEOFFSET	( GOTOENVOFFSET + sizeof rtlocs.gotoenv )
#define	DSAVEOFFSET	( CURFILEOFFSET + sizeof rtlocs.curfile )

    /*
     *	this is a cookie used to communicate between the
     *	routine entry code and the routine exit code.
     *	mostly it's for labels shared between the two.
     */
#ifdef vax
struct entry_exit_cookie {
    struct nl	*nlp;
    char	extname[BUFSIZ];
    int		toplabel;
    int		savlabel;
};
#define	FRAME_SIZE_LABEL	"LF"
#define	SAVE_MASK_LABEL		"L"
#endif vax

#ifdef tahoe
struct entry_exit_cookie {
    struct nl	*nlp;
    char	extname[BUFSIZ];
    int		toplabel;
    int		savlabel;
};
#define	FRAME_SIZE_LABEL	"LF"
#define	SAVE_MASK_LABEL		"L"
#endif tahoe

#ifdef mc68000
struct entry_exit_cookie {
    struct nl	*nlp;
    char	extname[BUFSIZ];
    int		toplabel;
};
#define	FRAME_SIZE_LABEL	"LF"
#define	PAGE_BREAK_LABEL	"LP"
#define	SAVE_MASK_LABEL		"LS"
#endif mc68000

    /*
     *	formats for various names
     *	    NAMEFORMAT		arbitrary length strings.
     *	    EXTFORMAT		for externals, a preceding underscore.
     *	    LABELFORMAT		for label names, a preceding dollar-sign.
     *	    PREFIXFORMAT	used to print made up names with prefixes.
     *	    LABELPREFIX		with getlab() makes up label names.
     *	    LLABELPREFIX	with getlab() makes up sdb labels.
     *	    FORMALPREFIX	prefix for EXTFORMAT for formal entry points.
     *	a typical use might be to print out a name with a preceeding underscore
     *	with putprintf( EXTFORMAT , 0 , name );
     */
#define	NAMEFORMAT	"%s"
#define	EXTFORMAT	"_%s"
#define	LABELFORMAT	"$%s"
#define	PREFIXFORMAT	"%s%d"
#define	LABELPREFIX	"L"
#define	LLABELPREFIX	"LL"
#define	FORMALPREFIX	"__"

    /*
     *	the name of the statement counter
     */
#define	STMTCOUNT	"__stcnt"

    /*
     *	the name of the pcp counters
     */
#define	PCPCOUNT	"__pcpcount"

    /*
     *	a vector of pointer to enclosing functions for fully qualified names.
     */
char	*enclosing[ DSPLYSZ ];

#ifdef vax
    /*
     *	the runtime framepointer and argumentpointer registers
     */
#   define	P2FP		13
#   define	P2FPNAME	"fp"
#   define	P2AP		12
#   define	P2APNAME	"ap"

    /*
     *	the register save mask for saving no registers
     */
#   define	RSAVEMASK	( 0 )

    /*
     *	runtime check mask for divide check and integer overflow
     */
#   define	RUNCHECK	( ( 1 << 15 ) | ( 1 << 14 ) )

    /*
     *	and of course ...
     */
#   define	BITSPERBYTE	8
#endif vax

#ifdef tahoe
    /*
     *	the runtime framepointer and argumentpointer registers
     */
#   define	P2FP		13
#   define	P2FPNAME	"fp"
#   define	P2AP		13
#   define	P2APNAME	"fp"

    /*
     *	the register save mask for saving no registers
     */
#   define	RSAVEMASK	( 0 )

    /*
     *	divide check and integer overflow don't exist in the save mask
     */
#   define	RUNCHECK	( 0 )

    /*
     *	and of course ...
     */
#   define	BITSPERBYTE	8
#endif tahoe

#ifdef mc68000
    /*
     *	these magic numbers lifted from pcc/mac2defs
     *	the offsets are for mapping data and address register numbers
     *	to linear register numbers.  e.g. d2 ==> r2, and a2 ==> r10.
     */
#   define	DATA_REG_OFFSET	0
#   define	ADDR_REG_OFFSET 8
#   define	P2FPNAME	"a6"
#   define	P2FP		(ADDR_REG_OFFSET + 6)
#   define	P2APNAME	"a6"
#   define	P2AP		(ADDR_REG_OFFSET + 6)

    /*
     *	and still ...
     */
#   define	BITSPERBYTE	8
#endif mc68000
