/* Copyright (c) 1979 Regents of the University of California */

/* static	char sccsid[] = "@(#)pc.h 1.2 10/14/80"; */

    /*
     *		random constants for pc
     */
    
    /*
     *	the name of the display.
     *	the display is made up of saved AP's and FP's.
     *	FP's are used to find locals, and AP's are used to find parameters.
     *	FP and AP are untyped pointers, but are used throughout as (char *).
     *	the display is used by adding AP_OFFSET or FP_OFFSET to the 
     *	address of the approriate display entry.
     */
#define	DISPLAYNAME	"__disply"
struct dispsave {
    char	*savedAP;
    char	*savedFP;
};
#define	AP_OFFSET	( 0 )
#define FP_OFFSET	( sizeof(char *) )

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
struct rtlocals {
    struct iorec	*curfile;
    struct dispsave	dsave;
    struct dispsave	*dptr;
    int			(*unwind)();
} rtlocs;
#define	CURFILEOFFSET	( ( -sizeof rtlocs ) + sizeof rtlocs.unwind )
#define	DSAVEOFFSET	( CURFILEOFFSET + sizeof rtlocs.curfile )
#define	DPTROFFSET	( DSAVEOFFSET + sizeof rtlocs.dsave )
#define	UNWINDOFFSET	( DPTROFFSET + sizeof rtlocs.dptr )
#define	UNWINDNAME	"_UNWIND"

    /*
     *	the register save mask for saving no registers
     */
#define	RSAVEMASK	( 0 )

    /*
     *	runtime check mask for divide check and integer overflow
     */
#define	RUNCHECK	( ( 1 << 15 ) | ( 1 << 14 ) )

    /*
     *	formats for various names
     *	    NAMEFORMAT		arbitrary length strings.
     *	    EXTFORMAT		for externals, a preceding underscore.
     *	    PREFIXFORMAT	used to print made up names with prefixes.
     *	    LABELPREFIX		with getlab() makes up label names.
     *	    LLABELPREFIX	with getlab() makes up sdb labels.
     *	a typical use might be to print out a name with a preceeding underscore
     *	with putprintf( EXTFORMAT , 0 , name );
     */
#define	NAMEFORMAT	"%s"
#define	EXTFORMAT	"_%s"
#define	PREFIXFORMAT	"%s%d"
#define	LABELPREFIX	"L"
#define	LLABELPREFIX	"LL"

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

    /*
     *	and of course ...
     */
#define	BITSPERBYTE	8

    /*
     *	error number for case label not found (ECASE)
     *	stolen from ~mckusick/px/lib/h01errs.h
     */
#define	ECASE		5
