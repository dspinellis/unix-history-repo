#include "../h/config.h"

#ifdef VAX
#define r(i) (1<<(i))
   /* descriptor flags */
	.set	F_NQUAL,0x80000000       # set if NOT string qualifier
	.set	F_VAR,0x40000000         # set if variable
	.set	F_TVAR,0x20000000        # set if trapped variable
	.set	F_PTR,0x10000000         # set if value field is pointer
	.set	F_NUM,0x08000000         # set if numeric value
	.set	F_INT,0x04000000         # set if integer value
	.set	F_AGGR,0x02000000        # set if aggregrate
	.set	TYPEMASK,0x0000003f      # type mask
	.set	OFFSETMASK,0x1fffffff    # offset mask for variables
	
	.set	MAXSTRING,257	      # largest string in conversions

   /* type codes (descriptors and blocks) */

        .set    T_INTEGER,1             # short integer (not in heap)
        .set    T_LONGINT,1             # long integer type
        .set    T_REAL,3                # real number
        .set    T_CSET,4                # cset
        .set    T_FILE,5                # file block
        .set    T_PROC,6                # procedure block
        .set    T_LIST,7                # list header
        .set    T_TABLE,8               # table header
        .set    T_RECORD,9              # record block
        .set    T_TELEM,10              # table element
        .set    T_LISTB,11              # list element block
        .set    T_TVSUBS,12             # substring trapped variable
        .set    T_TVTBL,14              # table element trapped variable
        .set    T_TVPOS,15              # &pos trapped variable
        .set    T_TVRAND,16             # &random trapped variable
        .set    T_TVTRACE,17            # &trace trapped variable
        .set    T_ESTACK,18             # expression stack block
        .set    T_EBLOCK,19             # expression heap block

   /* descriptor types and flags */

        .set    D_VAR,F_VAR|F_NQUAL
        .set    D_TVAR,F_VAR|F_TVAR|F_NQUAL

        .set    D_NULL,0
        .set    D_INTEGER,T_INTEGER|F_NUM|F_INT|F_NQUAL
        .set    D_LONGINT,T_LONGINT|F_NUM|F_INT|F_PTR|F_NQUAL
        .set    D_REAL,T_REAL|F_NUM|F_PTR|F_NQUAL
        .set    D_CSET,T_CSET|F_PTR|F_NQUAL
        .set    D_FILE,T_FILE|F_PTR|F_NQUAL
        .set    D_PROC,T_PROC|F_PTR|F_NQUAL
        .set    D_LIST,T_LIST|F_AGGR|F_PTR|F_NQUAL
        .set    D_TABLE,T_TABLE|F_AGGR|F_PTR|F_NQUAL
        .set    D_RECORD,T_RECORD|F_AGGR|F_PTR|F_NQUAL
        .set    D_TELEM,T_TELEM|F_AGGR|F_PTR|F_NQUAL
        .set    D_LISTB,T_LISTB|F_AGGR|F_PTR|F_NQUAL
        .set    D_TVSUBS,T_TVSUBS|D_TVAR
        .set    D_TVTBL,T_TVTBL|D_TVAR
        .set    D_TVPOS,T_TVPOS|D_TVAR
        .set    D_TVRAND,T_TVRAND|D_TVAR
        .set    D_TVTRACE,T_TVTRACE|D_TVAR
        .set    D_ESTACK,T_ESTACK|F_PTR|F_NQUAL
        .set    D_EBLOCK,T_EBLOCK|F_PTR|F_NQUAL
#endif VAX

#ifdef PDP11
/ descriptor flags

F_NQUAL		= 0100000	/ set if NOT string qualifier
F_VAR		= 0040000	/ set if variable
F_TVAR		= 0020000	/ set if trapped variable
F_PTR		= 0010000	/ set if value field is pointer
F_NUM		= 0004000	/ set if numeric value
F_INT		= 0002000	/ set if integer value
F_AGGR		= 0001000	/ set if aggregrate

TYPEMASK	= 0000077	/ mask to access type code
OFFSETMASK	= 0017777	/ offset mask for variables
MAXSTRING	= 257.		/ longest string in conversions

/ type codes (descriptors and blocks)

T_INTEGER	=  1.		/ short integer (not in heap)
T_LONGINT	=  2.		/ long integer type
T_REAL		=  3.		/ real number
T_CSET		=  4.		/ cset
T_FILE		=  5.		/ file block
T_PROC		=  6.		/ procedure block
T_LIST		=  7.		/ list header
T_TABLE 	=  8.		/ table header
T_RECORD	=  9.		/ record block
T_TELEM 	= 10.		/ table element
T_LISTB 	= 11.		/ list element block
T_TVSUBS	= 12.		/ substring trapped variable
T_TVTBL 	= 14.		/ table element trapped variable
T_TVPOS 	= 15.		/ &pos trapped variable
T_TVRAND	= 16.		/ &random trapped variable
T_TVTRACE	= 17.		/ &trace trapped variable
T_ESTACK	= 18.		/ expression stack block
T_EBLOCK	= 19.		/ expression heap block

/ descriptor types and flags

D_VAR		= F_VAR|F_NQUAL
D_TVAR		= F_VAR|F_TVAR|F_NQUAL

D_NULL		= 0
D_INTEGER	= T_INTEGER|F_NUM|F_INT|F_NQUAL
D_LONGINT	= T_LONGINT|F_NUM|F_INT|F_PTR|F_NQUAL
D_REAL		= T_REAL|F_NUM|F_PTR|F_NQUAL
D_CSET		= T_CSET|F_PTR|F_NQUAL
D_FILE		= T_FILE|F_PTR|F_NQUAL
D_PROC		= T_PROC|F_PTR|F_NQUAL
D_LIST		= T_LIST|F_AGGR|F_PTR|F_NQUAL
D_TABLE 	= T_TABLE|F_AGGR|F_PTR|F_NQUAL
D_RECORD	= T_RECORD|F_AGGR|F_PTR|F_NQUAL
D_TELEM 	= T_TELEM|F_AGGR|F_PTR|F_NQUAL
D_LISTB 	= T_LISTB|F_AGGR|F_PTR|F_NQUAL
D_TVSUBS	= T_TVSUBS|D_TVAR
D_TVTBL 	= T_TVTBL|D_TVAR
D_TVPOS 	= T_TVPOS|D_TVAR
D_TVRAND	= T_TVRAND|D_TVAR
D_TVTRACE	= T_TVTRACE|D_TVAR
D_ESTACK	= T_ESTACK|F_PTR|F_NQUAL
D_EBLOCK	= T_EBLOCK|F_PTR|F_NQUAL
#endif PDP11
