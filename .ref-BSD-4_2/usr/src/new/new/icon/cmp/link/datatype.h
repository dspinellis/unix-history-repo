/*
 * descriptor flags
 */
#ifdef VAX
#define F_NQUAL    0x80000000              /* set if NOT string qualifier */
#define F_VAR      0x40000000              /* set if variable */
#define F_TRAP     0x20000000              /* set if trapped variable */
#define F_PTR      0x10000000              /* set if value field is pointer */
#define F_NUM      0x08000000              /* set if numeric value */
#define F_INT      0x04000000              /* set if integer value */
#define F_AGGR     0x02000000              /* set if aggregrate */
#endif
#ifdef PDP11
#define F_NQUAL    0100000              /* set if NOT string qualifier */
#define F_VAR      0040000              /* set if variable */
#define F_TRAP     0020000              /* set if trapped variable */
#define F_PTR      0010000              /* set if value field is pointer */
#define F_NUM      0004000              /* set if numeric value */
#define F_INT      0002000              /* set if integer value */
#define F_AGGR     0001000              /* set if aggregrate */
#endif

/*
 * type codes (descriptors and blocks)
 */

#define T_INTEGER        1              /* short integer (not placed in heap) */
#define T_LONGINT        2              /* long integer type */
#define T_REAL           3              /* real number */
#define T_CSET           4              /* cset */
#define T_FILE           5              /* file block */
#define T_PROC           6              /* procedure block */
#define T_LIST           7              /* list header */
#define T_STACK          8              /* stack header */
#define T_TABLE          9              /* table header */
#define T_RECORD        10              /* record block */
#define T_TELEM         11              /* table element */
#define T_LISTB         12              /* list element block */
#define T_STACKB        13              /* stack element block */
#define T_TVSUBS        14              /* substring trapped variable */
#define T_TVSUBJ        15              /* subject trapped variable */
#define T_TVTBL	        16              /* table element trapped variable */
#define T_TVLIST        17              /* list element trapped variable */
#define T_TVPOS	        18              /* &pos trapped variable */
#define T_TVRAND        19              /* &random trapped variable */
#define T_TVTRACE       20              /* &trace trapped variable */
#define T_ESTACK        21              /* expression stack block */

/*
 * descriptor types and flags
 */

#define D_NULL          0
#define D_INTEGER       (T_INTEGER | F_NUM | F_INT | F_NQUAL)
#define D_LONGINT       (T_LONGINT | F_NUM | F_INT | F_PTR | F_NQUAL)
#define D_REAL          (T_REAL | F_NUM | F_PTR | F_NQUAL)
#define D_CSET          (T_CSET | F_PTR | F_NQUAL)
#define D_FILE          (T_FILE | F_PTR | F_NQUAL)
#define D_PROC          (T_PROC | F_PTR | F_NQUAL)
#define D_LIST          (T_LIST | F_AGGR | F_PTR | F_NQUAL)
#define D_STACK         (T_STACK | F_AGGR | F_PTR | F_NQUAL)
#define D_TABLE         (T_TABLE | F_AGGR | F_PTR | F_NQUAL)
#define D_RECORD        (T_RECORD | F_AGGR | F_PTR | F_NQUAL)
#define D_TELEM		(T_TELEM | F_AGGR | F_PTR | F_NQUAL)
#define D_LISTB		(T_LISTB | F_AGGR | F_PTR | F_NQUAL)
#define D_STACKB        (T_STACKB | F_AGGR | F_PTR | F_NQUAL)
#define	D_TVSUBS	(T_TVSUBS | D_TVAR)
#define	D_TVSUBJ	(T_TVSUBJ | D_TVAR)
#define	D_TVTBL		(T_TVTBL | D_TVAR)
#define	D_TVLIST	(T_TVLIST | D_TVAR)
#define D_TVPOS		(T_TVPOS | D_TVAR)
#define D_TVRAND  	(T_TVRAND | D_TVAR)
#define D_TVTRACE	(T_TVTRACE | D_TVAR)
#define D_ESTACK        (T_ESTACK | F_PTR | F_NQUAL)

#define D_VAR           (F_VAR | F_NQUAL)
#define D_TVAR          (F_VAR | F_TVAR | F_NQUAL)
