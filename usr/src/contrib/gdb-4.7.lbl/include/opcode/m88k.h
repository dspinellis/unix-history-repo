/* This file has been modified by Data General Corporation, November 1989. */


/*
*			Disassembler Instruction Table
*
*	The first field of the table is the opcode field. If an opcode
*	is specified which has any non-opcode bits on, a system error
*	will occur when the system attempts the install it into the
*	instruction table.  The second parameter is a pointer to the
*	instruction mnemonic. Each operand is specified by offset, width,
*	and type. The offset is the bit number of the least significant
*	bit of the operand with bit 0 being the least significant bit of
*	the instruction. The width is the number of bits used to specify
*	the operand. The type specifies the output format to be used for
*	the operand. The valid formats are: register, register indirect,
*	hex constant, and bit field specification.  The last field is a
*	pointer to the next instruction in the linked list.  These pointers
*	are initialized by init_disasm().
*
*				Structure Format
*
*       struct INSTAB {
*          UPINT opcode;
*          char *mnemonic;
*          struct OPSPEC op1,op2,op3;
*          struct SIM_FLAGS flgs;
*          struct INSTAB *next;
*       }
*
*       struct OPSPEC {
*          UPINT offset:5;
*          UPINT width:6;
*          UPINT type:5;
*       }
*
*				Revision History
*
*	Revision 1.0	11/08/85	Creation date
*		 1.1	02/05/86	Updated instruction mnemonic table MD
*		 1.2	06/16/86	Updated SIM_FLAGS for floating point
*		 1.3	09/20/86	Updated for new encoding
*		 	05/11/89	R. Trawick adapted from Motorola disassembler
*/

#include <stdio.h>


/*
 * This file contains the structures and constants needed to build the M88000
 * simulator.  It is the main include file, containing all the
 * structures, macros and definitions except for the floating point
 * instruction set.
 */

/*
 * The following flag informs the Simulator as to what type of byte ordering
 * will be used. For instance, a BOFLAG = 1 indicates a DEC VAX and IBM type
 * of ordering shall be used.
*/

/* # define     BOFLAG   1                       /* BYTE ORDERING FLAG */

/* define the number of bits in the primary opcode field of the instruction,
 * the destination field, the source 1 and source 2 fields.
 */
# define    OP       8                        /* size of opcode field */ 
# define    DEST     6                        /* size of destination  */
# define    SOURCE1  6                        /* size of source1      */
# define    SOURCE2  6                        /* size of source2      */

# define    REGs    32                        /* number of registers  */

# define    WORD    long
# define    FLAG    unsigned
# define    STATE   short 

# define    TRUE     1
# define    FALSE    0

# define    READ     0
# define    WRITE    1

/* The next four equates define the priorities that the various classes
 * of instructions have regarding writing results back into registers and
 * signalling exceptions.
 */

# define    PINT  0   /* Integer Priority */
# define    PFLT  1   /* Floating Point Priority */
# define    PMEM  2   /* Memory Priority */
# define    NA    3   /* Not Applicable, instruction doesnt write to regs */
# define    HIPRI 3   /* highest of these priorities */

/* The instruction registers are an artificial mechanism to speed up
 * simulator execution.  In the real processor, an instruction register
 * is 32 bits wide.  In the simulator, the 32 bit instruction is kept in
 * a structure field called rawop, and the instruction is partially decoded,
 * and split into various fields and flags which make up the other fields
 * of the structure.
 * The partial decode is done when the instructions are initially loaded
 * into simulator memory.  The simulator code memory is not an array of
 * 32 bit words, but is an array of instruction register structures.
 * Yes this wastes memory, but it executes much quicker.
 */

struct IR_FIELDS {
		    unsigned long   op:OP,
				    dest: DEST,
				    src1: SOURCE1,
				    src2: SOURCE2;
			      int   ltncy,
				    extime,
				    wb_pri;     /* writeback priority     */
		    unsigned short  imm_flags:2,/* immediate size         */
				    rs1_used:1, /* register source 1 used */
				    rs2_used:1, /* register source 2 used */
				    rsd_used:1, /* register source/dest. used */
				    c_flag:1,   /* complement      */
				    u_flag:1,   /* upper half word */
				    n_flag:1,   /* execute next    */
				    wb_flag:1,  /* uses writeback slot */
				    dest_64:1,  /* dest size       */
				    s1_64:1,    /* source 1 size   */
				    s2_64:1,    /* source 2 size   */
				    scale_flag:1, /* scaled register */
				    brk_flg:1;
                 };

struct	mem_segs {
	struct mem_wrd *seg;			/* pointer (returned by calloc) to segment */
	unsigned long baseaddr;			/* base load address from file headers */
	unsigned long endaddr;			/* Ending address of segment */
	int	      flags;			/* segment control flags (none defined 12/5/86) */
};

#define	MAXSEGS		(10)			/* max number of segment allowed */
#define	MEMSEGSIZE	(sizeof(struct mem_segs))/* size of mem_segs structure */


#define BRK_RD		(0x01)			/* break on memory read */
#define BRK_WR		(0x02)			/* break on memory write */
#define BRK_EXEC	(0x04)			/* break on execution */
#define	BRK_CNT		(0x08)			/* break on terminal count */


struct	mem_wrd {
	struct IR_FIELDS opcode;		/* simulator instruction break down */
	union {
		unsigned long  l;		/* memory element break down */
		unsigned short s[2];
		unsigned char  c[4];
	} mem;
};

#define	MEMWRDSIZE	(sizeof(struct mem_wrd))	/* size of each 32 bit memory model */

/* External declarations */

extern	struct mem_segs memory[];
extern	struct PROCESSOR m78000;

struct  PROCESSOR   {
	     unsigned WORD
			    ip,          /* execute instruction pointer */
			    vbr,         /* vector base register */
			    psr;         /* processor status register */

		    WORD    S1bus, /* source 1 */
                            S2bus, /* source 2 */
                            Dbus,  /* destination */
			    DAbus, /* data address bus */
                            ALU,
			    Regs[REGs],       /* data registers */
			    time_left[REGs],  /* max clocks before reg is available */
			    wb_pri[REGs],     /* writeback priority of reg */
			    SFU0_regs[REGs],  /* integer unit control regs */
			    SFU1_regs[REGs],  /* floating point control regs */
			    Scoreboard[REGs],
			    Vbr;
	    unsigned WORD   scoreboard,
			    Psw,
			    Tpsw;
		    FLAG   jump_pending:1;   /* waiting for a jump instr. */
                    };

# define    i26bit      1    /* size of immediate field */
# define    i16bit      2
# define    i10bit      3

/* Definitions for fields in psr */

# define mode  31
# define rbo   30
# define ser   29
# define carry 28
# define sf7m  11
# define sf6m  10
# define sf5m   9
# define sf4m   8
# define sf3m   7
# define sf2m   6
# define sf1m   5
# define mam    4
# define inm    3
# define exm    2
# define trm    1
# define ovfm   0

#define	    MODEMASK   (1<<(mode-1))
# define    SILENT     0   /* simulate without output to crt */
# define    VERBOSE    1   /* simulate in verbose mode */
# define    PR_INSTR   2   /* only print instructions */

# define    RESET      16 /* reset phase */

# define    PHASE1     0  /* data path phases */
# define    PHASE2     1

/* the 1 clock operations */

# define    ADDU        1
# define    ADDC        2
# define    ADDUC       3
# define    ADD         4

# define    SUBU    ADD+1
# define    SUBB    ADD+2
# define    SUBUB   ADD+3
# define    SUB     ADD+4

# define    AND     ADD+5
# define    OR      ADD+6
# define    XOR     ADD+7
# define    CMP     ADD+8
                        
/* the LOADS */

# define    LDAB    CMP+1
# define    LDAH    CMP+2
# define    LDA     CMP+3
# define    LDAD    CMP+4

# define    LDB   LDAD+1
# define    LDH   LDAD+2
# define    LD    LDAD+3
# define    LDD   LDAD+4
# define    LDBU  LDAD+5
# define    LDHU  LDAD+6

/* the STORES */

# define    STB    LDHU+1
# define    STH    LDHU+2
# define    ST     LDHU+3
# define    STD    LDHU+4

/* the exchange */

# define    XMEMBU LDHU+5
# define    XMEM   LDHU+6

/* the branches */
# define    JSR    STD+1
# define    BSR    STD+2
# define    BR     STD+3
# define    JMP    STD+4
# define    BB1    STD+5
# define    BB0    STD+6
# define    RTN    STD+7
# define    BCND   STD+8

/* the TRAPS */
# define    TB1    BCND+1
# define    TB0    BCND+2
# define    TCND   BCND+3
# define    RTE    BCND+4
# define    TBND   BCND+5

/* the MISC instructions */
# define    MUL     TBND + 1
# define    DIV     MUL  +2
# define    DIVU    MUL  +3
# define    MASK    MUL  +4
# define    FF0     MUL  +5
# define    FF1     MUL  +6
# define    CLR     MUL  +7
# define    SET     MUL  +8
# define    EXT     MUL  +9
# define    EXTU    MUL  +10
# define    MAK     MUL  +11
# define    ROT     MUL  +12

/* control register manipulations */

# define    LDCR    ROT  +1
# define    STCR    ROT  +2
# define    XCR     ROT  +3

# define    FLDCR    ROT  +4
# define    FSTCR    ROT  +5
# define    FXCR     ROT  +6


# define    NOP     XCR +1

/* floating point instructions */

# define    FADD    NOP +1
# define    FSUB    NOP +2
# define    FMUL    NOP +3
# define    FDIV    NOP +4
# define    FSQRT   NOP +5
# define    FCMP    NOP +6
# define    FIP     NOP +7
# define    FLT     NOP +8
# define    INT     NOP +9
# define    NINT    NOP +10
# define    TRNC    NOP +11
# define    FLDC   NOP +12
# define    FSTC   NOP +13
# define    FXC    NOP +14

# define UEXT(src,off,wid) ((((unsigned int)(src))>>(off)) & ((1<<(wid)) - 1))
# define SEXT(src,off,wid) (((((int)(src))<<(32-((off)+(wid)))) >>(32-(wid))) )
# define MAKE(src,off,wid) \
  ((((unsigned int)(src)) & ((1<<(wid)) - 1)) << (off))

# define opword(n) (unsigned long) (memaddr->mem.l)

/*  Constants and Masks */

#define SFU0       0x80000000
#define SFU1       0x84000000
#define SFU7       0x9c000000
#define RRI10      0xf0000000
#define RRR        0xf4000000
#define SFUMASK    0xfc00ffe0
#define RRRMASK    0xfc00ffe0
#define RRI10MASK  0xfc00fc00
#define DEFMASK    0xfc000000
#define CTRL       0x0000f000
#define CTRLMASK   0xfc00f800

/* Operands types */

#define HEX          1
#define REG          2
#define IND          3
#define CONT         3
#define IND          3
#define BF           4
#define REGSC        5    /* scaled register */
#define CRREG        6    /* control register */
#define FCRREG       7    /* floating point control register */
#define PCREL	     8
#define CONDMASK     9

/* Hashing Specification */

#define HASHVAL     79

/* Type definitions */

typedef unsigned int UINT;

/* Structure templates */

typedef struct {
   unsigned int offset:5;
   unsigned int width:6;
   unsigned int type:5;
} OPSPEC;

	struct SIM_FLAGS {
	      int  ltncy,   /* latency (max number of clocks needed to execute) */
		  extime,   /* execution time (min number of clocks needed to execute) */
		  wb_pri;   /* writeback slot priority */
   unsigned long   op:OP,   /* simulator version of opcode */
	     imm_flags:2,   /* 10,16 or 26 bit immediate flags */
	      rs1_used:1,   /* register source 1 used */
	      rs2_used:1,   /* register source 2 used */
	      rsd_used:1,   /* register source/dest used */
		c_flag:1,   /* complement */
		u_flag:1,   /* upper half word */
		n_flag:1,   /* execute next */
	       wb_flag:1,   /* uses writeback slot */
	       dest_64:1,   /* double precision dest */
		 s1_64:1,   /* double precision source 1 */
		 s2_64:1,   /* double precision source 2 */
	    scale_flag:1;   /* register is scaled */
};

typedef struct INSTRUCTAB {
   unsigned int  opcode;
   char          *mnemonic;
   OPSPEC        op1,op2,op3;
   struct SIM_FLAGS flgs;
   struct INSTRUCTAB    *next;
} INSTAB;


/* Opcode     Mnemonic       Op 1 Spec     Op 2 Spec    Op 3 Spec         Simflags             Next  */

static INSTAB  instructions[] =
{0xf400c800,"jsr      ",{0,5,REG}   ,{0,0,0}      ,{0,0,0}   , {2,2,NA,JSR ,          0,0,1,0,0,0,0,1,0,0,0,0}, NULL,
 0xf400cc00,"jsr.n    ",{0,5,REG}   ,{0,0,0}      ,{0,0,0}   , {1,1,NA,JSR ,          0,0,1,0,0,0,1,1,0,0,0,0}, NULL,
 0xf400c000,"jmp      ",{0,5,REG}   ,{0,0,0}      ,{0,0,0}   , {2,2,NA,JMP ,          0,0,1,0,0,0,0,1,0,0,0,0}, NULL,
 0xf400c400,"jmp.n    ",{0,5,REG}   ,{0,0,0}      ,{0,0,0}   , {1,1,NA,JMP ,          0,0,1,0,0,0,1,1,0,0,0,0}, NULL,
 0xc8000000,"bsr      ",{0,26,PCREL},{0,0,0}      ,{0,0,0}   , {2,2,NA,BSR ,     i26bit,0,0,0,0,0,0,1,0,0,0,0}, NULL,
 0xcc000000,"bsr.n    ",{0,26,PCREL},{0,0,0}      ,{0,0,0}   , {1,1,NA,BSR ,     i26bit,0,0,0,0,0,1,1,0,0,0,0}, NULL,
 0xc0000000,"br       ",{0,26,PCREL},{0,0,0}      ,{0,0,0}   , {2,2,NA,BR  ,     i26bit,0,0,0,0,0,0,1,0,0,0,0}, NULL,
 0xc4000000,"br.n     ",{0,26,PCREL},{0,0,0}      ,{0,0,0}   , {1,1,NA,BR  ,     i26bit,0,0,0,0,0,1,1,0,0,0,0}, NULL,
 0xd0000000,"bb0      ",{21,5,HEX}  ,{16,5,REG}   ,{0,16,PCREL},{2,2,NA,BB0,     i16bit,0,1,0,0,0,0,1,0,0,0,0}, NULL,
 0xd4000000,"bb0.n    ",{21,5,HEX}  ,{16,5,REG}   ,{0,16,PCREL},{1,1,NA,BB0,     i16bit,0,1,0,0,0,1,1,0,0,0,0}, NULL,
 0xd8000000,"bb1      ",{21,5,HEX},{16,5,REG}     ,{0,16,PCREL},{2,2,NA,BB1,     i16bit,0,1,0,0,0,0,1,0,0,0,0}, NULL,
 0xdc000000,"bb1.n    ",{21,5,HEX},{16,5,REG}     ,{0,16,PCREL},{1,1,NA,BB1,     i16bit,0,1,0,0,0,1,1,0,0,0,0}, NULL,
 0xf000d000,"tb0      ",{21,5,HEX}  ,{16,5,REG}   ,{0,10,HEX}, {2,2,NA,TB0 ,     i10bit,0,1,0,0,0,0,1,0,0,0,0}, NULL,
 0xf000d800,"tb1      ",{21,5,HEX}  ,{16,5,REG}   ,{0,10,HEX}, {2,2,NA,TB1 ,     i10bit,0,1,0,0,0,0,1,0,0,0,0}, NULL,
 0xe8000000,"bcnd     ",{21,5,CONDMASK},{16,5,REG},{0,16,PCREL},{2,2,NA,BCND,    i16bit,0,1,0,0,0,0,1,0,0,0,0}, NULL,
 0xec000000,"bcnd.n   ",{21,5,CONDMASK},{16,5,REG},{0,16,PCREL},{1,1,NA,BCND,    i16bit,0,1,0,0,0,1,1,0,0,0,0}, NULL,
 0xf000e800,"tcnd     ",{21,5,CONDMASK},{16,5,REG},{0,10,HEX}, {2,2,NA,TCND,     i10bit,0,1,0,0,0,0,1,0,0,0,0}, NULL,
 0xf8000000,"tbnd     ",{16,5,REG}  ,{0,16,HEX}   ,{0,0,0}   , {2,2,NA,TBND,     i10bit,1,0,0,0,0,0,1,0,0,0,0}, NULL,
 0xf400f800,"tbnd     ",{16,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {2,2,NA,TBND,          0,1,1,0,0,0,0,1,0,0,0,0}, NULL,
 0xf400fc00,"rte      ",{0,0,0}     ,{0,0,0}      ,{0,0,0}   , {2,2,NA,RTE ,          0,0,0,0,0,0,0,1,0,0,0,0}, NULL,
 0x1c000000,"ld.b     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {3,1,PMEM,LDB    ,i16bit,1,0,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4001c00,"ld.b     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {3,1,PMEM,LDB     ,    0,1,1,1,0,0,0,1,0,0,0,0}, NULL,
 0x0c000000,"ld.bu    ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {3,1,PMEM,LDBU,   i16bit,1,0,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4000c00,"ld.bu    ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {3,1,PMEM,LDBU        ,0,1,1,1,0,0,0,1,0,0,0,0}, NULL,
 0x18000000,"ld.h     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {3,1,PMEM,LDH    ,i16bit,1,0,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4001800,"ld.h     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {3,1,PMEM,LDH         ,0,1,1,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4001a00,"ld.h     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{3,1,PMEM,LDH         ,0,1,1,1,0,0,0,1,0,0,0,1}, NULL,
 0x08000000,"ld.hu    ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {3,1,PMEM,LDHU,   i16bit,1,0,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4000800,"ld.hu    ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {3,1,PMEM,LDHU        ,0,1,1,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4000a00,"ld.hu    ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{3,1,PMEM,LDHU        ,0,1,1,1,0,0,0,1,0,0,0,1}, NULL,
 0x14000000,"ld       ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {3,1,PMEM,LD     ,i16bit,1,0,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4001400,"ld       ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {3,1,PMEM,LD          ,0,1,1,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4001600,"ld       ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{3,1,PMEM,LD          ,0,1,1,1,0,0,0,1,0,0,0,1}, NULL,
 0x10000000,"ld.d     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {3,1,PMEM,LDD    ,i16bit,1,0,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4001000,"ld.d     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {3,1,PMEM,LDD         ,0,1,1,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4001200,"ld.d     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{3,1,PMEM,LDD         ,0,1,1,1,0,0,0,1,0,0,0,1}, NULL,
 0xf4001500,"ld.usr   ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {3,1,PMEM,LD          ,0,1,1,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4001700,"ld.usr   ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{3,1,PMEM,LD          ,0,1,1,1,0,0,0,1,0,0,0,1}, NULL,
 0x2c000000,"st.b     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,NA,STB      ,i16bit,1,0,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4002c00,"st.b     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,NA,STB           ,0,1,1,1,0,0,0,1,0,0,0,0}, NULL,
 0x28000000,"st.h     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,NA,STH      ,i16bit,1,0,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4002800,"st.h     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,NA,STH           ,0,1,1,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4002a00,"st.h     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{1,1,NA,STH           ,0,1,1,1,0,0,0,1,0,0,0,1}, NULL,
 0x24000000,"st       ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,NA,ST       ,i16bit,1,0,1,0,0,0,1,0,0,0,0}, NULL,
 0xf4002400,"st       ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,NA,ST            ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0xf4002600,"st       ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{1,1,NA,ST            ,0,1,1,1,0,0,0,1,0,0,0,1}   ,NULL,
 0x20000000,"st.d     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,NA,STD      ,i16bit,0,1,0,0,0,0,1,0,0,0,0}   ,NULL,
 0xf4002000,"st.d     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,NA,STD           ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0xf4002200,"st.d     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{1,1,NA,STD           ,0,1,1,1,0,0,0,1,0,0,0,1}   ,NULL,
 0xf4002500,"st.usr   ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,NA,ST            ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0xf4002700,"st.usr   ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{1,1,NA,ST            ,0,1,1,1,0,0,0,1,0,0,0,1}   ,NULL,
 0x00000000,"xmem.bu  ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {3,1,PMEM,XMEMBU ,i16bit,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0xf4000000,"xmem.bu  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {3,1,PMEM,XMEM        ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x04000000,"xmem     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {3,1,PMEM,XMEM   ,i16bit,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0xf4000400,"xmem     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {3,1,PMEM,XMEM        ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0xf4000600,"xmem     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{3,1,PMEM,XMEM        ,0,1,1,1,0,0,0,1,0,0,0,1}   ,NULL,
 0xf4000500,"xmem.usr ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {3,1,PMEM,XMEM        ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0xf4000700,"xmem.usr ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{3,1,PMEM,XMEM        ,0,1,1,1,0,0,0,1,0,0,0,1}   ,NULL,
 0xf4003e00,"lda.b    ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{1,1,PINT,LDAH,        0,1,1,1,0,0,0,0,0,0,0,1}   ,NULL,
 0xf4003a00,"lda.h    ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{1,1,PINT,LDAH,        0,1,1,1,0,0,0,0,0,0,0,1}   ,NULL,
 0xf4003600,"lda      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{1,1,PINT,LDA ,        0,1,1,1,0,0,0,0,0,0,0,1}   ,NULL,
 0xf4003200,"lda.d    ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REGSC},{1,1,PINT,LDAD,        0,1,1,1,0,0,0,0,0,0,0,1}   ,NULL,

 0x80004000,"ldcr     ",{21,5,REG}  ,{5,6,CRREG}  ,{0,0,0}    ,{1,1,PINT,LDCR,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x80008000,"stcr     ",{16,5,REG}  ,{5,6,CRREG}  ,{0,0,0}    ,{1,1,PINT,STCR,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x8000c000,"xcr      ",{21,5,REG}  ,{16,5,REG}   ,{5,6,CRREG},{1,1,PINT,XCR,         0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,

 0xf4006000,"addu     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,ADDU,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4006200,"addu.ci  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,ADDU,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4006100,"addu.co  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,ADDU,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4006300,"addu.cio ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,ADDU,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4006400,"subu     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,SUBU,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4006600,"subu.ci  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,SUBU,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4006500,"subu.co  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,SUBU,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4006700,"subu.cio ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,SUBU,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4006900,"divu     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {32,32,PINT,DIVU,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4006d00,"mul      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,4,PINT,MUL,      0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4007000,"add      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,ADD ,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4007200,"add.ci   ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,ADD ,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4007100,"add.co   ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,ADD ,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4007300,"add.cio  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,ADD ,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4007400,"sub      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,SUB ,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4007600,"sub.ci   ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,SUB ,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4007500,"sub.co   ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,SUB ,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4007700,"sub.cio  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,SUB ,        0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4007900,"div      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {32,32,PINT,DIV ,      0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4007d00,"cmp      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,CMP,         0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,

 0x60000000,"addu     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,ADDU,   i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x64000000,"subu     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,SUBU,   i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,

 0x68000000,"divu     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {32,32,PINT,DIVU, i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x6c000000,"mul      ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {4,1,PINT,MUL,    i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x70000000,"add      ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,ADD,    i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x74000000,"sub      ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,SUB,    i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x78000000,"div      ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {32,32,PINT,DIV,  i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x7c000000,"cmp      ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,CMP,    i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,

 0xf4004000,"and      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,AND         ,0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4004400,"and.c    ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,AND         ,0,1,1,1,1,0,0,0,0,0,0,0}   ,NULL,
 0xf4005800,"or       ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,OR          ,0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4005c00,"or.c     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,OR          ,0,1,1,1,1,0,0,0,0,0,0,0}   ,NULL,
 0xf4005000,"xor      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,XOR         ,0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4005400,"xor.c    ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,XOR         ,0,1,1,1,1,0,0,0,0,0,0,0}   ,NULL,
 0x40000000,"and      ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,AND    ,i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x44000000,"and.u    ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,AND    ,i16bit,1,0,1,0,1,0,0,0,0,0,0}   ,NULL,
 0x58000000,"or       ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,OR     ,i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x5c000000,"or.u     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,OR     ,i16bit,1,0,1,0,1,0,0,0,0,0,0}   ,NULL,
 0x50000000,"xor      ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,XOR    ,i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x54000000,"xor.u    ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,XOR    ,i16bit,1,0,1,0,1,0,0,0,0,0,0}   ,NULL,
 0x48000000,"mask     ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,MASK   ,i16bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0x4c000000,"mask.u   ",{21,5,REG}  ,{16,5,REG}   ,{0,16,HEX}, {1,1,PINT,MASK   ,i16bit,1,0,1,0,1,0,0,0,0,0,0}   ,NULL,
 0xf400ec00,"ff0      ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {1,1,PINT,FF0         ,0,0,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf400e800,"ff1      ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {1,1,PINT,FF1         ,0,0,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf0008000,"clr      ",{21,5,REG}  ,{16,5,REG}   ,{0,10,BF} , {1,1,PINT,CLR    ,i10bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf0008800,"set      ",{21,5,REG}  ,{16,5,REG}   ,{0,10,BF} , {1,1,PINT,SET    ,i10bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf0009000,"ext      ",{21,5,REG}  ,{16,5,REG}   ,{0,10,BF} , {1,1,PINT,EXT    ,i10bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf0009800,"extu     ",{21,5,REG}  ,{16,5,REG}   ,{0,10,BF} , {1,1,PINT,EXTU   ,i10bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf000a000,"mak      ",{21,5,REG}  ,{16,5,REG}   ,{0,10,BF} , {1,1,PINT,MAK    ,i10bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf000a800,"rot      ",{21,5,REG}  ,{16,5,REG}   ,{0,10,BF} , {1,1,PINT,ROT    ,i10bit,1,0,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4008000,"clr      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,CLR         ,0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4008800,"set      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,SET         ,0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4009000,"ext      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,EXT         ,0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf4009800,"extu     ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,EXTU        ,0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf400a000,"mak      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,MAK         ,0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,
 0xf400a800,"rot      ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {1,1,PINT,ROT         ,0,1,1,1,0,0,0,0,0,0,0,0}   ,NULL,

 0x84002800,"fadd.sss ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {5,1,PFLT,FADD        ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84002880,"fadd.ssd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FADD        ,0,1,1,1,0,0,0,1,0,0,1,0}   ,NULL,
 0x84002a00,"fadd.sds ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FADD        ,0,1,1,1,0,0,0,1,0,1,0,0}   ,NULL,
 0x84002a80,"fadd.sdd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FADD        ,0,1,1,1,0,0,0,1,0,1,1,0}   ,NULL,
 0x84002820,"fadd.dss ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FADD        ,0,1,1,1,0,0,0,1,1,0,0,0}   ,NULL,
 0x840028a0,"fadd.dsd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FADD        ,0,1,1,1,0,0,0,1,1,0,1,0}   ,NULL,
 0x84002a20,"fadd.dds ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FADD        ,0,1,1,1,0,0,0,1,1,1,0,0}   ,NULL,
 0x84002aa0,"fadd.ddd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FADD        ,0,1,1,1,0,0,0,1,1,1,1,0}   ,NULL,
 0x84003000,"fsub.sss ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {5,1,PFLT,FSUB        ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84003080,"fsub.ssd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FSUB        ,0,1,1,1,0,0,0,1,0,0,1,0}   ,NULL,
 0x84003200,"fsub.sds ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FSUB        ,0,1,1,1,0,0,0,1,0,1,0,0}   ,NULL,
 0x84003280,"fsub.sdd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FSUB        ,0,1,1,1,0,0,0,1,0,1,1,0}   ,NULL,
 0x84003020,"fsub.dss ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FSUB        ,0,1,1,1,0,0,0,1,1,0,0,0}   ,NULL,
 0x840030a0,"fsub.dsd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FSUB        ,0,1,1,1,0,0,0,1,1,0,1,0}   ,NULL,
 0x84003220,"fsub.dds ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FSUB        ,0,1,1,1,0,0,0,1,1,1,0,0}   ,NULL,
 0x840032a0,"fsub.ddd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,2,PFLT,FSUB        ,0,1,1,1,0,0,0,1,1,1,1,0}   ,NULL,
 0x84000000,"fmul.sss ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,1,PFLT,FMUL        ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84000080,"fmul.ssd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {9,2,PFLT,FMUL        ,0,1,1,1,0,0,0,1,0,0,1,0}   ,NULL,
 0x84000200,"fmul.sds ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {9,2,PFLT,FMUL        ,0,1,1,1,0,0,0,1,0,1,0,0}   ,NULL,
 0x84000280,"fmul.sdd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {9,2,PFLT,FMUL        ,0,1,1,1,0,0,0,1,0,1,1,0}   ,NULL,
 0x84000020,"fmul.dss ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {9,2,PFLT,FMUL        ,0,1,1,1,0,0,0,1,1,0,0,0}   ,NULL,
 0x840000a0,"fmul.dsd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {9,2,PFLT,FMUL        ,0,1,1,1,0,0,0,1,1,0,1,0}   ,NULL,
 0x84000220,"fmul.dds ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {9,2,PFLT,FMUL        ,0,1,1,1,0,0,0,1,1,1,0,0}   ,NULL,
 0x840002a0,"fmul.ddd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {9,2,PFLT,FMUL        ,0,1,1,1,0,0,0,1,1,1,1,0}   ,NULL,
 0x84007000,"fdiv.sss ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {30,30,PFLT,FDIV      ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84007080,"fdiv.ssd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {60,60,PFLT,FDIV      ,0,1,1,1,0,0,0,1,0,0,1,0}   ,NULL,
 0x84007200,"fdiv.sds ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {60,60,PFLT,FDIV      ,0,1,1,1,0,0,0,1,0,1,0,0}   ,NULL,
 0x84007280,"fdiv.sdd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {60,60,PFLT,FDIV      ,0,1,1,1,0,0,0,1,0,1,1,0}   ,NULL,
 0x84007020,"fdiv.dss ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {60,60,PFLT,FDIV      ,0,1,1,1,0,0,0,1,1,0,0,0}   ,NULL,
 0x840070a0,"fdiv.dsd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {60,60,PFLT,FDIV      ,0,1,1,1,0,0,0,1,1,0,1,0}   ,NULL,
 0x84007220,"fdiv.dds ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {60,60,PFLT,FDIV      ,0,1,1,1,0,0,0,1,1,1,0,0}   ,NULL,
 0x840072a0,"fdiv.ddd ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {60,60,PFLT,FDIV      ,0,1,1,1,0,0,0,1,1,1,1,0}   ,NULL,
 0x84007800,"fsqrt.ss ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {5,1,PFLT,FLT         ,0,0,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84007880,"fsqrt.sd ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {5,1,PFLT,FLT         ,0,0,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84007820,"fsqrt.ds ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {5,1,PFLT,FLT         ,0,0,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x840078a0,"fsqrt.dd ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {6,1,PFLT,FLT         ,0,0,1,1,0,0,0,1,1,0,0,0}   ,NULL,
 0x84003800,"fcmp.ss  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {5,1,PFLT,FCMP        ,0,1,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84003880,"fcmp.sd  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,1,PFLT,FCMP        ,0,1,1,1,0,0,0,1,0,1,0,0}   ,NULL,
 0x84003a00,"fcmp.ds  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,1,PFLT,FCMP        ,0,1,1,1,0,0,0,1,1,0,0,0}   ,NULL,
 0x84003a80,"fcmp.dd  ",{21,5,REG}  ,{16,5,REG}   ,{0,5,REG} , {6,1,PFLT,FCMP        ,0,1,1,1,0,0,0,1,1,1,0,0}   ,NULL,
 0x84002000,"flt.s    ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {5,1,PFLT,FLT         ,0,0,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84002020,"flt.d    ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {6,1,PFLT,FLT         ,0,0,1,1,0,0,0,1,1,0,0,0}   ,NULL,
 0x84004800,"int.s    ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {5,1,PFLT,INT         ,0,0,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84004880,"int.d    ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {6,1,PFLT,INT         ,0,0,1,1,0,0,0,1,1,0,0,0}   ,NULL,
 0x84005000,"nint.s   ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {5,1,PFLT,INT         ,0,0,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84005080,"nint.d   ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {6,1,PFLT,INT         ,0,0,1,1,0,0,0,1,1,0,0,0}   ,NULL,
 0x84005800,"trnc.s   ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {5,1,PFLT,TRNC        ,0,0,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x84005880,"trnc.d   ",{21,5,REG}  ,{0,5,REG}    ,{0,0,0}   , {6,1,PFLT,TRNC        ,0,0,1,1,0,0,0,1,1,0,0,0}   ,NULL,

 0x80004800,"fldcr    ",{21,5,REG}  ,{5,6,FCRREG} ,{0,0,0}   , {1,1,PFLT,FLDC        ,0,0,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x80008800,"fstcr    ",{16,5,REG}  ,{5,6,FCRREG} ,{0,0,0}   , {1,1,PFLT,FSTC        ,0,0,1,1,0,0,0,1,0,0,0,0}   ,NULL,
 0x8000c800,"fxcr     ",{21,5,REG}  ,{16,5,REG}   ,{5,6,FCRREG} , {1,1,PFLT,FXC         ,0,0,1,1,0,0,0,1,0,0,0,0}   ,NULL};

