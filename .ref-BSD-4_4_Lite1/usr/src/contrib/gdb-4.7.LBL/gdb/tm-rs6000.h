/* Parameters for target execution on an RS6000, for GDB, the GNU debugger.
   Copyright (C) 1986, 1987, 1989, 1991 Free Software Foundation, Inc.
   Contributed by IBM Corporation.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

extern int	symtab_relocated;

/* Minimum possible text address in AIX */

#define TEXT_SEGMENT_BASE	0x10000000


/* text addresses in a core file does not necessarily match to symbol table,
   if symbol table relocation wasn't done yet. */

#define	CORE_NEEDS_RELOCATION(PC)	\
  if (!symtab_relocated && !inferior_pid && (PC) >  TEXT_SEGMENT_BASE)	\
    (PC) -= ( TEXT_SEGMENT_BASE + text_adjustment (exec_bfd));

/* Load segment of a given pc value. */

#define	PC_LOAD_SEGMENT(PC)	pc_load_segment_name(PC)


/* Conversion between a register number in stab string to actual register num. */

#define	STAB_REG_TO_REGNUM(value)	(value)

/* return true if a given `pc' value is in `call dummy' function. */

#define	PC_IN_CALL_DUMMY(STOP_PC, STOP_SP, STOP_FRAME_ADDR)	\
	(STOP_SP < STOP_PC && STOP_PC < STACK_END_ADDR)

/* For each symtab, we keep track of which BFD it came from.  */
#define	EXTRA_SYMTAB_INFO	\
	unsigned    nonreloc:1;		/* TRUE if non relocatable */

#define	INIT_EXTRA_SYMTAB_INFO(symtab)	\
	symtab->nonreloc = 0;		\

extern unsigned int text_start, data_start;
extern int inferior_pid;
extern char *corefile;

/* setpgrp() messes up controling terminal. The other version of it
   requires libbsd.a. */
#define	setpgrp(XX,YY)		setpgid (XX, YY)

/* We are missing register descriptions in the system header files. Sigh! */

struct regs {
	int	gregs [32];		/* general purpose registers */
	int	pc;			/* program conter	*/
	int	ps;			/* processor status, or machine state */
};

struct fp_status {
	double	fpregs [32];		/* floating GP registers */
};


/* To be used by function_frame_info. */

struct aix_framedata {
  int	offset;				/* # of bytes in gpr's and fpr's are saved */
  int	saved_gpr;			/* smallest # of saved gpr */
  int	saved_fpr;			/* smallest # of saved fpr */
  int	alloca_reg;			/* alloca register number (frame ptr) */
  char	frameless;			/* true if frameless functions. */
};

void 
function_frame_info PARAMS ((CORE_ADDR, struct aix_framedata *));

/* Define the byte order of the machine.  */

#define TARGET_BYTE_ORDER	BIG_ENDIAN

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#undef NAMES_HAVE_UNDERSCORE

/* AIX's assembler doesn't grok dollar signs in identifiers.
   So we use dots instead.  This item must be coordinated with G++. */
#undef CPLUS_MARKER
#define CPLUS_MARKER '.'

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

#define SKIP_PROLOGUE(pc)	pc = skip_prologue (pc)

/* If PC is in some function-call trampoline code, return the PC
   where the function itself actually starts.  If not, return NULL.  */

#define	SKIP_TRAMPOLINE_CODE(pc)	skip_trampoline_code (pc)

/* When a child process is just starting, we sneak in and relocate
   the symbol table (and other stuff) after the dynamic linker has
   figured out where they go. But we want to do this relocation just
   once. */

extern int loadinfotextindex;

#define	SOLIB_CREATE_INFERIOR_HOOK(PID)	\
  do {					\
    if (loadinfotextindex == 0)	\
	xcoff_relocate_symtab (PID);	\
  } while (0)
	

/* Number of trap signals we need to skip over, once the inferior process
   starts running. */

#define	START_INFERIOR_TRAPS_EXPECTED	2

/* AIX might return a sigtrap, with a "stop after load" status. It should
   be ignored by gdb, shouldn't be mixed up with breakpoint traps. */

/* Another little glitch  in AIX is signal 0. I have no idea why wait(2)
   returns with this status word. It looks harmless. */

#define SIGTRAP_STOP_AFTER_LOAD(W)	\
 if ( (W) == 0x57c || (W) == 0x7f) {	\
   if ((W)==0x57c && breakpoints_inserted) {	\
     mark_breakpoints_out ();		\
     insert_breakpoints ();		\
     insert_step_breakpoint ();		\
   }					\
   resume (0, 0);			\
   continue;				\
 }

/* In xcoff, we cannot process line numbers when we see them. This is
   mainly because we don't know the boundaries of the include files. So,
   we postpone that, and then enter and sort(?) the whole line table at
   once, when we are closing the current symbol table in end_symtab(). */

#define	PROCESS_LINENUMBER_HOOK()	aix_process_linenos ()
   
   
/* When a target process or core-file has been attached, we sneak in
   and figure out where the shared libraries have got to. In case there
   is no inferior_process exists (e.g. bringing up a core file), we can't
   attemtp to relocate symbol table, since we don't have information about
   load segments. */

#define	SOLIB_ADD(a, b, c)	\
   if (inferior_pid)	xcoff_relocate_symtab (inferior_pid)

/* Immediately after a function call, return the saved pc.
   Can't go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define	SAVED_PC_AFTER_CALL(frame)	\
	(register_valid [LR_REGNUM] ? 	\
	  (*(int*)&registers[REGISTER_BYTE (LR_REGNUM)]) :	\
	  read_register (LR_REGNUM))

/*#define SAVED_PC_AFTER_CALL(frame)	saved_pc_after_call(frame) */


/* Address of end of stack space.  */

#define STACK_END_ADDR 0x2ff80000

/* Stack grows downward.  */

#define INNER_THAN <

#if 0
/* No, we shouldn't use this. push_arguments() should leave stack in a
   proper alignment! */
/* Stack has strict alignment. */

#define STACK_ALIGN(ADDR)	(((ADDR)+7)&-8)
#endif

/* This is how argumets pushed onto stack or passed in registers. */

#define	PUSH_ARGUMENTS(nargs, args, sp, struct_return, struct_addr) \
  sp = push_arguments(nargs, args, sp, struct_return, struct_addr)

/* Sequence of bytes for breakpoint instruction.  */

#define BREAKPOINT {0x7d, 0x82, 0x10, 0x08}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 0

/* Nonzero if instruction at PC is a return instruction.  */
/* Allow any of the return instructions, including a trapv and a return
   from interrupt.  */

#define ABOUT_TO_RETURN(pc)  \
   ((read_memory_integer (pc, 4) & 0xfe8007ff) == 0x4e800020)

/* Return 1 if P points to an invalid floating point value.  */

#define INVALID_FLOAT(p, len) 0   /* Just a first guess; not checked */

/* Largest integer type */

#define LONGEST long

/* Name of the builtin type for the LONGEST type above. */

#define BUILTIN_TYPE_LONGEST builtin_type_long

/* Say how long (ordinary) registers are.  */

#define REGISTER_TYPE long

/* Number of machine registers */

#define NUM_REGS 71

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES  \
 {"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",  \
  "r8", "r9", "r10","r11","r12","r13","r14","r15", \
  "r16","r17","r18","r19","r20","r21","r22","r23", \
  "r24","r25","r26","r27","r28","r29","r30","r31", \
  "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",  \
  "f8", "f9", "f10","f11","f12","f13","f14","f15", \
  "f16","f17","f18","f19","f20","f21","f22","f23", \
  "f24","f25","f26","f27","f28","f29","f30","f31", \
  "pc", "ps", "cnd", "lr", "cnt", "xer", "mq" }

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#define FP_REGNUM 1		/* Contains address of executing stack frame */
#define SP_REGNUM 1		/* Contains address of top of stack */
#define	TOC_REGNUM 2		/* TOC register */
#define FP0_REGNUM 32		/* Floating point register 0 */
#define	GP0_REGNUM 0		/* GPR register 0 */
#define FP0_REGNUM 32		/* FPR (Floating point) register 0 */
#define FPLAST_REGNUM 63	/* Last floating point register */  

/* Special purpose registers... */
/* P.S. keep these in the same order as in /usr/mstsave.h `mstsave' structure, for
   easier processing */

#define PC_REGNUM 64		/* Program counter (instruction address %iar) */
#define PS_REGNUM 65		/* Processor (or machine) status (%msr) */
#define	CR_REGNUM 66		/* Condition register */
#define	LR_REGNUM 67		/* Link register */
#define	CTR_REGNUM 68		/* Count register */
#define	XER_REGNUM 69		/* Fixed point exception registers */
#define	MQ_REGNUM 70		/* Multiply/quotient register */

#define	FIRST_SP_REGNUM 64	/* first special register number */
#define LAST_SP_REGNUM  70	/* last special register number */

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.

	32 4-byte gpr's
	32 8-byte fpr's
	7  4-byte special purpose registers, 

   total 416 bytes. Keep some extra space for now, in case to add more. */

#define REGISTER_BYTES 420


/* Index within `registers' of the first byte of the space for
   register N.  */

#define REGISTER_BYTE(N)  \
 (								\
  ((N) > FPLAST_REGNUM) ? ((((N) - FPLAST_REGNUM -1) * 4) + 384)\
  :((N) >= FP0_REGNUM) ? ((((N) - FP0_REGNUM) * 8) + 128)	\
  :((N) * 4) )

/* Number of bytes of storage in the actual machine representation
   for register N. */
/* Note that the unsigned cast here forces the result of the
   subtractiion to very high positive values if N < FP0_REGNUM */

#define REGISTER_RAW_SIZE(N) (((unsigned)(N) - FP0_REGNUM) < 32 ? 8 : 4)

/* Number of bytes of storage in the program's representation
   for register N.  On the RS6000, all regs are 4 bytes
   except the floating point regs which are 8-byte doubles.  */

#define REGISTER_VIRTUAL_SIZE(N) (((unsigned)(N) - FP0_REGNUM) < 32 ? 8 : 4)

/* Largest value REGISTER_RAW_SIZE can have.  */

#define MAX_REGISTER_RAW_SIZE 8

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */

#define MAX_REGISTER_VIRTUAL_SIZE 8

/* convert a dbx stab register number (from `r' declaration) to a gdb REGNUM */

#define STAB_REG_TO_REGNUM(value)	(value)

/* Nonzero if register N requires conversion
   from raw format to virtual format.  */

#define REGISTER_CONVERTIBLE(N) ((N) >= FP0_REGNUM && (N) <= FPLAST_REGNUM)

/* Convert data from raw format for register REGNUM
   to virtual format for register REGNUM.  */

#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,FROM,TO)	\
   bcopy ((FROM), (TO), REGISTER_RAW_SIZE (REGNUM))

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */

#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO)	\
   bcopy ((FROM), (TO), REGISTER_RAW_SIZE (REGNUM))

/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#define REGISTER_VIRTUAL_TYPE(N) \
 (((unsigned)(N) - FP0_REGNUM) < 32 ? builtin_type_double : builtin_type_int)

/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function. */
/* in RS6000, struct return addresses are passed as an extra parameter in r3.
   In function return, callee is not responsible of returning this address back.
   Since gdb needs to find it, we will store in a designated variable
   `rs6000_struct_return_address'. */

extern unsigned int rs6000_struct_return_address;

#define STORE_STRUCT_RETURN(ADDR, SP)	\
  { write_register (3, (ADDR));		\
    rs6000_struct_return_address = (unsigned int)(ADDR); }

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  */

/* #define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  bcopy (REGBUF, VALBUF, TYPE_LENGTH (TYPE)) */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  extract_return_value(TYPE,REGBUF,VALBUF)

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  {									\
    if (TYPE_CODE (TYPE) == TYPE_CODE_FLT)				\
									\
     /* Floating point values are returned starting from FPR1 and up.	\
	Say a double_double_double type could be returned in		\
	FPR1/FPR2/FPR3 triple. */					\
									\
      write_register_bytes (REGISTER_BYTE (FP0_REGNUM+1), (VALBUF),	\
						TYPE_LENGTH (TYPE));	\
    else								\
      /* Everything else is returned in GPR3 and up. */			\
      write_register_bytes (REGISTER_BYTE (GP0_REGNUM+3), (VALBUF),	\
						TYPE_LENGTH (TYPE));	\
  }


/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF)	rs6000_struct_return_address

/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer. */

/* In the case of the RS6000, the frame's nominal address
   is the address of a 4-byte word containing the calling frame's address.  */

#define FRAME_CHAIN(thisframe)  \
  (!inside_entry_file ((thisframe)->pc) ?	\
   read_memory_integer ((thisframe)->frame, 4) :\
   0)

/* Define other aspects of the stack frame.  */

/* A macro that tells us whether the function invocation represented
   by FI does not have a frame on the stack associated with it.  If it
   does not, FRAMELESS is set to 1, else 0.  */

#define FRAMELESS_FUNCTION_INVOCATION(FI, FRAMELESS) \
	FRAMELESS = frameless_function_invocation (FI)

/* Functions calling alloca() change the value of the stack pointer. We
   need to use initial stack pointer (which is saved in r31 by gcc) in 
   such cases. If a compiler emits traceback table, then we should use the
   alloca register specified in traceback table. FIXME. */
/* Also, it is a good idea to cache information about frame's saved registers
   in the frame structure to speed things up. See tm-m88k.h. FIXME. */

#define	EXTRA_FRAME_INFO	\
	CORE_ADDR initial_sp;			/* initial stack pointer. */ \
	struct frame_saved_regs *cache_fsr;	/* saved registers	  */

/* Frameless function invocation in IBM RS/6000 is half-done. It perfectly
   sets up a new frame, e.g. a new frame (in fact stack) pointer, etc, but it 
   doesn't save the %pc. In the following, even though it is considered a 
   frameless invocation, we still need to walk one frame up. */

#define	INIT_EXTRA_FRAME_INFO(fromleaf, fi)	\
	fi->initial_sp = 0;		\
	fi->cache_fsr = 0;

#define FRAME_SAVED_PC(FRAME)		\
	read_memory_integer (read_memory_integer ((FRAME)->frame, 4)+8, 4)

#define FRAME_ARGS_ADDRESS(FI)	\
  (((struct frame_info*)(FI))->initial_sp ?		\
	((struct frame_info*)(FI))->initial_sp :	\
	frame_initial_stack_address (FI))

#define FRAME_LOCALS_ADDRESS(FI)	FRAME_ARGS_ADDRESS(FI)


/* Set VAL to the number of args passed to frame described by FI.
   Can set VAL to -1, meaning no way to tell.  */

/* We can't tell how many args there are
   now that the C compiler delays popping them.  */

#define FRAME_NUM_ARGS(val,fi) (val = -1)

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 8	/* Not sure on this. FIXMEmgo */

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */
/* In the following implementation for RS6000, we did *not* save sp. I am
   not sure if it will be needed. The following macro takes care of gpr's
   and fpr's only. */

#define FRAME_FIND_SAVED_REGS(FRAME_INFO, FRAME_SAVED_REGS)		\
{									\
  int ii;								\
  CORE_ADDR frame_addr, func_start;					\
  struct aix_framedata fdata;						\
									\
  /* find the start of the function and collect info about its frame. */\
									\
  func_start = get_pc_function_start ((FRAME_INFO)->pc) + FUNCTION_START_OFFSET; \
  function_frame_info (func_start, &fdata);				\
  bzero (&(FRAME_SAVED_REGS), sizeof (FRAME_SAVED_REGS));		\
									\
  /* if there were any saved registers, figure out parent's stack pointer. */ \
  frame_addr = 0;							\
  /* the following is true only if the frame doesn't have a call to alloca(), \
      FIXME. */								\
  if (fdata.saved_fpr >= 0 || fdata.saved_gpr >= 0) {			\
    if ((FRAME_INFO)->prev && (FRAME_INFO)->prev->frame)		\
      frame_addr = (FRAME_INFO)->prev->frame;				\
    else								\
      frame_addr = read_memory_integer ((FRAME_INFO)->frame, 4);	\
  }									\
									\
  /* if != -1, fdata.saved_fpr is the smallest number of saved_fpr. All fpr's \
     from saved_fpr to fp31 are saved right underneath caller stack pointer, \
     starting from fp31 first. */					\
									\
  if (fdata.saved_fpr >= 0) {						\
    for (ii=31; ii >= fdata.saved_fpr; --ii) 				\
      (FRAME_SAVED_REGS).regs [FP0_REGNUM + ii] = frame_addr - ((32 - ii) * 8); \
    frame_addr -= (32 - fdata.saved_fpr) * 8;				\
  }									\
									\
  /* if != -1, fdata.saved_gpr is the smallest number of saved_gpr. All gpr's \
     from saved_gpr to gpr31 are saved right under saved fprs, starting	\
     from r31 first. */							\
									\
  if (fdata.saved_gpr >= 0)						\
    for (ii=31; ii >= fdata.saved_gpr; --ii)				\
      (FRAME_SAVED_REGS).regs [ii] = frame_addr - ((32 - ii) * 4);	\
}


/* Things needed for making the inferior call functions.  */

/* Push an empty stack frame, to record the current PC, etc.  */
/* Change these names into rs6k_{push, pop}_frame(). FIXMEmgo. */

#define PUSH_DUMMY_FRAME	push_dummy_frame ()

/* Discard from the stack the innermost frame, 
   restoring all saved registers.  */

#define POP_FRAME	pop_frame ()

/* This sequence of words is the instructions:

	mflr	r0		// 0x7c0802a6
				// save fpr's
	stfd	r?, num(r1)	// 0xd8010000 there should be 32 of this??
				// save gpr's
	stm	r0, num(r1)	// 0xbc010000
	stu	r1, num(r1)	// 0x94210000

	// the function we want to branch might be in a different load 
	// segment. reset the toc register. Note that the actual toc address
	// will be fix by fix_call_dummy () along with function address.

	st	r2, 0x14(r1)	// 0x90410014 save toc register
	liu	r2, 0x1234	// 0x3c401234 reset a new toc value 0x12345678
	oril	r2, r2,0x5678   // 0x60425678	

				// load absolute address 0x12345678 to r0
	liu	r0, 0x1234	// 0x3c001234
	oril	r0, r0,0x5678	// 0x60005678
	mtctr	r0		// 0x7c0903a6 ctr <- r0
	bctrl			// 0x4e800421 jump subroutine 0x12345678 (%ctr)
	cror	0xf, 0xf, 0xf	// 0x4def7b82
	brpt			// 0x7d821008, breakpoint
	cror	0xf, 0xf, 0xf	// 0x4def7b82 (for 8 byte alignment)


  We actually start executing by saving the toc register first, since the pushing 
  of the registers is done by PUSH_DUMMY_FRAME.  If this were real code,
  the arguments for the function called by the `bctrl' would be pushed
  between the `stu' and the `bctrl', and we could allow it to execute through.
  But the arguments have to be pushed by GDB after the PUSH_DUMMY_FRAME is done,
  and we cannot allow to push the registers again.
*/
	
#define CALL_DUMMY {0x7c0802a6, 0xd8010000, 0xbc010000, 0x94210000, \
		    0x90410014, 0x3c401234, 0x60425678,		    \
		    0x3c001234, 0x60005678, 0x7c0903a6, 0x4e800421, \
		    0x4def7b82, 0x7d821008, 0x4def7b82 }


/* keep this as multiple of 8 (%sp requires 8 byte alignment) */
#define CALL_DUMMY_LENGTH 56

#define CALL_DUMMY_START_OFFSET 16

/* Insert the specified number of args and function address
   into a call sequence of the above form stored at DUMMYNAME.  */

#define FIX_CALL_DUMMY(dummyname, pc, fun, nargs, args, type, using_gcc) \
	fix_call_dummy(dummyname, pc, fun, nargs, type)


/* Signal handler for SIGWINCH `window size changed'. */

#define	SIGWINCH_HANDLER  aix_resizewindow
extern	void	aix_resizewindow ();

/* `lines_per_page' and `chars_per_line' are local to utils.c. Rectify this. */

#define	SIGWINCH_HANDLER_BODY	\
									\
/* Respond to SIGWINCH `window size changed' signal, and reset GDB's	\
   window settings approproatelt. */					\
									\
void 						\
aix_resizewindow ()				\
{						\
  int fd = fileno (stdout);			\
  if (isatty (fd)) {				\
    int val;					\
						\
    val = atoi (termdef (fd, 'l'));		\
    if (val > 0)				\
      lines_per_page = val;			\
    val = atoi (termdef (fd, 'c'));		\
    if (val > 0)				\
      chars_per_line = val;			\
  }						\
}


/* Flag for machine-specific stuff in shared files.  FIXME */
#define IBM6000_TARGET

/* RS6000/AIX does not support PT_STEP.  Has to be simulated.  */

#define NO_SINGLE_STEP
