/* Parameters for targe of a Gould Powernode, for GDB, the GNU debugger.
   Copyright (C) 1986, 1987, 1989, 1991 Free Software Foundation, Inc.

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

#define GOULD_PN

#define TARGET_BYTE_ORDER BIG_ENDIAN

/* This code appears in libraries on Gould machines.  Ignore it. */
#define IGNORE_SYMBOL(type) (type == N_ENTRY)

/* We don't want the extra gnu symbols on the machine;
   they will interfere with the shared segment symbols.  */
#define NO_GNU_STABS

/* Macro for text-offset and data info (in PN a.out format).  */
#define	TEXTINFO						\
	text_offset = N_TXTOFF (exec_coffhdr);			\
	exec_data_offset = N_TXTOFF (exec_coffhdr)		\
		+ exec_aouthdr.a_text

/* Macro for number of symbol table entries */
#define END_OF_TEXT_DEFAULT					\
	(0xffffff)

/* Macro for number of symbol table entries */
#define NUMBER_OF_SYMBOLS					\
	(coffhdr.f_nsyms)

/* Macro for file-offset of symbol table (in usual a.out format).  */
#define SYMBOL_TABLE_OFFSET					\
	N_SYMOFF (coffhdr)

/* Macro for file-offset of string table (in usual a.out format).  */
#define STRING_TABLE_OFFSET					\
	(N_STROFF (coffhdr) + sizeof(int))

/* Macro to store the length of the string table data in INTO.  */
#define READ_STRING_TABLE_SIZE(INTO)				\
	{ INTO = hdr.a_stsize; }

/* Macro to declare variables to hold the file's header data.  */
#define DECLARE_FILE_HEADERS  struct old_exec hdr;		\
			      FILHDR coffhdr

/* Macro to read the header data from descriptor DESC and validate it.
   NAME is the file name, for error messages.  */
#define READ_FILE_HEADERS(DESC, NAME)				\
{ val = myread (DESC, &coffhdr, sizeof coffhdr);		\
  if (val < 0)							\
    perror_with_name (NAME);					\
  val = myread (DESC, &hdr, sizeof hdr);			\
  if (val < 0)							\
    perror_with_name (NAME);					\
  if (coffhdr.f_magic != GNP1MAGIC)				\
    error ("File \"%s\" not in coff executable format.", NAME);	\
  if (N_BADMAG (hdr))						\
    error ("File \"%s\" not in executable format.", NAME); }

/* Define COFF and other symbolic names needed on NP1 */
#define	NS32GMAGIC	GDPMAGIC
#define	NS32SMAGIC	PN_MAGIC
/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */
#define NAMES_HAVE_UNDERSCORE

/* Offset from address of function to start of its code.
   Zero on most machines.  */
#define FUNCTION_START_OFFSET	4

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  One PN we can have one or two startup
   sequences depending on the size of the local stack:

   Either:
      "suabr b2, #"
   of
      "lil r4, #", "suabr b2, #(r4)"

   "lwbr b6, #", "stw r1, 8(b2)"
   Optional "stwbr b3, c(b2)"
   Optional "trr r2,r7"      (Gould first argument register passing)
     or
   Optional "stw r2,8(b3)"   (Gould first argument register passing)
 */
#define SKIP_PROLOGUE(pc) { 						\
	register int op = read_memory_integer ((pc), 4);		\
	if ((op & 0xffff0000) == 0x580B0000) { 				\
	    pc += 4;							\
	    op = read_memory_integer ((pc), 4);				\
	    if ((op & 0xffff0000) == 0x59400000) {			\
		pc += 4;						\
	        op = read_memory_integer ((pc), 4);			\
		if ((op & 0xffff0000) == 0x5F000000) {			\
		    pc += 4;						\
	            op = read_memory_integer ((pc), 4);			\
		    if (op == 0xD4820008) {				\
		    	pc += 4;					\
	            	op = read_memory_integer ((pc), 4);		\
		    	if (op == 0x5582000C) {				\
		    	    pc += 4;					\
	                    op = read_memory_integer ((pc), 2);		\
		            if (op == 0x2fa0) {				\
		    	        pc += 2;				\
		            } else {					\
	                        op = read_memory_integer ((pc), 4);	\
		                if (op == 0xd5030008) {			\
		    	            pc += 4;				\
		                }					\
		            }						\
		        } else {					\
	                    op = read_memory_integer ((pc), 2);		\
		            if (op == 0x2fa0) {				\
		    	        pc += 2;				\
		            }						\
		        }						\
		    }							\
		}							\
	    }								\
	} 								\
	if ((op & 0xffff0000) == 0x59000000) { 				\
	    pc += 4;							\
	    op = read_memory_integer ((pc), 4);				\
	    if ((op & 0xffff0000) == 0x5F000000) {			\
		pc += 4;						\
	        op = read_memory_integer ((pc), 4);			\
		if (op == 0xD4820008) {					\
		    pc += 4;						\
	            op = read_memory_integer ((pc), 4);			\
		    if (op == 0x5582000C) {				\
		    	pc += 4;					\
	                op = read_memory_integer ((pc), 2);		\
		        if (op == 0x2fa0) {				\
		    	    pc += 2;					\
		        } else {					\
	                    op = read_memory_integer ((pc), 4);		\
		            if (op == 0xd5030008) {			\
		    	        pc += 4;				\
		            }						\
		        }						\
		    } else {						\
	                op = read_memory_integer ((pc), 2);		\
		        if (op == 0x2fa0) {				\
		    	    pc += 2;					\
		        }						\
		    }							\
		}							\
	    }								\
	} 								\
}

/* Immediately after a function call, return the saved pc.
   Can't go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  True on PN!  Return address is in R1.
   Note: true return location is 4 bytes past R1! */
#define SAVED_PC_AFTER_CALL(frame) \
	(read_register(R1_REGNUM) + 4)

/* Address of end of stack space.  */
#define STACK_END_ADDR 		0x480000

/* Stack grows downward.  */
#define INNER_THAN 		<

/* Sequence of bytes for breakpoint instruction.  */
#define BREAKPOINT 		{0x28, 0x09}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */
#define DECR_PC_AFTER_BREAK	2

/* Nonzero if instruction at PC is a return instruction. "bu 4(r1)" */
#define ABOUT_TO_RETURN(pc)	(read_memory_integer (pc, 4) == 0xEC100004)

/* Return 1 if P points to an invalid floating point value.  */
#define INVALID_FLOAT(p, len) 	((*(short *)p & 0xff80) == 0x8000)

/* Say how long (ordinary) registers are.  */
#define REGISTER_TYPE 		long

/* Number of machine registers */
#define NUM_REGS 		19
#define NUM_GEN_REGS		16
#define NUM_CPU_REGS		3

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */
#define REGISTER_NAMES { \
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", \
  "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", \
  "sp", "ps", "pc", \
}

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */
#define R1_REGNUM	1	/* Gr1 => return address of caller */
#define R4_REGNUM	4	/* Gr4 => register save area */
#define R5_REGNUM	5	/* Gr5 => register save area */
#define R6_REGNUM	6	/* Gr6 => register save area */
#define R7_REGNUM	7	/* Gr7 => register save area */
#define B1_REGNUM	9	/* Br1 => start of this code routine */
#define FP_REGNUM	10	/* Br2 == (sp) */
#define AP_REGNUM	11	/* Br3 == (ap) */
#define SP_REGNUM 	16	/* A copy of Br2 saved in trap */
#define PS_REGNUM 	17	/* Contains processor status */
#define PC_REGNUM 	18	/* Contains program counter */

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */
#define REGISTER_BYTES			(NUM_GEN_REGS*4 + NUM_CPU_REGS*4)

/* Index within `registers' of the first byte of the space for
   register N.  */
#define REGISTER_BYTE(N)  		((N) * 4)

/* Number of bytes of storage in the actual machine representation
   for register N.  On the PN, all normal regs are 4 bytes. */
#define REGISTER_RAW_SIZE(N) 		(4)

/* Number of bytes of storage in the program's representation
   for register N.  On the PN, all regs are 4 bytes. */
#define REGISTER_VIRTUAL_SIZE(N)	(4)

/* Largest value REGISTER_RAW_SIZE can have.  */
#define MAX_REGISTER_RAW_SIZE		(4)

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */
#define MAX_REGISTER_VIRTUAL_SIZE	(4)

/* Nonzero if register N requires conversion
   from raw format to virtual format.  */
#define REGISTER_CONVERTIBLE(N)		(0)

/* Convert data from raw format for register REGNUM
   to virtual format for register REGNUM.  */
#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,FROM,TO)	\
	bcopy ((FROM), (TO), REGISTER_RAW_SIZE(REGNUM));

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */
#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO)	\
	bcopy ((FROM), (TO), REGISTER_VIRTUAL_SIZE(REGNUM));

/* Return the GDB type object for the "standard" data type
   of data in register N.  */
#define REGISTER_VIRTUAL_TYPE(N)	(builtin_type_int)

/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function.

   On this machine this is a no-op, because gcc isn't used on it
   yet.  So this calling convention is not used. */

#define STORE_STRUCT_RETURN(ADDR, SP)

/* Extract from an arrary REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF. */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
	bcopy (REGBUF, VALBUF, TYPE_LENGTH (TYPE))

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
	write_register_bytes (0, VALBUF, TYPE_LENGTH (TYPE))

/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) (*(int *)(REGBUF))


/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer.

   However, if FRAME_CHAIN_VALID returns zero,
   it means the given frame is the outermost one and has no caller.  */

/* In the case of the NPL, the frame's norminal address is Br2 and the 
   previous routines frame is up the stack X bytes, where X is the
   value stored in the code function header xA(Br1). */
#define FRAME_CHAIN(thisframe)		(findframe(thisframe))

#define FRAME_CHAIN_VALID(chain, thisframe) \
        (chain != 0 && chain != (thisframe)->frame)

/* Define other aspects of the stack frame on NPL.  */
#define FRAME_SAVED_PC(frame) \
	(read_memory_integer ((frame)->frame + 8, 4))

#define FRAME_ARGS_ADDRESS(fi) \
	((fi)->next_frame ? \
	 read_memory_integer ((fi)->frame + 12, 4) : \
	 read_register (AP_REGNUM))

#define FRAME_LOCALS_ADDRESS(fi)	((fi)->frame + 80)

/* Set VAL to the number of args passed to frame described by FI.
   Can set VAL to -1, meaning no way to tell.  */

/* We can check the stab info to see how
   many arg we have.  No info in stack will tell us */
#define FRAME_NUM_ARGS(val,fi)		(val = findarg(fi))

/* Return number of bytes at start of arglist that are not really args.  */
#define FRAME_ARGS_SKIP			8

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */

#define FRAME_FIND_SAVED_REGS(frame_info, frame_saved_regs)		\
{                                                                       \
  bzero (&frame_saved_regs, sizeof frame_saved_regs);			\
  (frame_saved_regs).regs[PC_REGNUM] = (frame_info)->frame + 8;		\
  (frame_saved_regs).regs[R4_REGNUM] = (frame_info)->frame + 0x30;	\
  (frame_saved_regs).regs[R5_REGNUM] = (frame_info)->frame + 0x34;	\
  (frame_saved_regs).regs[R6_REGNUM] = (frame_info)->frame + 0x38;	\
  (frame_saved_regs).regs[R7_REGNUM] = (frame_info)->frame + 0x3C;	\
}

/* Things needed for making the inferior call functions.  */

/* Push an empty stack frame, to record the current PC, etc.  */

#define PUSH_DUMMY_FRAME \
{ register CORE_ADDR sp = read_register (SP_REGNUM);			\
  register int regnum;							\
  sp = push_word (sp, read_register (PC_REGNUM));			\
  sp = push_word (sp, read_register (FP_REGNUM));			\
  write_register (FP_REGNUM, sp);					\
  for (regnum = FP_REGNUM - 1; regnum >= 0; regnum--)			\
    sp = push_word (sp, read_register (regnum));			\
  sp = push_word (sp, read_register (PS_REGNUM));			\
  write_register (SP_REGNUM, sp);  }

/* Discard from the stack the innermost frame, 
   restoring all saved registers.  */

#define POP_FRAME  \
{ register FRAME frame = get_current_frame ();			 \
  register CORE_ADDR fp;					 \
  register int regnum;						 \
  struct frame_saved_regs fsr;					 \
  struct frame_info *fi;					 \
  fi = get_frame_info (frame);					 \
  fp = fi->frame;						 \
  get_frame_saved_regs (fi, &fsr);				 \
  for (regnum = FP_REGNUM - 1; regnum >= 0; regnum--)		 \
    if (fsr.regs[regnum])					 \
      write_register (regnum, read_memory_integer (fsr.regs[regnum], 4)); \
  if (fsr.regs[PS_REGNUM])					 \
    write_register (PS_REGNUM, read_memory_integer (fsr.regs[PS_REGNUM], 4)); \
  write_register (FP_REGNUM, read_memory_integer (fp, 4));	 \
  write_register (PC_REGNUM, read_memory_integer (fp + 4, 4));   \
  write_register (SP_REGNUM, fp + 8);				 \
  flush_cached_frames ();					 \
  set_current_frame ( create_new_frame (read_register (FP_REGNUM),\
					read_pc ())); }

/* This sequence of words is the instructions:
     halt
     halt
     halt
     halt
     suabr	b2, #<stacksize>
     lwbr	b6, #con
     stw	r1, 8(b2)	- save caller address, do we care?
     lw		r2, 60(b2)	- arg1
     labr	b3, 50(b2)
     std	r4, 30(b2)	- save r4-r7
     std	r6, 38(b2)
     lwbr	b1, #<func>	- load function call address
     brlnk	r1, 8(b1)	- call function
     halt
     halt
     ld		r4, 30(b2)	- restore r4-r7
     ld		r6, 38(b2)

   Setup our stack frame, load argumemts, call and then restore registers.
*/

/* FIXME:  The below defines an m68k CALL_DUMMY, which looks nothing like what
   is documented above. */

#define CALL_DUMMY {0xf227e0ff, 0x48e7fffc, 0x426742e7, 0x4eb93232, 0x3232dffc, 0x69696969, 0x4e4f4e71}

#define CALL_DUMMY_LENGTH 28

#define CALL_DUMMY_START_OFFSET 12

/* Insert the specified number of args and function address
   into a call sequence of the above form stored at DUMMYNAME.  */

#define FIX_CALL_DUMMY(dummyname, pc, fun, nargs, args, type, gcc_p)     \
{ *(int *)((char *) dummyname + 20) = nargs * 4;  \
  *(int *)((char *) dummyname + 14) = fun; }
