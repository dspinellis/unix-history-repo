/* Target machine description for generic Motorola 88000, for GDB.
   Copyright 1986, 1987, 1988, 1989, 1990, 1991 Free Software Foundation, Inc.

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

/* g++ support is not yet included.  */

#define TARGET_BYTE_ORDER BIG_ENDIAN

/* We cache information about saved registers in the frame structure,
   to save us from having to re-scan function prologues every time
   a register in a non-current frame is accessed.  */

#define EXTRA_FRAME_INFO 	\
	struct frame_saved_regs *fsr;	\
	CORE_ADDR locals_pointer;	\
	CORE_ADDR args_pointer;

/* Zero the frame_saved_regs pointer when the frame is initialized,
   so that FRAME_FIND_SAVED_REGS () will know to allocate and
   initialize a frame_saved_regs struct the first time it is called.
   Set the arg_pointer to -1, which is not valid; 0 and other values
   indicate real, cached values.  */

#define INIT_EXTRA_FRAME_INFO(fromleaf, fi) \
	init_extra_frame_info (fromleaf, fi)
extern void init_extra_frame_info ();

#define IEEE_FLOAT

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#define NAMES_HAVE_UNDERSCORE

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

#define SKIP_PROLOGUE(frompc)   \
	skip_prologue (frompc)
extern CORE_ADDR skip_prologue ();

/* The m88k kernel aligns all instructions on 4-byte boundaries.  The
   kernel also uses the least significant two bits for its own hocus
   pocus.  When gdb receives an address from the kernel, it needs to
   preserve those right-most two bits, but gdb also needs to be careful
   to realize that those two bits are not really a part of the address
   of an instruction.  Shrug.  */

#define ADDR_BITS_REMOVE(addr) ((addr) & ~3)
#define ADDR_BITS_SET(addr) (((addr) | 0x00000002) - 4)

/* Immediately after a function call, return the saved pc.
   Can't always go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define SAVED_PC_AFTER_CALL(frame) \
  (ADDR_BITS_REMOVE (read_register (SRP_REGNUM)))

/* Stack grows downward.  */

#define INNER_THAN <

/* Sequence of bytes for breakpoint instruction.  */

/* instruction 0xF000D1FF is 'tb0 0,r0,511'
   If Bit bit 0 of r0 is clear (always true),
   initiate exception processing (trap).
 */
#define BREAKPOINT {0xF0, 0x00, 0xD1, 0xFF}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 0

/* Nonzero if instruction at PC is a return instruction.  */
/* 'jmp r1' or 'jmp.n r1' is used to return from a subroutine. */

#define ABOUT_TO_RETURN(pc) (read_memory_integer (pc, 2) == 0xF800)

/* Return 1 if P points to an invalid floating point value.
   LEN is the length in bytes.  */

#define INVALID_FLOAT(p, len) IEEE_isNAN(p,len)

/* Say how long (ordinary) registers are.  */

#define REGISTER_TYPE long

/* Number of machine registers */

#define NUM_REGS 38

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES {\
 	"r0",\
	"r1",\
	"r2",\
	"r3",\
	"r4",\
	"r5",\
	"r6",\
	"r7",\
	"r8",\
	"r9",\
	"r10",\
	"r11",\
	"r12",\
	"r13",\
	"r14",\
	"r15",\
	"r16",\
	"r17",\
	"r18",\
	"r19",\
	"r20",\
	"r21",\
	"r22",\
	"r23",\
	"r24",\
	"r25",\
	"r26",\
	"r27",\
	"r28",\
	"r29",\
	"r30",\
	"r31",\
	"psr",\
	"fpsr",\
	"fpcr",\
	"sxip",\
	"snip",\
	"sfip",\
	"vbr",\
	"dmt0",\
	"dmd0",\
	"dma0",\
	"dmt1",\
	"dmd1",\
	"dma1",\
	"dmt2",\
	"dmd2",\
	"dma2",\
	"sr0",\
	"sr1",\
	"sr2",\
	"sr3",\
	"fpecr",\
	"fphs1",\
	"fpls1",\
	"fphs2",\
	"fpls2",\
	"fppt",\
	"fprh",\
	"fprl",\
	"fpit",\
	"fpsr",\
	"fpcr",\
	}


/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#define SRP_REGNUM 1		/* Contains subroutine return pointer */
#define RV_REGNUM 2		/* Contains simple return values */
#define SRA_REGNUM 12		/* Contains address of struct return values */
#define FP_REGNUM 31		/* Reg fetched to locate frame when pgm stops */
#define SP_REGNUM 31		/* Contains address of top of stack */
#define SXIP_REGNUM 35		/* Contains Shadow Execute Instruction Pointer */
#define SNIP_REGNUM 36		/* Contains Shadow Next Instruction Pointer */
#define PC_REGNUM SXIP_REGNUM	/* Program Counter */
#define NPC_REGNUM SNIP_REGNUM	/* Next Program Counter */
#define PSR_REGNUM 32           /* Processor Status Register */
#define FPSR_REGNUM 33		/* Floating Point Status Register */
#define FPCR_REGNUM 34		/* Floating Point Control Register */
#define SFIP_REGNUM 37		/* Contains Shadow Fetched Intruction pointer */
#define NNPC_REGNUM SFIP_REGNUM /* Next Next Program Counter */

/* PSR status bit definitions.  */

#define PSR_MODE		0x80000000
#define PSR_BYTE_ORDER		0x40000000
#define PSR_SERIAL_MODE		0x20000000
#define PSR_CARRY		0x10000000
#define PSR_SFU_DISABLE		0x000003f0
#define PSR_SFU1_DISABLE	0x00000008
#define PSR_MXM			0x00000004
#define PSR_IND			0x00000002
#define PSR_SFRZ		0x00000001

/* BCS requires that the SXIP_REGNUM (or PC_REGNUM) contain the address
   of the next instr to be executed when a breakpoint occurs.  Because
   the kernel gets the next instr (SNIP_REGNUM), the instr in SNIP needs
   to be put back into SFIP, and the instr in SXIP should be shifted
   to SNIP */

/* Are you sitting down?  It turns out that the 88K BCS (binary compatibility
  standard) folks originally felt that the debugger should be responsible
  for backing up the IPs, not the kernel (as is usually done).  Well, they
  have reversed their decision, and in future releases our kernel will be
  handling the backing up of the IPs.  So, eventually, we won't need to
  do the SHIFT_INST_REGS stuff.  But, for now, since there are 88K systems out
  there that do need the debugger to do the IP shifting, and since there
  will be systems where the kernel does the shifting, the code is a little
  more complex than perhaps it needs to be (we still go inside SHIFT_INST_REGS,
  and if the shifting hasn't occurred then gdb goes ahead and shifts).  */

#define SHIFT_INST_REGS

/* Number of bytes of storage in the actual machine representation
   for register N.  */

#define REGISTER_RAW_SIZE(N) 4

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */

#define REGISTER_BYTES (NUM_REGS * REGISTER_RAW_SIZE(0))

/* Index within `registers' of the first byte of the space for
   register N.  */

#define REGISTER_BYTE(N) ((N)*REGISTER_RAW_SIZE(0))

/* Number of bytes of storage in the program's representation
   for register N. */

#define REGISTER_VIRTUAL_SIZE(N) (REGISTER_RAW_SIZE(N))

/* Largest value REGISTER_RAW_SIZE can have.  */

#define MAX_REGISTER_RAW_SIZE (REGISTER_RAW_SIZE(0))

/* Largest value REGISTER_VIRTUAL_SIZE can have.
/* Are FPS1, FPS2, FPR "virtual" regisers? */

#define MAX_REGISTER_VIRTUAL_SIZE (REGISTER_RAW_SIZE(0))

/* Nonzero if register N requires conversion
   from raw format to virtual format.  */

#define REGISTER_CONVERTIBLE(N) (0)

/* Convert data from raw format for register REGNUM
   to virtual format for register REGNUM.  */

#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,FROM,TO) \
  {bcopy ((FROM), (TO), REGISTER_RAW_SIZE (REGNUM));}

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */

#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO)
  {bcopy ((FROM), (TO), REGISTER_RAW_SIZE (REGNUM));}

/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#define REGISTER_VIRTUAL_TYPE(N) (builtin_type_int)

/* The 88k call/return conventions call for "small" values to be returned
   into consecutive registers starting from r2.  */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  bcopy (&(((char *)REGBUF)[REGISTER_BYTE(RV_REGNUM)]), (VALBUF), TYPE_LENGTH (TYPE))

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) (*(int *)(REGBUF))

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  write_register_bytes (2*REGISTER_RAW_SIZE(0), (VALBUF), TYPE_LENGTH (TYPE))

/* In COFF, if PCC says a parameter is a short or a char, do not
   change it to int (it seems the convention is to change it). */

#define BELIEVE_PCC_PROMOTION 1

/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer.

   However, if FRAME_CHAIN_VALID returns zero,
   it means the given frame is the outermost one and has no caller.  */

extern CORE_ADDR frame_chain ();
extern int frame_chain_valid ();
extern int frameless_function_invocation ();

#define FRAME_CHAIN(thisframe) \
	frame_chain (thisframe)

#define FRAME_CHAIN_VALID(chain, thisframe)	\
	frame_chain_valid (chain, thisframe)

#define	FRAMELESS_FUNCTION_INVOCATION(frame, fromleaf)	\
	fromleaf = frameless_function_invocation (frame)

/* Define other aspects of the stack frame.  */

#define FRAME_SAVED_PC(FRAME)	\
	frame_saved_pc (FRAME)
extern CORE_ADDR frame_saved_pc ();

#define FRAME_ARGS_ADDRESS(fi)	\
	frame_args_address (fi)
extern CORE_ADDR frame_args_address ();

#define FRAME_LOCALS_ADDRESS(fi) \
	frame_locals_address (fi)
extern CORE_ADDR frame_locals_address ();

/* Return number of args passed to a frame.
   Can return -1, meaning no way to tell.  */

#define FRAME_NUM_ARGS(numargs, fi)  ((numargs) = -1)

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 0

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */

/* On the 88k, parameter registers get stored into the so called "homing"
   area.  This *always* happens when you compiled with GCC and use -g.
   Also, (with GCC and -g) the saving of the parameter register values
   always happens right within the function prologue code, so these register
   values can generally be relied upon to be already copied into their
   respective homing slots by the time you will normally try to look at
   them (we hope).

   Note that homing area stack slots are always at *positive* offsets from
   the frame pointer.  Thus, the homing area stack slots for the parameter
   registers (passed values) for a given function are actually part of the
   frame area of the caller.  This is unusual, but it should not present
   any special problems for GDB.

   Note also that on the 88k, we are only interested in finding the
   registers that might have been saved in memory.  This is a subset of
   the whole set of registers because the standard calling sequence allows
   the called routine to clobber many registers.

   We could manage to locate values for all of the so called "preserved"
   registers (some of which may get saved within any particular frame) but
   that would require decoding all of the tdesc information.  Tht would be
   nice information for GDB to have, but it is not strictly manditory if we
   can live without the ability to look at values within (or backup to)
   previous frames.
*/

#define FRAME_FIND_SAVED_REGS(frame_info, frame_saved_regs) \
        frame_find_saved_regs (frame_info, &frame_saved_regs)


/* There is not currently a functioning way to call functions in the
   inferior.  */

/* But if there was this is where we'd put the call dummy.  */
/* #define CALL_DUMMY_LOCATION AFTER_TEXT_END */

/* When popping a frame on the 88k (say when doing a return command), the
   calling function only expects to have the "preserved" registers restored.
   Thus, those are the only ones that we even try to restore here.   */

#define POP_FRAME pop_frame ()
extern void pop_frame ();
