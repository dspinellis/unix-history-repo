/****************************************************************************

		THIS SOFTWARE IS NOT COPYRIGHTED

   HP offers the following for use in the public domain.  HP makes no
   warranty with regard to the software or it's performance and the
   user accepts the software "AS IS" with all faults.

   HP DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD
   TO THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES
   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

****************************************************************************/

/****************************************************************************
 *  Header: remcom.c,v 1.34 91/03/09 12:29:49 glenne Exp $
 *
 *  Module name: remcom.c $
 *  Revision: 1.34 $
 *  Date: 91/03/09 12:29:49 $
 *  Contributor:     Lake Stevens Instrument Division$
 *
 *  Description:     low level support for gdb debugger. $
 *
 *  Considerations:  only works on target hardware $
 *
 *  Written by:      Glenn Engel $
 *  ModuleState:     Experimental $
 *
 *  NOTES:           See Below $
 *
 *  Modified for SPARC by Stu Grossman, Cygnus Support.
 *
 *  To enable debugger support, two things need to happen.  One, a
 *  call to set_debug_traps() is necessary in order to allow any breakpoints
 *  or error conditions to be properly intercepted and reported to gdb.
 *  Two, a breakpoint needs to be generated to begin communication.  This
 *  is most easily accomplished by a call to breakpoint().  Breakpoint()
 *  simulates a breakpoint by executing a trap #1.
 *
 *************
 *
 *    The following gdb commands are supported:
 *
 * command          function                               Return value
 *
 *    g             return the value of the CPU registers  hex data or ENN
 *    G             set the value of the CPU registers     OK or ENN
 *
 *    mAA..AA,LLLL  Read LLLL bytes at address AA..AA      hex data or ENN
 *    MAA..AA,LLLL: Write LLLL bytes at address AA.AA      OK or ENN
 *
 *    c             Resume at current address              SNN   ( signal NN)
 *    cAA..AA       Continue at address AA..AA             SNN
 *
 *    s             Step one instruction                   SNN
 *    sAA..AA       Step one instruction from AA..AA       SNN
 *
 *    k             kill
 *
 *    ?             What was the last sigval ?             SNN   (signal NN)
 *
 *    bBB..BB	    Set baud rate to BB..BB		   OK or BNN, then sets
 *							   baud rate
 *
 * All commands and responses are sent with a packet which includes a
 * checksum.  A packet consists of
 *
 * $<packet info>#<checksum>.
 *
 * where
 * <packet info> :: <characters representing the command or response>
 * <checksum>    :: < two hex digits computed as modulo 256 sum of <packetinfo>>
 *
 * When a packet is received, it is first acknowledged with either '+' or '-'.
 * '+' indicates a successful transfer.  '-' indicates a failed transfer.
 *
 * Example:
 *
 * Host:                  Reply:
 * $m0,10#2a               +$00010203040506070809101112131415#42
 *
 ****************************************************************************/

#include <string.h>
#include <signal.h>
#include <memory.h>

/************************************************************************
 *
 * external low-level support routines
 */

extern putDebugChar();   /* write a single character      */
extern getDebugChar();   /* read and return a single char */

/************************************************************************/
/* BUFMAX defines the maximum number of characters in inbound/outbound buffers*/
/* at least NUMREGBYTES*2 are needed for register packets */
#define BUFMAX 2048

static int initialized;  /* boolean flag. != 0 means we've been initialized */

static void set_mem_fault_trap();

static const char hexchars[]="0123456789abcdef";

#define NUMREGS 72

/* Number of bytes of registers.  */
#define NUMREGBYTES (NUMREGS * 4)
enum regnames {G0, G1, G2, G3, G4, G5, G6, G7,
		 O0, O1, O2, O3, O4, O5, SP, O7,
		 L0, L1, L2, L3, L4, L5, L6, L7,
		 I0, I1, I2, I3, I4, I5, FP, I7,

		 F0, F1, F2, F3, F4, F5, F6, F7,
		 F8, F9, F10, F11, F12, F13, F14, F15,
		 F16, F17, F18, F19, F20, F21, F22, F23,
		 F24, F25, F26, F27, F28, F29, F30, F31,
		 Y, PSR, WIM, TBR, PC, NPC, FPSR, CPSR };

/***************************  ASSEMBLY CODE MACROS *************************/
/* 									   */

#define BREAKPOINT() asm("   ta 1");

extern unsigned long rdtbr();

asm("
	.text
	.align 4

! Read the TBR.

	.globl _rdtbr
_rdtbr:
	retl
	mov	%tbr, %o0

! This function is called when any SPARC trap (except window overflow or
! underflow) occurs.  It makes sure that the invalid register window is still
! available before jumping into C code.  It will also restore the world if you
! return from handle_exception.

trap_low:
	mov	%psr, %l0
	mov	%wim, %l3

	srl	%l3, %l0, %l4		! wim >> cwp
	cmp	%l4, 1
	bne	window_fine		! Branch if not in the invalid window
	nop

! Handle window overflow

	mov	%g1, %l4		! Save g1, we use it to hold the wim
	srl	%l3, 1, %g1		! Rotate wim right
	sll	%l3, 8-1, %l5
	or	%l5, %g1, %g1

	save	%g0, %g0, %g0		! Slip into next window
	mov	%g1, %wim		! Install the new wim

	std	%l0, [%sp + 0 * 4]	! save L & I registers
	std	%l2, [%sp + 2 * 4]
	std	%l4, [%sp + 4 * 4]
	std	%l6, [%sp + 6 * 4]

	std	%i0, [%sp + 8 * 4]
	std	%i2, [%sp + 10 * 4]
	std	%i4, [%sp + 12 * 4]
	std	%i6, [%sp + 14 * 4]

	restore				! Go back to trap window.
	mov	%l4, %g1		! Restore %g1

window_fine:
	sub	%fp, (16+1+6+1+72)*4, %sp ! Make room for input & locals
 					! + hidden arg + arg spill
					! + doubleword alignment
					! + registers[72] local var

	std	%g0, [%fp + (-72 + 0) * 4] ! registers[Gx]
	std	%g2, [%fp + (-72 + 2) * 4]
	std	%g4, [%fp + (-72 + 4) * 4]
	std	%g6, [%fp + (-72 + 6) * 4]

	std	%i0, [%fp + (-72 + 8) * 4] ! registers[Ox]
	std	%i2, [%fp + (-72 + 10) * 4]
	std	%i4, [%fp + (-72 + 12) * 4]
	std	%i6, [%fp + (-72 + 14) * 4]
					! F0->F31 not implemented
	mov	%y, %l4
	mov	%tbr, %l5
	st	%l4, [%fp + (-72 + 64) * 4] ! Y
	st	%l0, [%fp + (-72 + 65) * 4] ! PSR
	st	%l3, [%fp + (-72 + 66) * 4] ! WIM
	st	%l5, [%fp + (-72 + 67) * 4] ! TBR
	st	%l1, [%fp + (-72 + 68) * 4] ! PC
	st	%l2, [%fp + (-72 + 69) * 4] ! NPC

					! CPSR and FPSR not impl

	or	%l0, 0xf20, %l4
	mov	%l4, %psr		! Turn on traps, disable interrupts

	call	_handle_exception
	add	%fp, -72 * 4, %o0	! Pass address of registers

	restore				! Ensure that previous window is valid
	save	%g0, %g0, %g0		!  by causing a window_underflow trap

! Reload all of the registers that aren't on the stack

	ld	[%fp + (-72 + 1) * 4], %g1 ! registers[Gx]
	ldd	[%fp + (-72 + 2) * 4], %g2
	ldd	[%fp + (-72 + 4) * 4], %g4
	ldd	[%fp + (-72 + 6) * 4], %g6

	ldd	[%fp + (-72 + 8) * 4], %o0 ! registers[Ox]
	ldd	[%fp + (-72 + 10) * 4], %o2
	ldd	[%fp + (-72 + 12) * 4], %o4
	ldd	[%fp + (-72 + 14) * 4], %o6

	ldd	[%fp + (-72 + 64) * 4], %l0 ! Y & PSR
	ldd	[%fp + (-72 + 68) * 4], %l2 ! PC & NPC
	mov	%l0, %y
	mov	%l1, %psr		! Make sure that traps are disabled
					! for rett
	jmpl	%l2, %g0		! Restore old PC
	rett	%l3			! Restore old nPC
");

/* Convert ch from a hex digit to an int */

static int
hex(ch)
     unsigned char ch;
{
  if (ch >= 'a' && ch <= 'f')
    return ch-'a'+10;
  if (ch >= '0' && ch <= '9')
    return ch-'0';
  if (ch >= 'A' && ch <= 'F')
    return ch-'A'+10;
  return -1;
}

/* scan for the sequence $<data>#<checksum>     */

static void
getpacket(buffer)
     char *buffer;
{
  unsigned char checksum;
  unsigned char xmitcsum;
  int i;
  int count;
  unsigned char ch;

  do
    {
      /* wait around for the start character, ignore all other characters */
      while ((ch = getDebugChar()) != '$') ;

      checksum = 0;
      xmitcsum = -1;

      count = 0;

      /* now, read until a # or end of buffer is found */
      while (count < BUFMAX)
	{
	  ch = getDebugChar();
	  if (ch == '#')
	    break;
	  checksum = checksum + ch;
	  buffer[count] = ch;
	  count = count + 1;
	}

      if (count >= BUFMAX)
	continue;

      buffer[count] = 0;

      if (ch == '#')
	{
	  xmitcsum = hex(getDebugChar()) << 4;
	  xmitcsum |= hex(getDebugChar());
	  if (checksum != xmitcsum)
	    putDebugChar('-');	/* failed checksum */
	  else
	    {
	      putDebugChar('+'); /* successful transfer */
	      /* if a sequence char is present, reply the sequence ID */
	      if (buffer[2] == ':')
		{
		  putDebugChar(buffer[0]);
		  putDebugChar(buffer[1]);
		  /* remove sequence chars from buffer */
		  count = strlen(buffer);
		  for (i=3; i <= count; i++)
		    buffer[i-3] = buffer[i];
		}
	    }
	}
    }
  while (checksum != xmitcsum);
}

/* send the packet in buffer.  */

static void
putpacket(buffer)
     unsigned char *buffer;
{
  unsigned char checksum;
  int count;
  unsigned char ch;

  /*  $<packet info>#<checksum>. */
  do
    {
      putDebugChar('$');
      checksum = 0;
      count = 0;

      while (ch = buffer[count])
	{
	  if (! putDebugChar(ch))
	    return;
	  checksum += ch;
	  count += 1;
	}

      putDebugChar('#');
      putDebugChar(hexchars[checksum >> 4]);
      putDebugChar(hexchars[checksum & 0xf]);

    }
  while (getDebugChar() != '+');
}

static char remcomInBuffer[BUFMAX];
static char remcomOutBuffer[BUFMAX];

/* Indicate to caller of mem2hex or hex2mem that there has been an
   error.  */
static volatile int mem_err = 0;

/* Convert the memory pointed to by mem into hex, placing result in buf.
 * Return a pointer to the last char put in buf (null), in case of mem fault,
 * return 0.
 * If MAY_FAULT is non-zero, then we will handle memory faults by returning
 * a 0, else treat a fault like any other fault in the stub.
 */

static unsigned char *
mem2hex(mem, buf, count, may_fault)
     unsigned char *mem;
     unsigned char *buf;
     int count;
     int may_fault;
{
  unsigned char ch;

  set_mem_fault_trap(may_fault);

  while (count-- > 0)
    {
      ch = *mem++;
      if (mem_err)
	return 0;
      *buf++ = hexchars[ch >> 4];
      *buf++ = hexchars[ch & 0xf];
    }

  *buf = 0;

  set_mem_fault_trap(0);

  return buf;
}

/* convert the hex array pointed to by buf into binary to be placed in mem
 * return a pointer to the character AFTER the last byte written */

static char *
hex2mem(buf, mem, count, may_fault)
     unsigned char *buf;
     unsigned char *mem;
     int count;
     int may_fault;
{
  int i;
  unsigned char ch;

  set_mem_fault_trap(may_fault);

  for (i=0; i<count; i++)
    {
      ch = hex(*buf++) << 4;
      ch |= hex(*buf++);
      *mem++ = ch;
      if (mem_err)
	return 0;
    }

  set_mem_fault_trap(0);

  return mem;
}

/* This table contains the mapping between SPARC hardware trap types, and
   signals, which are primarily what GDB understands.  It also indicates
   which hardware traps we need to commandeer when initializing the stub. */

static struct hard_trap_info
{
  unsigned char tt;		/* Trap type code for SPARClite */
  unsigned char signo;		/* Signal that we map this trap into */
} hard_trap_info[] = {
  {1, SIGSEGV},			/* instruction access error */
  {2, SIGILL},			/* privileged instruction */
  {3, SIGILL},			/* illegal instruction */
  {4, SIGEMT},			/* fp disabled */
  {36, SIGEMT},			/* cp disabled */
  {7, SIGBUS},			/* mem address not aligned */
  {9, SIGSEGV},			/* data access exception */
  {10, SIGEMT},			/* tag overflow */
  {128+1, SIGTRAP},		/* ta 1 - normal breakpoint instruction */
  {0, 0}			/* Must be last */
};

/* Each entry in the trap vector occupies four words. */

struct trap_entry
{
  unsigned long ti[4];
};

extern struct trap_entry fltr_proto;
extern struct trap_entry fltr_set_mem_err;
asm ("
	.data
	.globl _fltr_proto
	.align 4
_fltr_proto:			! First level trap routine prototype
	sethi %hi(trap_low), %l0
	jmpl %lo(trap_low)+%l0, %g0
	nop
	nop

! Trap handler for memory errors.  This just sets mem_err to be non-zero.  It
! assumes that %l1 is non-zero.  This should be safe, as it is doubtful that
! 0 would ever contain code that could mem fault.  This routine will skip
! past the faulting instruction after setting mem_err.

_fltr_set_mem_err:
	sethi %hi(_mem_err), %l0
	st %l1, [%l0 + %lo(_mem_err)]
	jmpl %l2, %g0
	rett %l2+4

	.text
");

/* Set up exception handlers for tracing and breakpoints */

void
set_debug_traps()
{
  struct trap_entry *tb;	/* Trap vector base address */
  struct hard_trap_info *ht;

  tb = (struct trap_entry *)(rdtbr() & ~0xfff);

  for (ht = hard_trap_info; ht->tt && ht->signo; ht++)
    tb[ht->tt] = fltr_proto;

  /* In case GDB is started before us, ack any packets (presumably
     "$?#xx") sitting there.  */

  putDebugChar ('+');

  initialized = 1;
}

static void
set_mem_fault_trap(enable)
     int enable;
{
  struct trap_entry *tb;	/* Trap vector base address */

  mem_err = 0;

  tb = (struct trap_entry *)(rdtbr() & ~0xfff);

  if (enable)
    tb[9] = fltr_set_mem_err;
  else
    tb[9] = fltr_proto;
}

/* Convert the SPARC hardware trap type code to a unix signal number. */

static int
computeSignal(tt)
     int tt;
{
  struct hard_trap_info *ht;

  for (ht = hard_trap_info; ht->tt && ht->signo; ht++)
    if (ht->tt == tt)
      return ht->signo;

  return SIGHUP;		/* default for things we don't know about */
}

/*
 * While we find nice hex chars, build an int.
 * Return number of chars processed.
 */

static int
hexToInt(char **ptr, int *intValue)
{
  int numChars = 0;
  int hexValue;

  *intValue = 0;

  while (**ptr)
    {
      hexValue = hex(**ptr);
      if (hexValue < 0)
	break;

      *intValue = (*intValue << 4) | hexValue;
      numChars ++;

      (*ptr)++;
    }

  return (numChars);
}

/*
 * This function does all command procesing for interfacing to gdb.  It
 * returns 1 if you should skip the instruction at the trap address, 0
 * otherwise.
 */

static void
handle_exception (registers)
     unsigned long *registers;
{
  int tt;			/* Trap type */
  int sigval;
  int addr;
  int length;
  char *ptr;
  unsigned long *sp;

/* First, we must force all of the windows to be spilled out */

  asm("	save %sp, -64, %sp
	save %sp, -64, %sp
	save %sp, -64, %sp
	save %sp, -64, %sp
	save %sp, -64, %sp
	save %sp, -64, %sp
	save %sp, -64, %sp
	save %sp, -64, %sp
	restore
	restore
	restore
	restore
	restore
	restore
	restore
	restore
");

  sp = (unsigned long *)registers[SP];

  tt = (registers[TBR] >> 4) & 0xff;

  /* reply to host that an exception has occurred */
  sigval = computeSignal(tt);
  ptr = remcomOutBuffer;

  *ptr++ = 'T';
  *ptr++ = hexchars[sigval >> 4];
  *ptr++ = hexchars[sigval & 0xf];

  *ptr++ = hexchars[PC >> 4];
  *ptr++ = hexchars[PC & 0xf];
  *ptr++ = ':';
  ptr = mem2hex((char *)&registers[PC], ptr, 4, 0);
  *ptr++ = ';';

  *ptr++ = hexchars[FP >> 4];
  *ptr++ = hexchars[FP & 0xf];
  *ptr++ = ':';
  ptr = mem2hex(sp + 8 + 6, ptr, 4, 0); /* FP */
  *ptr++ = ';';

  *ptr++ = hexchars[SP >> 4];
  *ptr++ = hexchars[SP & 0xf];
  *ptr++ = ':';
  ptr = mem2hex((char *)&sp, ptr, 4, 0);
  *ptr++ = ';';

  *ptr++ = hexchars[NPC >> 4];
  *ptr++ = hexchars[NPC & 0xf];
  *ptr++ = ':';
  ptr = mem2hex((char *)&registers[NPC], ptr, 4, 0);
  *ptr++ = ';';

  *ptr++ = hexchars[O7 >> 4];
  *ptr++ = hexchars[O7 & 0xf];
  *ptr++ = ':';
  ptr = mem2hex((char *)&registers[O7], ptr, 4, 0);
  *ptr++ = ';';

  *ptr++ = 0;

  putpacket(remcomOutBuffer);

  while (1)
    {
      remcomOutBuffer[0] = 0;

      getpacket(remcomInBuffer);
      switch (remcomInBuffer[0])
	{
	case '?':
	  remcomOutBuffer[0] = 'S';
	  remcomOutBuffer[1] = hexchars[sigval >> 4];
	  remcomOutBuffer[2] = hexchars[sigval & 0xf];
	  remcomOutBuffer[3] = 0;
	  break;

	case 'd':
				/* toggle debug flag */
	  break;

	case 'g':		/* return the value of the CPU registers */
	  {
	    ptr = remcomOutBuffer;
	    ptr = mem2hex((char *)registers, ptr, 16 * 4, 0); /* G & O regs */
	    ptr = mem2hex(sp + 0, ptr, 8 * 4, 0); /* L regs */
	    ptr = mem2hex(sp + 8, ptr, 8 * 4, 0); /* I regs */
	    memset(ptr, '0', 32 * 8); /* Floating point */
	    mem2hex((char *)&registers[Y],
		    ptr + 32 * 4 * 2,
		    8 * 4,
		    0);		/* Y, PSR, WIM, TBR, PC, NPC, FPSR, CPSR */
	  }
	  break;

	case 'G':	   /* set the value of the CPU registers - return OK */
	  {
	    ptr = &remcomInBuffer[1];
	    hex2mem(ptr, (char *)registers, 16 * 4, 0); /* G & O regs */
	    hex2mem(ptr + 16 * 4 * 2, sp + 0, 8 * 4, 0); /* L regs */
	    hex2mem(ptr + 24 * 4 * 2, sp + 8, 8 * 4, 0); /* I regs */
	    hex2mem(ptr + 64 * 4 * 2, (char *)&registers[Y],
		    8 * 4, 0);	/* Y, PSR, WIM, TBR, PC, NPC, FPSR, CPSR */
	    strcpy(remcomOutBuffer,"OK");
	  }
	  break;

	case 'm':	  /* mAA..AA,LLLL  Read LLLL bytes at address AA..AA */
	  /* Try to read %x,%x.  */

	  ptr = &remcomInBuffer[1];

	  if (hexToInt(&ptr, &addr)
	      && *ptr++ == ','
	      && hexToInt(&ptr, &length))
	    {
	      if (mem2hex((char *)addr, remcomOutBuffer, length, 1))
		break;

	      strcpy (remcomOutBuffer, "E03");
	    }
	  else
	    strcpy(remcomOutBuffer,"E01");
	  break;

	case 'M': /* MAA..AA,LLLL: Write LLLL bytes at address AA.AA return OK */
	  /* Try to read '%x,%x:'.  */

	  ptr = &remcomInBuffer[1];

	  if (hexToInt(&ptr, &addr)
	      && *ptr++ == ','
	      && hexToInt(&ptr, &length)
	      && *ptr++ == ':')
	    {
	      if (hex2mem(ptr, (char *)addr, length, 1))
		strcpy(remcomOutBuffer, "OK");
	      else
		strcpy(remcomOutBuffer, "E03");
	    }
	  else
	    strcpy(remcomOutBuffer, "E02");
	  break;

	case 'c':    /* cAA..AA    Continue at address AA..AA(optional) */
	case 's':    /* sAA..AA   Step one instruction from AA..AA(optional) */
	  /* try to read optional parameter, pc unchanged if no parm */

	  ptr = &remcomInBuffer[1];
	  if (hexToInt(&ptr, &addr))
	    {
	      registers[PC] = addr;
	      registers[NPC] = addr + 4;
	    }

/* Need to flush the instruction cache here, as we may have deposited a
   breakpoint, and the icache probably has no way of knowing that a data ref to
   some location may have changed something that is in the instruction cache.
 */

	  flush_i_cache();
	  return;

	  /* kill the program */
	case 'k' :		/* do nothing */
	  break;
#if 0
Disabled until we can unscrew this properly

	case 'b':	  /* bBB...  Set baud rate to BB... */
	  {
	    int baudrate;
	    extern void set_timer_3();

	    ptr = &remcomInBuffer[1];
	    if (!hexToInt(&ptr, &baudrate))
	      {
		strcpy(remcomOutBuffer,"B01");
		break;
	      }

	    /* Convert baud rate to uart clock divider */
	    switch (baudrate)
	      {
	      case 38400:
		baudrate = 16;
		break;
	      case 19200:
		baudrate = 33;
		break;
	      case 9600:
		baudrate = 65;
		break;
	      default:
		strcpy(remcomOutBuffer,"B02");
		goto x1;
	      }

	    putpacket("OK");	/* Ack before changing speed */
	    set_timer_3(baudrate); /* Set it */
	  }
x1:	  break;
#endif
	}			/* switch */

      /* reply to the request */
      putpacket(remcomOutBuffer);
    }
}

/* This function will generate a breakpoint exception.  It is used at the
   beginning of a program to sync up with a debugger and can be used
   otherwise as a quick means to stop program execution and "break" into
   the debugger. */

void
breakpoint()
{
  if (initialized)
    BREAKPOINT();
}
