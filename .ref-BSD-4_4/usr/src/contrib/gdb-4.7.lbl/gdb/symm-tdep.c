/* Sequent Symmetry target interface, for GDB when running under Unix.
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

/* many 387-specific items of use taken from i386-dep.c */

#include "defs.h"
#include "frame.h"
#include "inferior.h"
#include "symtab.h"

#include <signal.h>
#include <sys/param.h>
#include <sys/user.h>
#include <sys/dir.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include "gdbcore.h"
#include <fcntl.h>

static long i386_get_frame_setup ();
static i386_follow_jump ();

#include <sgtty.h>
#define TERMINAL struct sgttyb

exec_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
  int val;

  /* Eliminate all traces of old exec file.
     Mark text segment as empty.  */

  if (execfile)
    free (execfile);
  execfile = 0;
  data_start = 0;
  data_end -= exec_data_start;
  text_start = 0;
  text_end = 0;
  exec_data_start = 0;
  exec_data_end = 0;
  if (execchan >= 0)
    close (execchan);
  execchan = -1;

  /* Now open and digest the file the user requested, if any.  */

  if (filename)
    {
      filename = tilde_expand (filename);
      make_cleanup (free, filename);
      
      execchan = openp (getenv ("PATH"), 1, filename, O_RDONLY, 0,
			&execfile);
      if (execchan < 0)
	perror_with_name (filename);

#ifdef COFF_FORMAT
      {
	int aout_hdrsize;
	int num_sections;

	if (read_file_hdr (execchan, &file_hdr) < 0)
	  error ("\"%s\": not in executable format.", execfile);

	aout_hdrsize = file_hdr.f_opthdr;
	num_sections = file_hdr.f_nscns;

	if (read_aout_hdr (execchan, &exec_aouthdr, aout_hdrsize) < 0)
	  error ("\"%s\": can't read optional aouthdr", execfile);

	if (read_section_hdr (execchan, _TEXT, &text_hdr, num_sections,
			      aout_hdrsize) < 0)
	  error ("\"%s\": can't read text section header", execfile);

	if (read_section_hdr (execchan, _DATA, &data_hdr, num_sections,
			      aout_hdrsize) < 0)
	  error ("\"%s\": can't read data section header", execfile);

	text_start = exec_aouthdr.text_start;
	text_end = text_start + exec_aouthdr.tsize;
	text_offset = text_hdr.s_scnptr;
	exec_data_start = exec_aouthdr.data_start;
	exec_data_end = exec_data_start + exec_aouthdr.dsize;
	exec_data_offset = data_hdr.s_scnptr;
	data_start = exec_data_start;
	data_end += exec_data_start;
	exec_mtime = file_hdr.f_timdat;
      }
#else /* not COFF_FORMAT */
      {
	struct stat st_exec;

	val = myread (execchan, &exec_aouthdr, sizeof (AOUTHDR));

	if (val < 0)
	  perror_with_name (filename);

	text_start = N_ADDRADJ(exec_aouthdr);
        exec_data_start = round(exec_aouthdr.a_text, NBPG*CLSIZE);
	text_offset = N_TXTOFF (exec_aouthdr);
	exec_data_offset = N_TXTOFF (exec_aouthdr) + exec_aouthdr.a_text;
	text_end = exec_aouthdr.a_text;
        exec_data_end = exec_data_start + exec_aouthdr.a_data;
	data_start = exec_data_start;
	data_end = data_start + exec_aouthdr.a_data;
	exec_data_offset = N_TXTOFF(exec_aouthdr);
	fstat (execchan, &st_exec);
	exec_mtime = st_exec.st_mtime;
      }
#endif /* not COFF_FORMAT */

      validate_files ();
    }
  else if (from_tty)
    printf ("No exec file now.\n");

  /* Tell display code (if any) about the changed file name.  */
  if (exec_file_display_hook)
    (*exec_file_display_hook) (filename);
}

/* rounds 'one' up to divide evenly by 'two' */

int
round(one,two)
register int one, two;

{
    register int temp;
    temp = (one/two)*two;
    if (one != temp) {
	temp += two;
    }
    return temp;
}


static CORE_ADDR codestream_next_addr;
static CORE_ADDR codestream_addr;
static unsigned char codestream_buf[sizeof (int)];
static int codestream_off;
static int codestream_cnt;

#define codestream_tell() (codestream_addr + codestream_off)
#define codestream_peek() (codestream_cnt == 0 ? \
			   codestream_fill(1): codestream_buf[codestream_off])
#define codestream_get() (codestream_cnt-- == 0 ? \
			 codestream_fill(0) : codestream_buf[codestream_off++])


static unsigned char 
codestream_fill (peek_flag)
{
  codestream_addr = codestream_next_addr;
  codestream_next_addr += sizeof (int);
  codestream_off = 0;
  codestream_cnt = sizeof (int);
  read_memory (codestream_addr,
	       (unsigned char *)codestream_buf,
	       sizeof (int));
  
  if (peek_flag)
    return (codestream_peek());
  else
    return (codestream_get());
}

static void
codestream_seek (place)
{
  codestream_next_addr = place & -sizeof (int);
  codestream_cnt = 0;
  codestream_fill (1);
  while (codestream_tell() != place)
    codestream_get ();
}

static void
codestream_read (buf, count)
     unsigned char *buf;
{
  unsigned char *p;
  int i;
  p = buf;
  for (i = 0; i < count; i++)
    *p++ = codestream_get ();
}

/*
 * Following macro translates i386 opcode register numbers to Symmetry
 * register numbers.  This is used by FRAME_FIND_SAVED_REGS.
 *
 *           %eax  %ecx  %edx  %ebx  %esp  %ebp  %esi  %edi
 * i386        0     1     2     3     4     5     6     7
 * Symmetry    0     2     1     5    14    15     6     7
 *
 */
#define I386_REGNO_TO_SYMMETRY(n) \
((n)==0?0 :(n)==1?2 :(n)==2?1 :(n)==3?5 :(n)==4?14 :(n)==5?15 :(n))

/* from i386-dep.c */
i386_frame_find_saved_regs (fip, fsrp)
     struct frame_info *fip;
     struct frame_saved_regs *fsrp;
{
  unsigned long locals;
  unsigned char *p;
  unsigned char op;
  CORE_ADDR dummy_bottom;
  CORE_ADDR adr;
  int i;
  
  bzero (fsrp, sizeof *fsrp);
  
  /* if frame is the end of a dummy, compute where the
   * beginning would be
   */
  dummy_bottom = fip->frame - 4 - NUM_REGS*4 - CALL_DUMMY_LENGTH;
  
  /* check if the PC is in the stack, in a dummy frame */
  if (dummy_bottom <= fip->pc && fip->pc <= fip->frame) 
    {
      /* all regs were saved by push_call_dummy () */
      adr = fip->frame - 4;
      for (i = 0; i < NUM_REGS; i++) 
	{
	  fsrp->regs[i] = adr;
	  adr -= 4;
	}
      return;
    }
  
  locals = i386_get_frame_setup (get_pc_function_start (fip->pc));
  
  if (locals >= 0) 
    {
      adr = fip->frame - 4 - locals;
      for (i = 0; i < 8; i++) 
	{
	  op = codestream_get ();
	  if (op < 0x50 || op > 0x57)
	    break;
	  fsrp->regs[I386_REGNO_TO_SYMMETRY(op - 0x50)] = adr;
	  adr -= 4;
	}
    }
  
  fsrp->regs[PC_REGNUM] = fip->frame + 4;
  fsrp->regs[FP_REGNUM] = fip->frame;
}

static long
i386_get_frame_setup (pc)
{
  unsigned char op;
  
  codestream_seek (pc);
  
  i386_follow_jump ();
  
  op = codestream_get ();
  
  if (op == 0x58) /* popl %eax */
    {
      /*
       * this function must start with
       * 
       *    popl %eax		  0x58
       *    xchgl %eax, (%esp)  0x87 0x04 0x24
       * or xchgl %eax, 0(%esp) 0x87 0x44 0x24 0x00
       *
       * (the system 5 compiler puts out the second xchg
       * inst, and the assembler doesn't try to optimize it,
       * so the 'sib' form gets generated)
       * 
       * this sequence is used to get the address of the return
       * buffer for a function that returns a structure
       */
      int pos;
      unsigned char buf[4];
      static unsigned char proto1[3] = { 0x87,0x04,0x24 };
      static unsigned char proto2[4] = { 0x87,0x44,0x24,0x00 };
      pos = codestream_tell ();
      codestream_read (buf, 4);
      if (bcmp (buf, proto1, 3) == 0)
	pos += 3;
      else if (bcmp (buf, proto2, 4) == 0)
	pos += 4;
      
      codestream_seek (pos);
      op = codestream_get (); /* update next opcode */
    }
  
  if (op == 0x55) 			/* pushl %esp */
    {
      if (codestream_get () != 0x8b)	/* movl %esp, %ebp (2bytes) */
	return (-1);
      if (codestream_get () != 0xec)
	return (-1);
      /*
       * check for stack adjustment 
       *
       *  subl $XXX, %esp
       *
       * note: you can't subtract a 16 bit immediate
       * from a 32 bit reg, so we don't have to worry
       * about a data16 prefix 
       */
      op = codestream_peek ();
      if (op == 0x83)  /* subl with 8 bit immed */
	{
	  codestream_get ();
	  if (codestream_get () != 0xec)
	    return (-1);
	  /* subl with signed byte immediate 
	   * (though it wouldn't make sense to be negative)
	   */
	  return (codestream_get());
	}
      else if (op == 0x81)  /* subl with 32 bit immed */
	{
	  int locals;
	  if (codestream_get () != 0xec)
	    return (-1);
	  /* subl with 32 bit immediate */
	  codestream_read ((unsigned char *)&locals, 4);
	  return (locals);
	} 
      else 
	{
	  return (0);
	}
    } 
  else if (op == 0xc8) 
    {
      /* enter instruction: arg is 16 unsigned immed */
      unsigned short slocals;
      codestream_read ((unsigned char *)&slocals, 2);
      codestream_get (); /* flush final byte of enter instruction */
      return (slocals);
    }
  return (-1);
}

/* next instruction is a jump, move to target */
static
i386_follow_jump ()
{
  int long_delta;
  short short_delta;
  char byte_delta;
  int data16;
  int pos;
  
  pos = codestream_tell ();
  
  data16 = 0;
  if (codestream_peek () == 0x66)
    {
      codestream_get ();
      data16 = 1;
    }
  
  switch (codestream_get ())
    {
    case 0xe9:
      /* relative jump: if data16 == 0, disp32, else disp16 */
      if (data16)
	{
	  codestream_read ((unsigned char *)&short_delta, 2);
	  pos += short_delta + 3; /* include size of jmp inst */
	}
      else
	{
	  codestream_read ((unsigned char *)&long_delta, 4);
	  pos += long_delta + 5;
	}
      break;
    case 0xeb:
      /* relative jump, disp8 (ignore data16) */
      codestream_read ((unsigned char *)&byte_delta, 1);
      pos += byte_delta + 2;
      break;
    }
  codestream_seek (pos + data16);
}

/* return pc of first real instruction */
/* from i386-dep.c */

i386_skip_prologue (pc)
{
  unsigned char op;
  int i;
  
  if (i386_get_frame_setup (pc) < 0)
    return (pc);
  
  /* found valid frame setup - codestream now points to 
   * start of push instructions for saving registers
   */
  
  /* skip over register saves */
  for (i = 0; i < 8; i++)
    {
      op = codestream_peek ();
      /* break if not pushl inst */
      if (op < 0x50 || op > 0x57) 
	break;
      codestream_get ();
    }
  
  i386_follow_jump ();
  
  return (codestream_tell ());
}

symmetry_extract_return_value(type, regbuf, valbuf)
     struct type *type;
     char *regbuf;
     char *valbuf;
{
  union { 
    double	d; 
    int	l[2]; 
  } xd; 
  struct minimal_symbol *msymbol;
  float f;

  if (TYPE_CODE_FLT == TYPE_CODE(type)) { 
    msymbol = lookup_minimal_symbol ("1167_flt", (struct objfile *) NULL);
    if (msymbol != NULL) {
      /* found "1167_flt" means 1167, %fp2-%fp3 */ 
      /* float & double; 19= %fp2, 20= %fp3 */
      /* no single precision on 1167 */
      xd.l[1] = *((int *)&regbuf[REGISTER_BYTE(19)]);
      xd.l[0] = *((int *)&regbuf[REGISTER_BYTE(20)]);
      switch (TYPE_LENGTH(type)) {
      case 4:
	f = (float) xd.d;
	bcopy(&f, valbuf, TYPE_LENGTH(type));
	break;
      case 8:
	bcopy(&xd.d, valbuf, TYPE_LENGTH(type)); 
	break;
      default:
	error("Unknown floating point size");
	break;
      }
    } else { 
      /* 387 %st(0), gcc uses this */ 
      i387_to_double(((int *)&regbuf[REGISTER_BYTE(3)]),
		     &xd.d); 
      switch (TYPE_LENGTH(type)) {
      case 4:			/* float */
	f = (float) xd.d;
	bcopy(&f, valbuf, 4); 
	break;
      case 8:			/* double */
	bcopy(&xd.d, valbuf, 8);
	break;
      default:
	error("Unknown floating point size");
	break;
      }
    }
  } else { 
    bcopy (regbuf, valbuf, TYPE_LENGTH (type)); 
  }
}
