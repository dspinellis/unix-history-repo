/* Sequent Symmetry host interface, for GDB when running under Unix.
   Copyright 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.

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

/* FIXME, some 387-specific items of use taken from i387-tdep.c -- ought to be
   merged back in. */

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
#include <sgtty.h>
#define TERMINAL struct sgttyb

#include "gdbcore.h"

void
store_inferior_registers(regno)
int regno;
{
  struct pt_regset regs;
  int reg_tmp, i;
  extern char registers[];
  
#if 0
  /* PREPARE_TO_STORE deals with this.  */
  if (-1 == regno)
    {
#endif
      regs.pr_eax = *(int *)&registers[REGISTER_BYTE(0)];
      regs.pr_ebx = *(int *)&registers[REGISTER_BYTE(5)];
      regs.pr_ecx = *(int *)&registers[REGISTER_BYTE(2)];
      regs.pr_edx = *(int *)&registers[REGISTER_BYTE(1)];
      regs.pr_esi = *(int *)&registers[REGISTER_BYTE(6)];
      regs.pr_edi = *(int *)&registers[REGISTER_BYTE(7)];
      regs.pr_esp = *(int *)&registers[REGISTER_BYTE(14)];
      regs.pr_ebp = *(int *)&registers[REGISTER_BYTE(15)];
      regs.pr_eip = *(int *)&registers[REGISTER_BYTE(16)];
      regs.pr_flags = *(int *)&registers[REGISTER_BYTE(17)];
      for (i = 0; i < 31; i++) {
	regs.pr_fpa.fpa_regs[i] =
	  *(int *)&registers[REGISTER_BYTE(FP1_REGNUM+i)];
      }
#if 0
    }
  else
    {
      reg_tmp = *(int *)&registers[REGISTER_BYTE(regno)];
      ptrace(XPT_RREGS, inferior_pid, (PTRACE_ARG3_TYPE) &regs, 0);
      switch (regno)
	{
	case 0:
	  regs.pr_eax = *(int *)&registers[REGISTER_BYTE(0)];
	  break;
	case 5:
	  regs.pr_ebx = *(int *)&registers[REGISTER_BYTE(5)];
	  break;
	case 2:
	  regs.pr_ecx = *(int *)&registers[REGISTER_BYTE(2)];
	  break;
	case 1:
	  regs.pr_edx = *(int *)&registers[REGISTER_BYTE(1)];
	  break;
	case 6:
	  regs.pr_esi = *(int *)&registers[REGISTER_BYTE(6)];
	  break;
	case 7:
	  regs.pr_edi = *(int *)&registers[REGISTER_BYTE(7)];
	  break;
	case 15:
	  regs.pr_ebp = *(int *)&registers[REGISTER_BYTE(15)];
	  break;
	case 14:
	  regs.pr_esp = *(int *)&registers[REGISTER_BYTE(14)];
	  break;
	case 16:
	  regs.pr_eip = *(int *)&registers[REGISTER_BYTE(16)];
	  break;
	case 17:
	  regs.pr_flags = *(int *)&registers[REGISTER_BYTE(17)];
	  break;
	}
    }
#endif /* 0 */
  ptrace(XPT_WREGS, inferior_pid, (PTRACE_ARG3_TYPE) &regs, 0);
}

void
fetch_inferior_registers (regno)
     int regno;
{
    int i;
    struct pt_regset regs;
    extern char registers[];

    registers_fetched ();
    
    ptrace(XPT_RREGS, inferior_pid, (PTRACE_ARG3_TYPE) &regs, 0);
    *(int *)&registers[REGISTER_BYTE(0)] = regs.pr_eax;
    *(int *)&registers[REGISTER_BYTE(5)] = regs.pr_ebx;
    *(int *)&registers[REGISTER_BYTE(2)] = regs.pr_ecx;
    *(int *)&registers[REGISTER_BYTE(1)] = regs.pr_edx;
    *(int *)&registers[REGISTER_BYTE(6)] = regs.pr_esi;
    *(int *)&registers[REGISTER_BYTE(7)] = regs.pr_edi;
    *(int *)&registers[REGISTER_BYTE(15)] = regs.pr_ebp;
    *(int *)&registers[REGISTER_BYTE(14)] = regs.pr_esp;
    *(int *)&registers[REGISTER_BYTE(16)] = regs.pr_eip;
    *(int *)&registers[REGISTER_BYTE(17)] = regs.pr_flags;
    for (i = 0; i < FPA_NREGS; i++) {
	*(int *)&registers[REGISTER_BYTE(FP1_REGNUM+i)] = regs.pr_fpa.fpa_regs[i];
    }
    bcopy(regs.pr_fpu.fpu_stack[0], &registers[REGISTER_BYTE(3)], 10);
    bcopy(regs.pr_fpu.fpu_stack[1], &registers[REGISTER_BYTE(4)], 10);
    bcopy(regs.pr_fpu.fpu_stack[2], &registers[REGISTER_BYTE(8)], 10);
    bcopy(regs.pr_fpu.fpu_stack[3], &registers[REGISTER_BYTE(9)], 10);
    bcopy(regs.pr_fpu.fpu_stack[4], &registers[REGISTER_BYTE(10)], 10);
    bcopy(regs.pr_fpu.fpu_stack[5], &registers[REGISTER_BYTE(11)], 10);
    bcopy(regs.pr_fpu.fpu_stack[6], &registers[REGISTER_BYTE(12)], 10);
    bcopy(regs.pr_fpu.fpu_stack[7], &registers[REGISTER_BYTE(13)], 10);
}


/* Work with core dump and executable files, for GDB. 
   This code would be in core.c if it weren't machine-dependent. */

void
core_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
  int val;
  extern char registers[];

  /* Discard all vestiges of any previous core file
     and mark data and stack spaces as empty.  */

  if (corefile)
    free (corefile);
  corefile = 0;

  if (corechan >= 0)
    close (corechan);
  corechan = -1;

  data_start = 0;
  data_end = 0;
  stack_start = STACK_END_ADDR;
  stack_end = STACK_END_ADDR;

  /* Now, if a new core file was specified, open it and digest it.  */

  if (filename)
    {
      filename = tilde_expand (filename);
      make_cleanup (free, filename);
      
      if (have_inferior_p ())
	error ("To look at a core file, you must kill the inferior with \"kill\".");
      corechan = open (filename, O_RDONLY, 0);
      if (corechan < 0)
	perror_with_name (filename);
      /* 4.2-style (and perhaps also sysV-style) core dump file.  */
      {
	struct user u;
	int reg_offset;

	val = myread (corechan, &u, sizeof u);
	if (val < 0)
	  perror_with_name (filename);
	data_start = exec_data_start;

	data_end = data_start + NBPG * (u.u_dsize - u.u_tsize);
	stack_start = stack_end - NBPG * u.u_ssize;
	data_offset = NBPG * UPAGES;
	stack_offset = ctob(UPAGES + u.u_dsize - u.u_tsize);
	reg_offset = (int) u.u_ar0 - KERNEL_U_ADDR;
printf("u.u_tsize= %#x, u.u_dsize= %#x, u.u_ssize= %#x, stack_off= %#x\n",
       u.u_tsize, u.u_dsize, u.u_ssize, stack_offset);

	core_aouthdr.a_magic = 0;

	/* Read the register values out of the core file and store
	   them where `read_register' will find them.  */

	{
	  register int regno;

	  for (regno = 0; regno < NUM_REGS; regno++)
	    {
	      char buf[MAX_REGISTER_RAW_SIZE];

	      val = lseek (corechan, register_addr (regno, reg_offset), 0);
	      if (val < 0)
		perror_with_name (filename);

 	      val = myread (corechan, buf, sizeof buf);
	      if (val < 0)
		perror_with_name (filename);
	      supply_register (regno, buf);
	    }
	}
      }
      if (filename[0] == '/')
	corefile = savestring (filename, strlen (filename));
      else
	{
	  corefile = concat (current_directory, "/", filename, NULL);
	}

      set_current_frame(create_new_frame(read_register(FP_REGNUM),
					 read_pc()));
/*      set_current_frame (read_register (FP_REGNUM));*/
      select_frame (get_current_frame (), 0);
      validate_files ();
    }
  else if (from_tty)
    printf ("No core file now.\n");
}

/* FIXME:  This should be merged with i387-tdep.c as well. */
static
print_fpu_status(ep)
struct pt_regset ep;
{
    int i;
    int bothstatus;
    int top;
    int fpreg;
    unsigned char *p;
    
    printf("80387:");
    if (ep.pr_fpu.fpu_ip == 0) {
	printf(" not in use.\n");
	return;
    } else {
	printf("\n");
    }
    if (ep.pr_fpu.fpu_status != 0) {
	print_387_status_word (ep.pr_fpu.fpu_status);
    }
    print_387_control_word (ep.pr_fpu.fpu_control);
    printf ("last exception: ");
    printf ("opcode 0x%x; ", ep.pr_fpu.fpu_rsvd4);
    printf ("pc 0x%x:0x%x; ", ep.pr_fpu.fpu_cs, ep.pr_fpu.fpu_ip);
    printf ("operand 0x%x:0x%x\n", ep.pr_fpu.fpu_data_offset, ep.pr_fpu.fpu_op_sel);
    
    top = (ep.pr_fpu.fpu_status >> 11) & 7;
    
    printf ("regno  tag  msb              lsb  value\n");
    for (fpreg = 7; fpreg >= 0; fpreg--) 
	{
	    double val;
	    
	    printf ("%s %d: ", fpreg == top ? "=>" : "  ", fpreg);
	    
	    switch ((ep.pr_fpu.fpu_tag >> (fpreg * 2)) & 3) 
		{
		case 0: printf ("valid "); break;
		case 1: printf ("zero  "); break;
		case 2: printf ("trap  "); break;
		case 3: printf ("empty "); break;
		}
	    for (i = 9; i >= 0; i--)
		printf ("%02x", ep.pr_fpu.fpu_stack[fpreg][i]);
	    
	    i387_to_double (ep.pr_fpu.fpu_stack[fpreg], (char *)&val);
	    printf ("  %g\n", val);
	}
    if (ep.pr_fpu.fpu_rsvd1)
	warning ("rsvd1 is 0x%x\n", ep.pr_fpu.fpu_rsvd1);
    if (ep.pr_fpu.fpu_rsvd2)
	warning ("rsvd2 is 0x%x\n", ep.pr_fpu.fpu_rsvd2);
    if (ep.pr_fpu.fpu_rsvd3)
	warning ("rsvd3 is 0x%x\n", ep.pr_fpu.fpu_rsvd3);
    if (ep.pr_fpu.fpu_rsvd5)
	warning ("rsvd5 is 0x%x\n", ep.pr_fpu.fpu_rsvd5);
}


print_1167_control_word(pcr)
unsigned int pcr;

{
    int pcr_tmp;

    pcr_tmp = pcr & FPA_PCR_MODE;
    printf("\tMODE= %#x; RND= %#x ", pcr_tmp, pcr_tmp & 12);
    switch (pcr_tmp & 12) {
    case 0:
	printf("RN (Nearest Value)");
	break;
    case 1:
	printf("RZ (Zero)");
	break;
    case 2:
	printf("RP (Positive Infinity)");
	break;
    case 3:
	printf("RM (Negative Infinity)");
	break;
    }
    printf("; IRND= %d ", pcr_tmp & 2);
    if (0 == pcr_tmp & 2) {
	printf("(same as RND)\n");
    } else {
	printf("(toward zero)\n");
    }
    pcr_tmp = pcr & FPA_PCR_EM;
    printf("\tEM= %#x", pcr_tmp);
    if (pcr_tmp & FPA_PCR_EM_DM) printf(" DM");
    if (pcr_tmp & FPA_PCR_EM_UOM) printf(" UOM");
    if (pcr_tmp & FPA_PCR_EM_PM) printf(" PM");
    if (pcr_tmp & FPA_PCR_EM_UM) printf(" UM");
    if (pcr_tmp & FPA_PCR_EM_OM) printf(" OM");
    if (pcr_tmp & FPA_PCR_EM_ZM) printf(" ZM");
    if (pcr_tmp & FPA_PCR_EM_IM) printf(" IM");
    printf("\n");
    pcr_tmp = FPA_PCR_CC;
    printf("\tCC= %#x", pcr_tmp);
    if (pcr_tmp & FPA_PCR_20MHZ) printf(" 20MHZ");
    if (pcr_tmp & FPA_PCR_CC_Z) printf(" Z");
    if (pcr_tmp & FPA_PCR_CC_C2) printf(" C2");
    if (pcr_tmp & FPA_PCR_CC_C1) printf(" C1");
    switch (pcr_tmp) {
    case FPA_PCR_CC_Z:
	printf(" (Equal)");
	break;
    case FPA_PCR_CC_C1:
	printf(" (Less than)");
	break;
    case 0:
	printf(" (Greater than)");
	break;
    case FPA_PCR_CC_Z | FPA_PCR_CC_C1 | FPA_PCR_CC_C2:
	printf(" (Unordered)");
	break;
    default:
	printf(" (Undefined)");
	break;
    }
    printf("\n");
    pcr_tmp = pcr & FPA_PCR_AE;
    printf("\tAE= %#x", pcr_tmp);
    if (pcr_tmp & FPA_PCR_AE_DE) printf(" DE");
    if (pcr_tmp & FPA_PCR_AE_UOE) printf(" UOE");
    if (pcr_tmp & FPA_PCR_AE_PE) printf(" PE");
    if (pcr_tmp & FPA_PCR_AE_UE) printf(" UE");
    if (pcr_tmp & FPA_PCR_AE_OE) printf(" OE");
    if (pcr_tmp & FPA_PCR_AE_ZE) printf(" ZE");
    if (pcr_tmp & FPA_PCR_AE_EE) printf(" EE");
    if (pcr_tmp & FPA_PCR_AE_IE) printf(" IE");
    printf("\n");
}

print_1167_regs(regs)
long regs[FPA_NREGS];

{
    int i;

    union {
	double	d;
	long	l[2];
    } xd;
    union {
	float	f;
	long	l;
    } xf;


    for (i = 0; i < FPA_NREGS; i++) {
	xf.l = regs[i];
	printf("%%fp%d: raw= %#x, single= %f", i+1, regs[i], xf.f);
	if (!(i & 1)) {
	    printf("\n");
	} else {
	    xd.l[1] = regs[i];
	    xd.l[0] = regs[i+1];
	    printf(", double= %f\n", xd.d);
	}
    }
}

print_fpa_status(ep)
struct pt_regset ep;

{

    printf("WTL 1167:");
    if (ep.pr_fpa.fpa_pcr !=0) {
	printf("\n");
	print_1167_control_word(ep.pr_fpa.fpa_pcr);
	print_1167_regs(ep.pr_fpa.fpa_regs);
    } else {
	printf(" not in use.\n");
    }
}

i386_float_info ()
{
    char ubuf[UPAGES*NBPG];
    struct pt_regset regset;
    extern int corechan;
    
    if (have_inferior_p()) {
	call_ptrace(XPT_RREGS, inferior_pid, (PTRACE_ARG3_TYPE) &regset, 0);
    } else {
	if (lseek (corechan, 0, 0) < 0) {
	    perror ("seek on core file");
	}
	if (myread (corechan, ubuf, UPAGES*NBPG) < 0) {
	    perror ("read on core file");
	}
	/* only interested in the floating point registers */
	regset.pr_fpu = ((struct user *) ubuf)->u_fpusave;
	regset.pr_fpa = ((struct user *) ubuf)->u_fpasave;
    }
    print_fpu_status(regset);
    print_fpa_status(regset);
}
