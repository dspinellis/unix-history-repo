/* A.out "format 1" file handling code for BFD.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
   Written by Cygnus Support.

This file is part of BFD, the Binary File Descriptor library.

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

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"

#include "aout/sun4.h"
#include "libaout.h"           /* BFD a.out internal data structures */

#include "aout/aout64.h"
#include "aout/stab_gnu.h"
#include "aout/ar.h"

/*
The file @code{aoutf1.h} contains the code for BFD's
a.out back end. Control over the generated back end is given by these
two preprocessor names:
@table @code
@item ARCH_SIZE
This value should be either 32 or 64, depending upon the size of an
int in the target format. It changes the sizes of the structs which
perform the memory/disk mapping of structures.

The 64 bit backend may only be used if the host compiler supports 64
ints (eg long long with gcc), by defining the name @code{HOST_64_BIT} in @code{bfd.h}.
With this name defined, @emph{all} bfd operations are performed with 64bit
arithmetic, not just those to a 64bit target.

@item TARGETNAME
The name put into the target vector.
@item
@end table

*/

void (*bfd_error_trap)();

/*SUPPRESS558*/
/*SUPPRESS529*/

void
DEFUN(NAME(sunos,set_arch_mach), (abfd, machtype),
      bfd *abfd AND int machtype)
{
  /* Determine the architecture and machine type of the object file.  */
  enum bfd_architecture arch;
  long machine;
  switch (machtype) {

  case M_UNKNOWN:
      /* Some Sun3s make magic numbers without cpu types in them, so
	 we'll default to the 68020. */
    arch = bfd_arch_m68k;
    machine = 68020;
    break;
    
  case M_68010:
  case M_HP200:
    arch = bfd_arch_m68k;
    machine = 68010;
    break;
    
  case M_68020:
  case M_HP300:
    arch = bfd_arch_m68k;
    machine = 68020;
    break;
    
  case M_SPARC:
    arch = bfd_arch_sparc;
    machine = 0;
    break;
    
  case M_386:
    arch = bfd_arch_i386;
    machine = 0;
    break;
    
  case M_29K:
    arch = bfd_arch_a29k;
    machine = 0;
    break;
    
  case M_HPUX:
    arch = bfd_arch_m68k;
    machine = 0;
    break;

  default:
    arch = bfd_arch_obscure;
    machine = 0;
    break;
  }
  bfd_set_arch_mach(abfd, arch, machine);  
}

#define SET_ARCH_MACH(ABFD, EXEC) \
  NAME(sunos,set_arch_mach)(ABFD, N_MACHTYPE (EXEC)); \
  choose_reloc_size(ABFD);

/* Determine the size of a relocation entry, based on the architecture */
static void
DEFUN(choose_reloc_size,(abfd),
bfd *abfd)
{
  switch (bfd_get_arch(abfd)) {
  case bfd_arch_sparc:
  case bfd_arch_a29k:
    obj_reloc_entry_size (abfd) = RELOC_EXT_SIZE;
    break;
  default:
    obj_reloc_entry_size (abfd) = RELOC_STD_SIZE;
    break;
  }
}

/* Write an object file in SunOS format.
  Section contents have already been written.  We write the
  file header, symbols, and relocation.  */

static boolean
DEFUN(NAME(aout,sunos4_write_object_contents),
      (abfd),
      bfd *abfd)
{
  struct external_exec exec_bytes;
  struct internal_exec *execp = exec_hdr (abfd);
    
  /* Magic number, maestro, please!  */
  switch (bfd_get_arch(abfd)) {
  case bfd_arch_m68k:
    switch (bfd_get_mach(abfd)) {
    case 68010:
      N_SET_MACHTYPE(*execp, M_68010);
      break;
    default:
    case 68020:
      N_SET_MACHTYPE(*execp, M_68020);
      break;
    }
    break;
  case bfd_arch_sparc:
    N_SET_MACHTYPE(*execp, M_SPARC);
    break;
  case bfd_arch_i386:
    N_SET_MACHTYPE(*execp, M_386);
    break;
  case bfd_arch_a29k:
    N_SET_MACHTYPE(*execp, M_29K);
    break;
  default:
    N_SET_MACHTYPE(*execp, M_UNKNOWN);
  }
    
  choose_reloc_size(abfd);
    
  /* FIXME */
  N_SET_FLAGS (*execp, 0x1);
    
  WRITE_HEADERS(abfd, execp);

  return true;
}

/* core files */

#define CORE_MAGIC 0x080456
#define CORE_NAMELEN 16

/* The core structure is taken from the Sun documentation.
  Unfortunately, they don't document the FPA structure, or at least I
  can't find it easily.  Fortunately the core header contains its own
  length.  So this shouldn't cause problems, except for c_ucode, which
  so far we don't use but is easy to find with a little arithmetic. */

/* But the reg structure can be gotten from the SPARC processor handbook.
  This really should be in a GNU include file though so that gdb can use
  the same info. */
struct regs {
  int r_psr;
  int r_pc;
  int r_npc;
  int r_y;
  int r_g1;
  int r_g2;
  int r_g3;
  int r_g4;
  int r_g5;
  int r_g6;
  int r_g7;
  int r_o0;
  int r_o1;
  int r_o2;
  int r_o3;
  int r_o4;
  int r_o5;
  int r_o6;
  int r_o7;
};

/* Taken from Sun documentation: */

/* FIXME:  It's worse than we expect.  This struct contains TWO substructs
  neither of whose size we know, WITH STUFF IN BETWEEN THEM!  We can't
  even portably access the stuff in between!  */

struct external_sparc_core {
  int c_magic;			/* Corefile magic number */
  int c_len;			/* Sizeof (struct core) */
#define	SPARC_CORE_LEN	432
  int c_regs[19];		/* General purpose registers -- MACHDEP SIZE */
  struct external_exec c_aouthdr; /* A.out header */
  int c_signo;			  /* Killing signal, if any */
  int c_tsize;			  /* Text size (bytes) */
  int c_dsize;			  /* Data size (bytes) */
  int c_ssize;			  /* Stack size (bytes) */
  char c_cmdname[CORE_NAMELEN + 1]; /* Command name */
  double fp_stuff[1];		    /* external FPU state (size unknown by us) */
  /* The type "double" is critical here, for alignment.
    SunOS declares a struct here, but the struct's alignment
      is double since it contains doubles.  */
  int c_ucode;			/* Exception no. from u_code */
  /* (this member is not accessible by name since we don't
    portably know the size of fp_stuff.) */
};

struct external_sun3_core {
  int c_magic;			/* Corefile magic number */
  int c_len;			/* Sizeof (struct core) */
#define	SUN3_CORE_LEN	826	/* As of SunOS 4.1.1 */
  int c_regs[18];		/* General purpose registers -- MACHDEP SIZE */
  struct external_exec c_aouthdr;	/* A.out header */
  int c_signo;			/* Killing signal, if any */
  int c_tsize;			/* Text size (bytes) */
  int c_dsize;			/* Data size (bytes) */
  int c_ssize;			/* Stack size (bytes) */
  char c_cmdname[CORE_NAMELEN + 1]; /* Command name */
  double fp_stuff[1];		    /* external FPU state (size unknown by us) */
  /* The type "double" is critical here, for alignment.
    SunOS declares a struct here, but the struct's alignment
      is double since it contains doubles.  */
  int c_ucode;			/* Exception no. from u_code */
  /* (this member is not accessible by name since we don't
    portably know the size of fp_stuff.) */
};

struct internal_sunos_core {
  int c_magic;			/* Corefile magic number */
  int c_len;			/* Sizeof (struct core) */
  long c_regs_pos;		/* file offset of General purpose registers */
  int c_regs_size;		/* size of General purpose registers */
  struct internal_exec c_aouthdr; /* A.out header */
  int c_signo;			  /* Killing signal, if any */
  int c_tsize;			  /* Text size (bytes) */
  int c_dsize;			  /* Data size (bytes) */
  int c_ssize;			  /* Stack size (bytes) */
  long c_stacktop;		  /* Stack top (address) */
  char c_cmdname[CORE_NAMELEN + 1]; /* Command name */
  long fp_stuff_pos;		/* file offset of external FPU state (regs) */
  int fp_stuff_size;		/* Size of it */
  int c_ucode;			/* Exception no. from u_code */
};

/* byte-swap in the Sun-3 core structure */
static void
DEFUN(swapcore_sun3,(abfd, ext, intcore),
      bfd *abfd AND
      char *ext AND
      struct internal_sunos_core *intcore)
{
  struct external_sun3_core *extcore = (struct external_sun3_core *)ext;

  intcore->c_magic = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_magic);
  intcore->c_len   = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_len  );
  intcore->c_regs_pos  = (long) (((struct external_sun3_core *)0)->c_regs);
  intcore->c_regs_size = sizeof (extcore->c_regs);
  NAME(aout,swap_exec_header_in)(abfd, &extcore->c_aouthdr,&intcore->c_aouthdr);
  intcore->c_signo = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_signo);
  intcore->c_tsize = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_tsize);
  intcore->c_dsize = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_dsize);
  intcore->c_ssize = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_ssize);
  memcpy (intcore->c_cmdname, extcore->c_cmdname, sizeof (intcore->c_cmdname));
  intcore->fp_stuff_pos = (long) (((struct external_sun3_core *)0)->fp_stuff);
  /* FP stuff takes up whole rest of struct, except c_ucode. */
  intcore->fp_stuff_size = intcore->c_len - (sizeof extcore->c_ucode) -
    (file_ptr)(((struct external_sun3_core *)0)->fp_stuff);
  /* Ucode is the last thing in the struct -- just before the end */
  intcore->c_ucode = 
    bfd_h_get_32 (abfd, 
		  intcore->c_len - sizeof (extcore->c_ucode) + (unsigned char *)extcore);
  intcore->c_stacktop = 0x0E000000; /* By experimentation */
}


/* byte-swap in the Sparc core structure */
static void
DEFUN(swapcore_sparc,(abfd, ext, intcore),
      bfd *abfd AND
      char *ext AND
      struct internal_sunos_core *intcore)
{
  struct external_sparc_core *extcore = (struct external_sparc_core *)ext;
  
  intcore->c_magic = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_magic);
  intcore->c_len   = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_len  );
  intcore->c_regs_pos  = (long) (((struct external_sparc_core *)0)->c_regs);
  intcore->c_regs_size = sizeof (extcore->c_regs);
  NAME(aout,swap_exec_header_in)(abfd, &extcore->c_aouthdr,&intcore->c_aouthdr);
  intcore->c_signo = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_signo);
  intcore->c_tsize = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_tsize);
  intcore->c_dsize = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_dsize);
  intcore->c_ssize = bfd_h_get_32 (abfd, (unsigned char *)&extcore->c_ssize);
  memcpy (intcore->c_cmdname, extcore->c_cmdname, sizeof (intcore->c_cmdname));
  intcore->fp_stuff_pos = (long) (((struct external_sparc_core *)0)->fp_stuff);
  /* FP stuff takes up whole rest of struct, except c_ucode. */
  intcore->fp_stuff_size = intcore->c_len - (sizeof extcore->c_ucode) -
    (file_ptr)(((struct external_sparc_core *)0)->fp_stuff);
  /* Ucode is the last thing in the struct -- just before the end */
  intcore->c_ucode =
    bfd_h_get_32 (abfd, 
		  intcore->c_len - sizeof (extcore->c_ucode) + (unsigned char *)extcore);
  /* Supposedly the user stack grows downward from the bottom of kernel memory.
     Presuming that this remains true, this definition will work. */
#define SPARC_USRSTACK (-(128*1024*1024))
  intcore->c_stacktop = SPARC_USRSTACK;	/* By experimentation */
}

/* need this cast because ptr is really void * */
#define core_hdr(bfd) ((bfd)->tdata.sun_core_data)
#define core_datasec(bfd) (core_hdr(bfd)->data_section)
#define core_stacksec(bfd) (core_hdr(bfd)->stack_section)
#define core_regsec(bfd) (core_hdr(bfd)->reg_section)
#define core_reg2sec(bfd) (core_hdr(bfd)->reg2_section)

/* These are stored in the bfd's tdata */
struct sun_core_struct {
  struct internal_sunos_core *hdr;             /* core file header */
  asection *data_section;
  asection *stack_section;
  asection *reg_section;
  asection *reg2_section;
};

static bfd_target *
DEFUN(sunos4_core_file_p,(abfd),
      bfd *abfd)
{
  unsigned char longbuf[4];	/* Raw bytes of various header fields */
  int core_size;
  int core_mag;
  struct internal_sunos_core *core;
  char *extcore;
  struct mergem {
    struct sun_core_struct suncoredata;
    struct internal_sunos_core internal_sunos_core;
    char external_core[1];
  } *mergem;
  
  bfd_error = system_call_error;
  
  if (bfd_read ((PTR)longbuf, 1, sizeof (longbuf), abfd) !=
      sizeof (longbuf))
    return 0;
  core_mag = bfd_h_get_32 (abfd, longbuf);

  if (core_mag != CORE_MAGIC) return 0;

  /* SunOS core headers can vary in length; second word is size; */
  if (bfd_read ((PTR)longbuf, 1, sizeof (longbuf), abfd) !=
      sizeof (longbuf))
    return 0;
  core_size = bfd_h_get_32 (abfd, longbuf);
  /* Sanity check */
  if (core_size > 20000)
    return 0;

  if (bfd_seek (abfd, (file_ptr) 0, SEEK_SET) < 0) return 0;

  mergem = (struct mergem *)bfd_zalloc (abfd, core_size + sizeof (struct mergem));
  if (mergem == NULL) {
    bfd_error = no_memory;
    return 0;
  }

  extcore = mergem->external_core;

  if ((bfd_read ((PTR) extcore, 1, core_size, abfd)) != core_size) {
    bfd_error = system_call_error;
    bfd_release (abfd, (char *)mergem);
    return 0;
  }

  /* Validate that it's a core file we know how to handle, due to sun
     botching the positioning of registers and other fields in a machine
     dependent way.  */
  core = &mergem->internal_sunos_core;
  switch (core_size) {
  case SPARC_CORE_LEN:
    swapcore_sparc (abfd, extcore, core);
    break;
  case SUN3_CORE_LEN:
    swapcore_sun3 (abfd, extcore, core);
    break;
  default:
    bfd_error = system_call_error;		/* FIXME */
    bfd_release (abfd, (char *)mergem);
    return 0;
  }

 abfd->tdata.sun_core_data = &mergem->suncoredata;
 abfd->tdata.sun_core_data->hdr = core;

  /* create the sections.  This is raunchy, but bfd_close wants to reclaim
     them */
  core_stacksec (abfd) = (asection *) bfd_zalloc (abfd, sizeof (asection));
  if (core_stacksec (abfd) == NULL) {
  loser:
    bfd_error = no_memory;
    bfd_release (abfd, (char *)mergem);
    return 0;
  }
  core_datasec (abfd) = (asection *) bfd_zalloc (abfd, sizeof (asection));
  if (core_datasec (abfd) == NULL) {
  loser1:
    bfd_release (abfd, core_stacksec (abfd));
    goto loser;
  }
  core_regsec (abfd) = (asection *) bfd_zalloc (abfd, sizeof (asection));
  if (core_regsec (abfd) == NULL) {
  loser2:
    bfd_release (abfd, core_datasec (abfd));
    goto loser1;
  }
  core_reg2sec (abfd) = (asection *) bfd_zalloc (abfd, sizeof (asection));
  if (core_reg2sec (abfd) == NULL) {
    bfd_release (abfd, core_regsec (abfd));
    goto loser2;
  }

  core_stacksec (abfd)->name = ".stack";
  core_datasec (abfd)->name = ".data";
  core_regsec (abfd)->name = ".reg";
  core_reg2sec (abfd)->name = ".reg2";

  core_stacksec (abfd)->flags = SEC_ALLOC + SEC_LOAD + SEC_HAS_CONTENTS;
  core_datasec (abfd)->flags = SEC_ALLOC + SEC_LOAD + SEC_HAS_CONTENTS;
  core_regsec (abfd)->flags = SEC_ALLOC + SEC_HAS_CONTENTS;
  core_reg2sec (abfd)->flags = SEC_ALLOC + SEC_HAS_CONTENTS;

  core_stacksec (abfd)->_raw_size = core->c_ssize;
  core_datasec (abfd)->_raw_size = core->c_dsize;
  core_regsec (abfd)->_raw_size = core->c_regs_size;
  core_reg2sec (abfd)->_raw_size = core->fp_stuff_size;

  core_stacksec (abfd)->vma = (core->c_stacktop - core->c_ssize);
  core_datasec (abfd)->vma = N_DATADDR(core->c_aouthdr);
  core_regsec (abfd)->vma = 0;
  core_reg2sec (abfd)->vma = 0;

  core_stacksec (abfd)->filepos = core->c_len + core->c_dsize;
  core_datasec (abfd)->filepos = core->c_len;
  /* We'll access the regs afresh in the core file, like any section: */
  core_regsec (abfd)->filepos = (file_ptr)core->c_regs_pos;
  core_reg2sec (abfd)->filepos = (file_ptr)core->fp_stuff_pos;

  /* Align to word at least */
  core_stacksec (abfd)->alignment_power = 2;
  core_datasec (abfd)->alignment_power = 2;
  core_regsec (abfd)->alignment_power = 2;
  core_reg2sec (abfd)->alignment_power = 2;

  abfd->sections = core_stacksec (abfd);
  core_stacksec (abfd)->next = core_datasec (abfd);
  core_datasec (abfd)->next = core_regsec (abfd);
  core_regsec (abfd)->next = core_reg2sec (abfd);

  abfd->section_count = 4;

  return abfd->xvec;
}

static char *sunos4_core_file_failing_command (abfd)
bfd *abfd;
  {
  return core_hdr (abfd)->hdr->c_cmdname;
}

static int
DEFUN(sunos4_core_file_failing_signal,(abfd),
      bfd *abfd)
{
  return core_hdr (abfd)->hdr->c_signo;
}

static boolean
DEFUN(sunos4_core_file_matches_executable_p, (core_bfd, exec_bfd),
      bfd *core_bfd AND
      bfd *exec_bfd)
{
  if (core_bfd->xvec != exec_bfd->xvec) {
    bfd_error = system_call_error;
    return false;
  }

  return (memcmp ((char *)&((core_hdr (core_bfd)->hdr)->c_aouthdr), 
		  (char *) exec_hdr (exec_bfd),
		  sizeof (struct internal_exec)) == 0) ? true : false;
}

extern reloc_howto_type aout_32_ext_howto_table[];

static reloc_howto_type *
DEFUN (sunos4_reloc_type_lookup, (abfd, code),
       bfd *abfd AND
       bfd_reloc_code_real_type code)
{
  switch (bfd_get_arch (abfd))
    {
    default:
      return 0;
    case bfd_arch_sparc:
      switch (code)
	{
	default:
	  return 0;
#define IDX(i,j)	case i: return &aout_32_ext_howto_table[j]
	  IDX (BFD_RELOC_CTOR, 2);
	  IDX (BFD_RELOC_32, 2);
	  IDX (BFD_RELOC_HI22, 8);
	  IDX (BFD_RELOC_LO10, 11);
	  IDX (BFD_RELOC_32_PCREL_S2, 6);
	}
    }
}

static boolean
DEFUN (sunos4_set_sizes, (abfd),
       bfd *abfd)
{
  switch (bfd_get_arch (abfd))
    {
    default:
      return false;
    case bfd_arch_sparc:
      adata(abfd).page_size = 0x2000;
      adata(abfd).segment_size = 0x2000;
      adata(abfd).exec_bytes_size = EXEC_BYTES_SIZE;
      return true;
    case bfd_arch_m68k:
      adata(abfd).page_size = 0x2000;
      adata(abfd).segment_size = 0x20000;
      adata(abfd).exec_bytes_size = EXEC_BYTES_SIZE;
      return true;
    }
}

static CONST struct aout_backend_data sunos4_aout_backend = {
  0, 1, 0, sunos4_set_sizes, 0,
};

#define	MY_core_file_failing_command 	sunos4_core_file_failing_command
#define	MY_core_file_failing_signal	sunos4_core_file_failing_signal
#define	MY_core_file_matches_executable_p sunos4_core_file_matches_executable_p

#define MY_bfd_debug_info_start		bfd_void
#define MY_bfd_debug_info_end		bfd_void
#define MY_bfd_debug_info_accumulate	(PROTO(void,(*),(bfd*, struct sec *))) bfd_void
#define MY_core_file_p sunos4_core_file_p
#define MY_write_object_contents NAME(aout,sunos4_write_object_contents)
#define MY_backend_data			&sunos4_aout_backend

#define TARGET_IS_BIG_ENDIAN_P

#include "aout-target.h"
