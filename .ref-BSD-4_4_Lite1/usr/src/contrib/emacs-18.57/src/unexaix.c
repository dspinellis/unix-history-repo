/* Modified by Andrew.Vignaux@comp.vuw.ac.nz to get it to work :-) */

/* Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */


/*
 * unexec.c - Convert a running program into an a.out file.
 *
 * Author:	Spencer W. Thomas
 * 		Computer Science Dept.
 * 		University of Utah
 * Date:	Tue Mar  2 1982
 * Modified heavily since then.
 *
 * Synopsis:
 *	unexec (new_name, a_name, data_start, bss_start, entry_address)
 *	char *new_name, *a_name;
 *	unsigned data_start, bss_start, entry_address;
 *
 * Takes a snapshot of the program and makes an a.out format file in the
 * file named by the string argument new_name.
 * If a_name is non-NULL, the symbol table will be taken from the given file.
 * On some machines, an existing a_name file is required.
 *
 * The boundaries within the a.out file may be adjusted with the data_start
 * and bss_start arguments.  Either or both may be given as 0 for defaults.
 *
 * Data_start gives the boundary between the text segment and the data
 * segment of the program.  The text segment can contain shared, read-only
 * program code and literal data, while the data segment is always unshared
 * and unprotected.  Data_start gives the lowest unprotected address.
 * The value you specify may be rounded down to a suitable boundary
 * as required by the machine you are using.
 *
 * Specifying zero for data_start means the boundary between text and data
 * should not be the same as when the program was loaded.
 * If NO_REMAP is defined, the argument data_start is ignored and the
 * segment boundaries are never changed.
 *
 * Bss_start indicates how much of the data segment is to be saved in the
 * a.out file and restored when the program is executed.  It gives the lowest
 * unsaved address, and is rounded up to a page boundary.  The default when 0
 * is given assumes that the entire data segment is to be stored, including
 * the previous data and bss as well as any additional storage allocated with
 * break (2).
 *
 * The new file is set up to start at entry_address.
 *
 * If you make improvements I'd like to get them too.
 * harpo!utah-cs!thomas, thomas@Utah-20
 *
 */

/* There are several compilation parameters affecting unexec:

* COFF

Define this if your system uses COFF for executables.
Otherwise we assume you use Berkeley format.

* NO_REMAP

Define this if you do not want to try to save Emacs's pure data areas
as part of the text segment.

Saving them as text is good because it allows users to share more.

However, on machines that locate the text area far from the data area,
the boundary cannot feasibly be moved.  Such machines require
NO_REMAP.

Also, remapping can cause trouble with the built-in startup routine
/lib/crt0.o, which defines `environ' as an initialized variable.
Dumping `environ' as pure does not work!  So, to use remapping,
you must write a startup routine for your machine in Emacs's crt0.c.
If NO_REMAP is defined, Emacs uses the system's crt0.o.

* SECTION_ALIGNMENT

Some machines that use COFF executables require that each section
start on a certain boundary *in the COFF file*.  Such machines should
define SECTION_ALIGNMENT to a mask of the low-order bits that must be
zero on such a boundary.  This mask is used to control padding between
segments in the COFF file.

If SECTION_ALIGNMENT is not defined, the segments are written
consecutively with no attempt at alignment.  This is right for
unmodified system V.

* SEGMENT_MASK

Some machines require that the beginnings and ends of segments
*in core* be on certain boundaries.  For most machines, a page
boundary is sufficient.  That is the default.  When a larger
boundary is needed, define SEGMENT_MASK to a mask of
the bits that must be zero on such a boundary.

* A_TEXT_OFFSET(HDR)

Some machines count the a.out header as part of the size of the text
segment (a_text); they may actually load the header into core as the
first data in the text segment.  Some have additional padding between
the header and the real text of the program that is counted in a_text.

For these machines, define A_TEXT_OFFSET(HDR) to examine the header
structure HDR and return the number of bytes to add to `a_text'
before writing it (above and beyond the number of bytes of actual
program text).  HDR's standard fields are already correct, except that
this adjustment to the `a_text' field has not yet been made;
thus, the amount of offset can depend on the data in the file.
  
* A_TEXT_SEEK(HDR)

If defined, this macro specifies the number of bytes to seek into the
a.out file before starting to write the text segment.a

* EXEC_MAGIC

For machines using COFF, this macro, if defined, is a value stored
into the magic number field of the output file.

* ADJUST_EXEC_HEADER

This macro can be used to generate statements to adjust or
initialize nonstandard fields in the file header

* ADDR_CORRECT(ADDR)

Macro to correct an int which is the bit pattern of a pointer to a byte
into an int which is the number of a byte.

This macro has a default definition which is usually right.
This default definition is a no-op on most machines (where a
pointer looks like an int) but not on all machines.

*/

#define XCOFF
#define COFF
#define NO_REMAP

#ifndef emacs
#define PERROR(arg) perror (arg); return -1
#else
#include "config.h"
#define PERROR(file) report_error (file, new)
#endif

#ifndef CANNOT_DUMP  /* all rest of file!  */

#ifndef CANNOT_UNEXEC /* most of rest of file */

#include <a.out.h>
/* Define getpagesize () if the system does not.
   Note that this may depend on symbols defined in a.out.h
 */
#include "getpagesize.h"

#ifndef makedev			/* Try to detect types.h already loaded */
#include <sys/types.h>
#endif
#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>

extern char *start_of_text ();		/* Start of text */
extern char *start_of_data ();		/* Start of initialized data */

extern int _data;
extern int _edata;
extern int _text;
extern int _etext;
extern int _end;
#ifdef COFF
#ifndef USG
#ifndef STRIDE
#ifndef UMAX
#ifndef sun386
/* I have a suspicion that these are turned off on all systems
   and can be deleted.  Try it in version 19.  */
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <syms.h>
#endif /* not sun386 */
#endif /* not UMAX */
#endif /* Not STRIDE */
#endif /* not USG */
static long block_copy_start;		/* Old executable start point */
static struct filehdr f_hdr;		/* File header */
static struct aouthdr f_ohdr;		/* Optional file header (a.out) */
long bias;			/* Bias to add for growth */
long lnnoptr;			/* Pointer to line-number info within file */
#define SYMS_START block_copy_start

static long text_scnptr;
static long data_scnptr;
#ifdef XCOFF
static long load_scnptr;
static long orig_load_scnptr;
static long orig_data_scnptr;
#endif
static long data_st;

#ifndef MAX_SECTIONS
#define MAX_SECTIONS	10
#endif

#endif /* COFF */

static int pagemask;

/* Correct an int which is the bit pattern of a pointer to a byte
   into an int which is the number of a byte.
   This is a no-op on ordinary machines, but not on all.  */

#ifndef ADDR_CORRECT   /* Let m-*.h files override this definition */
#define ADDR_CORRECT(x) ((char *)(x) - (char*)0)
#endif

#ifdef emacs

static
report_error (file, fd)
     char *file;
     int fd;
{
  if (fd)
    close (fd);
  error ("Failure operating on %s", file);
}
#endif /* emacs */

#define ERROR0(msg) report_error_1 (new, msg, 0, 0); return -1
#define ERROR1(msg,x) report_error_1 (new, msg, x, 0); return -1
#define ERROR2(msg,x,y) report_error_1 (new, msg, x, y); return -1

static
report_error_1 (fd, msg, a1, a2)
     int fd;
     char *msg;
     int a1, a2;
{
  close (fd);
#ifdef emacs
  error (msg, a1, a2);
#else
  fprintf (stderr, msg, a1, a2);
  fprintf (stderr, "\n");
#endif
}

/* ****************************************************************
 * unexec
 *
 * driving logic.
 */
unexec (new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned data_start, bss_start, entry_address;
{
  int new, a_out = -1;

  if (a_name && (a_out = open (a_name, 0)) < 0)
    {
      PERROR (a_name);
    }
  if ((new = creat (new_name, 0666)) < 0)
    {
      PERROR (new_name);
    }
  if (make_hdr (new,a_out,data_start,bss_start,entry_address,a_name,new_name) < 0
      || copy_text_and_data (new) < 0
      || copy_sym (new, a_out, a_name, new_name) < 0
#ifdef COFF; 
      || adjust_lnnoptrs (new, a_out, new_name) < 0
#endif
#ifdef XCOFF; 
      || unrelocate_symbols (new, a_out, a_name, new_name) < 0
#endif
      )
    {
      close (new);
      /* unlink (new_name);	    	/* Failed, unlink new a.out */
      return -1;	
    }

  close (new);
  if (a_out >= 0)
    close (a_out);
  mark_x (new_name);
  return 0;
}

/* ****************************************************************
 * make_hdr
 *
 * Make the header in the new a.out from the header in core.
 * Modify the text and data sizes.
 */
static int
make_hdr (new, a_out, data_start, bss_start, entry_address, a_name, new_name)
     int new, a_out;
     unsigned data_start, bss_start, entry_address;
     char *a_name;
     char *new_name;
{
  register int scns;
  unsigned int bss_end;

  struct scnhdr section[MAX_SECTIONS];
  struct scnhdr * f_thdr;		/* Text section header */
  struct scnhdr * f_dhdr;		/* Data section header */
  struct scnhdr * f_bhdr;		/* Bss section header */
  struct scnhdr * f_lhdr;		/* Loader section header */
  struct scnhdr * f_tchdr;		/* Typechk section header */
  struct scnhdr * f_dbhdr;		/* Debug section header */
  struct scnhdr * f_xhdr;		/* Except section header */

  load_scnptr = orig_load_scnptr = lnnoptr = 0;
  pagemask = getpagesize () - 1;

  /* Adjust text/data boundary. */
#ifdef NO_REMAP
  data_start = (long) start_of_data ();
#endif /*  NO_REMAP */
  data_start = ADDR_CORRECT (data_start);

#ifdef SEGMENT_MASK
  data_start = data_start & ~SEGMENT_MASK; /* (Down) to segment boundary. */
#else
  data_start = data_start & ~pagemask; /* (Down) to page boundary. */
#endif


  bss_end = ADDR_CORRECT (sbrk (0)) + pagemask;
  bss_end &= ~ pagemask;
  /* Adjust data/bss boundary. */
  if (bss_start != 0)
    {
      bss_start = (ADDR_CORRECT (bss_start) + pagemask);
      /* (Up) to page bdry. */
      bss_start &= ~ pagemask;
      if (bss_start > bss_end)
	{
	  ERROR1 ("unexec: Specified bss_start (%u) is past end of program",
		  bss_start);
	}
    }
  else
    bss_start = bss_end;

  if (data_start > bss_start)	/* Can't have negative data size. */
    {
      ERROR2 ("unexec: data_start (%u) can't be greater than bss_start (%u)",
	      data_start, bss_start);
    }

#ifdef COFF
  /* Salvage as much info from the existing file as possible */
  block_copy_start = 0;
  f_thdr = NULL; f_dhdr = NULL; f_bhdr = NULL;
  f_lhdr = NULL; f_tchdr = NULL; f_dbhdr = NULL; f_xhdr = NULL;
  if (a_out >= 0)
    {
      if (read (a_out, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
	{
	  PERROR (a_name);
	}
      block_copy_start += sizeof (f_hdr);
      if (f_hdr.f_opthdr > 0)
	{
	  if (read (a_out, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr))
	    {
	      PERROR (a_name);
	    }
	  block_copy_start += sizeof (f_ohdr);
	}
      if (f_hdr.f_nscns > MAX_SECTIONS)
	{
	  ERROR0 ("unexec: too many section headers -- increase MAX_SECTIONS");
	}
      /* Loop through section headers */
      for (scns = 0; scns < f_hdr.f_nscns; scns++) {
	struct scnhdr *s = &section[scns];
	if (read (a_out, s, sizeof (*s)) != sizeof (*s))
	  {
	    PERROR (a_name);
	  }
	if (s->s_scnptr > 0L)
	  {
            if (block_copy_start < s->s_scnptr + s->s_size)
	      block_copy_start = s->s_scnptr + s->s_size;
	  }

#define CHECK_SCNHDR(ptr, name, flags) \
  if (strcmp(s->s_name, name) == 0) { \
    if (s->s_flags != flags) { \
      fprintf(stderr, "unexec: %x flags where %x expected in %s section.\n", \
	      s->s_flags, flags, name); \
    } \
    if (ptr) { \
      fprintf(stderr, "unexec: duplicate section header for section %s.\n", \
	      name); \
    } \
    ptr = s; \
  }
	CHECK_SCNHDR(f_thdr, _TEXT, STYP_TEXT);
	CHECK_SCNHDR(f_dhdr, _DATA, STYP_DATA);
	CHECK_SCNHDR(f_bhdr, _BSS, STYP_BSS);
	CHECK_SCNHDR(f_lhdr, _LOADER, STYP_LOADER);
	CHECK_SCNHDR(f_dbhdr, _DEBUG,  STYP_DEBUG);
	CHECK_SCNHDR(f_tchdr, _TYPCHK,  STYP_TYPCHK);
	CHECK_SCNHDR(f_xhdr, _EXCEPT,  STYP_EXCEPT);
      }

      if (f_thdr == 0)
	{
	  ERROR1 ("unexec: couldn't find \"%s\" section", _TEXT);
	}
      if (f_dhdr == 0)
	{
	  ERROR1 ("unexec: couldn't find \"%s\" section", _DATA);
	}
      if (f_bhdr == 0)
	{
	  ERROR1 ("unexec: couldn't find \"%s\" section", _BSS);
	}
    }
  else
    {
      ERROR0 ("can't build a COFF file from scratch yet");
    }
  orig_data_scnptr = f_dhdr->s_scnptr;
  orig_load_scnptr = f_lhdr ? f_lhdr->s_scnptr : 0;

  /* Now we alter the contents of all the f_*hdr variables
     to correspond to what we want to dump.  */
  f_hdr.f_flags |= (F_RELFLG | F_EXEC);		/* Why? */
#ifdef EXEC_MAGIC
  f_ohdr.magic = EXEC_MAGIC;
#endif
#ifndef NO_REMAP
  f_ohdr.tsize = data_start - f_ohdr.text_start;
  f_ohdr.text_start = (long) start_of_text ();
#endif
  f_ohdr.dsize = bss_start - ((unsigned) &_data);
  f_ohdr.bsize = bss_end - bss_start;

  f_dhdr->s_size = f_ohdr.dsize;
  f_bhdr->s_size = f_ohdr.bsize;
  f_bhdr->s_paddr = f_ohdr.dsize;
  f_bhdr->s_vaddr = f_ohdr.dsize;

  /* fix scnptr's */
  {
    long ptr;

    for (scns = 0; scns < f_hdr.f_nscns; scns++) {
      struct scnhdr *s = &section[scns];
      if (scns == 0)
	ptr = s->s_scnptr;

      if (s->s_scnptr != 0)
	{
	  s->s_scnptr = ptr;
	}

      if ((s->s_flags & 0xffff) == STYP_PAD)
	{
	  /*
	   * the text_start should probably be o_algntext but that doesn't
	   * seem to change
	   */
	  if (f_ohdr.text_start != 0) /* && scns != 0 */
	    {
	      s->s_size = 512 - (s->s_scnptr % 512);
	      if (s->s_size == 512)
		s->s_size = 0;
	    }
	}

      ptr = ptr + s->s_size;
    }

    bias = ptr - block_copy_start;
  }

  /* fix other pointers */
  for (scns = 0; scns < f_hdr.f_nscns; scns++) {
    struct scnhdr *s = &section[scns];

    if (s->s_relptr != 0)
      {
	s->s_relptr += bias;
      }
    if (s->s_lnnoptr != 0)
      {
	if (lnnoptr == 0) lnnoptr = s->s_lnnoptr;
	s->s_lnnoptr += bias;
      }
  }

  if (f_hdr.f_symptr > 0L)
    {
      f_hdr.f_symptr += bias;
    }

  data_st = data_start;
  text_scnptr = f_thdr->s_scnptr;
  data_scnptr = f_dhdr->s_scnptr;
  load_scnptr = f_lhdr ? f_lhdr->s_scnptr : 0;
  block_copy_start = orig_load_scnptr;

#ifdef ADJUST_EXEC_HEADER
  ADJUST_EXEC_HEADER
#endif /* ADJUST_EXEC_HEADER */

  if (write (new, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
    {
      PERROR (new_name);
    }

  if (f_hdr.f_opthdr > 0)
    {
      if (write (new, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr))
	{
	  PERROR (new_name);
	}
    }

  for (scns = 0; scns < f_hdr.f_nscns; scns++) {
    struct scnhdr *s = &section[scns];
    if (write (new, s, sizeof (*s)) != sizeof (*s))
      {
	PERROR (new_name);
      }
  }

  return (0);

#endif /* COFF */
}

/* ****************************************************************
 
 *
 * Copy the text and data segments from memory to the new a.out
 */
static int
copy_text_and_data (new)
     int new;
{
  register char *end;
  register char *ptr;

  lseek (new, (long) text_scnptr, 0);
  ptr = start_of_text () + text_scnptr;
  end = ptr + f_ohdr.tsize;
  write_segment (new, ptr, end);

  lseek (new, (long) data_scnptr, 0);
  ptr = (char *) &_data;
  end = ptr + f_ohdr.dsize;
  write_segment (new, ptr, end);

  return 0;
}

write_segment (new, ptr, end)
     int new;
     register char *ptr, *end;
{
  register int i, nwrite, ret;
  char buf[80];
  extern int errno;
  char zeros[128];

  bzero (zeros, sizeof zeros);

  for (i = 0; ptr < end;)
    {
      /* distance to next multiple of 128.  */
      nwrite = (((int) ptr + 128) & -128) - (int) ptr;
      /* But not beyond specified end.  */
      if (nwrite > end - ptr) nwrite = end - ptr;
      ret = write (new, ptr, nwrite);
      /* If write gets a page fault, it means we reached
	 a gap between the old text segment and the old data segment.
	 This gap has probably been remapped into part of the text segment.
	 So write zeros for it.  */
      if (ret == -1 && errno == EFAULT)
	{
	write (new, zeros, nwrite);
	}
      else if (nwrite != ret)
	{
	  sprintf (buf,
		   "unexec write failure: addr 0x%x, fileno %d, size 0x%x, wrote 0x%x, errno %d",
		   ptr, new, nwrite, ret, errno);
	  PERROR (buf);
	}
      i += nwrite;
      ptr += nwrite;
    }
}

/* ****************************************************************
 * copy_sym
 *
 * Copy the relocation information and symbol table from the a.out to the new
 */
static int
copy_sym (new, a_out, a_name, new_name)
     int new, a_out;
     char *a_name, *new_name;
{
  char page[1024];
  int n;

  if (a_out < 0)
    return 0;

  if (SYMS_START == 0L)
    return 0;

  if (lnnoptr && lnnoptr < SYMS_START)	/* if there is line number info */
    lseek (a_out, lnnoptr, 0);		/* start copying from there */
  else
    lseek (a_out, SYMS_START, 0);	/* Position a.out to symtab. */

  while ((n = read (a_out, page, sizeof page)) > 0)
    {
      if (write (new, page, n) != n)
	{
	  PERROR (new_name);
	}
    }
  if (n < 0)
    {
      PERROR (a_name);
    }
  return 0;
}

/* ****************************************************************
 * mark_x
 *
 * After succesfully building the new a.out, mark it executable
 */
static
mark_x (name)
     char *name;
{
  struct stat sbuf;
  int um;
  int new = 0;  /* for PERROR */

  um = umask (777);
  umask (um);
  if (stat (name, &sbuf) == -1)
    {
      PERROR (name);
    }
  sbuf.st_mode |= 0111 & ~um;
  if (chmod (name, sbuf.st_mode) == -1)
    PERROR (name);
}

/*
 *	If the COFF file contains a symbol table and a line number section,
 *	then any auxiliary entries that have values for x_lnnoptr must
 *	be adjusted by the amount that the line number section has moved
 *	in the file (bias computed in make_hdr).  The #@$%&* designers of
 *	the auxiliary entry structures used the absolute file offsets for
 *	the line number entry rather than an offset from the start of the
 *	line number section!
 *
 *	When I figure out how to scan through the symbol table and pick out
 *	the auxiliary entries that need adjustment, this routine will
 *	be fixed.  As it is now, all such entries are wrong and sdb
 *	will complain.   Fred Fish, UniSoft Systems Inc.
 */

#ifdef COFF

/* This function is probably very slow.  Instead of reopening the new
   file for input and output it should copy from the old to the new
   using the two descriptors already open (WRITEDESC and READDESC).
   Instead of reading one small structure at a time it should use
   a reasonable size buffer.  But I don't have time to work on such
   things, so I am installing it as submitted to me.  -- RMS.  */

adjust_lnnoptrs (writedesc, readdesc, new_name)
     int writedesc;
     int readdesc;
     char *new_name;
{
  register int nsyms;
  register int new;
#ifdef amdahl_uts
  SYMENT symentry;
  AUXENT auxentry;
#else
  struct syment symentry;
  union auxent auxentry;
#endif

  if (!lnnoptr || !f_hdr.f_symptr)
    return 0;

  if ((new = open (new_name, 2)) < 0)
    {
      PERROR (new_name);
      return -1;
    }

  lseek (new, f_hdr.f_symptr, 0);
  for (nsyms = 0; nsyms < f_hdr.f_nsyms; nsyms++)
    {
      read (new, &symentry, SYMESZ);
      if (symentry.n_numaux)
	{
	  read (new, &auxentry, AUXESZ);
	  nsyms++;
	  if (ISFCN (symentry.n_type)) {
	    auxentry.x_sym.x_fcnary.x_fcn.x_lnnoptr += bias;
	    lseek (new, -AUXESZ, 1);
	    write (new, &auxentry, AUXESZ);
	  }
	}
    }
  close (new);
}

#endif /* COFF */

#ifdef XCOFF

/* It is probably a false economy to optimise this routine (it used to
   read one LDREL and do do two lseeks per iteration) but the wrath of
   RMS (see above :-) would be too much to bear */

unrelocate_symbols (new, a_out, a_name, new_name)
     int new, a_out;
     char *a_name, *new_name;
{
  register int i;
  register int l;
  register LDREL *ldrel;
  LDHDR ldhdr;
  LDREL ldrel_buf [20];
  ulong t_start = &_text;
  ulong d_start = &_data;
  int * p;
  int dirty;

  if (load_scnptr == 0)
    return 0;

  lseek (a_out, orig_load_scnptr, 0);
  if (read (a_out, &ldhdr, sizeof (ldhdr)) != sizeof (ldhdr))
    {
      PERROR (new_name);
    }

#define SYMNDX_TEXT	0
#define SYMNDX_DATA	1
#define SYMNDX_BSS	2
  l = 0;
  for (i = 0; i < ldhdr.l_nreloc; i++, l--, ldrel++)
    {
      if (l == 0) {
	lseek (a_out,
	       orig_load_scnptr + LDHDRSZ + LDSYMSZ*ldhdr.l_nsyms + LDRELSZ*i,
	       0);

	l = ldhdr.l_nreloc - i;
	if (l > sizeof (ldrel_buf) / LDRELSZ)
	  l = sizeof (ldrel_buf) / LDRELSZ;

	if (read (a_out, ldrel_buf, l * LDRELSZ) != l * LDRELSZ)
	  {
	    PERROR (a_name);
	  }
	ldrel = ldrel_buf;
      }
      dirty = 0;

      /* this code may not be necessary */
      /* I originally had == in the "assignment" and it still unrelocated */

      /* move the BSS loader symbols to the DATA segment */
      if (ldrel->l_rsecnm == f_ohdr.o_snbss)
	ldrel->l_rsecnm = f_ohdr.o_sndata, dirty++;

      if (ldrel->l_symndx == SYMNDX_BSS)
	ldrel->l_symndx = SYMNDX_DATA, dirty++;

      if (dirty)
	{
	  lseek (new,
		 load_scnptr + LDHDRSZ + LDSYMSZ*ldhdr.l_nsyms + LDRELSZ*i,
		 0);

	  if (write (new, ldrel, LDRELSZ) != LDRELSZ)
	    {
	      PERROR (new_name);
	    }
	}

      if (ldrel->l_rsecnm == f_ohdr.o_sndata)
	{
	  int orig_int;

	  lseek (a_out, orig_data_scnptr + ldrel->l_vaddr, 0);

	  if (read (a_out, (void *) &orig_int, sizeof (orig_int)) != sizeof (orig_int))
	    {
	      PERROR (a_name);
	    }

	  switch (ldrel->l_symndx) {
	  case SYMNDX_TEXT:
	    p = (int *) (d_start + ldrel->l_vaddr);
	    orig_int = * p - (t_start - f_ohdr.text_start);
	    break;

	  case SYMNDX_DATA:
	  case SYMNDX_BSS:
	    p = (int *) (d_start + ldrel->l_vaddr);
	    orig_int = * p - (d_start - f_ohdr.data_start);
	    break;
	  }

	  lseek (new, data_scnptr + ldrel->l_vaddr, 0);
	  if (write (new, (void *) &orig_int, sizeof (orig_int)) != sizeof (orig_int))
	    {
	      PERROR (new_name);
	    }
	}
    }
}
#endif /* XCOFF */

#endif /* not CANNOT_UNEXEC */

#endif /* not CANNOT_DUMP */
