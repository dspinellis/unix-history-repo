/* Copyright (C) 1985 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


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
 * 
 * The boundaries within the a.out file may be adjusted with the data_start 
 * and bss_start arguments.  Either or both may be given as 0 for defaults.
 * 
 * Data_start gives the boundary between the text segment and the data
 * segment of the program.  The text segment can contain shared, read-only
 * program code and literal data, while the data segment is always unshared
 * and unprotected.  Data_start gives the lowest unprotected address.  Since
 * the granularity of write-protection is on 1k page boundaries on the VAX, a
 * given data_start value which is not on a page boundary is rounded down to
 * the beginning of the page it is on.  The default when 0 is given leaves the
 * number of protected pages the same as it was before.
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

#ifndef emacs
#define PERROR(arg) perror (arg); return -1
#else
#include "config.h"
#define PERROR(file) report_error (file, new)
#endif

#ifndef CANNOT_DUMP  /* all rest of file!  */

#include <sys/param.h>
#ifndef makedev			/* Try to detect types.h already loaded */
#include <sys/types.h>
#endif
#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>
  
extern char *start_of_text ();		/* Start of text */
extern char *start_of_data ();		/* Start of initialized data */
  
#ifdef COFF
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>
#include <syms.h>
static long block_copy_start;		/* Old executable start point */
static struct filehdr f_hdr;		/* File header */
static struct aouthdr f_ohdr;		/* Optional file header (a.out) */
long bias;			/* Bias to add for growth */
long lnnoptr;			/* Pointer to line-number info within file */
#define SYMS_START block_copy_start

static int text_scnptr;

#else /* not COFF */

extern char *sbrk ();

#include <a.out.h>
#define SYMS_START ((long) N_SYMOFF (ohdr))

#ifdef HPUX
#ifdef hp9000s200
#define MY_ID HP9000S200_ID
#else
#include <model.h>
#define MY_ID MYSYS
#endif /* not hp9000s200 */
static MAGIC OLDMAGIC = {MY_ID, SHARE_MAGIC};
static MAGIC NEWMAGIC = {MY_ID, DEMAND_MAGIC};
#define N_TXTOFF(x) TEXT_OFFSET(x)
#define N_SYMOFF(x) LESYM_OFFSET(x)
static struct exec hdr, ohdr;

#else /* not HPUX */

#ifdef USG
static struct bhdr hdr, ohdr;
#define a_magic fmagic
#define a_text tsize
#define a_data dsize
#define a_bss bsize
#define a_syms ssize
#define a_trsize rtsize
#define a_drsize rdsize
#define a_entry entry
#define	N_BADMAG(x) \
    (((x).fmagic)!=OMAGIC && ((x).fmagic)!=NMAGIC &&\
     ((x).fmagic)!=FMAGIC && ((x).fmagic)!=IMAGIC)
#define NEWMAGIC FMAGIC
#else /* not USG */
static struct exec hdr, ohdr;
#define NEWMAGIC ZMAGIC
#endif /* not USG */
#endif /* not HPUX */

#endif /* not COFF */

static int pagemask;

#if defined (BSD4_1) || defined (USG)
#ifdef EXEC_PAGESIZE
#define getpagesize() EXEC_PAGESIZE
#else
#ifdef NBPG
#define getpagesize() NBPG * CLSIZE
#ifndef CLSIZE
#define CLSIZE 1
#endif /* no CLSIZE */
#else /* no NBPG */
#define getpagesize() NBPC
#endif /* no NBPG */
#endif /* no EXEC_PAGESIZE */
#endif /* BSD4_1 or USG */

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
  if (make_hdr (new, a_out, data_start, bss_start, entry_address, a_name, new_name) < 0
      || copy_text_and_data (new) < 0
      || copy_sym (new, a_out, a_name, new_name) < 0
#ifdef COFF
      || adjust_lnnoptrs (new, a_out, new_name) < 0
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
  int tem;
#ifdef COFF
  auto struct scnhdr f_thdr;		/* Text section header */
  auto struct scnhdr f_dhdr;		/* Data section header */
  auto struct scnhdr f_bhdr;		/* Bss section header */
  auto struct scnhdr scntemp;		/* Temporary section header */
  register int scns;

  /* Salvage as much info from the existing file as possible */
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
      /* Loop through section headers, copying them in */
      for (scns = f_hdr.f_nscns; scns > 0; scns--) {
	if (read (a_out, &scntemp, sizeof (scntemp)) != sizeof (scntemp))
	  {
	    PERROR (a_name);
	  }
	block_copy_start += sizeof (scntemp);
	if (scntemp.s_scnptr > 0L)
	  {
	    block_copy_start += scntemp.s_size;
	  }
	if (strcmp (scntemp.s_name, ".text") == 0)
	  {
	    f_thdr = scntemp;
	  }
	else if (strcmp (scntemp.s_name, ".data") == 0)
	  {
	    f_dhdr = scntemp;
	  }
	else if (strcmp (scntemp.s_name, ".bss") == 0)
	  {
	    f_bhdr = scntemp;
	  }
      }	    
    }
  else
    {
      ERROR0 ("can't build a COFF file from scratch yet");
    }

  pagemask = getpagesize () - 1;

#ifdef NO_REMAP
  data_start = (int) start_of_data ();
#else /* not NO_REMAP */
  if (!data_start)
    data_start = (int) start_of_data ();
#endif /* not NO_REMAP */
  data_start = ADDR_CORRECT (data_start);
  data_start = data_start & ~pagemask; /* down to a page boundary */

  f_hdr.f_flags |= (F_RELFLG | F_EXEC);
#ifdef EXEC_MAGIC
  f_ohdr.magic = EXEC_MAGIC;
#endif
  f_ohdr.text_start = (long) start_of_text ();
  f_ohdr.tsize = data_start - f_ohdr.text_start;
  f_ohdr.data_start = data_start;
  f_ohdr.dsize = (long) sbrk (0) - f_ohdr.data_start;
  f_ohdr.bsize = 0;
  f_thdr.s_size = f_ohdr.tsize;
  f_thdr.s_scnptr = sizeof (f_hdr) + sizeof (f_ohdr);
  f_thdr.s_scnptr += (f_hdr.f_nscns) * (sizeof (f_thdr));
  lnnoptr = f_thdr.s_lnnoptr;
#ifdef UMAX
  /* Umax is bsd using coff; it has restrictions on alignment
     of the sections in the file itself.  */
  f_thdr.s_scnptr = (f_thdr.s_scnptr + pagemask) & ~pagemask;  /* round up */
#endif /* UMAX */
  text_scnptr = f_thdr.s_scnptr;
  f_dhdr.s_paddr = f_ohdr.data_start;
  f_dhdr.s_vaddr = f_ohdr.data_start;
  f_dhdr.s_size = f_ohdr.dsize;
  f_dhdr.s_scnptr = f_thdr.s_scnptr + f_thdr.s_size;
#ifdef UMAX
  f_dhdr.s_scnptr &= ~pagemask; /* round down to page boundary */
#endif /* UMAX */
  f_bhdr.s_paddr = f_ohdr.data_start + f_ohdr.dsize;
  f_bhdr.s_vaddr = f_ohdr.data_start + f_ohdr.dsize;
  f_bhdr.s_size = f_ohdr.bsize;
  f_bhdr.s_scnptr = 0L;
  bias = f_dhdr.s_scnptr + f_dhdr.s_size - block_copy_start;

  if (f_hdr.f_symptr > 0L)
    {
      f_hdr.f_symptr += bias;
    }

  if (f_thdr.s_lnnoptr > 0L) 
    {
      f_thdr.s_lnnoptr += bias;
    }

  if (write (new, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_thdr, sizeof (f_thdr)) != sizeof (f_thdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_dhdr, sizeof (f_dhdr)) != sizeof (f_dhdr))
    {
      PERROR (new_name);
    }

  if (write (new, &f_bhdr, sizeof (f_bhdr)) != sizeof (f_bhdr))
    {
      PERROR (new_name);
    }
  return (0);
    
#else /* if not COFF */

  /* Get symbol table info from header of a.out file if given one. */
  if (a_out >= 0)
    {
      if (read (a_out, &ohdr, sizeof hdr) != sizeof hdr)
	{
	  PERROR (a_name);
	}

      if N_BADMAG (ohdr)
	{
	  ERROR1 ("invalid magic number in %s", a_name);
	}
#ifdef celerity
      hdr.a_scovrfl = ohdr.a_scovrfl;
#endif
#ifdef HPUX
      hdr.a_lesyms = ohdr.a_lesyms;
      hdr.a_sltsize = ohdr.a_sltsize;
      hdr.a_dnttsize = ohdr.a_dnttsize;
      hdr.a_vtsize = ohdr.a_vtsize;
#else /* not HPUX */
      hdr.a_syms = ohdr.a_syms;
#endif /* not HPUX */
    }
  else
    {
#ifdef celerity
      hdr.a_scovrfl = 0;
#endif
#ifdef HPUX
      hdr.a_lesyms = 0;
      hdr.a_sltsize = 0;
      hdr.a_dnttsize = 0;
      hdr.a_vtsize = 0;
#else /* not HPUX */
      hdr.a_syms = 0;			/* No a.out, so no symbol info. */
#endif /* not HPUX */
    }

  /* Construct header from user structure. */
#ifdef HPUX
  /* (((MAGIC) ohdr.a_magic) == ((MAGIC) OLDMAGIC)) This does not work */
  hdr.a_magic = ((ohdr.a_magic.file_type == OLDMAGIC.file_type) ?
		 NEWMAGIC : ohdr.a_magic);
#else /* not HPUX */
/* hdr.a_magic = NEWMAGIC; */
  hdr.a_magic = ohdr.a_magic;
#endif /* not HPUX */

#ifdef sun3
  hdr.a_machtype = ohdr.a_machtype;
#endif /* sun3 */
  hdr.a_trsize = 0;
  hdr.a_drsize = 0;
  hdr.a_entry = entry_address;

  pagemask = getpagesize () - 1;

  /* Adjust data/bss boundary. */
  if (bss_start != 0)
    {
      bss_start = (ADDR_CORRECT (bss_start) + pagemask) & ~pagemask;	      /* (Up) to page bdry. */
      if (bss_start > ADDR_CORRECT (sbrk (0)))
	{
	  ERROR1 ("unexec: Specified bss_start (%u) is past end of program",
		  bss_start);
	}
    }
  else
    {
      bss_start = ADDR_CORRECT (sbrk (0));
      bss_start = (bss_start + pagemask) & ~pagemask;
    }

  /* Adjust text/data boundary. */
#ifdef NO_REMAP
  data_start = (int) start_of_data ();
#else /* not NO_REMAP */
  if (!data_start)
    data_start = (int) start_of_data ();
#endif /* not NO_REMAP */
  data_start = ADDR_CORRECT (data_start);

#ifdef sun
  data_start = data_start & ~(SEGSIZ - 1); /* (Down) to segment boundary. */
#else
  data_start = data_start & ~pagemask; /* (Down) to page boundary. */
#endif

  if (data_start > bss_start)	/* Can't have negative data size. */
    {
      ERROR2 ("unexec: data_start (%u) can't be greater than bss_start (%u)",
	      data_start, bss_start);
    }

  tem = ADDR_CORRECT (sbrk (0));
  hdr.a_bss = tem - bss_start;
  if (tem < bss_start)		/* Note a_bss is unsigned on some systems */
    hdr.a_bss = 0;
  hdr.a_data = bss_start - data_start;
#if defined(sequent)
  hdr.a_text = data_start - (long) start_of_text () + sizeof(hdr) + N_ADDRADJ(ohdr);
#else
  hdr.a_text = data_start - (long) start_of_text ();
#endif /* not sequent */

  if (write (new, &hdr, sizeof hdr) != sizeof hdr)
    {
      PERROR (new_name);
    }
  return 0;

#endif /* not COFF */
}

/* ****************************************************************
 * copy_text_and_data
 *
 * Copy the text and data segments from memory to the new a.out
 */
static int
copy_text_and_data (new)
     int new;
{
  register int nwrite, ret;
  register char *end;
  int i;
  register char *ptr;
  char buf[80];
  extern int errno;
  
#ifdef COFF
  lseek (new, (long) text_scnptr, 0);
  ptr = (char *) f_ohdr.text_start;
  end = ptr + f_ohdr.tsize + f_ohdr.dsize;
  while (ptr < end)
    {
      nwrite = 128;
      if (nwrite > end - ptr) nwrite = end - ptr;
      ret = write (new, ptr, nwrite);
      if (nwrite != ret)
	{
	  sprintf (buf,
		   "unexec write failure: addr 0x%x, fileno %d, size 0x%x, wrote 0x%x, errno %d",
		   ptr, new, nwrite, ret, errno);
	  PERROR (buf);
	}
      ptr += nwrite;
    }
  return (0);

#else /* if not COFF */

#if defined(sun3) || defined(sequent)
  lseek (new, (long) (N_TXTOFF (hdr) + sizeof (hdr)), 0);
#else
  lseek (new, (long) N_TXTOFF (hdr), 0);
#endif

  ptr = start_of_text ();
  end = ptr + hdr.a_text + hdr.a_data;
#if defined(sequent)
  end -= (sizeof(hdr) + N_ADDRADJ(hdr));
#endif
  for (i = 0; ptr < end;)
    {
      nwrite = 128;
      if (nwrite > end - ptr) nwrite = end - ptr;
      ret = write (new, ptr, nwrite);
      if (ret == -1 && errno == EFAULT)
	{
          /* BZS - again, see above about N_TXTOFF on a SUN */
#if defined(sun3) || defined(sequent)
	  lseek (new, (long) (N_TXTOFF (hdr) + i + nwrite + sizeof (hdr)), 0);
#else
	  lseek (new, (long) (N_TXTOFF (hdr) + i + nwrite), 0);
#endif
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

  return 0;
#endif /* not COFF */
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

#ifdef COFF
  if (SYMS_START == 0L)
    return 0;
#endif  /* COFF */

#ifdef sun3
  /* BZS - I might be covering a sin with this */
  lseek (new, N_SYMOFF (hdr), 0);
#else
#ifdef COFF
  if (lnnoptr)			/* if there is line number info */
    lseek (a_out, lnnoptr, 0);	/* start copying from there */
  else
#endif /* COFF */
    lseek (a_out, SYMS_START, 0);	/* Position a.out to symtab. */
#endif
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
  struct syment symentry;
  struct auxent auxentry;

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

#endif /* not CANNOT_DUMP */
