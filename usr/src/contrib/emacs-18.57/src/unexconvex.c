/* Modified version of unexec for convex machines.
   Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.

   Note that the GNU project considers support for the peculiarities
   of the Convex operating system a peripheral activity which should
   not be allowed to divert effort from development of the GNU system.
   Changes in this code will be installed when Convex system
   maintainers send them in, but aside from that we don't plan to
   think about it, or about whether other Emacs maintenance might
   break it.

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


/* modifyed for C-1 arch by jthomp@convex 871103 */
/* Corrected to support convex SOFF object file formats and thread specific
 * regions.  streepy@convex 890302
*/

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

#include "config.h"
#define PERROR(file) report_error (file, new)

#include <a.out.h>
/* Define getpagesize () if the system does not.
   Note that this may depend on symbols defined in a.out.h
 */
#include "getpagesize.h"

#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <errno.h>

extern char *start_of_text ();		/* Start of text */
extern char *start_of_data ();		/* Start of initialized data */

#include <machine/filehdr.h>
#include <machine/opthdr.h>
#include <machine/scnhdr.h>
#include <machine/pte.h>

static long block_copy_start;	/* Old executable start point */
static struct filehdr f_hdr;	/* File header */
static struct opthdr f_ohdr;	/* Optional file header (a.out) */
long bias;			/* Bias to add for growth */
#define SYMS_START block_copy_start

static long text_scnptr;
static long data_scnptr;

static int pagemask;
static int pagesz;

static
report_error (file, fd)
     char *file;
     int fd;
{
    if (fd)
	close (fd);
    error ("Failure operating on %s", file);
}

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
    error (msg, a1, a2);
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

    if (a_name && (a_out = open (a_name, 0)) < 0) {
	PERROR (a_name);
    }
    if ((new = creat (new_name, 0666)) < 0) {
	PERROR (new_name);
    }

    if (make_hdr (new, a_out, data_start, bss_start, entry_address, a_name, new_name) < 0
      || copy_text_and_data (new) < 0
      || copy_sym (new, a_out, a_name, new_name) < 0 ) {
	close (new);
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

 struct scnhdr *stbl;		/* Table of all scnhdr's */
 struct scnhdr *f_thdr;		/* Text section header */
 struct scnhdr *f_dhdr;		/* Data section header */
 struct scnhdr *f_tdhdr;	/* Thread Data section header */
 struct scnhdr *f_bhdr;		/* Bss section header */
 struct scnhdr *f_tbhdr;	/* Thread Bss section header */

static int
make_hdr (new, a_out, data_start, bss_start, entry_address, a_name, new_name)
     int new, a_out;
     unsigned data_start, bss_start, entry_address;
     char *a_name;
     char *new_name;
{
    register int scns;
    unsigned int bss_end;
    unsigned int eo_data;	/* End of initialized data in new exec file */
    int scntype;		/* Section type */
    int i;			/* Var for sorting by vaddr */
    struct scnhdr scntemp;	/* For swapping entries in sort */
    extern char *start_of_data();

    pagemask = (pagesz = getpagesize()) - 1;

    /* Adjust text/data boundary. */
    if (!data_start)
	data_start = (unsigned) start_of_data ();

    data_start = data_start & ~pagemask; /* (Down) to page boundary. */

    bss_end = (sbrk(0) + pagemask) & ~pagemask;

    /* Adjust data/bss boundary. */
    if (bss_start != 0) {
	bss_start = (bss_start + pagemask) & ~pagemask;/* (Up) to page bdry. */
	if (bss_start > bss_end) {
	    ERROR1 ("unexec: Specified bss_start (%x) is past end of program",
		    bss_start);
	}
    } else
	bss_start = bss_end;

    if (data_start > bss_start)	{ /* Can't have negative data size. */
	ERROR2 ("unexec: data_start (%x) can't be greater than bss_start (%x)",
		data_start, bss_start);
    }

    /* Salvage as much info from the existing file as possible */
    if (a_out < 0) {
	ERROR0 ("can't build a COFF file from scratch yet");
	/*NOTREACHED*/
    }

    if (read (a_out, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr)) {
	PERROR (a_name);
    }
    block_copy_start += sizeof (f_hdr);
    if (f_hdr.h_opthdr > 0) {
	if (read (a_out, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr)) {
	    PERROR (a_name);
	}
	block_copy_start += sizeof (f_ohdr);
    }

    /* Allocate room for scn headers */
    stbl = (struct scnhdr *)malloc( sizeof(struct scnhdr) * f_hdr.h_nscns );
    if( stbl == NULL ) {
	ERROR0( "unexec: malloc of stbl failed" );
    }

    f_tdhdr = f_tbhdr = NULL;

    /* Loop through section headers, copying them in */
    for (scns = 0; scns < f_hdr.h_nscns; scns++) {

	if( read( a_out, &stbl[scns], sizeof(*stbl)) != sizeof(*stbl)) {
	    PERROR (a_name);
	}

	scntype = stbl[scns].s_flags & S_TYPMASK; /* What type of section */

	if( stbl[scns].s_scnptr > 0L) {
	    if( block_copy_start < stbl[scns].s_scnptr + stbl[scns].s_size )
		block_copy_start = stbl[scns].s_scnptr + stbl[scns].s_size;
	}

	if( scntype == S_TEXT) {
	    f_thdr = &stbl[scns];
	} else if( scntype == S_DATA) {
	    f_dhdr = &stbl[scns];
#ifdef S_TDATA
	} else if( scntype == S_TDATA ) {
	    f_tdhdr = &stbl[scns];
	} else if( scntype == S_TBSS ) {
	    f_tbhdr = &stbl[scns];
#endif /* S_TDATA (thread stuff) */

	} else if( scntype == S_BSS) {
	    f_bhdr = &stbl[scns];
	}

    }

    /* We will now convert TEXT and DATA into TEXT, BSS into DATA, and leave
     * all thread stuff alone.
     */

    /* Now we alter the contents of all the f_*hdr variables
       to correspond to what we want to dump.  */

    f_thdr->s_vaddr = (long) start_of_text ();
    f_thdr->s_size = data_start - f_thdr->s_vaddr;
    f_thdr->s_scnptr = pagesz;
    f_thdr->s_relptr = 0;
    f_thdr->s_nrel = 0;

    eo_data = f_thdr->s_scnptr + f_thdr->s_size;

    if( f_tdhdr ) {		/* Process thread data */

	f_tdhdr->s_vaddr = data_start;
	f_tdhdr->s_size += f_dhdr->s_size - (data_start - f_dhdr->s_vaddr);
	f_tdhdr->s_scnptr = eo_data;
	f_tdhdr->s_relptr = 0;
	f_tdhdr->s_nrel = 0;

	eo_data += f_tdhdr->s_size;

	/* And now for DATA */

	f_dhdr->s_vaddr = f_bhdr->s_vaddr; /* Take BSS start address */
	f_dhdr->s_size = bss_end - f_bhdr->s_vaddr;
	f_dhdr->s_scnptr = eo_data;
	f_dhdr->s_relptr = 0;
	f_dhdr->s_nrel = 0;

	eo_data += f_dhdr->s_size;

    } else {

	f_dhdr->s_vaddr = data_start;
	f_dhdr->s_size = bss_start - data_start;
	f_dhdr->s_scnptr = eo_data;
	f_dhdr->s_relptr = 0;
	f_dhdr->s_nrel = 0;

	eo_data += f_dhdr->s_size;

    }

    f_bhdr->s_vaddr = bss_start;
    f_bhdr->s_size = bss_end - bss_start + pagesz /* fudge */;
    f_bhdr->s_scnptr = 0;
    f_bhdr->s_relptr = 0;
    f_bhdr->s_nrel = 0;

    text_scnptr = f_thdr->s_scnptr;
    data_scnptr = f_dhdr->s_scnptr;
    bias = eo_data - block_copy_start;

    if (f_ohdr.o_symptr > 0L) {
	f_ohdr.o_symptr += bias;
    }

    if (f_hdr.h_strptr > 0) {
	f_hdr.h_strptr += bias;
    }

    if (write (new, &f_hdr, sizeof (f_hdr)) != sizeof (f_hdr)) {
	PERROR (new_name);
    }

    if (write (new, &f_ohdr, sizeof (f_ohdr)) != sizeof (f_ohdr)) {
	PERROR (new_name);
    }

    for( scns = 0; scns < f_hdr.h_nscns; scns++ ) {

	/* This is a cheesey little loop to write out the section headers
	 * in order of increasing virtual address. Dull but effective.
	 */

	for( i = scns+1; i < f_hdr.h_nscns; i++ ) {
	    if( stbl[i].s_vaddr < stbl[scns].s_vaddr ) { /* Swap */
		scntemp = stbl[i];
		stbl[i] = stbl[scns];
		stbl[scns] = scntemp;
	    }
	}

    }

    for( scns = 0; scns < f_hdr.h_nscns; scns++ ) {

	if( write( new, &stbl[scns], sizeof(*stbl)) != sizeof(*stbl)) {
	    PERROR (new_name);
	}

    }

    return (0);

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
    register int scns;

    for( scns = 0; scns < f_hdr.h_nscns; scns++ )
	write_segment( new, &stbl[scns] );

    return 0;
}

write_segment( new, sptr )
int new;
struct scnhdr *sptr;
{
    register char *ptr, *end;
    register int nwrite, ret;
    char buf[80];
    extern int errno;
    char zeros[128];

    if( sptr->s_scnptr == 0 )
	return;			/* Nothing to do */

    if( lseek( new, (long) sptr->s_scnptr, 0 ) == -1 )
	PERROR( "unexecing" );

    bzero (zeros, sizeof zeros);

    ptr = (char *) sptr->s_vaddr;
    end = ptr + sptr->s_size;

    while( ptr < end ) {

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
	    write (new, zeros, nwrite);
	else if (nwrite != ret) {
	    sprintf (buf,
		     "unexec write failure: addr 0x%x, fileno %d, size 0x%x, wrote 0x%x, errno %d",
		     ptr, new, nwrite, ret, errno);
	    PERROR (buf);
	}
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

    lseek (a_out, SYMS_START, 0);	/* Position a.out to symtab. */
    lseek( new, (long)f_ohdr.o_symptr, 0 );

    while ((n = read (a_out, page, sizeof page)) > 0) {
	if (write (new, page, n) != n) {
	    PERROR (new_name);
	}
    }
    if (n < 0) {
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
    if (stat (name, &sbuf) == -1) {
	PERROR (name);
    }
    sbuf.st_mode |= 0111 & ~um;
    if (chmod (name, sbuf.st_mode) == -1)
	PERROR (name);
}

/* Find the first pty letter.  This is usually 'p', as in ptyp0, but
   is sometimes configured down to 'm', 'n', or 'o' for some reason. */

first_pty_letter ()
{
  struct stat buf;
  char pty_name[16];
  char c;

  for (c = 'o'; c >= 'a'; c--)
    {
      sprintf (pty_name, "/dev/pty%c0", c);
      if (stat (pty_name, &buf) < 0)
	return c + 1;
    }
  return 'a';
}

