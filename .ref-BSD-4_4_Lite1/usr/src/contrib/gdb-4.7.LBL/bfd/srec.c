/* BFD back-end for s-record objects.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
   Written by Steve Chamberlain of Cygnus Support <sac@cygnus.com>.

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

/*
SUBSECTION
	S-record handling

DESCRIPTION
	
	S-records cannot hold anything but addresses and data, so
	that's all that we implement.
   
	The only interesting thing is that s-records may come out of
	order and there is no header, so an initial scan is required
	to discover the minimum and maximum addresses used to create
	the vma and size of the only section we create.  We
	arbitrarily call this section ".text". 

	When bfd_get_section_contents is called the file is read
	again, and this time the data is placed into a bfd_alloc'd
	area.

	Any number of sections may be created for output, we save them
	up and output them when it's time to close the bfd.

	An s record looks like:
	
EXAMPLE
	S<type><length><address><data><checksum>
	
DESCRIPTION
	Where
	o length
	is the number of bytes following upto the checksum. Note that
	this is not the number of chars following, since it takes two
	chars to represent a byte.
	o type
	is one of:
	0) header record
	1) two byte address data record
	2) three byte address data record
	3) four byte address data record
	7) four byte address termination record
	8) three byte address termination record
	9) two byte address termination record
	
	o address
	is the start address of the data following, or in the case of
	a termination record, the start address of the image
	o data
	is the data.
	o checksum
	is the sum of all the raw byte data in the record, from the length
	upwards, modulo 256 and subtracted from 255.
	
*/

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"

/* Macros for converting between hex and binary */

static CONST char digs[] = "0123456789ABCDEF";

static char hex_value[1 + (unsigned char)~0];

#define NOT_HEX 20
#define NIBBLE(x) hex_value[(unsigned char)(x)]
#define HEX(buffer) ((NIBBLE((buffer)[0])<<4) + NIBBLE((buffer)[1]))
#define TOHEX(d, x, ch) \
	d[1] = digs[(x) & 0xf]; \
	d[0] = digs[((x)>>4)&0xf]; \
	ch += ((x) & 0xff);
#define	ISHEX(x)  (hex_value[(unsigned char)(x)] != NOT_HEX)



static void
DEFUN_VOID(srec_init) 
{
    unsigned int i;
    static boolean inited = false;
    
    if (inited == false) 
    {
	
	inited = true;
	
	for (i = 0; i < sizeof (hex_value); i++) 
	{
	    hex_value[i] = NOT_HEX;
	}
    
	for (i = 0; i < 10; i++) 
	{
	    hex_value[i + '0'] = i;
	
	}
	for (i = 0; i < 6; i++) 
	{
	    hex_value[i + 'a'] = i+10;
	    hex_value[i + 'A'] = i+10;
	}
    }    
}


/* The maximum number of bytes on a line is FF */
#define MAXCHUNK 0xff 
/* The number of bytes we fit onto a line on output */
#define CHUNK 21

/* We cannot output our srecords as we see them, we have to glue them
   together, this is done in this structure : */

struct srec_data_list_struct
{
    unsigned    char *data;
    bfd_vma where;
    bfd_size_type size;
    struct srec_data_list_struct *next;
    
} ;
typedef struct srec_data_list_struct srec_data_list_type;


typedef struct  srec_data_struct
{
    srec_data_list_type *head;    
    unsigned int type;

} tdata_type;



#define enda(x) (x->vma + x->size)
/* 
   called once per input s-record, used to work out vma and size of data.
 */

static bfd_vma low,high;

static void
DEFUN(size_srec,(abfd, section, address, raw, length),
      bfd *abfd AND
      asection *section AND
      bfd_vma address AND
      bfd_byte *raw AND
      unsigned int length)
{
  if (address < low)
    low = address;
  if (address + length > high) 
    high = address + length -1;
}


/*
 called once per input s-record, copies data from input into bfd_alloc'd area
 */

static void
DEFUN(fillup,(abfd, section, address, raw, length),
bfd *abfd AND
asection *section AND
bfd_vma address AND
bfd_byte *raw AND
unsigned int length)
{
    unsigned int i;
    bfd_byte *dst =
     (bfd_byte *)(section->used_by_bfd) +
     address - section->vma;
    /* length -1 because we don't read in the checksum */
    for (i = 0; i < length -1 ; i++) {
	    *dst = HEX(raw);
	    dst++;
	    raw+=2;
	}
}

/* Pass over an s-record file, calling one of the above functions on each
   record.  */

static void
DEFUN(pass_over,(abfd, func, section),
      bfd *abfd AND
      void (*func)() AND
      asection *section)
{
    unsigned int bytes_on_line;
    boolean eof = false;

    /* To the front of the file */
    bfd_seek(abfd, (file_ptr)0, SEEK_SET);
    while (eof == false)
    {
	char buffer[MAXCHUNK];
	char *src = buffer;
	char type;
	bfd_vma address = 0;

	/* Find first 'S' */
	eof =  (boolean)(bfd_read(src, 1, 1, abfd) != 1);
	while (*src!= 'S' && !eof) {
		eof = (boolean)(bfd_read(src, 1, 1, abfd) != 1);
	    }
	if (eof) break;
	src++;

	/* Fetch the type and the length */
	bfd_read(src, 1, 3, abfd);

	type = *src++;

	if (!ISHEX (src[0]) || !ISHEX (src[1]))
	 break;

	bytes_on_line = HEX(src);

	if (bytes_on_line > MAXCHUNK/2)
	 break;
	src+=2 ;

	bfd_read(src, 1 , bytes_on_line * 2, abfd);

	switch (type) {
	      case '0':
	      case '5':
		/* Prologue - ignore */
		break;
	     case '3':
		address = HEX(src);
		src+=2;
		bytes_on_line--;
		
	      case '2':
		address = HEX(src) | (address<<8) ;
		src+=2;
		bytes_on_line--;
	      case '1':
		address = HEX(src) | (address<<8) ;
		src+=2;
		address = HEX(src) | (address<<8) ;
		src+=2;
		bytes_on_line-=2;
		func(abfd,section, address, src, bytes_on_line);
		break;
	      default:
		return;
	    }
    }

}



static bfd_target *
DEFUN(srec_object_p, (abfd),
      bfd *abfd)
{
  char b[4];
  asection *section;
  srec_init();
  
  bfd_seek(abfd, (file_ptr)0, SEEK_SET);
  bfd_read(b, 1, 4, abfd);

  if (b[0] != 'S' || !ISHEX(b[1]) || !ISHEX(b[2]) || !ISHEX(b[3]))
    return (bfd_target*) NULL;
  
  /* We create one section called .text for all the contents, 
     and allocate enough room for the entire file.  */

  section =  bfd_make_section(abfd, ".text");
  section->_raw_size = 0;
  section->vma = 0xffffffff;
  low = 0xffffffff;
  high = 0;
  pass_over(abfd, size_srec, section);
  section->_raw_size = high - low;
  section->vma = low;
  section->flags = SEC_HAS_CONTENTS | SEC_LOAD | SEC_ALLOC;
  
  return abfd->xvec;
}


static boolean
DEFUN(srec_get_section_contents,(abfd, section, location, offset, count),
      bfd *abfd AND
      asection *section AND
      PTR location AND
      file_ptr offset AND
      bfd_size_type count)
{
    if (section->used_by_bfd == (PTR)NULL) 
    {
	section->used_by_bfd = (PTR)bfd_alloc (abfd, section->_raw_size);
	pass_over(abfd, fillup, section);
    }
    (void) memcpy((PTR)location,
		  (PTR)((char *)(section->used_by_bfd) + offset),
		  count);
    return true;
}
      


boolean
DEFUN(srec_set_arch_mach,(abfd, arch, machine),
      bfd *abfd AND
      enum bfd_architecture arch AND
      unsigned long machine)
{
  return bfd_default_set_arch_mach(abfd, arch, machine);
}


/* we have to save up all the Srecords for a splurge before output,
   also remember   */

static boolean
DEFUN(srec_set_section_contents,(abfd, section, location, offset, bytes_to_do),
      bfd *abfd AND
      sec_ptr section AND
      PTR location AND
      file_ptr offset AND
      bfd_size_type bytes_to_do)
{
  tdata_type  *tdata = abfd->tdata.srec_data;
  srec_data_list_type *entry = (srec_data_list_type *)
   bfd_alloc(abfd, sizeof(srec_data_list_type));
  if ((section->flags & SEC_ALLOC)
      && (section->flags & SEC_LOAD)) 
  {
    unsigned  char *data = (unsigned char *) bfd_alloc(abfd, bytes_to_do);
    memcpy(data, location, bytes_to_do);

    if ((section->vma + offset + bytes_to_do) <= 0xffff)  
    {

    }
    else if ((section->vma + offset + bytes_to_do) <= 0xffffff 
	     && tdata->type < 2) 
    {
      tdata->type = 2;
    }
    else 
    {
      tdata->type = 3;
    }

    entry->data = data;
    entry->where = section->vma + offset;
    entry->size = bytes_to_do;
    entry->next = tdata->head;
    tdata->head = entry;
  }
  return true;    
}

/* Write a record of type, of the supplied number of bytes. The
   supplied bytes and length don't have a checksum. That's worked out
   here
*/
static
void DEFUN(srec_write_record,(abfd, type, address, data, end),
	   bfd *abfd AND
	   char type AND
	   bfd_vma address AND
	   CONST unsigned char *data AND
	   CONST unsigned char *end)

{
    char buffer[MAXCHUNK];
    
    unsigned int check_sum = 0;
    unsigned CONST char *src = data;
    char *dst =buffer;
    char *length;
    

    *dst++ = 'S';
    *dst++ = '0' + type;

    length = dst;
    dst+=2;			/* leave room for dst*/
    
    switch (type) 
    {
      case 3:
      case 7:
	TOHEX(dst, (address >> 24), check_sum);
	dst+=2;
      case 8:
      case 2:
	TOHEX(dst, (address >> 16), check_sum);
	dst+=2;
      case 9:
      case 1:
      case 0:
	TOHEX(dst, (address >> 8), check_sum);
	dst+=2;
	TOHEX(dst, (address), check_sum);
	dst+=2;
	break;

    }
    for (src = data; src < end; src++) 
    {
	TOHEX(dst, *src, check_sum);
	dst+=2;
    }

    /* Fill in the length */
    TOHEX(length, (dst - length)/2, check_sum);
    check_sum &= 0xff;
    check_sum = 255 - check_sum;
    TOHEX(dst, check_sum, check_sum);
    dst+=2;
    
    *dst ++ = '\n';
    bfd_write((PTR)buffer, 1, dst - buffer , abfd);
}



static void
DEFUN(srec_write_header,(abfd),
      bfd *abfd)
{
    unsigned char buffer[MAXCHUNK];
    unsigned char *dst = buffer;
    unsigned int i;

    /* I'll put an arbitary 40 char limit on header size */
    for (i = 0; i < 40 && abfd->filename[i];  i++) 
    {
	*dst++ = abfd->filename[i];
    }
    srec_write_record(abfd,0, 0, buffer, dst);
}

static void
DEFUN(srec_write_section,(abfd, tdata, list),
       bfd *abfd AND
       tdata_type *tdata AND
       srec_data_list_type *list)
{
    unsigned int bytes_written = 0;
    unsigned char *location = list->data;

    while (bytes_written < list->size)
    {
	bfd_vma address;
	
	unsigned int bytes_this_chunk = list->size - bytes_written;

	if (bytes_this_chunk > CHUNK) 
	{
	    bytes_this_chunk = CHUNK;
	}

	address = list->where +  bytes_written;

	srec_write_record(abfd,
			  tdata->type,
			  address,
			  location,
			  location + bytes_this_chunk);

	bytes_written += bytes_this_chunk;
	location += bytes_this_chunk;
    }

}

static void
DEFUN(srec_write_terminator,(abfd, tdata),
      bfd *abfd AND
      tdata_type *tdata)
{
    unsigned    char buffer[2];
    
    srec_write_record(abfd, 10 - tdata->type,
		      abfd->start_address, buffer, buffer);


}
static boolean
DEFUN(srec_mkobject, (abfd), 
      bfd *abfd)
{
    tdata_type *tdata = (tdata_type *)bfd_alloc(abfd,  sizeof(tdata_type));
    abfd->tdata.srec_data = tdata;
    tdata->type = 1;
    tdata->head = (srec_data_list_type *)NULL;
    return true;
    
}

      
static boolean
DEFUN(srec_write_object_contents,(abfd),
     bfd *abfd)
{
    int bytes_written;
    tdata_type *tdata = abfd->tdata.srec_data;
    srec_data_list_type *list;

    bytes_written = 0;


    
    srec_write_header(abfd);
    
    /* Now wander though all the sections provided and output them */
    list = tdata->head;

    while (list != (srec_data_list_type*)NULL) 
    {
	srec_write_section(abfd, tdata, list); 
	list = list->next;
    }
    srec_write_terminator(abfd, tdata);
    return true;
}

static int 
DEFUN(srec_sizeof_headers,(abfd, exec),
      bfd *abfd AND
      boolean exec)
{
return 0;
}

static asymbol *
DEFUN(srec_make_empty_symbol, (abfd),
      bfd*abfd)
{
  asymbol *new=  (asymbol *)bfd_zalloc (abfd, sizeof (asymbol));
  new->the_bfd = abfd;
  return new;
}
#define FOO PROTO
#define srec_new_section_hook (FOO(boolean, (*), (bfd *, asection *)))bfd_true
#define srec_get_symtab_upper_bound (PROTO(unsigned int, (*),(bfd *)))bfd_false
#define srec_get_symtab (FOO(unsigned int, (*), (bfd *, asymbol **)))bfd_0
#define srec_get_reloc_upper_bound (FOO(unsigned int, (*),(bfd*, asection *)))bfd_false
#define srec_canonicalize_reloc (FOO(unsigned int, (*),(bfd*,asection *, arelent **, asymbol **))) bfd_0

#define srec_print_symbol (FOO(void,(*),(bfd *, PTR, asymbol *, bfd_print_symbol_type))) bfd_void

#define srec_openr_next_archived_file (FOO(bfd *, (*), (bfd*,bfd*))) bfd_nullvoidptr
#define srec_find_nearest_line (FOO(boolean, (*),(bfd*,asection*,asymbol**,bfd_vma, CONST char**, CONST char**, unsigned int *))) bfd_false
#define srec_generic_stat_arch_elt  (FOO(int, (*), (bfd *,struct stat *))) bfd_0


#define srec_core_file_failing_command (char *(*)())(bfd_nullvoidptr)
#define srec_core_file_failing_signal (int (*)())bfd_0
#define srec_core_file_matches_executable_p (FOO(boolean, (*),(bfd*, bfd*)))bfd_false
#define srec_slurp_armap bfd_true
#define srec_slurp_extended_name_table bfd_true
#define srec_truncate_arname (void (*)())bfd_nullvoidptr
#define srec_write_armap  (FOO( boolean, (*),(bfd *, unsigned int, struct orl *, unsigned int, int))) bfd_nullvoidptr
#define srec_get_lineno (struct lineno_cache_entry *(*)())bfd_nullvoidptr
#define	srec_close_and_cleanup	bfd_generic_close_and_cleanup
#define srec_bfd_debug_info_start bfd_void
#define srec_bfd_debug_info_end bfd_void
#define srec_bfd_debug_info_accumulate  (FOO(void, (*), (bfd *,	 asection *))) bfd_void
#define srec_bfd_get_relocated_section_contents bfd_generic_get_relocated_section_contents
#define srec_bfd_relax_section bfd_generic_relax_section
bfd_target srec_vec =
{
    "srec",			/* name */
    bfd_target_srec_flavour,
    true,			/* target byte order */
    true,			/* target headers byte order */
    (HAS_RELOC | EXEC_P |	/* object flags */
     HAS_LINENO | HAS_DEBUG |
     HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT | D_PAGED),
    (SEC_CODE|SEC_DATA|SEC_ROM|SEC_HAS_CONTENTS
     |SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
     0,				/* leading underscore */
    ' ',			/* ar_pad_char */
    16,				/* ar_max_namelen */
    1,				/* minimum alignment */
    _do_getb64, _do_putb64,  _do_getb32,
    _do_putb32, _do_getb16, _do_putb16, /* data */
    _do_getb64, _do_putb64,  _do_getb32,
    _do_putb32, _do_getb16, _do_putb16, /* hdrs */

  {
      _bfd_dummy_target,
      srec_object_p,		/* bfd_check_format */
      (struct bfd_target *(*)()) bfd_nullvoidptr,
      (struct bfd_target *(*)())     bfd_nullvoidptr,
  },
  {
      bfd_false,
      srec_mkobject,
      _bfd_generic_mkarchive,
      bfd_false,
  },
  {				/* bfd_write_contents */
      bfd_false,
      srec_write_object_contents,
      _bfd_write_archive_contents,
      bfd_false,
  },
    JUMP_TABLE(srec)
 };

