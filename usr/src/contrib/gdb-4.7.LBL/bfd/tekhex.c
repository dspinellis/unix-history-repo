/* BFD backend for Extended Tektronix Hex Format  objects.
   Copyright (C) 1992 Free Software Foundation, Inc.

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
	Tektronix Hex Format handling

DESCRIPTION
	
	Tek Hex records can hold symbols and data, but not
	relocations. Their main application is communication with
	devices like PROM programmers and ICE equipment.
	
	It seems that the sections are descibed as being really big,
        the example I have says that the text section is 0..ffffffff.
	BFD would barf with this, many apps would try to alloc 4GB to
	read in the file. 

	Tex Hex may contain many sections, but the data which comes in
	has no tag saying which section it belongs to, so we create
	one section for each block of data, called "blknnnn" which we
	stick all the data into.

	TekHex may come out of 	order and there is no header, so an
	initial scan is required  to discover the minimum and maximum
	addresses used to create the vma and size of the sections we
	create.  
	We read in the data into pages of CHUNK_MASK+1 size and read
	them out from that whenever we need to.

	Any number of sections may be created for output, we save them
	up and output them when it's time to close the bfd.


	A TekHex record looks like:
EXAMPLE
	%<block length><type><checksum><stuff><cr>
	
DESCRIPTION
	Where
	o length
	is the number of bytes in the record not including the % sign.
	o type
	is one of:
	3) symbol record
	6) data record
	8) termination record
	

The data can come out of order, and may be discontigous. This is a
serial protocol, so big files are unlikely, so we keep a list of 8k chunks
*/

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"

typedef struct {
bfd_vma low;
bfd_vma high;
} addr_range_type;

typedef struct tekhex_symbol_struct {

  asymbol symbol;
struct  tekhex_symbol_struct *prev;

} tekhex_symbol_type;

static char digs[] = "0123456789ABCDEF";

static char sum_block[256];
/* Horrible ascii dependent macros for converting between hex and
   binary */

#define CHARS_IN_SET 256
static char hex_value[CHARS_IN_SET];
#define NOT_HEX 20
#define NIBBLE(x) hex_value[x]
#define HEX(buffer) ((NIBBLE((buffer)[0])<<4) + NIBBLE((buffer)[1]))
#define TOHEX(d,x) \
(d)[1] = digs[(x) & 0xf]; \
(d)[0] = digs[((x)>>4)&0xf];
#define	ISHEX(x)  (hex_value[x] != NOT_HEX)

/*
Here's an example
%3A6C6480004E56FFFC4E717063B0AEFFFC6D0652AEFFFC60F24E5E4E75
%1B3709T_SEGMENT1108FFFFFFFF
%2B3AB9T_SEGMENT7Dgcc_compiled$1087hello$c10
%373829T_SEGMENT80int$t1$r1$$214741080char$t2$r2$0$12710
%373769T_SEGMENT80long$int$t3$r1$$1080unsigned$int$t4$10
%373CA9T_SEGMENT80long$unsigned$in1080short$int$t6$r1$10
%373049T_SEGMENT80long$long$int$t71080short$unsigned$i10
%373A29T_SEGMENT80long$long$unsign1080signed$char$t10$10
%373D69T_SEGMENT80unsigned$char$t11080float$t12$r1$4$010
%373D19T_SEGMENT80double$t13$r1$8$1080long$double$t14$10
%2734D9T_SEGMENT8Bvoid$t15$151035_main10
%2F3CA9T_SEGMENT81$1081$1681$1E81$21487main$F110
%2832F9T_SEGMENT83i$18FFFFFFFC81$1481$214
%07 8 10 10

explanation:
%3A6C6480004E56FFFC4E717063B0AEFFFC6D0652AEFFFC60F24E5E4E75
 ^ ^^ ^     ^-data
 | || +------ 4 char integer 0x8000
 | |+-------- checksum
 | +--------- type 6 (data record)
 +----------- length 3a chars
 <---------------------- 3a (58 chars) ------------------->

%1B3709T_SEGMENT1108FFFFFFFF
      ^         ^^ ^- 8 character integer 0xffffffff
      |         |+-   1 character integer 0
      |         +--   type 1 symbol (section definition)
      +------------   9 char symbol T_SEGMENT

%2B3AB9T_SEGMENT7Dgcc_compiled$1087hello$c10
%373829T_SEGMENT80int$t1$r1$$214741080char$t2$r2$0$12710
%373769T_SEGMENT80long$int$t3$r1$$1080unsigned$int$t4$10
%373CA9T_SEGMENT80long$unsigned$in1080short$int$t6$r1$10
%373049T_SEGMENT80long$long$int$t71080short$unsigned$i10
%373A29T_SEGMENT80long$long$unsign1080signed$char$t10$10
%373D69T_SEGMENT80unsigned$char$t11080float$t12$r1$4$010
%373D19T_SEGMENT80double$t13$r1$8$1080long$double$t14$10
%2734D9T_SEGMENT8Bvoid$t15$151035_main10
%2F3CA9T_SEGMENT81$1081$1681$1E81$21487main$F110
%2832F9T_SEGMENT83i$18FFFFFFFC81$1481$214
%0781010

Turns into 
sac@thepub$ ./objdump -dx -m m68k f

f:     file format tekhex
-----x--- 9/55728 -134219416 Sep 29 15:13 1995 f
architecture: UNKNOWN!, flags 0x00000010:
HAS_SYMS
start address 0x00000000
SECTION 0 [D00000000]	: size 00020000 vma 00000000 align 2**0
 ALLOC, LOAD
SECTION 1 [D00008000]	: size 00002001 vma 00008000 align 2**0
 
SECTION 2 [T_SEGMENT]	: size ffffffff vma 00000000 align 2**0
 
SYMBOL TABLE:
00000000  g       T_SEGMENT gcc_compiled$
00000000  g       T_SEGMENT hello$c
00000000  g       T_SEGMENT int$t1$r1$$21474
00000000  g       T_SEGMENT char$t2$r2$0$127
00000000  g       T_SEGMENT long$int$t3$r1$$
00000000  g       T_SEGMENT unsigned$int$t4$
00000000  g       T_SEGMENT long$unsigned$in
00000000  g       T_SEGMENT short$int$t6$r1$
00000000  g       T_SEGMENT long$long$int$t7
00000000  g       T_SEGMENT short$unsigned$i
00000000  g       T_SEGMENT long$long$unsign
00000000  g       T_SEGMENT signed$char$t10$
00000000  g       T_SEGMENT unsigned$char$t1
00000000  g       T_SEGMENT float$t12$r1$4$0
00000000  g       T_SEGMENT double$t13$r1$8$
00000000  g       T_SEGMENT long$double$t14$
00000000  g       T_SEGMENT void$t15$15
00000000  g       T_SEGMENT _main
00000000  g       T_SEGMENT $
00000000  g       T_SEGMENT $
00000000  g       T_SEGMENT $
00000010  g       T_SEGMENT $
00000000  g       T_SEGMENT main$F1
fcffffff  g       T_SEGMENT i$1
00000000  g       T_SEGMENT $
00000010  g       T_SEGMENT $


RELOCATION RECORDS FOR [D00000000]: (none)

RELOCATION RECORDS FOR [D00008000]: (none)

RELOCATION RECORDS FOR [T_SEGMENT]: (none)

Disassembly of section D00000000:
...
00008000 ($+)7ff0 linkw fp,#-4
00008004 ($+)7ff4 nop
00008006 ($+)7ff6 movel #99,d0
00008008 ($+)7ff8 cmpl fp@(-4),d0
0000800c ($+)7ffc blts 00008014 ($+)8004
0000800e ($+)7ffe addql #1,fp@(-4)
00008012 ($+)8002 bras 00008006 ($+)7ff6
00008014 ($+)8004 unlk fp
00008016 ($+)8006 rts
...

*/

static void
DEFUN_VOID(tekhex_init) 
{
  unsigned int i;
  static boolean inited = false;
  int val;    
  if (inited == false) 
    {
	
      inited = true;
	
      for (i = 0; i < CHARS_IN_SET; i++) 
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
      val = 0;
      for (i = 0; i < 10; i++) {
	sum_block[i + '0'] = val++;
      }
      for (i = 'A'; i <= 'Z'; i++) {
	sum_block[i] = val++;
      }
      sum_block['$'] = val++;
      sum_block['%'] = val++;
      sum_block['.'] = val++;
      sum_block['_'] = val++;
      for (i = 'a'; i <= 'z'; i++) {
	sum_block[i] = val++;
      }
    }    
}


/* The maximum number of bytes on a line is FF */
#define MAXCHUNK 0xff 
/* The number of bytes we fit onto a line on output */
#define CHUNK 21

/* We cannot output our tekhexords as we see them, we have to glue them
   together, this is done in this structure : */

struct tekhex_data_list_struct
{
    unsigned    char *data;
    bfd_vma where;
    bfd_size_type size;
    struct tekhex_data_list_struct *next;
    
} ;
typedef struct tekhex_data_list_struct tekhex_data_list_type;



#define CHUNK_MASK 0x1fff

struct data_struct {
  char chunk_data[CHUNK_MASK+1];
  char chunk_init[CHUNK_MASK+1];
  bfd_vma vma;
  struct data_struct *next;
} ;

typedef struct  tekhex_data_struct
{
    tekhex_data_list_type *head;    
    unsigned int type;
    struct tekhex_symbol_struct *symbols;
    struct data_struct *data;
} tdata_type;



#define enda(x) (x->vma + x->size)


static bfd_vma 
DEFUN(getvalue,(srcp),
      char **srcp)
{
  char *src = *srcp;
  bfd_vma value = 0;
  unsigned int len = hex_value[*src++];
  if (len == 0) len=16;
  while (len --) {
    value = value << 4 | hex_value[*src++];
  }
  *srcp = src;
  return value;
}

static unsigned int 
DEFUN(getsym,(dstp,srcp),
      char *dstp AND
      char **srcp)
{
  char *src = *srcp;
  unsigned int i;
  unsigned int len = hex_value[*src++];
  if (len == 0) len=16;
  for (i = 0; i < len; i++)
    dstp[i] = src[i];
  dstp[i] = 0;
  *srcp = src + i;
  return len;
}

struct data_struct *
DEFUN(find_chunk,(abfd, vma),
      bfd *abfd  AND
      bfd_vma vma)
{
  struct data_struct *d = abfd->tdata.tekhex_data->data;
  vma &= ~CHUNK_MASK;
  while (d && (d->vma) != vma)
    {
      d = d->next;
    }
  if (!d) {
    char *sname = bfd_alloc(abfd, 12);
    asection *s;
    /* No chunk for this address, so make one up */
    d =(struct data_struct *)
      bfd_alloc(abfd, sizeof(struct data_struct));
    memset(d->chunk_init, 0, CHUNK_MASK+1);
    memset(d->chunk_data, 0, CHUNK_MASK+1);
    d->next =   abfd->tdata.tekhex_data->data ;
    d->vma = vma;
    abfd->tdata.tekhex_data->data = d;
  }
  return d;
}

DEFUN(insert_byte,(abfd, value, addr),
      bfd *abfd AND
      int value AND
      bfd_vma addr)
{
  /* Find the chunk that this byte needs and put it in */
  struct data_struct *d = find_chunk(abfd, addr);
  d->chunk_data[addr &CHUNK_MASK] = value;
  d->chunk_init[addr &CHUNK_MASK] = 1;

}
/* The first pass is to find the names of all the sections, and see 
  how big the data is */
static void
DEFUN(first_phase,(abfd, type, src),
      bfd *abfd AND
      char type AND
      char *src)
{
  asection *section = &bfd_abs_section;
  int len;
  char sym[17];			/* A symbol can only be 16chars long */
  switch (type)
    {
    case '6':
      /* Data record - read it and store it */
      {
	bfd_vma addr = getvalue(&src);
	while (*src) {
	  insert_byte(abfd, HEX(src), addr);
	  src+=2;
	  addr++;
	}
      }
      
      return;
    case '3':
      /* Symbol record, read the segment */
      len = getsym(sym, &src);
      section = bfd_get_section_by_name(abfd, sym);
      if (section == (asection *)NULL) 
	{
	  char *n = bfd_alloc(abfd, len+1);
	  memcpy(n, sym, len+1);
	  section = bfd_make_section(abfd, n);
	}
      while (*src) 
	{
	  switch (*src) {
	    asection *s;
	  case '1':		/* section range */
	    src++;
	    section->vma = getvalue(&src);
	    section->_raw_size = getvalue(&src) - section->vma;
	    section->flags = SEC_HAS_CONTENTS | SEC_LOAD |  SEC_ALLOC; 
	    break;

	  case '2':
	  case '3':
	  case '4':
	  case '6':
	  case '7':
	  case '8':
	    /* Symbols, add to section */
	    {
	      tekhex_symbol_type *new =
		(tekhex_symbol_type *)bfd_alloc(abfd,
						sizeof(tekhex_symbol_type));
	      char type=(*src);
	      new->symbol.the_bfd = abfd;
	      src++;
	      abfd->symcount++;
	      abfd->flags |= HAS_SYMS;
	      new->prev = abfd->tdata.tekhex_data->symbols;
	      abfd->tdata.tekhex_data->symbols = new;
	      len = getsym(sym, &src);
	      new->symbol.name = bfd_alloc(abfd,len+1);
	      memcpy((char *)(new->symbol.name), sym, len+1);
	      new->symbol.section = section ;
	      if (type <= '4')
		  new->symbol.flags = (BSF_GLOBAL|BSF_EXPORT);
	      else
		  new->symbol.flags = BSF_LOCAL;
	      new->symbol.value = getvalue(&src) - section->vma;
	    }
	  }
	}  
    }
}


/* Pass over an tekhex, calling one of the above functions on each
   record.  */

static void
    DEFUN(pass_over,(abfd, func),
      bfd *abfd AND
      void EXFUN((*func),(bfd *, char, char *)))
{
  unsigned int chars_on_line;
  boolean eof = false;

  /* To the front of the file */
  bfd_seek(abfd, (file_ptr)0, SEEK_SET);
  while (eof == false)
    {
      char buffer[MAXCHUNK];
      char *src = buffer;
      char type;
      bfd_vma address = 0;

      /* Find first '%' */
      eof =  (boolean)(bfd_read(src, 1, 1, abfd) != 1);
      while (*src!= '%' && !eof) {
	eof = (boolean)(bfd_read(src, 1, 1, abfd) != 1);
      }
      if (eof) break;
      src++;


      /* Fetch the type and the length and the checksum */
      bfd_read(src, 1, 5, abfd);

      type = src[2];

      if (!ISHEX (src[0]) || !ISHEX (src[1]))
	break;

      chars_on_line = HEX(src) - 5; /* Already read five char */

      bfd_read(src, 1 , chars_on_line , abfd);
      src[chars_on_line] = 0;	/* put a null at the end */



      func(abfd, type, src);
    }

}




unsigned int
DEFUN(tekhex_get_symtab,(abfd, table),
      bfd *abfd AND
      asymbol **table)
{
  tekhex_symbol_type *p = abfd->tdata.tekhex_data->symbols;
  unsigned int c = bfd_get_symcount(abfd)  ;
  table[c]= 0;
  while (p) {
    table[--c] = &(p->symbol);
    p = p->prev;
  }

  return bfd_get_symcount(abfd);
}
      
unsigned int
DEFUN(tekhex_get_symtab_upper_bound,(abfd),
      bfd *abfd)
{
  return (abfd->symcount+1) * (sizeof(struct tekhex_asymbol_struct *));

}

static boolean
DEFUN(tekhex_mkobject, (abfd), 
      bfd *abfd)
{
    tdata_type *tdata = (tdata_type *)bfd_alloc(abfd,  sizeof(tdata_type));
    abfd->tdata.tekhex_data = tdata;
    tdata->type = 1;
    tdata->head = (tekhex_data_list_type *)NULL;
    tdata->symbols = (struct tekhex_symbol_struct *)NULL;
tdata->data = (struct data_struct *)NULL;
    return true;
    
}

/* 
  Return true if the file looks like it's in TekHex format. Just look
  for a percent sign and some hex digits */
	

static bfd_target *
DEFUN(tekhex_object_p, (abfd),
      bfd *abfd)
{
  char b[4];
  asection *section;
  tekhex_init();
  
  bfd_seek(abfd, (file_ptr)0, SEEK_SET);
  bfd_read(b, 1, 4, abfd);

  if (b[0] != '%' || !ISHEX(b[1]) || !ISHEX(b[2]) || !ISHEX(b[3]))
    return (bfd_target*) NULL;

  tekhex_mkobject(abfd);

  pass_over(abfd, first_phase);
  return abfd->xvec;
}


static boolean
DEFUN(move_section_contents,(abfd, section, locationp, offset,count,
			     get),
      
      bfd *abfd AND
      asection *section AND
      PTR locationp AND
      file_ptr offset AND
      bfd_size_type count AND
      boolean get)
{
  bfd_vma addr;
  char *location = (char *)locationp;
  bfd_vma prev_number = 1;	/* Nothing can have this as a high bit*/
  struct data_struct *d = (struct data_struct *)NULL;  
  for (addr = section->vma; count != 0; count --, addr++)
    {

      bfd_vma chunk_number = addr & ~ CHUNK_MASK; /* Get high bits of address */
      bfd_vma low_bits = addr & CHUNK_MASK;
      if (chunk_number != prev_number) 
	{
	  /* Different chunk, so move pointer */
	  d = find_chunk(abfd, chunk_number);
	}

      if (get)    {
	if (d->chunk_init[low_bits]) 
	  {
	    *location = d->chunk_data[low_bits];
	  }
	else
	  {
	    *location = 0;
	  }
      }
      else {
	d->chunk_data[low_bits] =  *location ;
	d->chunk_init[low_bits] = (*location != 0);
      }


      location ++;

    }

}      
static boolean
DEFUN(tekhex_get_section_contents,(abfd, section, locationp, offset, count),
      bfd *abfd AND
      asection *section AND
      PTR locationp AND
      file_ptr offset AND
      bfd_size_type count)
{
  if (section->flags & (SEC_LOAD | SEC_ALLOC))
    {
      move_section_contents(abfd, section, locationp, offset, count, true);
      return true;
    }
  else return false;
}
      


boolean
DEFUN(tekhex_set_arch_mach,(abfd, arch, machine),
      bfd *abfd AND
      enum bfd_architecture arch AND
      unsigned long machine)
{
  return bfd_default_set_arch_mach(abfd, arch, machine);
}


/* we have to save up all the Tekhexords for a splurge before output,
    */

static boolean
DEFUN(tekhex_set_section_contents,(abfd, section, locationp, offset, bytes_to_do),
      bfd *abfd AND
      sec_ptr section AND
      PTR locationp AND
      file_ptr offset AND
      bfd_size_type bytes_to_do)
{

  if (abfd->output_has_begun == false) {
    /* The first time around, allocate enough sections to hold all the chunks */
    asection *s = abfd->sections;
    bfd_vma vma;
    for (s = abfd->sections; s ; s = s->next) {
      if (s->flags & SEC_LOAD) {
	for (vma = s->vma & ~CHUNK_MASK;
	     vma < s->vma + s->_raw_size;
	     vma += CHUNK_MASK)
	  find_chunk(abfd, vma);
      }
    }

  }
  if (section->flags & (SEC_LOAD | SEC_ALLOC))
    {
      move_section_contents(abfd, section, locationp, offset, bytes_to_do, false);
      return true;
    }
  else return false;

}


static void
DEFUN(writevalue,(dst, value),
      char **dst AND
      bfd_vma value)
{
  char *p = *dst;
  int len ;
  int shift;
  for (len = 8, shift = 28; shift  ; shift-=4, len--)
    {
      if ((value>>shift) & 0xf) {
	*p ++ = len + '0';
	while (len) {
	  *p ++ = digs[(value>>shift) &0xf];
	  shift-=4;
	  len --;
	}
	*dst = p;
	return;

      }
    }
  *p++ = '1';
  *p++ = '0';
  *dst = p;
}

static void
DEFUN(writesym,(dst, sym),
      char **dst AND
     CONST char *sym)
{
  char *p = *dst;
  int len = (sym?strlen(sym):0);
  if (len >= 16) {
    *p++ = '0';
    len = 16;
  }

  else {
    if (len == 0) {
      *p++ =  '1';
      sym = "$";
      len=1;
    }
    else
      {
	*p++ = digs[len];
      }
  }

  while (len--) {
    *p++ = *sym++;
  }
  *dst = p;
}

static void 
DEFUN(out,(abfd, type, start, end),
      bfd *abfd AND
      char type AND
      char *start AND
      char *end)
{
  int sum = 0;
  char *s;
  char front[6];
  front[0] = '%';
  TOHEX(front+1, end - start  + 5);
  front[3] = type;

  for (s = start; s < end; s++) {
    sum += sum_block[*s];
  }


  sum += sum_block[front[1]];/*  length */
  sum += sum_block[front[2]];
  sum += sum_block[front[3]];/* type */
  TOHEX(front+4, sum);
  bfd_write(front, 1, 6,abfd);
  end[0] = '\n';
  bfd_write(start, 1,end-start+1,abfd);

}
static boolean
DEFUN(tekhex_write_object_contents,(abfd),
     bfd *abfd)
{
  int bytes_written;
  char buffer[100];
  asymbol **p;
  tdata_type *tdata = abfd->tdata.tekhex_data;
  tekhex_data_list_type *list;
  asection *s;
  struct data_struct *d;
  bytes_written = 0;

  /* And the raw data */
  for (d = abfd->tdata.tekhex_data->data;
       d != (struct data_struct *)NULL;
       d = d->next)
    {
      int low;

      CONST int span=32;
      int addr;
      /* Write it in blocks of 32 bytes */

      for (addr = 0; addr < CHUNK_MASK + 1; addr+=span) 
	{
	  int need = 0;
	  /* Check to see if necessary */
	  for (low = 0; !need && low < span; low++) {
	    if (d->chunk_init[addr + low]) need = 1;
	  }
	  if (need){
	    char * dst = buffer;
	    writevalue(&dst, addr + d->vma);
	    for (low = 0; low <span; low++) {
	      TOHEX(dst, d->chunk_data[addr + low]);
	      dst+=2;
	    }
	    out(abfd, '6', buffer, dst);
	  }
	}
    }
  /* write all the section headers for the sections */
  for (s = abfd->sections ; s != (asection *)NULL ; s = s->next) 
    {
      char *dst = buffer;
      writesym(&dst, s->name);
      *dst++ = '1';
      writevalue(&dst, s->vma);
      writevalue(&dst,  s->vma + s->_raw_size);
      out(abfd, '3',buffer, dst);
    }

  /* And the symbols */
  for (p = abfd->outsymbols; *p; p++) {
    int section_code = bfd_decode_symclass  (*p);
    if (section_code != '?') { /* do not include debug symbols */
	asymbol *s = *p;
	char *dst = buffer;
	writesym(&dst, s->section->name);
  
	switch(section_code) {
	  case 'A':
	    *dst++ = '2';
	    break;
	  case 'a':
	    *dst++ = '6';
	    break;
	  case 'D':
	  case 'B':
	  case 'O':
	    *dst++ = '4';
	    break;
	  case 'd':
	  case 'b':
	  case 'o':
	    *dst++ = '8';
	    break;
	  case 'T':
	    *dst++ = '3';
	    break;
	  case 't':
	    *dst++ = '7';
	    break;
	  case 'C':
	  case 'U':
	    bfd_error=wrong_format;
	    return false;
	}

	writesym(&dst, s->name);
	writevalue(&dst, s->value + s->section->vma);
	out(abfd, '3', buffer, dst);
    }
  }

    
  /* And the terminator */
  bfd_write("%7081010\n", 1,9 , abfd);
return true;
}
    

static int 
DEFUN(tekhex_sizeof_headers,(abfd, exec),
      bfd *abfd AND
      boolean exec)
{
return 0;
}

static asymbol *
DEFUN(tekhex_make_empty_symbol, (abfd),
      bfd*abfd)
{
  tekhex_symbol_type *new=
    (tekhex_symbol_type *)bfd_zalloc (abfd, sizeof (struct tekhex_symbol_struct));
  new->symbol.the_bfd = abfd;
  new->prev = (struct tekhex_symbol_struct *)NULL;
  return &(new->symbol);
}

static void
DEFUN(tekhex_print_symbol,(ignore_abfd, filep, symbol, how),
      bfd            *ignore_abfd AND
      PTR           filep AND
      asymbol        *symbol AND
      bfd_print_symbol_type how)
{
  FILE *file = (FILE *)filep;
  switch (how) {
  case bfd_print_symbol_name:
    fprintf(file, "%s", symbol->name);
    break;
  case bfd_print_symbol_more:
    break;
  case bfd_print_symbol_nm:
    {
      int section_code = bfd_decode_symclass  (symbol);

      if (section_code == 'U')
	fprintf(file, "        ");
      fprintf_vma(file, symbol->value+symbol->section->vma);
      if (section_code == '?')
	{
	fprintf(file," ?");
        }
      else
	fprintf(file," %c", section_code);
      if (symbol->name)
        fprintf(file," %s", symbol->name);
    }
    break;
  case bfd_print_symbol_all:
    {
      CONST char *section_name = symbol->section->name;
      bfd_print_symbol_vandf((PTR) file, symbol);

	
      fprintf(file, " %-5s %s",
	      section_name,
	      symbol->name);
    }	
  }
}

#define FOO PROTO
#define tekhex_new_section_hook (FOO(boolean, (*), (bfd *, asection *)))bfd_true
#define tekhex_get_reloc_upper_bound (FOO(unsigned int, (*),(bfd*, asection *)))bfd_false
#define tekhex_canonicalize_reloc (FOO(unsigned int, (*),(bfd*,asection *, arelent **, asymbol **))) bfd_0


#define tekhex_openr_next_archived_file (FOO(bfd *, (*), (bfd*,bfd*))) bfd_nullvoidptr
#define tekhex_find_nearest_line (FOO(boolean, (*),(bfd*,asection*,asymbol**,bfd_vma, CONST char**, CONST char**, unsigned int *))) bfd_false
#define tekhex_generic_stat_arch_elt  (FOO(int, (*), (bfd *,struct stat *))) bfd_0


#define tekhex_core_file_failing_command (char *(*)())(bfd_nullvoidptr)
#define tekhex_core_file_failing_signal (int (*)())bfd_0
#define tekhex_core_file_matches_executable_p (FOO(boolean, (*),(bfd*, bfd*)))bfd_false
#define tekhex_slurp_armap bfd_true
#define tekhex_slurp_extended_name_table bfd_true
#define tekhex_truncate_arname (void (*)())bfd_nullvoidptr
#define tekhex_write_armap  (FOO( boolean, (*),(bfd *, unsigned int, struct orl *, unsigned int, int))) bfd_nullvoidptr
#define tekhex_get_lineno (struct lineno_cache_entry *(*)())bfd_nullvoidptr
#define	tekhex_close_and_cleanup	bfd_generic_close_and_cleanup
#define tekhex_bfd_debug_info_start bfd_void
#define tekhex_bfd_debug_info_end bfd_void
#define tekhex_bfd_debug_info_accumulate  (FOO(void, (*), (bfd *,	 asection *))) bfd_void
#define tekhex_bfd_get_relocated_section_contents bfd_generic_get_relocated_section_contents
#define tekhex_bfd_relax_section bfd_generic_relax_section
bfd_target tekhex_vec =
{
    "tekhex",			/* name */
    bfd_target_tekhex_flavour,
    true,			/* target byte order */
    true,			/* target headers byte order */
    ( EXEC_P |	/* object flags */
     HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT | D_PAGED),
    (SEC_CODE|SEC_DATA|SEC_ROM|SEC_HAS_CONTENTS
     |SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
    ' ',			/* ar_pad_char */
    16,				/* ar_max_namelen */
    1,				/* minimum alignment */
    _do_getb64, _do_putb64,  _do_getb32,
    _do_putb32, _do_getb16, _do_putb16, /* data */
    _do_getb64, _do_putb64,  _do_getb32,
    _do_putb32, _do_getb16, _do_putb16, /* hdrs */

  {
      _bfd_dummy_target,
      tekhex_object_p,		/* bfd_check_format */
      (struct bfd_target *(*)()) bfd_nullvoidptr,
      (struct bfd_target *(*)())     bfd_nullvoidptr,
  },
  {
      bfd_false,
      tekhex_mkobject,
      _bfd_generic_mkarchive,
      bfd_false,
  },
  {				/* bfd_write_contents */
      bfd_false,
      tekhex_write_object_contents,
      _bfd_write_archive_contents,
      bfd_false,
  },
    JUMP_TABLE(tekhex)
 };

