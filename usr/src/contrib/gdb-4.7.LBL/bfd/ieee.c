/* BFD back-end for ieee-695 objects.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
   Written by Steve Chamberlain of Cygnus Support.

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

#define KEEPMINUSPCININST 1

/* IEEE 695 format is a stream of records, which we parse using a simple one-
   token (which is one byte in this lexicon) lookahead recursive decent
   parser.  */

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "ieee.h"
#include "libieee.h"


#include "obstack.h"
#define obstack_chunk_alloc bfd_xmalloc
#define obstack_chunk_free free

/* Functions for writing to ieee files in the strange way that the
   standard requires. */

static void
DEFUN(ieee_write_byte,(abfd, byte),
      bfd *abfd AND
      bfd_byte byte)
{
  bfd_write((PTR)&byte, 1, 1, abfd);
}


static void
DEFUN(ieee_write_twobyte,(abfd, twobyte),
      bfd *abfd AND
      int twobyte)
{
  bfd_byte b[2];
  b[1] = twobyte & 0xff;
  b[0] = twobyte >> 8;
  bfd_write((PTR)&b[0], 1, 2, abfd);
}



static void
DEFUN(ieee_write_2bytes,(abfd, bytes),
      bfd *abfd AND
      int bytes)
{
  bfd_byte buffer[2];
  buffer[0] = bytes >> 8;
  buffer[1] = bytes & 0xff;

  bfd_write((PTR)buffer, 1, 2, abfd);
}

static void
DEFUN(ieee_write_int,(abfd, value),
      bfd *abfd AND
      bfd_vma value)
{
  if (((unsigned)value) <= 127) {
    ieee_write_byte(abfd, value);
  }
  else {
    unsigned int length;
    /* How many significant bytes ? */
    /* FIXME FOR LONGER INTS */
    if (value & 0xff000000) {
      length = 4;
    }
    else if (value & 0x00ff0000) {
      length  = 3;
    }
    else if (value & 0x0000ff00) {
      length = 2;
    }
    else length = 1;

    ieee_write_byte(abfd, (int)ieee_number_repeat_start_enum + length);
    switch (length) {
    case 4:
      ieee_write_byte(abfd, value >> 24);
    case 3:
      ieee_write_byte(abfd, value >> 16);
    case 2:
      ieee_write_byte(abfd, value >> 8);
    case 1:
      ieee_write_byte(abfd, value);
    }
  }
}

static void
DEFUN(ieee_write_id,(abfd, id),
      bfd *abfd AND
      CONST char *id)
{
  size_t length = strlen(id);
  if (length >= 0 && length <= 127) {
    ieee_write_byte(abfd, length);
  }
  else if (length < 255) {
    ieee_write_byte(abfd, ieee_extension_length_1_enum);
    ieee_write_byte(abfd, length);
  }
  else if (length < 65535) {
    ieee_write_byte(abfd, ieee_extension_length_2_enum);
    ieee_write_byte(abfd, length >> 8);
    ieee_write_byte(abfd, length & 0xff);  
  }
  else {
    BFD_FAIL();
  }
  bfd_write((PTR)id, 1, length, abfd);
}
/***************************************************************************
Functions for reading from ieee files in the strange way that the
standard requires:
*/


#define this_byte(ieee) *((ieee)->input_p)
#define next_byte(ieee) ((ieee)->input_p++)
#define this_byte_and_next(ieee) (*((ieee)->input_p++))


static unsigned short 
DEFUN(read_2bytes,(ieee),
   common_header_type *ieee)
{
  unsigned  char c1 = this_byte_and_next(ieee);
  unsigned  char c2 = this_byte_and_next(ieee);
  return (c1<<8 ) | c2;

}

static void
DEFUN(bfd_get_string,(ieee, string, length),
    common_header_type *ieee AND
      char *string AND
      size_t length)
{
  size_t i;
  for (i= 0; i < length; i++) {
    string[i] = this_byte_and_next(ieee);
  }
}

static char *
DEFUN(read_id,(ieee),
  common_header_type *ieee)
{
  size_t length;
  char *string;
  length = this_byte_and_next(ieee);
  if (length >= 0x00 && length <= 0x7f) {
    /* Simple string of length 0 to 127 */
  }
  else if (length == 0xde) {
    /* Length is next byte, allowing 0..255 */
    length = this_byte_and_next(ieee);
  }
  else if (length == 0xdf) {
    /* Length is next two bytes, allowing 0..65535 */
    length = this_byte_and_next(ieee) ;
    length = (length * 256) + this_byte_and_next(ieee);
  }
  /* Buy memory and read string */
  string = bfd_alloc(ieee->abfd, length+1);
  bfd_get_string(ieee, string, length);
  string[length] = 0;
  return string;
}

static void
DEFUN(ieee_write_expression,(abfd, value, symbol, pcrel, index),
      bfd*abfd AND
      bfd_vma value AND
      asymbol *symbol AND
      boolean pcrel AND
    unsigned int index)
{
  unsigned int term_count = 0;

  if (value != 0) 
  {
    ieee_write_int(abfd, value);
    term_count++;
  }
  
     

  if (symbol->section == &bfd_com_section
      || symbol->section == &bfd_und_section) 
  {
    /* Def of a common symbol */
    ieee_write_byte(abfd, ieee_variable_X_enum);
    ieee_write_int(abfd, symbol->value);
    term_count++;
  }
  else  if (symbol->section != &bfd_abs_section) 
  {
    /* Ref to defined symbol - */
  
    ieee_write_byte(abfd, ieee_variable_R_enum);
    ieee_write_byte(abfd, symbol->section->index + IEEE_SECTION_NUMBER_BASE);
    term_count++;
    if (symbol->flags & BSF_GLOBAL) 
    {
      ieee_write_byte(abfd, ieee_variable_I_enum);
      ieee_write_int(abfd, symbol->value);
      term_count++;
    }
    else if (symbol->flags & ( BSF_LOCAL | BSF_SECTION_SYM))
    {
      /* This is a reference to a defined local symbol, 
	 We can easily do a local as a section+offset */
      ieee_write_byte(abfd, ieee_variable_R_enum); /* or L */
      ieee_write_byte(abfd, symbol->section->index +
		      IEEE_SECTION_NUMBER_BASE);
      ieee_write_int(abfd, symbol->value);
      term_count++;

    }
    else   {
	BFD_FAIL();
      }

  }

 
  
  if(pcrel) {
      /* subtract the pc from here by asking for PC of this section*/
      ieee_write_byte(abfd, ieee_variable_P_enum);
      ieee_write_byte(abfd, index  +IEEE_SECTION_NUMBER_BASE);
      ieee_write_byte(abfd, ieee_function_minus_enum);
    }

  if (term_count == 1) 
  {
    ieee_write_byte(abfd,0);
  }
  else {    
      while (term_count > 1) {
	  ieee_write_byte(abfd, ieee_function_plus_enum);
	  term_count--;
	}

    }

}








/*****************************************************************************/

/*
writes any integer into the buffer supplied and always takes 5 bytes
*/
static void
DEFUN(ieee_write_int5,(buffer, value),
      bfd_byte*buffer AND
      bfd_vma value )
{
  buffer[0] = (bfd_byte)ieee_number_repeat_4_enum;
  buffer[1] = (value >> 24 ) & 0xff;
  buffer[2] = (value >> 16 ) & 0xff;
  buffer[3] = (value >> 8 ) & 0xff;
  buffer[4] = (value >> 0 ) & 0xff;
}
static void
DEFUN(ieee_write_int5_out, (abfd, value),
      bfd *abfd AND
      bfd_vma value)
{
  bfd_byte b[5];
  ieee_write_int5(b, value);
  bfd_write((PTR)b,1,5,abfd);
}


static boolean 
DEFUN(parse_int,(ieee, value_ptr),
      common_header_type  *ieee AND
      bfd_vma *value_ptr)
{
  int value = this_byte(ieee);
  int result;
  if (value >= 0 && value <= 127) {
    *value_ptr = value;
    next_byte(ieee);
    return true;
  } 
  else if (value >= 0x80 && value <= 0x88) {
    unsigned int count = value & 0xf;
    result = 0;
    next_byte(ieee);
    while (count) {
      result =(result << 8) | this_byte_and_next(ieee);
      count--;
    }
    *value_ptr = result;
    return true;
  } 
  return false;
}
static int
DEFUN(parse_i,(ieee, ok),
   common_header_type *ieee AND
      boolean *ok)
{
  bfd_vma x;
  *ok = parse_int(ieee, &x);
  return x;
}

static bfd_vma 
DEFUN(must_parse_int,(ieee),
     common_header_type *ieee)
{
  bfd_vma result;
  BFD_ASSERT(parse_int(ieee, &result) == true);
  return result;
}

typedef struct 
{
  bfd_vma value;
  asection *section;
  ieee_symbol_index_type symbol;
} ieee_value_type;


static 
reloc_howto_type abs32_howto 
 = HOWTO(1,0,2,32,false,0,false,true,0,"abs32",true,0xffffffff, 0xffffffff,false);
static
reloc_howto_type abs16_howto 
 = HOWTO(1,0,1,16,false,0,false,true,0,"abs16",true,0x0000ffff, 0x0000ffff,false);

static
reloc_howto_type abs8_howto 
 = HOWTO(1,0,0,8,false,0,false,true,0,"abs8",true,0x000000ff, 0x000000ff,false);

static 
reloc_howto_type rel32_howto 
 = HOWTO(1,0,2,32,true,0,false,true,0,"rel32",true,0xffffffff,
	 0xffffffff,false);

static
reloc_howto_type rel16_howto 
 = HOWTO(1,0,1,16,true,0,false,true,0,"rel16",true,0x0000ffff, 0x0000ffff,false);

static
reloc_howto_type rel8_howto 
 = HOWTO(1,0,0,8,true,0,false,true,0,"rel8",true,0x000000ff, 0x000000ff,false);


static ieee_symbol_index_type NOSYMBOL = {  0, 0};


static void
DEFUN(parse_expression,(ieee, value, symbol, pcrel, extra, section),
      ieee_data_type *ieee AND
      bfd_vma *value AND
      ieee_symbol_index_type *symbol AND
      boolean *pcrel AND
      unsigned int *extra AND
      asection **section)

{
#define POS sp[1]
#define TOS sp[0]
#define NOS sp[-1]
#define INC sp++;
#define DEC sp--;

  
  boolean loop = true;
  ieee_value_type stack[10];

  /* The stack pointer always points to the next unused location */
#define PUSH(x,y,z) TOS.symbol=x;TOS.section=y;TOS.value=z;INC;
#define POP(x,y,z) DEC;x=TOS.symbol;y=TOS.section;z=TOS.value;
  ieee_value_type *sp = stack;

  while (loop) {
    switch (this_byte(&(ieee->h))) 
	{
	case ieee_variable_P_enum:
	  /* P variable, current program counter for section n */
	    {
	      int section_n ;
	      next_byte(&(ieee->h));
	      *pcrel = true;
	      section_n  = must_parse_int(&(ieee->h));
	      PUSH(NOSYMBOL, &bfd_abs_section,
		   TOS.value = ieee->section_table[section_n]->vma +
		   ieee_per_section(ieee->section_table[section_n])->pc);
	      break;
	    }
	case ieee_variable_L_enum:
	  /* L variable  address of section N */
	  next_byte(&(ieee->h));
	  PUSH(NOSYMBOL,ieee->section_table[must_parse_int(&(ieee->h))],0);
	  break;
	case ieee_variable_R_enum:
	  /* R variable, logical address of section module */
	  /* FIXME, this should be different to L */
	  next_byte(&(ieee->h));
	  PUSH(NOSYMBOL,ieee->section_table[must_parse_int(&(ieee->h))],0);
	  break;
	case ieee_variable_S_enum:
	  /* S variable, size in MAUS of section module */
	  next_byte(&(ieee->h));
	  PUSH(NOSYMBOL,
	       0,
	       ieee->section_table[must_parse_int(&(ieee->h))]->_raw_size);
	  break;
	  case ieee_variable_I_enum:
	case ieee_variable_X_enum:
	  /* Push the address of external variable n */
	    {
	      ieee_symbol_index_type sy;
	      next_byte(&(ieee->h));
	      sy.index  = (int)(must_parse_int(&(ieee->h))) ;
	      sy.letter = 'X';

	      PUSH(sy, &bfd_und_section, 0);
	    }	
	  break;
	case ieee_function_minus_enum:
	    {
	      bfd_vma value1, value2;
	      asection *section1, *section_dummy;
	      ieee_symbol_index_type sy;
	      next_byte(&(ieee->h));

	      POP(sy, section1, value1);
	      POP(sy, section_dummy, value2);
	      PUSH(sy, section1 ? section1 : section_dummy, value1-value2);
	    }
	  break;
	case ieee_function_plus_enum:
	    {
	      bfd_vma value1, value2;
	      asection *section1;
	      asection *section2;
	      ieee_symbol_index_type sy1;
	      ieee_symbol_index_type sy2;
	      next_byte(&(ieee->h));

	      POP(sy1, section1, value1);
	      POP(sy2, section2, value2);
	      PUSH(sy1.letter ? sy1 : sy2, section1!=&bfd_abs_section ? section1: section2, value1+value2);
	    }
	  break;
	default: 
	    {
	      bfd_vma va;
	      BFD_ASSERT(this_byte(&(ieee->h)) < (int)ieee_variable_A_enum 
			 || this_byte(&(ieee->h)) > (int)ieee_variable_Z_enum);
	      if (parse_int(&(ieee->h), &va)) 
		  {
		    PUSH(NOSYMBOL, &bfd_abs_section, va);
		  }
	      else {
		/* 
		  Thats all that we can understand. As far as I can see
		  there is a bug in the Microtec IEEE output which I'm
		  using to scan, whereby the comma operator is ommited
		  sometimes in an expression, giving expressions with too
		  many terms. We can tell if that's the case by ensuring
		  that sp == stack here. If not, then we've pushed
		  something too far, so we keep adding
		  */

		while (sp != stack+1) {
		  asection *section1;
		  ieee_symbol_index_type sy1;
		  POP(sy1, section1, *extra);
		}
	      {
		asection *dummy;

		POP(*symbol, dummy, *value);
		if (section) *section = dummy;
	      }
		
		loop = false;
	      }
	    }

	}
  }
}



#define ieee_seek(abfd, offset) \
  IEEE_DATA(abfd)->h.input_p = IEEE_DATA(abfd)->h.first_byte + offset

#define ieee_pos(abfd)   IEEE_DATA(abfd)->h.input_p -IEEE_DATA(abfd)->h.first_byte 

static unsigned int last_index;

static ieee_symbol_type *
DEFUN(get_symbol,(abfd, 
		  ieee,  
		  last_symbol,
		  symbol_count,
pptr,
max_index
		  ),
      bfd *abfd AND
      ieee_data_type *ieee AND
      ieee_symbol_type *last_symbol AND
      unsigned int *symbol_count AND
		  ieee_symbol_type *** pptr AND
      unsigned int *max_index
      )
{
  /* Need a new symbol */
  unsigned int new_index = must_parse_int(&(ieee->h));
  if (new_index != last_index) {
    ieee_symbol_type  *   new_symbol = (ieee_symbol_type *)bfd_alloc(ieee->h.abfd,
								     sizeof(ieee_symbol_type));

    new_symbol->index = new_index;
    last_index = new_index;
    ( *symbol_count)++;	
    ** pptr= new_symbol;
    *pptr = &new_symbol->next;
    if (new_index > *max_index) {
      *max_index = new_index;
    }
    return new_symbol;
  }
  return last_symbol;
}
static void
DEFUN(ieee_slurp_external_symbols,(abfd),
      bfd *abfd)
{
  ieee_data_type *ieee = IEEE_DATA(abfd);
  file_ptr offset = ieee->w.r.external_part;


  ieee_symbol_type **prev_symbols_ptr = &ieee->external_symbols;
  ieee_symbol_type **prev_reference_ptr = &ieee->external_reference;
  ieee_symbol_type  *symbol = (ieee_symbol_type *)NULL;
  unsigned int symbol_count = 0;
  boolean loop = true;
  last_index = 0xffffff;
  ieee->symbol_table_full = true;

  ieee_seek(abfd, offset );

  while (loop) {
    switch (this_byte(&(ieee->h))) {
    case ieee_nn_record:
      next_byte(&(ieee->h));
      symbol = get_symbol(abfd, ieee, symbol, &symbol_count,
			  &prev_symbols_ptr, 
			  &ieee->external_symbol_max_index);

      symbol->symbol.the_bfd = abfd;
      symbol->symbol.name = read_id(&(ieee->h));
      symbol->symbol.udata = (PTR)NULL;
      symbol->symbol.flags = BSF_NO_FLAGS;

      
      break;
    case ieee_external_symbol_enum:
      next_byte(&(ieee->h));
					     
      symbol = get_symbol(abfd, ieee, symbol, &symbol_count,
			  &prev_symbols_ptr,
			  &ieee->external_symbol_max_index);


      BFD_ASSERT (symbol->index >= ieee->external_symbol_min_index);

      symbol->symbol.the_bfd = abfd;
      symbol->symbol.name = read_id(&(ieee->h));
      symbol->symbol.udata = (PTR)NULL;
      symbol->symbol.flags = BSF_NO_FLAGS;
      break;
    case ieee_attribute_record_enum >> 8:
	{
	  unsigned int symbol_name_index;
	  unsigned int symbol_type_index;
	  unsigned int symbol_attribute_def;
	  bfd_vma value;
	  next_byte(&(ieee->h)); /* Skip prefix */
	  next_byte(&(ieee->h));
	  symbol_name_index = must_parse_int(&(ieee->h));
	  symbol_type_index = must_parse_int(&(ieee->h));
	  symbol_attribute_def = must_parse_int(&(ieee->h));
	  switch (symbol_attribute_def) {
	  case 63:
	    /* Module misc; followed by two fields which describe the
	       current module block. The first fired is the type id
	       number, the second is the number of asn records
	       associated with the directive */
	    parse_int(&(ieee->h),&value);
	    parse_int(&(ieee->h),&value);
	    break;

	  default:
	    parse_int(&(ieee->h),&value);
	    break;
	  }
	}
      break;
    case ieee_value_record_enum >> 8:
	{
	  unsigned int symbol_name_index;
	  ieee_symbol_index_type symbol_ignore;
	  boolean pcrel_ignore;
	  unsigned int extra;
	  next_byte(&(ieee->h));
	  next_byte(&(ieee->h));

	  symbol_name_index = must_parse_int(&(ieee->h));
	  parse_expression(ieee,
			   &symbol->symbol.value,
			   &symbol_ignore, 
			   &pcrel_ignore, 
			   &extra,
			   &symbol->symbol.section);

	    symbol->symbol.flags  = BSF_GLOBAL | BSF_EXPORT;

	}
      break;
    case ieee_weak_external_reference_enum:
	{ bfd_vma size;
	  bfd_vma value ;
	  next_byte(&(ieee->h));
	  /* Throw away the external reference index */
	  (void)must_parse_int(&(ieee->h));
	  /* Fetch the default size if not resolved */
	  size = must_parse_int(&(ieee->h));
	  /* Fetch the defautlt value if available */
	  if (  parse_int(&(ieee->h), &value) == false) {
	    value = 0;
	  }
	  /* This turns into a common */
	  symbol->symbol.section = &bfd_com_section;
	  symbol->symbol.value = size;
	}
      break;

    case ieee_external_reference_enum: 
      next_byte(&(ieee->h));

      symbol = get_symbol(abfd, ieee, symbol, &symbol_count,
			  &prev_reference_ptr,
			  &ieee->external_reference_max_index);


      symbol->symbol.the_bfd = abfd;
      symbol->symbol.name = read_id(&(ieee->h));
      symbol->symbol.udata = (PTR)NULL;
      symbol->symbol.section = &bfd_und_section;
      symbol->symbol.value = (bfd_vma)0;
      symbol->symbol.flags =  0;

      BFD_ASSERT (symbol->index >= ieee->external_reference_min_index);
      break;

    default:
      loop = false;
    }
  }

  if (ieee->external_symbol_max_index != 0) {
    ieee->external_symbol_count = 
      ieee->external_symbol_max_index -
	ieee->external_symbol_min_index + 1  ;
  }
  else  {
    ieee->external_symbol_count = 0;
  }


  if(ieee->external_reference_max_index != 0) {
    ieee->external_reference_count = 
      ieee->external_reference_max_index -
	ieee->external_reference_min_index + 1;
  }
  else {
    ieee->external_reference_count = 0;
  }

  abfd->symcount =
    ieee->external_reference_count +  ieee->external_symbol_count;

  if (symbol_count != abfd->symcount) {
    /* There are gaps in the table -- */
    ieee->symbol_table_full = false;
  }


  *prev_symbols_ptr = (ieee_symbol_type *)NULL;
  *prev_reference_ptr = (ieee_symbol_type *)NULL;
}

static void
DEFUN(ieee_slurp_symbol_table,(abfd),
      bfd *abfd)
{
  if (IEEE_DATA(abfd)->read_symbols == false) {
    ieee_slurp_external_symbols(abfd);
    IEEE_DATA(abfd)->read_symbols= true;
  }
}

unsigned int
DEFUN(ieee_get_symtab_upper_bound,(abfd),
      bfd *abfd)
{
  ieee_slurp_symbol_table (abfd);

  return (abfd->symcount != 0) ? 
    (abfd->symcount+1) * (sizeof (ieee_symbol_type *)) : 0;
}

/* 
Move from our internal lists to the canon table, and insert in
symbol index order
*/

extern bfd_target ieee_vec;
unsigned int
DEFUN(ieee_get_symtab,(abfd, location),
      bfd *abfd AND
      asymbol **location)
{
  ieee_symbol_type *symp;
  static bfd dummy_bfd;
  static asymbol empty_symbol =
  { &dummy_bfd," ieee empty",(symvalue)0,BSF_DEBUGGING , &bfd_abs_section};

  if (abfd->symcount) 
{
    ieee_data_type *ieee = IEEE_DATA(abfd);
    dummy_bfd.xvec= &ieee_vec;
    ieee_slurp_symbol_table(abfd);

    if (ieee->symbol_table_full == false) {
      /* Arrgh - there are gaps in the table, run through and fill them */
      /* up with pointers to a null place */
      unsigned int i;
      for (i= 0; i < abfd->symcount; i++) {
	location[i] = &empty_symbol;
      }
    }


    ieee->external_symbol_base_offset= -  ieee->external_symbol_min_index;
    for (symp = IEEE_DATA(abfd)->external_symbols;
	 symp != (ieee_symbol_type *)NULL;
	 symp = symp->next) {
      /* Place into table at correct index locations */
      location[symp->index + ieee->external_symbol_base_offset] = &symp->symbol;

    }

    /* The external refs are indexed in a bit */
    ieee->external_reference_base_offset   =
     -  ieee->external_reference_min_index +ieee->external_symbol_count ;

    for (symp = IEEE_DATA(abfd)->external_reference;
	 symp != (ieee_symbol_type *)NULL;
	 symp = symp->next) {
      location[symp->index + ieee->external_reference_base_offset] =
       &symp->symbol;

    }




  }
  location[abfd->symcount] = (asymbol *)NULL;
  return abfd->symcount;
}
static asection *
DEFUN(get_section_entry,(abfd, ieee,index),
 bfd *abfd AND
     ieee_data_type *ieee AND
      unsigned int index)
{
  if (ieee->section_table[index] == (asection *)NULL) {
    asection *section = bfd_make_section(abfd, " tempname");
    ieee->section_table[index] = section;
    section->flags = SEC_NO_FLAGS;
    section->target_index = index;
    ieee->section_table[index] = section;
  }
  return ieee->section_table[index];
}

static void
DEFUN(ieee_slurp_sections,(abfd),
      bfd *abfd)
{
  ieee_data_type *ieee = IEEE_DATA(abfd);
  file_ptr offset = ieee->w.r.section_part;

  asection *section = (asection *)NULL;

  if (offset != 0) {
    bfd_byte section_type[3];
    ieee_seek(abfd, offset);
    while (true) {
      switch (this_byte(&(ieee->h))) {
      case ieee_section_type_enum:
	  {
	    unsigned int section_index ;
	    next_byte(&(ieee->h));
	    section_index = must_parse_int(&(ieee->h));
	    /* Fixme to be nice about a silly number of sections */
	    BFD_ASSERT(section_index < NSECTIONS);

	    section =get_section_entry(abfd, ieee, section_index);

	    section_type[0] =  this_byte_and_next(&(ieee->h));
	    switch (section_type[0]) {
	    case 0xC1:
	      /* Normal attributes for absolute sections	*/
	      section_type[1] = this_byte(&(ieee->h));
	      section->flags = SEC_LOAD | SEC_ALLOC | SEC_HAS_CONTENTS;
	      switch(section_type[1]) {
	      case 0xD3:
		next_byte(&(ieee->h));
		section_type[2] = this_byte(&(ieee->h));
		switch (section_type[2]) 
		    {
		    case 0xD0:
		      /* Normal code */
		      next_byte(&(ieee->h));
		      section->flags |= SEC_LOAD | SEC_CODE;
		      break;
		    case 0xC4:
		      next_byte(&(ieee->h));
		      section->flags |= SEC_LOAD  | SEC_DATA;
		      /* Normal data */
		      break;
		    case 0xD2:
		      next_byte(&(ieee->h));
		      /* Normal rom data */
		      section->flags |= SEC_LOAD | SEC_ROM | SEC_DATA;
		      break;
		    default:
		      break;
		    }
	      }
	      break;
	    case 0xC3:
	      section_type[1] = this_byte(&(ieee->h));
	      section->flags = SEC_LOAD | SEC_ALLOC | SEC_HAS_CONTENTS;
	      switch (section_type[1]) {
	      case 0xD0:
		/* Normal code */
		next_byte(&(ieee->h));
		section->flags |= SEC_LOAD | SEC_CODE;
		break;
	      case 0xC4:
		next_byte(&(ieee->h));
		section->flags |= SEC_LOAD  | SEC_DATA;
		/* Normal data */
		break;
	      case 0xD2:
		next_byte(&(ieee->h));
		/* Normal rom data */
		section->flags |= SEC_LOAD | SEC_ROM | SEC_DATA;
		break;
	      default:
		break;
	      }
	    }
	    section->name = read_id(&(ieee->h));
	      { bfd_vma parent, brother, context;
		parse_int(&(ieee->h), &parent);
		parse_int(&(ieee->h), &brother);
		parse_int(&(ieee->h), &context);
	      }


	  }
	break;
      case ieee_section_alignment_enum:
	  { 
	    unsigned int section_index;
	    bfd_vma value;
	    asection *section;
	    next_byte(&(ieee->h));
	    section_index = must_parse_int(&ieee->h);
	    section = get_section_entry(abfd, ieee, section_index);
	    if (section_index > ieee->section_count) {
	      ieee->section_count = section_index;
	    }
	    section->alignment_power =
	      bfd_log2(must_parse_int(&ieee->h));
	    (void)parse_int(&(ieee->h), & value);
	  }
	break;
      case ieee_e2_first_byte_enum: 
	  {
	    ieee_record_enum_type t = (ieee_record_enum_type)(read_2bytes(&(ieee->h)));

	    switch (t) {
	    case ieee_section_size_enum:
	      section = ieee->section_table[must_parse_int(&(ieee->h))];
	      section->_raw_size = must_parse_int(&(ieee->h));
	      break;
	    case ieee_physical_region_size_enum:
	      section = ieee->section_table[must_parse_int(&(ieee->h))];
	      section->_raw_size = must_parse_int(&(ieee->h));
	      break;
	    case ieee_region_base_address_enum:
	      section = ieee->section_table[must_parse_int(&(ieee->h))];
	      section->vma = must_parse_int(&(ieee->h));
	      break;
	    case ieee_mau_size_enum:
	      must_parse_int(&(ieee->h));
	      must_parse_int(&(ieee->h));
	      break;
	    case ieee_m_value_enum:
	      must_parse_int(&(ieee->h));
	      must_parse_int(&(ieee->h));
	      break;
	    case ieee_section_base_address_enum:
	      section = ieee->section_table[must_parse_int(&(ieee->h))];
	      section->vma = must_parse_int(&(ieee->h));
	      break;
	    case ieee_section_offset_enum:
	      (void) must_parse_int(&(ieee->h));
	      (void) must_parse_int(&(ieee->h));
	      break;
	    default:
	      return;
	    }
	  }
	break;
      default:
	return;
      }
    }
  }
}

/***********************************************************************
*  archive stuff 
*/
bfd_target *
DEFUN(ieee_archive_p,(abfd),
      bfd *abfd)
{
  char *library;
  boolean loop;

  unsigned int i;
uint8e_type buffer[512];
  struct obstack ob;
  file_ptr buffer_offset = 0;
  ieee_ar_data_type *save = abfd->tdata.ieee_ar_data;
  ieee_ar_data_type *ieee ;
  abfd->tdata.ieee_ar_data = (ieee_ar_data_type *)bfd_alloc(abfd, sizeof(ieee_ar_data_type));
  ieee=  IEEE_AR_DATA(abfd);



  bfd_read((PTR)buffer, 1, sizeof(buffer), abfd);

  ieee->h.first_byte = buffer;
  ieee->h.input_p = buffer;

  ieee->h.abfd = abfd;

  if (this_byte(&(ieee->h)) != Module_Beginning) {
    abfd->tdata.ieee_ar_data = save;
      return (bfd_target*)NULL;
    }
  

  next_byte(&(ieee->h));
  library= read_id(&(ieee->h));
  if (strcmp(library , "LIBRARY") != 0) {
    bfd_release(abfd, ieee);
    abfd->tdata.ieee_ar_data = save;
    return (bfd_target *)NULL;
  }
  /* Throw away the filename */
  free( read_id(&(ieee->h)));
  /* This must be an IEEE archive, so we'll buy some space to do
     things */

  obstack_begin(&ob, 128);


  ieee->element_count = 0;
  ieee->element_index = 0;

  next_byte(&(ieee->h));	/* Drop the ad part */
  must_parse_int(&(ieee->h));	/* And the two dummy numbers */
  must_parse_int(&(ieee->h));

  loop = true;
  /* Read the index of the BB table */
  while (loop) {
    ieee_ar_obstack_type t; 
    int rec =read_2bytes(&(ieee->h));
    if (rec ==(int)ieee_assign_value_to_variable_enum) {
      int record_number = must_parse_int(&(ieee->h));
      t.file_offset = must_parse_int(&(ieee->h));
      t.abfd = (bfd *)NULL;
      ieee->element_count++;

      obstack_grow(&ob, (PTR)&t, sizeof(t));

      /* Make sure that we don't go over the end of the buffer */

      if (ieee_pos(abfd) > sizeof(buffer)/2) {
	/* Past half way, reseek and reprime */
	buffer_offset += ieee_pos(abfd);
	bfd_seek(abfd, buffer_offset, SEEK_SET);
	bfd_read((PTR)buffer, 1, sizeof(buffer), abfd);
	ieee->h.first_byte = buffer;
	ieee->h.input_p = buffer;
      }
    }
    else loop = false;
  }

  ieee->elements = (ieee_ar_obstack_type *)obstack_finish(&ob);

  /* Now scan the area again, and replace BB offsets with file */
  /* offsets */


  for (i = 2; i < ieee->element_count; i++) {
    bfd_seek(abfd, ieee->elements[i].file_offset, SEEK_SET);
    bfd_read((PTR)buffer, 1, sizeof(buffer), abfd);
    ieee->h.first_byte = buffer;
    ieee->h.input_p = buffer;
    
    next_byte(&(ieee->h));	/* Drop F8 */
    next_byte(&(ieee->h));	/* Drop 14 */
    must_parse_int(&(ieee->h));	/* Drop size of block */
    if (must_parse_int(&(ieee->h)) != 0) {
      /* This object has been deleted */
      ieee->elements[i].file_offset = 0;
    }
    else {
      ieee->elements[i].file_offset = must_parse_int(&(ieee->h));
    }
  }

  return abfd->xvec;

}

static boolean
DEFUN(ieee_mkobject,(abfd),
      bfd *abfd)
{ 
abfd->tdata.ieee_data = (ieee_data_type *)bfd_zalloc(abfd,sizeof(ieee_data_type));
  

  return true;
}

bfd_target *
DEFUN(ieee_object_p,(abfd),
      bfd *abfd)
{
  char *processor;
  unsigned int part;
  ieee_data_type *ieee;
  uint8e_type buffer[300];
  ieee_data_type *save = IEEE_DATA(abfd);
  abfd->tdata.ieee_data = 0;
  ieee_mkobject(abfd);
  
  ieee = IEEE_DATA(abfd);
  bfd_seek(abfd, (file_ptr) 0, SEEK_SET);
  /* Read the first few bytes in to see if it makes sense */
  bfd_read((PTR)buffer, 1, sizeof(buffer), abfd);

  ieee->h.input_p = buffer;
  if (this_byte_and_next(&(ieee->h)) != Module_Beginning) goto fail;

  ieee->read_symbols= false;
  ieee->read_data= false;
  ieee->section_count = 0;
  ieee->external_symbol_max_index = 0;
  ieee->external_symbol_min_index = IEEE_PUBLIC_BASE;
  ieee->external_reference_min_index =IEEE_REFERENCE_BASE;
  ieee->external_reference_max_index = 0;
  ieee->h.abfd = abfd;
  memset((PTR)ieee->section_table, 0,	 sizeof(ieee->section_table));

  processor = ieee->mb.processor = read_id(&(ieee->h));
  if (strcmp(processor,"LIBRARY") == 0) goto fail;
  ieee->mb.module_name = read_id(&(ieee->h));
  if (abfd->filename == (CONST char *)NULL) {
    abfd->filename =  ieee->mb.module_name;
  }
  /* Determine the architecture and machine type of the object file.
     */
    {
      bfd_arch_info_type *arch = bfd_scan_arch(processor);
      if (arch == 0) goto fail;
      abfd->arch_info = arch;
    }

  if (this_byte(&(ieee->h)) != (int)ieee_address_descriptor_enum) {
    goto fail;
  }
  next_byte(&(ieee->h));	

  if (parse_int(&(ieee->h), &ieee->ad.number_of_bits_mau) == false) {
    goto fail;
  }
  if(parse_int(&(ieee->h), &ieee->ad.number_of_maus_in_address) == false) {
    goto fail;
  }

  /* If there is a byte order info, take it */
  if (this_byte(&(ieee->h)) == (int)ieee_variable_L_enum ||
      this_byte(&(ieee->h)) == (int)ieee_variable_M_enum)
    next_byte(&(ieee->h));


  for (part = 0; part < N_W_VARIABLES; part++) {
    boolean ok;
    if (read_2bytes(&(ieee->h)) != (int) ieee_assign_value_to_variable_enum) {
      goto fail;
    }
    if (this_byte_and_next(&(ieee->h)) != part)  {
      goto fail;
    }

    ieee->w.offset[part] = parse_i(&(ieee->h), &ok);
    if (ok==false) {
      goto fail;
    }

  }
  abfd->flags = HAS_SYMS;
/* By now we know that this is a real IEEE file, we're going to read
   the whole thing into memory so that we can run up and down it
   quickly. We can work out how big the file is from the trailer
   record */

  IEEE_DATA(abfd)->h.first_byte = (uint8e_type *) bfd_alloc(ieee->h.abfd, ieee->w.r.me_record
					    + 50);
  bfd_seek(abfd, (file_ptr) 0, SEEK_SET);
  bfd_read((PTR)(IEEE_DATA(abfd)->h.first_byte), 1,   ieee->w.r.me_record+50,  abfd);

  ieee_slurp_sections(abfd);
  return abfd->xvec;
 fail:
  (void)  bfd_release(abfd, ieee);
abfd->tdata.ieee_data = save;
  return (bfd_target *)NULL;
}


void 
DEFUN(ieee_print_symbol,(ignore_abfd, afile,  symbol, how),
      bfd *ignore_abfd AND
      PTR afile AND
      asymbol *symbol AND
      bfd_print_symbol_type how)
{
  FILE *file = (FILE *)afile;

  switch (how) {
  case bfd_print_symbol_name:
    fprintf(file,"%s", symbol->name);
    break;
  case bfd_print_symbol_more:
#if 0
    fprintf(file,"%4x %2x",aout_symbol(symbol)->desc & 0xffff,
	    aout_symbol(symbol)->other  & 0xff);
#endif
    BFD_FAIL();
    break;
  case bfd_print_symbol_nm:
  case bfd_print_symbol_all:
      {
	CONST char *section_name = symbol->section == (asection *)NULL ?
	  (CONST char *)"*abs" : symbol->section->name;
	if (symbol->name[0] == ' ') {
	  fprintf(file,"* empty table entry ");
	}
	else {
	  bfd_print_symbol_vandf((PTR)file,symbol);

	  fprintf(file," %-5s %04x %02x %s",
		  section_name,
		  (unsigned)	      ieee_symbol(symbol)->index,
		  (unsigned)	      0, /*
					   aout_symbol(symbol)->desc & 0xffff,
					   aout_symbol(symbol)->other  & 0xff,*/
		  symbol->name);
	}
      }
    break;
  }
}


static void
DEFUN(do_one,(ieee, current_map, location_ptr,s),
      ieee_data_type *ieee AND
      ieee_per_section_type *current_map AND
      uint8e_type *location_ptr AND
      asection *s)
{
  switch (this_byte(&(ieee->h)))  
      {
      case ieee_load_constant_bytes_enum:
	  {
	    unsigned int number_of_maus;
	    unsigned int i;
	    next_byte(&(ieee->h));
	    number_of_maus = must_parse_int(&(ieee->h));

	    for (i = 0; i < number_of_maus; i++) {
	      location_ptr[current_map->pc++]= this_byte(&(ieee->h));
	      next_byte(&(ieee->h));
	    }
	  }
	break;

      case ieee_load_with_relocation_enum:
	  {
	    boolean loop = true;
	    next_byte(&(ieee->h));
	    while (loop) 
		{
		  switch (this_byte(&(ieee->h))) 
		      {
		      case ieee_variable_R_enum:

		      case ieee_function_signed_open_b_enum:
		      case ieee_function_unsigned_open_b_enum:
		      case ieee_function_either_open_b_enum:
			  {
			    unsigned int extra = 4;
			    boolean pcrel = false;
asection *section;
			    ieee_reloc_type *r = 
			      (ieee_reloc_type *) bfd_alloc(ieee->h.abfd,
							    sizeof(ieee_reloc_type));

			    *(current_map->reloc_tail_ptr) = r;
			    current_map->reloc_tail_ptr= &r->next;
			    r->next = (ieee_reloc_type *)NULL;
			    next_byte(&(ieee->h));
/*			    abort();*/
			    
			    parse_expression(ieee,
					     &r->relent.addend,
					     &r->symbol,
					     &pcrel, &extra, &section);
			    r->relent.address = current_map->pc;
			    s->reloc_count++;
if (r->relent.sym_ptr_ptr == 0) {
			     r->relent.sym_ptr_ptr = section->symbol_ptr_ptr;
}			    

			    if (this_byte(&(ieee->h)) == (int)ieee_comma) {
			      next_byte(&(ieee->h));
			      /* Fetch number of bytes to pad */
			      extra = must_parse_int(&(ieee->h));
			    };
		   
			    switch (this_byte(&(ieee->h))) {
			    case ieee_function_signed_close_b_enum:
			      next_byte(&(ieee->h));
			      break;
			    case ieee_function_unsigned_close_b_enum:
			      next_byte(&(ieee->h));
			      break;
			    case ieee_function_either_close_b_enum:
			      next_byte(&(ieee->h));
			      break;
			    default:
			      break;
			    }
			    /* Build a relocation entry for this type */
			    /* If pc rel then stick -ve pc into instruction
			       and take out of reloc ..

			       I've changed this. It's all too
			       complicated. I keep 0 in the
			       instruction  now.
			       */
		    
			    switch (extra) 
				{
				case 0:
				case 4:
				  
				  if (pcrel == true) 
				      {
#if KEEPMINUSPCININST
					bfd_put_32(ieee->h.abfd, -current_map->pc, location_ptr +
						    current_map->pc);
					r->relent.howto = &rel32_howto;
					r->relent.addend -=
					  current_map->pc;
#else
					bfd_put_32(ieee->h.abfd,0, location_ptr +
						    current_map->pc);
					r->relent.howto = &rel32_howto;
#endif
				      }
				  else 
				      {
					bfd_put_32(ieee->h.abfd, 0, location_ptr +
						    current_map->pc);
					r->relent.howto = &abs32_howto;
				      }
				  current_map->pc +=4;
				  break;
				case 2:
				  if (pcrel == true) {
#if KEEPMINUSPCININST
				    bfd_put_16(ieee->h.abfd, (int)(-current_map->pc),  location_ptr +current_map->pc);
				    r->relent.addend -= current_map->pc;
				    r->relent.howto = &rel16_howto;
#else

				    bfd_put_16(ieee->h.abfd, 0,  location_ptr +current_map->pc);
				    r->relent.howto = &rel16_howto;
#endif
				  }

				  else {
				    bfd_put_16(ieee->h.abfd, 0,  location_ptr +current_map->pc);
				    r->relent.howto = &abs16_howto;
				  }
				  current_map->pc +=2;
				  break;
				case 1:
				  if (pcrel == true) {
#if KEEPMINUSPCININST
				    bfd_put_8(ieee->h.abfd, (int)(-current_map->pc),  location_ptr +current_map->pc);
				    r->relent.addend -= current_map->pc;
				    r->relent.howto = &rel8_howto;
#else
				    bfd_put_8(ieee->h.abfd,0,  location_ptr +current_map->pc);
				    r->relent.howto = &rel8_howto;
#endif
				  }
				  else {
				    bfd_put_8(ieee->h.abfd, 0,  location_ptr +current_map->pc);
				    r->relent.howto = &abs8_howto;
				  }
				  current_map->pc +=1;
				  break;

				default:
				  BFD_FAIL();
				  break;
				}
			  }
			break;
		      default: 
			  {
			    bfd_vma this_size ;
			    if (parse_int(&(ieee->h), &this_size) == true) {
			      unsigned int i;
			      for (i = 0; i < this_size; i++) {
				location_ptr[current_map->pc ++] = this_byte(&(ieee->h));
				next_byte(&(ieee->h));
			      }
			    }
			    else {
			      loop = false;
			    }
			  }
		      }
		}
	  }
      }
}

/* Read in all the section data and relocation stuff too */
static boolean 
DEFUN(ieee_slurp_section_data,(abfd),
      bfd *abfd)
{
  bfd_byte *location_ptr = (bfd_byte *)NULL;
  ieee_data_type *ieee = IEEE_DATA(abfd);
  unsigned int section_number ;

  ieee_per_section_type *current_map = (ieee_per_section_type *)NULL;
  asection *s;
  /* Seek to the start of the data area */
  if (ieee->read_data== true)  return true;
  ieee->read_data = true;
  ieee_seek(abfd, ieee->w.r.data_part);

  /* Allocate enough space for all the section contents */


  for (s = abfd->sections; s != (asection *)NULL; s = s->next) {
    ieee_per_section_type *per = (ieee_per_section_type *) s->used_by_bfd;
    per->data = (bfd_byte *) bfd_alloc(ieee->h.abfd, s->_raw_size);
    /*SUPPRESS 68*/
    per->reloc_tail_ptr =
      (ieee_reloc_type **)&(s->relocation);
  }



  while (true) {
    switch (this_byte(&(ieee->h))) 
	{
	  /* IF we see anything strange then quit */
	default:
	  return true;

	case ieee_set_current_section_enum:
	  next_byte(&(ieee->h));
	  section_number = must_parse_int(&(ieee->h));
	  s = ieee->section_table[section_number];
	  current_map = (ieee_per_section_type *) s->used_by_bfd;
	  location_ptr = current_map->data - s->vma;
	  /* The document I have says that Microtec's compilers reset */
	  /* this after a sec section, even though the standard says not */
	  /* to. SO .. */
	  current_map->pc =s->vma;
	  break;


	case ieee_e2_first_byte_enum:
	  next_byte(&(ieee->h));
	  switch (this_byte(&(ieee->h)))
	      {
	      case ieee_set_current_pc_enum & 0xff:
		  {
		    bfd_vma value;
		    asection *dsection;
		    ieee_symbol_index_type symbol;
		    unsigned int extra;
		    boolean pcrel;
		    next_byte(&(ieee->h));
		    must_parse_int(&(ieee->h)); /* Thow away section #*/
		    parse_expression(ieee, &value,
				     &symbol,
				     &pcrel, &extra,
				     0);
		    current_map->pc = value;
		    BFD_ASSERT((unsigned)(value - s->vma) <= s->_raw_size);
		  }
		break;

	      case ieee_value_starting_address_enum & 0xff:
		/* We've got to the end of the data now - */
		return true;
	      default:
		BFD_FAIL();
		return true;
	      }
	  break;
	case ieee_repeat_data_enum:
	    {
	      /* Repeat the following LD or LR n times - we do this by
		 remembering the stream pointer before running it and
		 resetting it and running it n times. We special case
		 the repetition of a repeat_data/load_constant
		 */

	      unsigned int iterations ;
	      uint8e_type *start ;
	      next_byte(&(ieee->h));
	      iterations = must_parse_int(&(ieee->h));
	      start =  ieee->h.input_p;
	      if (start[0] == (int)ieee_load_constant_bytes_enum &&
		  start[1] == 1) {
		while (iterations != 0) {
		  location_ptr[current_map->pc++] = start[2];
		  iterations--;
		}
		next_byte(&(ieee->h));
		next_byte(&(ieee->h));
		next_byte(&(ieee->h));
	      }
	      else {
		while (iterations != 0) {
		  ieee->h.input_p = start;
		  do_one(ieee, current_map, location_ptr,s);
		  iterations --;
		}
	      }
	    }
	  break;
	case ieee_load_constant_bytes_enum:
	case ieee_load_with_relocation_enum:
	    {
	      do_one(ieee, current_map, location_ptr,s);
	    }
	}
  }
}





boolean
DEFUN(ieee_new_section_hook,(abfd, newsect),
      bfd *abfd AND
      asection *newsect)
{
  newsect->used_by_bfd = (PTR)
    bfd_alloc(abfd, sizeof(ieee_per_section_type));
  ieee_per_section( newsect)->data = (bfd_byte *)NULL;
  ieee_per_section(newsect)->section = newsect;
  return true;
}


unsigned int
DEFUN(ieee_get_reloc_upper_bound,(abfd, asect),
      bfd *abfd AND
      sec_ptr asect)
{
  ieee_slurp_section_data(abfd);
  return (asect->reloc_count+1) * sizeof(arelent *);
}

static boolean
DEFUN(ieee_get_section_contents,(abfd, section, location, offset, count),
      bfd *abfd AND
      sec_ptr section AND
      PTR location AND
      file_ptr offset AND
      bfd_size_type count)
{
  ieee_per_section_type *p = (ieee_per_section_type *) section->used_by_bfd;
  ieee_slurp_section_data(abfd);
  (void)  memcpy((PTR)location, (PTR)(p->data + offset), (unsigned)count);
  return true;
}


unsigned int
DEFUN(ieee_canonicalize_reloc,(abfd, section, relptr, symbols),
      bfd *abfd AND
      sec_ptr section AND
      arelent **relptr AND
      asymbol **symbols)
{
/*  ieee_per_section_type *p = (ieee_per_section_type *) section->used_by_bfd;*/
  ieee_reloc_type *src = (ieee_reloc_type *)(section->relocation);
  ieee_data_type *ieee = IEEE_DATA(abfd);

  while (src != (ieee_reloc_type *)NULL) {
    /* Work out which symbol to attach it this reloc to */
    switch (src->symbol.letter) {
    case 'X':
      src->relent.sym_ptr_ptr =
	symbols + src->symbol.index +  ieee->external_reference_base_offset;
      break;
    case 0:
      src->relent.sym_ptr_ptr = bfd_abs_section.symbol_ptr_ptr;
      break;
    default:

      BFD_FAIL();
    }
    *relptr++ = &src->relent;
    src = src->next;
  }
  *relptr = (arelent *)NULL;
  return section->reloc_count;
}



static int 
DEFUN(comp,(ap, bp),
     CONST PTR ap AND
     CONST PTR bp)
{
  arelent *a = *((arelent **)ap);
  arelent *b = *((arelent **)bp);
  return a->address - b->address;
}

/*
Write the section headers
*/

static void
DEFUN(ieee_write_section_part,(abfd),
      bfd *abfd)
{
  ieee_data_type *ieee = IEEE_DATA(abfd);
  asection *s;
  ieee->w.r.section_part = bfd_tell(abfd);
  for (s = abfd->sections; s != (asection *)NULL; s=s->next) {
      if (s != &bfd_abs_section) 
      {
	
	ieee_write_byte(abfd, ieee_section_type_enum);
	ieee_write_byte(abfd, s->index + IEEE_SECTION_NUMBER_BASE);

	if (abfd->flags & EXEC_P) 
	{
	  /* This image is executable, so output absolute sections */
	  ieee_write_byte(abfd, ieee_variable_A_enum);
	  ieee_write_byte(abfd, ieee_variable_S_enum);
	}
	else  
	{
	  ieee_write_byte(abfd, ieee_variable_C_enum);
	}

	switch (s->flags &(SEC_CODE | SEC_DATA | SEC_ROM)) 
	{
	case SEC_CODE | SEC_LOAD:
	case SEC_CODE:
	  ieee_write_byte(abfd, ieee_variable_P_enum);
	  break;
	case SEC_DATA:
	default:
	  ieee_write_byte(abfd, ieee_variable_D_enum);
	  break;
	case SEC_ROM:
	case SEC_ROM | SEC_DATA:
	case SEC_ROM | SEC_LOAD:
	case SEC_ROM | SEC_DATA | SEC_LOAD:

	  ieee_write_byte(abfd, ieee_variable_R_enum);
	}


	ieee_write_id(abfd, s->name);
#if 0
	ieee_write_int(abfd, 0); /* Parent */
	ieee_write_int(abfd, 0); /* Brother */
	ieee_write_int(abfd, 0); /* Context */
#endif
	/* Alignment */
	ieee_write_byte(abfd, ieee_section_alignment_enum);
	ieee_write_byte(abfd, s->index + IEEE_SECTION_NUMBER_BASE);
	ieee_write_int(abfd, 1 << s->alignment_power);

	/* Size */
	ieee_write_2bytes(abfd, ieee_section_size_enum);
	ieee_write_byte(abfd, s->index + IEEE_SECTION_NUMBER_BASE);
	ieee_write_int(abfd, s->_raw_size);
	if (abfd->flags & EXEC_P) {
	    /* Relocateable sections don't have asl records */
	    /* Vma */
	    ieee_write_2bytes(abfd, ieee_section_base_address_enum);
	    ieee_write_byte(abfd, s->index + IEEE_SECTION_NUMBER_BASE);
	    ieee_write_int(abfd, s->vma);
	  }
      }
      
    }
}



static void 
DEFUN(do_with_relocs,(abfd, s),
      bfd *abfd AND
      asection *s)
{
  unsigned int relocs_to_go = s->reloc_count;


  bfd_byte *stream = ieee_per_section(s)->data;
  arelent **p = s->orelocation;

  bfd_size_type current_byte_index = 0;

  qsort(s->orelocation,
	relocs_to_go,
	sizeof(arelent **),
	comp);

  /* Output the section preheader */
  ieee_write_byte(abfd, ieee_set_current_section_enum);
  ieee_write_byte(abfd, s->index + IEEE_SECTION_NUMBER_BASE);

  ieee_write_twobyte(abfd, ieee_set_current_pc_enum);
  ieee_write_byte(abfd, s->index + IEEE_SECTION_NUMBER_BASE);
  ieee_write_expression(abfd, 0, s->symbol, 0, 0);

  if (relocs_to_go == 0) 
      {
	/* If there arn't any relocations then output the load constant byte
	   opcode rather than the load with relocation opcode */

	while (current_byte_index < s->_raw_size) {
	  bfd_size_type run;
	  unsigned int MAXRUN  = 32;
	  run = MAXRUN;
	  if (run > s->_raw_size - current_byte_index) {
	    run = s->_raw_size - current_byte_index;
	  }

	  if (run != 0) {
	    ieee_write_byte(abfd, ieee_load_constant_bytes_enum);
	    /* Output a stream of bytes */
	    ieee_write_int(abfd, run);
	    bfd_write((PTR)(stream + current_byte_index), 
		      1,
		      run,
		      abfd);
	    current_byte_index += run;
	  }
	}
      }
  else 
      {
	ieee_write_byte(abfd, ieee_load_with_relocation_enum);


	/* Output the data stream as the longest sequence of bytes
	   possible, allowing for the a reasonable packet size and
	   relocation stuffs */

	if ((PTR)stream == (PTR)NULL) {
	  /* Outputting a section without data, fill it up */
	  stream = (uint8e_type *)(bfd_alloc(abfd, s->_raw_size));
	  memset((PTR)stream, 0, s->_raw_size);
	}
	while (current_byte_index < s->_raw_size) {
	  bfd_size_type run;
	  unsigned int MAXRUN = 32;
	  if (relocs_to_go) {
	    run = (*p)->address - current_byte_index;
	  }
	  else {
	    run = MAXRUN;
	  }
	  if (run > s->_raw_size - current_byte_index) {
	    run = s->_raw_size - current_byte_index;
	  }

	  if (run != 0) {
	    /* Output a stream of bytes */
	    ieee_write_int(abfd, run);
	    bfd_write((PTR)(stream + current_byte_index), 
		      1,
		      run,
		      abfd);
	    current_byte_index += run;
	  }
	  /* Output any relocations here */
	  if (relocs_to_go && (*p) && (*p)->address == current_byte_index) {
	    while (relocs_to_go && (*p) && (*p)->address == current_byte_index) {

	      arelent *r = *p;
	      bfd_vma ov;

#if 0
	      if (r->howto->pc_relative) {
		r->addend += current_byte_index ;
	      }
#endif

	      switch (r->howto->size) {
	      case 2:

		ov = bfd_get_32(abfd,
				stream+current_byte_index);
		current_byte_index +=4;
		break;
	      case 1:
		ov = bfd_get_16(abfd,
				stream+current_byte_index);
		current_byte_index +=2;
		break;
	      case 0:
		ov = bfd_get_8(abfd,
			       stream+current_byte_index);
		current_byte_index ++;
		break;
	      default:
		ov = 0;
		BFD_FAIL();
	      }
	      ieee_write_byte(abfd, ieee_function_either_open_b_enum);
	      abort();
	    
	      if (r->sym_ptr_ptr != (asymbol **)NULL) {
		ieee_write_expression(abfd, r->addend + ov,
				      *(r->sym_ptr_ptr),
				      r->howto->pc_relative, s->index);
	      }
	      else {
		ieee_write_expression(abfd, r->addend + ov,
				      (asymbol *)NULL,
				      r->howto->pc_relative, s->index);
	      }

	      if (1 || r->howto->size != 2) {
		ieee_write_byte(abfd, ieee_comma);
		ieee_write_int(abfd, 1<< r->howto->size);
	      }
	      ieee_write_byte(abfd,
			      ieee_function_either_close_b_enum);

	      relocs_to_go --;
	      p++;
	    }

	  }
	}
      }
}

/* If there are no relocations in the output section then we can
be clever about how we write. We block items up into a max of 127
bytes */

static void 
DEFUN(do_as_repeat, (abfd, s),
      bfd *abfd AND
      asection *s)
{
  ieee_write_byte(abfd, ieee_set_current_section_enum);
  ieee_write_byte(abfd, s->index + IEEE_SECTION_NUMBER_BASE);
  ieee_write_byte(abfd, ieee_set_current_pc_enum >> 8);
 ieee_write_byte(abfd, ieee_set_current_pc_enum  & 0xff);
  ieee_write_byte(abfd, s->index + IEEE_SECTION_NUMBER_BASE);
  ieee_write_int(abfd,  s->vma );

  ieee_write_byte(abfd,ieee_repeat_data_enum);
  ieee_write_int(abfd, s->_raw_size);
  ieee_write_byte(abfd, ieee_load_constant_bytes_enum);
  ieee_write_byte(abfd, 1);
  ieee_write_byte(abfd, 0);
}

static void 
DEFUN(do_without_relocs, (abfd, s),
      bfd *abfd AND
      asection *s)
{
  bfd_byte *stream = ieee_per_section(s)->data;

  if (stream == 0 || ((s->flags & SEC_LOAD) == 0)) 
      {
	do_as_repeat(abfd, s);
      }
  else 
      {
	unsigned int i;
	for (i = 0; i < s->_raw_size; i++) {
	  if (stream[i] != 0) {
	    do_with_relocs(abfd, s);
	    return;
	  }
	}
	do_as_repeat(abfd, s);
      }
  
}


static unsigned char *output_ptr_start;
static unsigned char *output_ptr;
static unsigned char *output_ptr_end;
static unsigned char *input_ptr_start;
static unsigned char *input_ptr;
static unsigned char *input_ptr_end;
static bfd *input_bfd;
static bfd *output_bfd;
static int output_buffer;

static void fill()
{
  bfd_read((PTR)input_ptr_start, 1, input_ptr_end - input_ptr_start, input_bfd);
  input_ptr = input_ptr_start;
}
static void flush()
{
  bfd_write((PTR)(output_ptr_start),1,output_ptr - output_ptr_start, output_bfd);
  output_ptr = output_ptr_start;
  output_buffer++;
}

#define THIS() ( *input_ptr )
#define NEXT() { input_ptr++; if (input_ptr == input_ptr_end) fill(); }
#define OUT(x) { *output_ptr++ = (x); if(output_ptr == output_ptr_end)  flush(); }

static void write_int(value)
int value;
{
  if (value >= 0 && value <= 127) {
    OUT(value);
  }
  else {
    unsigned int length;
    /* How many significant bytes ? */
    /* FIXME FOR LONGER INTS */
    if (value & 0xff000000) {
      length = 4;
    }
    else if (value & 0x00ff0000) {
      length  = 3;
    }
    else if (value & 0x0000ff00) {
      length = 2;
    }
    else length = 1;

    OUT((int)ieee_number_repeat_start_enum + length);
    switch (length) {
    case 4:
      OUT( value >> 24);
    case 3:
      OUT( value >> 16);
    case 2:
      OUT( value >> 8);
    case 1:
      OUT( value);
    }

  }
}
static void copy_id() 
{
  int length  = THIS();
  char ch;
  OUT(length);
  NEXT();
  while (length--) {
    ch = THIS();
    OUT(ch);
    NEXT();
  }
}
#define VAR(x) ((x | 0x80))
static void copy_expression()
{
  int stack[10];
  int *tos = stack;
  int value = 0;
  while (1) {
    switch (THIS()) {
    case 0x84:
      NEXT();
      value =  THIS(); NEXT();
      value = (value << 8) | THIS(); NEXT();
      value = (value << 8) | THIS(); NEXT();
      value = (value << 8) | THIS(); NEXT();
      *tos ++ = value;
      break;
    case 0x83:
      NEXT();
      value =  THIS(); NEXT();
      value = (value << 8) | THIS(); NEXT();
      value = (value << 8) | THIS(); NEXT();
      *tos ++ = value;
      break;
    case 0x82:	
      NEXT();
      value =  THIS(); NEXT();
      value = (value << 8) | THIS(); NEXT();
      *tos ++ = value;
      break;
    case 0x81:
      NEXT();
      value =  THIS(); NEXT();
      *tos ++ = value;
      break;
      case 0x80:
      NEXT();
      *tos ++ = 0;
      break;
    default:
      if (THIS() >0x84) {
	/* Not a number, just bug out with the answer */
	write_int(*(--tos));
	return;
      }
      *tos++ = THIS();
NEXT();
      value = 0;
      break;
    case 0xa5:
      /* PLUS anything */
	{
	  int value = *(--tos);
	  value += *(--tos);
	  *tos++ = value;
	  NEXT();
	}
      break;
    case VAR('R') :
	{
	  int section_number ;
	  ieee_data_type *ieee;
	  asection *s;
	  NEXT();
	  section_number = THIS();
	
	  NEXT();
	  ieee= IEEE_DATA(input_bfd);
	  s = 	ieee->section_table[section_number];
	  if (s->output_section) {
	    value = s->output_section->vma ;
	  } else { value = 0; }
	  value += s->output_offset;
	  *tos++ = value;
	  value = 0;
	}
      break;
    case 0x90:
	{	
	  NEXT();
	  write_int(*(--tos));
	  OUT(0x90);
	  return;

	}
    }
  }

}

/* Drop the int in the buffer, and copy a null into the gap, which we
   will overwrite later */

struct output_buffer_struct {
unsigned  char *ptrp;
  int buffer;
} ;

static void
DEFUN(fill_int,(buf),
      struct output_buffer_struct *buf)
{
  if (buf->buffer == output_buffer) {
    /* Still a chance to output the size */
    int value = output_ptr - buf->ptrp + 3;
    buf->ptrp[0] =  value >> 24;
    buf->ptrp[1] =  value >> 16;
    buf->ptrp[2] =  value >> 8;
    buf->ptrp[3] =  value >> 0;
  }

}
static void
DEFUN(drop_int,(buf),
      struct output_buffer_struct *buf)
{
  int type = THIS();
  int ch;
  if (type <= 0x84) {
    NEXT();
    switch(type) {
    case 0x84: ch = THIS(); NEXT();
    case 0x83: ch = THIS(); NEXT();
    case 0x82: ch = THIS(); NEXT();
    case 0x81: ch = THIS(); NEXT();
    case 0x80: break;
    }
  }
  OUT(0x84);
  buf->ptrp = output_ptr;
  buf->buffer  = output_buffer;
  OUT(0);OUT(0);OUT(0);OUT(0);
}

static void copy_int()
{
  int type = THIS();
  int ch;
  if (type <= 0x84) {
    OUT(type);
    NEXT();
    switch(type) {
    case 0x84: ch = THIS(); NEXT(); OUT(ch);
    case 0x83: ch = THIS(); NEXT(); OUT(ch);
    case 0x82: ch = THIS(); NEXT(); OUT(ch);
    case 0x81: ch = THIS(); NEXT(); OUT(ch);
    case 0x80: break;
    }
  }
}

#define ID copy_id()
#define INT copy_int()
#define EXP copy_expression()
static void copy_till_end();
#define INTn(q) copy_int()
#define EXPn(q) copy_expression()
static void f1_record()
{
  int ch;
  /* ATN record */
  NEXT();
  ch = THIS();
  switch (ch)
      {
      default:
	OUT(0xf1); OUT(ch);
	break;
      case 0xc9:
	NEXT();
	OUT(0xf1); OUT(0xc9);
	INT; INT; ch = THIS(); 
	switch (ch) 
	    {
	    case 0x16: NEXT();break;
	    case 0x01: NEXT();break;
	    case 0x00: NEXT(); INT; break;
	    case 0x03: NEXT(); INT; break;
	    case 0x13: EXPn(instruction address); break;
	    default:
	      break;
	    }
	break;
      case 0xd8:
	/* EXternal ref */
	NEXT();	
	OUT(0xf1); OUT(0xd8);
	EXP ; EXP; EXP; EXP;
	break;
      case 0xce:
	NEXT();
	OUT(0xf1);OUT(0xce); INT; INT; ch = THIS(); INT;
	switch (ch) {
	case 0x01:
	  INT; INT; break;
	case 0x02:
	  INT; break;
	case 0x04:
	  EXPn(external function); break;
	case 0x05:
	  break;
	case 0x07: INTn(line number); INT;
	case 0x08: break;
	case 0x0a: INTn(locked register); INT; break;
	case 0x3f: copy_till_end(); break;
	case 0x3e: copy_till_end(); break;
	case 0x40: copy_till_end(); break;
	case 0x41: ID; break;
	}
      }

}
static void f0_record()
{
  /* Attribute record */
  NEXT();
  OUT(0xf0);
  INTn(Symbol name );
  ID;
}
static void copy_till_end()
{
  int  ch = THIS();
  while (1) {
    while (ch <= 0x80) 
	{
	  OUT(ch);
	  NEXT();
	  ch = THIS();
	}
    switch (ch) {
    case 0x84:
      OUT(THIS());
      NEXT();
    case 0x83:
      OUT(THIS());
      NEXT();
    case 0x82:
      OUT(THIS());
      NEXT();
    case 0x81:
      OUT(THIS());
      NEXT();
      OUT(THIS());
      NEXT();

      ch = THIS();
      break;
    default:
      return;
    }
  }    

}

static void f2_record()
{
  NEXT();
  OUT(0xf2);
  INT ;
  NEXT();
  OUT(0xce);
  INT ;
  copy_till_end();
}


static void block();
static void f8_record()
{
  int ch;
  NEXT();
  ch = THIS();
  switch (ch) 
      {
      case 0x01:
      case 0x02:
      case 0x03:
	/* Unique typedefs for module */
	/* GLobal typedefs  */
	/* High level module scope beginning */
	  {
	    struct output_buffer_struct ob;
	    NEXT();
	    OUT(0xf8); OUT(ch);
	    drop_int(&ob); ID ;

	    block();

	    NEXT();
	    fill_int(&ob);
	    OUT(0xf9);
	  }
	break;
      case 0x04:	
	/* Global function */
	  {
	    struct output_buffer_struct ob;
	    NEXT();
	    OUT(0xf8); OUT(0x04);
	    drop_int(&ob); ID ; INTn(stack size); INTn(ret val);
	    EXPn(offset); 

	    block();

	    NEXT();
	    OUT(0xf9);	
	    EXPn(size of block);
	    fill_int(&ob);
	  }
	break;

      case 0x05:
	/* File name for source line numbers */
	  {
	    struct output_buffer_struct ob;
	    NEXT();
	    OUT(0xf8); OUT(0x05);
	    drop_int(&ob);
	    ID; INTn(year); INTn(month); INTn(day);
	    INTn(hour); INTn(monute); INTn(second);
	    block();
	    NEXT();
	    OUT(0xf9);
	    fill_int(&ob);
	  }
	break;
	
      case 0x06:
	/* Local function */
	  { struct output_buffer_struct ob;
	    NEXT(); OUT(0xf8); OUT(0x06);
	    drop_int(&ob);
	    ID; INTn(stack size); INTn(type return);
	    EXPn(offset);
	    block();
	    NEXT();
	    OUT(0xf9);
	    EXPn(size);
	    fill_int(&ob);
	  }
	break;
	
      case 0x0a:
	/* Assembler module scope beginning -*/
	  { struct output_buffer_struct ob;

	    NEXT();
	    OUT(0xf8); OUT(0x0a); 
	    drop_int(&ob);
	      ID; ID; INT; ID; INT; INT; INT; INT; INT; INT;

	    block();	

	    NEXT();
	    OUT(0xf9);
	    fill_int(&ob);
	  }
	break;
      case 0x0b:
	  {
	    struct output_buffer_struct ob;
	    NEXT();
	    OUT(0xf8); OUT(0x0b); 
	    drop_int(&ob); ID ; INT; INTn(section index); EXPn(offset); INTn(stuff);

	    block();

	    OUT(0xf9);
	    NEXT();      
	    EXPn(Size in Maus);
	    fill_int(&ob);
	  }
	break;
      }
}

static void e2_record()
{
  OUT(0xe2);
  NEXT();
  OUT(0xce);
  NEXT();
  INT;
  EXP;
}

static void DEFUN_VOID(block)
{
  int ch ;
  while (1) {
    ch = THIS();
    switch (ch) {
    case 0xe1:
    case 0xe5:
      return;
    case 0xf9:
      return;
    case 0xf0:
      f0_record();
      break;
    case 0xf1:
      f1_record();    
      break;
    case 0xf2:
      f2_record();
      break;
    case 0xf8:
      f8_record();
      break;
    case 0xe2:
      e2_record();
      break;

    }  
  }
}
  


/* relocate_debug, 
   moves all the debug information from the source bfd to the output
   bfd, and relocates any expressions it finds
*/

static void
DEFUN(relocate_debug,(output, input),
      bfd *output AND
      bfd *input)
{
#define IBS 400
#define OBS 400
  unsigned char input_buffer[IBS];

  input_ptr_start = input_ptr = input_buffer;
  input_ptr_end = input_buffer + IBS;
  input_bfd = input;
  bfd_read((PTR)input_ptr_start, 1, IBS, input);
  block();
}
/* 
  During linking, we we told about the bfds which made up our
  contents, we have a list of them. They will still be open, so go to
  the debug info in each, and copy it out, relocating it as we go.
*/

static void 
DEFUN(ieee_write_debug_part, (abfd),
      bfd *abfd)
{
  ieee_data_type *ieee = IEEE_DATA(abfd);
  bfd_chain_type *chain = ieee->chain_root;
  unsigned char output_buffer[OBS];
  boolean some_debug = false;
  file_ptr here = bfd_tell(abfd);

  output_ptr_start = output_ptr = output_buffer ;
  output_ptr_end = output_buffer + OBS;
  output_ptr = output_buffer;
  output_bfd = abfd;

  if (chain == (bfd_chain_type *)NULL) {
#if 0
      /* There is no debug info, so we'll fake some up */
      CONST static char fake[] = {
	  0xf8, 0xa, 0, 5, 't', 't', 't', 't', 't', 0, 2, 3,
	  '1','.','1',0x82, 1991>>8, 1991 & 0xff, 9, 20, 11, 07,50 };
      ieee->w.r.debug_information_part = 0;


      here;


      /*    bfd_write(fake, 1, sizeof(fake), abfd);*/
      /* Now write a header for each section */
    {
      int i = 0;
      asection *s = abfd->sections;
      while (s) {
	  if (s != abfd->abs_section) 
	  {
	      
	    ieee_write_byte(abfd, 0xf8);	
	    ieee_write_byte(abfd, 0x0b);
	    ieee_write_byte(abfd, 0);
	    ieee_write_byte(abfd, 0);
	    ieee_write_byte(abfd, 1);
	    ieee_write_byte(abfd, i + IEEE_SECTION_NUMBER_BASE);
	    ieee_write_expression(abfd, 0, s->symbol, 0, 0, 0);
	    ieee_write_byte(abfd,0);
	    ieee_write_byte(abfd, 0xf9);
	    ieee_write_expression(abfd, s->size,
				  bfd_abs_section.symbol, 0, 0, 0);	
	    i++;
	  }
	    
	  s = s->next;

	}	
      /* Close the scope */
      ieee_write_byte(abfd, 0xf9);
    }
#endif
    }
  else{
      while (chain != (bfd_chain_type *)NULL) {
	  bfd *entry = chain->this;
	  ieee_data_type *entry_ieee = IEEE_DATA(entry);
	  if (entry_ieee->w.r.debug_information_part) {
	      bfd_seek(entry, entry_ieee->w.r.debug_information_part, SEEK_SET);
	      relocate_debug(abfd, entry);
	    }

	  chain = chain->next;
	}
      if (some_debug) {
	  ieee->w.r.debug_information_part = here;
	}
      else {
	  ieee->w.r.debug_information_part = 0;
	}
    }
  flush();

}  
/* write the data in an ieee way */
static void
DEFUN(ieee_write_data_part,(abfd),
      bfd *abfd)
{
  asection *s;
  ieee_data_type *ieee = IEEE_DATA(abfd);
  ieee->w.r.data_part = bfd_tell(abfd);
  for (s = abfd->sections; s != (asection *)NULL; s = s->next) 
      {
	/* Sort the reloc records so we can insert them in the correct
	   places */
	if (s->reloc_count != 0) 
	    {
	     do_with_relocs(abfd, s);
	    }
	else
	    {
	      do_without_relocs(abfd, s);
	    }
      }
}



static void
DEFUN(init_for_output,(abfd),
      bfd *abfd)
{
  asection *s; 
  for (s = abfd->sections; s != (asection *)NULL; s = s->next) {
    if (s->_raw_size != 0) {
      ieee_per_section(s)->data = (bfd_byte *)(bfd_alloc(abfd, s->_raw_size));
    }
  }
}

/** exec and core file sections */

/* set section contents is complicated with IEEE since the format is 
* not a byte image, but a record stream.
*/
boolean
DEFUN(ieee_set_section_contents,(abfd, section, location, offset, count),
      bfd *abfd AND
      sec_ptr section AND
      PTR location AND
      file_ptr offset AND
      bfd_size_type count)
{
  if (ieee_per_section(section)->data == (bfd_byte *)NULL) {
    init_for_output(abfd);
  }
  (void) memcpy((PTR)(ieee_per_section(section)->data + offset),
		(PTR)location,
		(unsigned int)count);
  return true;
}

/*
write the external symbols of a file, IEEE considers two sorts of
external symbols, public, and referenced. It uses to internal forms
to index them as well. When we write them out we turn their symbol
values into indexes from the right base.
*/
static void
DEFUN(ieee_write_external_part,(abfd),
      bfd *abfd)
{
  asymbol **q;
  ieee_data_type *ieee = IEEE_DATA(abfd);

  unsigned int reference_index = IEEE_REFERENCE_BASE;
  unsigned int public_index = IEEE_PUBLIC_BASE+2;
  file_ptr here = bfd_tell(abfd);
  boolean hadone = false;
  if (abfd->outsymbols != (asymbol **)NULL) {

    for (q = abfd->outsymbols; *q  != (asymbol *)NULL; q++) {
      asymbol *p = *q;
      hadone = true;
      if (p->section == &bfd_und_section) {
	/* This must be a symbol reference .. */
	ieee_write_byte(abfd, ieee_external_reference_enum);
	ieee_write_int(abfd, reference_index);
	ieee_write_id(abfd, p->name);
	p->value = reference_index;
	reference_index++;
      }
      else if(p->section == &bfd_com_section) {
	/* This is a weak reference */
	ieee_write_byte(abfd, ieee_external_reference_enum);
	ieee_write_int(abfd, reference_index);
	ieee_write_id(abfd, p->name);
	ieee_write_byte(abfd, ieee_weak_external_reference_enum);
	ieee_write_int(abfd, reference_index);
	ieee_write_int(abfd, p->value);
	ieee_write_int(abfd, BFD_FORT_COMM_DEFAULT_VALUE);
	p->value = reference_index;
	reference_index++;
      }
      else if(p->flags & BSF_GLOBAL) {
	/* This must be a symbol definition */


	ieee_write_byte(abfd, ieee_external_symbol_enum);
	ieee_write_int(abfd, public_index );
	ieee_write_id(abfd, p->name);

	ieee_write_twobyte(abfd, ieee_attribute_record_enum);
	ieee_write_int(abfd, public_index );
	ieee_write_byte(abfd, 15);	 /* instruction address */
	ieee_write_byte(abfd, 19);	/* static symbol */
	ieee_write_byte(abfd, 1);	/* one of them */


	/* Write out the value */
	ieee_write_2bytes(abfd, ieee_value_record_enum);
	ieee_write_int(abfd, public_index);
	if (p->section != &bfd_abs_section)
	    {
	      if (abfd->flags & EXEC_P) 
	      {
		/* If fully linked, then output all symbols
		   relocated */
		ieee_write_int(abfd,
			       p->value + p->section->output_offset+ p->section->output_section->vma);

	      }
	      else { 
		  ieee_write_expression(abfd,
					p->value + p->section->output_offset,
					p->section->output_section->symbol
					, false, 0);
		}
	    }
	else
	    {
	      ieee_write_expression(abfd,
				    p->value,
				    bfd_abs_section.symbol,
				    false, 0);
	    }
	p->value = public_index;
	public_index++;
      }
      else {
	/* This can happen - when there are gaps in the symbols read */
	/* from an input ieee file */
      }
    }
  }
  if (hadone)
    ieee->w.r.external_part = here;

}


CONST static unsigned char exten[] = 
  {
    0xf0, 0x20, 0x00,					
    0xf1, 0xce, 0x20, 0x00, 37, 3, 3,	/* Set version 3 rev 3   	*/
    0xf1, 0xce, 0x20, 0x00, 39, 2,	/* keep symbol in  original case */
    0xf1, 0xce, 0x20, 0x00, 38		/* set object type relocateable to x */
  };

CONST static unsigned char envi[] =
  {
    0xf0, 0x21, 0x00,

/*    0xf1, 0xce, 0x21, 00, 50, 0x82, 0x07, 0xc7, 0x09, 0x11, 0x11,
    0x19, 0x2c, 
*/
    0xf1, 0xce, 0x21, 00, 52, 0x00, /* exec ok */

    0xf1, 0xce, 0x21, 0, 53, 0x03,	/* host unix */
/*    0xf1, 0xce, 0x21, 0, 54, 2,1,1	tool & version # */
  };

static
void 
DEFUN(ieee_write_me_part,(abfd),
      bfd *abfd)
{
  ieee_data_type *ieee= IEEE_DATA(abfd);
  ieee->w.r.trailer_part = bfd_tell(abfd);
  if (abfd->start_address) {
    ieee->w.r.me_record = bfd_tell(abfd);
    ieee_write_2bytes(abfd, ieee_value_starting_address_enum);
    ieee_write_byte(abfd, ieee_function_either_open_b_enum);
    ieee_write_int(abfd, abfd->start_address);
    ieee_write_byte(abfd, ieee_function_either_close_b_enum);
  }
  else {
    ieee->w.r.me_record = bfd_tell(abfd);
  }
  ieee_write_byte(abfd, ieee_module_end_enum);

}
boolean
DEFUN(ieee_write_object_contents,(abfd),
      bfd *abfd)
{
  ieee_data_type *ieee = IEEE_DATA(abfd);
  unsigned int i;
  file_ptr   old;
  /* Fast forward over the header area */
  bfd_seek(abfd, (file_ptr) 0, SEEK_SET);
  ieee_write_byte(abfd, ieee_module_beginning_enum);

  ieee_write_id(abfd, bfd_printable_name(abfd));
  ieee_write_id(abfd, abfd->filename);

  /* Fast forward over the variable bits */	
  ieee_write_byte(abfd, ieee_address_descriptor_enum);

  /* Bits per MAU */
  ieee_write_byte(abfd, bfd_arch_bits_per_byte(abfd));
  /* MAU's per address */
  ieee_write_byte(abfd, bfd_arch_bits_per_address(abfd)  /
		  bfd_arch_bits_per_byte(abfd));

  old = bfd_tell(abfd);
  bfd_seek(abfd, (file_ptr) (8 * N_W_VARIABLES), SEEK_CUR);

  ieee->w.r.extension_record = bfd_tell(abfd);
  bfd_write((char *)exten, 1, sizeof(exten), abfd);
  if (abfd->flags & EXEC_P) 
    ieee_write_byte(abfd, 0x1); /* Absolute */
  else 
    ieee_write_byte(abfd, 0x2); /* Relocateable */    
  
  ieee->w.r.environmental_record = bfd_tell(abfd);
  bfd_write((char *)envi, 1, sizeof(envi), abfd);
  output_bfd = abfd;
  flush();

  ieee_write_section_part(abfd);
  /*
    First write the symbols, this changes their values into table 
    indeces so we cant use it after this point
    */
  ieee_write_external_part(abfd);     
  /*  ieee_write_byte(abfd, ieee_record_seperator_enum);*/


  /*  ieee_write_byte(abfd, ieee_record_seperator_enum);*/


  /*
    Write any debugs we have been told about 
    */
  ieee_write_debug_part(abfd);

  /* 
    Can only write the data once the symbols have been written since
    the data contains relocation information which points to the
    symbols 
    */
  ieee_write_data_part(abfd);     


  /*
    At the end we put the end !
    */
  ieee_write_me_part(abfd);


  /* Generate the header */
  bfd_seek(abfd, old, SEEK_SET);

  for (i= 0; i < N_W_VARIABLES; i++) {
    ieee_write_2bytes(abfd,ieee_assign_value_to_variable_enum);
    ieee_write_byte(abfd, i);
    ieee_write_int5_out(abfd, ieee->w.offset[i]);
  }
  return true;
}




/* Native-level interface to symbols. */

/* We read the symbols into a buffer, which is discarded when this
function exits.  We read the strings into a buffer large enough to
hold them all plus all the cached symbol entries. */

asymbol *
DEFUN(ieee_make_empty_symbol,(abfd),
      bfd *abfd)
{

  ieee_symbol_type  *new =
    (ieee_symbol_type *)zalloc (sizeof (ieee_symbol_type));
  new->symbol.the_bfd = abfd;
  return &new->symbol;

}

static bfd *
DEFUN(ieee_openr_next_archived_file,(arch, prev),
      bfd *arch AND
      bfd *prev)
{
  ieee_ar_data_type *ar = IEEE_AR_DATA(arch);
  /* take the next one from the arch state, or reset */
  if (prev == (bfd *)NULL) {
    /* Reset the index - the first two entries are bogus*/
    ar->element_index = 2;
  }
  while (true) {  
    ieee_ar_obstack_type *p = ar->elements + ar->element_index;
    ar->element_index++;
    if (ar->element_index <= ar->element_count) {
      if (p->file_offset != (file_ptr)0) {
	if (p->abfd == (bfd *)NULL) {
	  p->abfd = _bfd_create_empty_archive_element_shell(arch);
	  p->abfd->origin = p->file_offset;
	}
	return p->abfd;
      }
    }
    else {
      bfd_error = no_more_archived_files;
      return (bfd *)NULL;
    }

  }
}

static boolean
ieee_find_nearest_line(abfd,
			 section,
			 symbols,
			 offset,
			 filename_ptr,
			 functionname_ptr,
			 line_ptr)
bfd *abfd;
asection *section;
asymbol **symbols;
bfd_vma offset;
char **filename_ptr;
char **functionname_ptr;
int *line_ptr;
{
  return false;
}


static int
ieee_generic_stat_arch_elt(abfd, buf)
bfd *abfd;
struct stat *buf;
{
  ieee_ar_data_type *ar = abfd->my_archive->tdata.ieee_ar_data;
  if (ar == (ieee_ar_data_type *)NULL) {
    bfd_error = invalid_operation;
    return -1;
  }
  else {
    buf->st_size = 0x1;
    buf->st_mode = 0666;
    return !    ieee_object_p(abfd);
  }
}
static int 
DEFUN(ieee_sizeof_headers,(abfd, x),
      bfd *abfd AND
      boolean x)
{
  return 0;
}



static void 
DEFUN(ieee_bfd_debug_info_start,(abfd), 
      bfd *abfd)
  {

  }

static void 
DEFUN(ieee_bfd_debug_info_end,(abfd), 
      bfd *abfd)
  {

  }


/* Add this section to the list of sections we have debug info for, to
   be ready to output it at close time 
   */
static void 
DEFUN(ieee_bfd_debug_info_accumulate,(abfd, section), 
      bfd *abfd AND
      asection *section)
{
  ieee_data_type *ieee = IEEE_DATA(section->owner);
  ieee_data_type *output_ieee = IEEE_DATA(abfd);
  /* can only accumulate data from other ieee bfds */
  if (section->owner->xvec != abfd->xvec)
    return;
  /* Only bother once per bfd */
  if (ieee->done_debug == true) 
    return;
  ieee->done_debug = true;

  /* Don't bother if there is no debug info */
  if (ieee->w.r.debug_information_part == 0)
    return;


  /* Add to chain */
    {
      bfd_chain_type *n = (bfd_chain_type *) bfd_alloc(abfd, sizeof(bfd_chain_type));
      n->this = section->owner;
      n->next = (bfd_chain_type *)NULL;
	
      if (output_ieee->chain_head) {
	output_ieee->chain_head->next = n;
      }
      else {
	output_ieee->chain_root = n;

      }
	output_ieee->chain_head = n; 
    }
}






#define FOO PROTO
#define ieee_core_file_failing_command (char *(*)())(bfd_nullvoidptr)
#define ieee_core_file_failing_signal (int (*)())bfd_0
#define ieee_core_file_matches_executable_p ( FOO(boolean, (*),(bfd *, bfd *)))bfd_false
#define ieee_slurp_armap bfd_true
#define ieee_slurp_extended_name_table bfd_true
#define ieee_truncate_arname (void (*)())bfd_nullvoidptr
#define ieee_write_armap  (FOO( boolean, (*),(bfd *, unsigned int, struct orl *, unsigned int, int))) bfd_nullvoidptr
#define ieee_get_lineno (struct lineno_cache_entry *(*)())bfd_nullvoidptr
#define	ieee_close_and_cleanup		bfd_generic_close_and_cleanup
#define ieee_set_arch_mach bfd_default_set_arch_mach
#define ieee_bfd_get_relocated_section_contents  bfd_generic_get_relocated_section_contents
#define ieee_bfd_relax_section bfd_generic_relax_section
/*SUPPRESS 460 */
bfd_target ieee_vec =
{
  "ieee",			/* name */
  bfd_target_ieee_flavour,
  true,				/* target byte order */
  true,				/* target headers byte order */
  (HAS_RELOC | EXEC_P |		/* object flags */
   HAS_LINENO | HAS_DEBUG |
   HAS_SYMS | HAS_LOCALS | DYNAMIC | WP_TEXT | D_PAGED),
  ( SEC_CODE|SEC_DATA|SEC_ROM|SEC_HAS_CONTENTS
   |SEC_ALLOC | SEC_LOAD | SEC_RELOC), /* section flags */
   0,				/* leading underscore */
  ' ',				/* ar_pad_char */
  16,				/* ar_max_namelen */
    1,				/* minimum alignment */
_do_getb64, _do_putb64, _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* data */
_do_getb64, _do_putb64,  _do_getb32, _do_putb32, _do_getb16, _do_putb16, /* hdrs */

  { _bfd_dummy_target,
     ieee_object_p,		/* bfd_check_format */
     ieee_archive_p,
    _bfd_dummy_target,
     },
  {
    bfd_false,
    ieee_mkobject, 
    _bfd_generic_mkarchive,
    bfd_false
    },
  {
    bfd_false,
    ieee_write_object_contents,
    _bfd_write_archive_contents,
    bfd_false,
  },
  JUMP_TABLE(ieee)
};
