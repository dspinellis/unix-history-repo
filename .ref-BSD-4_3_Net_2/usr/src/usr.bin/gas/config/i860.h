/* i860.h -- Header file for the I860
   Copyright (C) 1989 Free Software Foundation, Inc.

This file is part of GAS, the GNU Assembler.

GAS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GAS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GAS; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

enum reloc_type /* NOTE: three bits max, see struct reloc_info_i860.r_type */
{
    NO_RELOC = 0, BRADDR, LOW0, LOW1, LOW2, LOW3, LOW4, SPLIT0, SPLIT1, SPLIT2, RELOC_32,
};
      
enum highlow_type       /* NOTE: two bits max, see reloc_info_i860.r_type */
{
    NO_SPEC = 0, PAIR, HIGH, HIGHADJ,
};
	   
struct reloc_info_i860
{
    unsigned long int r_address;
/*
 * Using bit fields here is a bad idea because the order is not portable. :-(
 */
    unsigned int r_symbolnum: 24;
    unsigned int r_pcrel    : 1;
    unsigned int r_extern   : 1;
    /* combining the two field simplifies the argument passing in "new_fix()" */
    /* and is compatible with the existing Sparc #ifdef's */
    /* r_type:  highlow_type - bits 5,4; reloc_type - bits 3-0 */
    unsigned int r_type     : 6;
    long int r_addend;
};

#define relocation_info reloc_info_i860


