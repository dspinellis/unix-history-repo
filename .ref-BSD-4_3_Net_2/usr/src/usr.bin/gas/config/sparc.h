/* sparc.h -- Header file for the SPARC
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

/*
 * The following enum and struct were borrowed from
 * sunOS  /usr/include/sun4/a.out.h
 *
 */

enum reloc_type
{
    RELOC_8,        RELOC_16,        RELOC_32,       RELOC_DISP8,
    RELOC_DISP16,   RELOC_DISP32,    RELOC_WDISP30,  RELOC_WDISP22,
    RELOC_HI22,     RELOC_22,        RELOC_13,       RELOC_LO10,
    RELOC_SFA_BASE, RELOC_SFA_OFF13, RELOC_BASE10,   RELOC_BASE13,
    RELOC_BASE22,   RELOC_PC10,      RELOC_PC22,     RELOC_JMP_TBL,
    RELOC_SEGOFF16, RELOC_GLOB_DAT,  RELOC_JMP_SLOT, RELOC_RELATIVE,

    NO_RELOC
};

struct reloc_info_sparc
{
    unsigned long int r_address;
/*
 * Using bit fields here is a bad idea because the order is not portable. :-(
 */
    unsigned int r_index    : 24;
    unsigned int r_extern   : 1;
    unsigned int unused     : 2;
    enum reloc_type r_type  : 5;
    long int r_addend;
};

#define relocation_info reloc_info_sparc

