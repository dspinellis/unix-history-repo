/* BFD library support routines for the i960 architecture.
   Copyright (C) 1990-1991 Free Software Foundation, Inc.
   Hacked by Steve Chamberlain of Cygnus Support.

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


/* This routine is provided a string, and tries to work out if it
   could possibly refer to the i960 machine pointed at in the
   info_struct pointer */

static boolean
DEFUN(scan_960_mach, (ap, string),
      CONST bfd_arch_info_type *ap AND
      CONST char *string)
{
  unsigned long machine;

  /* Look for the string i960, or somesuch at the front of the string  */

  if (strncmp("i960",string,4) == 0) {
    string+=4;
  }
  else {
    /* no match, can be us */
    return false;
  }
  if (string[0] == 0) {
    /* i960 on it's own means core to us*/
    if (ap->mach == bfd_mach_i960_core) return true;
    return false;
  }

  if (string[0] != ':') {
    return false;
  }
  string++;
  if (string[0] == '\0')
    return false;
  if (string[0] == 'c' && string[1] == 'o' && string[2] == 'r' &&
      string[3] == 'e' && string[4] == '\0')
    machine = bfd_mach_i960_core;
  else if (string[1] == '\0' || string[2] != '\0') /* rest are 2-char */
    return false;
  else if (string[0] == 'k' && string[1] == 'b')
    machine = bfd_mach_i960_kb_sb;
  else if (string[0] == 's' && string[1] == 'b')
    machine = bfd_mach_i960_kb_sb;
  else if (string[0] == 'm' && string[1] == 'c')
    machine = bfd_mach_i960_mc;
  else if (string[0] == 'x' && string[1] == 'a')
    machine = bfd_mach_i960_xa;
  else if (string[0] == 'c' && string[1] == 'a')
    machine = bfd_mach_i960_ca;
  else if (string[0] == 'k' && string[1] == 'a')
    machine = bfd_mach_i960_ka_sa;
  else if (string[0] == 's' && string[1] == 'a')
    machine = bfd_mach_i960_ka_sa;
  else
    return false;
  if (machine == ap->mach)   return true;
  return false;
}



/* This routine is provided two arch_infos and works out the i960
   machine which would be compatible with both and returns a pointer
   to its info structure */

CONST bfd_arch_info_type *
DEFUN(compatible,(a,b),
      CONST bfd_arch_info_type *a AND
      CONST bfd_arch_info_type *b)
{

  /* The i960 has two distinct subspecies which may not interbreed:
     CORE CA          
     CORE KA KB MC XA
     Any architecture on the same line is compatible, the one on
     the right is the least restrictive.  
     
     We represent this information in an array, each machine to a side */

#define ERROR	0
#define CORE	bfd_mach_i960_core  /*1*/  
#define KA 	bfd_mach_i960_ka_sa /*2*/ 
#define KB 	bfd_mach_i960_kb_sb /*3*/
#define MC 	bfd_mach_i960_mc    /*4*/
#define XA 	bfd_mach_i960_xa    /*5*/
#define CA 	bfd_mach_i960_ca    /*6*/


  static CONST char matrix[7][7] = 
      {
	ERROR,CORE,	KA,	KB,	MC,	XA,	CA,
	CORE,	CORE,	KA,	KB,	MC,	XA,	CA,
	KA,	KA,	KA,	KB,	MC,	XA,	ERROR,
	KB,	KB,	KB,	KB,	MC,	XA,	ERROR,
	MC,	MC,	MC,	MC,	MC,	XA,	ERROR,
	XA,	XA,	XA,	XA,	XA,	XA,	ERROR,
	CA,	CA,	ERROR,	ERROR,	ERROR,	ERROR,	CA 
	};


  if (a->arch != b->arch || matrix[a->mach][b->mach] == ERROR) 
    {
    return NULL;
    }
  else 
    {
    return (a->mach  ==  matrix[a->mach][b->mach]) ?  a : b;
    }
}



int bfd_default_scan_num_mach();
#define N(a,b,d) \
{ 32, 32, 8,bfd_arch_i960,a,"i960",b,3,d,compatible,scan_960_mach,0,}

static bfd_arch_info_type arch_info_struct[] = 
{ 
  N(bfd_mach_i960_core,"i960:core",true),
  N(bfd_mach_i960_ka_sa,"i960:ka_sa",false),
  N(bfd_mach_i960_kb_sb,"i960:kb_sb",false),
  N(bfd_mach_i960_mc,"i960:mc",false),
  N(bfd_mach_i960_xa,"i960:xa",false),
  N(bfd_mach_i960_ca,"i960:ca",false)
  };


void DEFUN_VOID(bfd_i960_arch)
{
  unsigned int i;
  for (i = 0; i < sizeof(arch_info_struct)/sizeof (*arch_info_struct); i++)  {
    bfd_arch_linkin(arch_info_struct + i);
  }
}
