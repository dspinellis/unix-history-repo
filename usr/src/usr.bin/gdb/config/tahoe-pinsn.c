/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by the
 * Distributed Computer Systems Lab, Department of Computer Science
 * at the State University of New York at Buffalo.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)tahoe-pinsn.c	6.3 (Berkeley) 5/8/91";
#endif /* not lint */

#include <stdio.h>

#include "defs.h"
#include "param.h"
#include "symtab.h"
#include "tahoe-opcode.h"

/* Tahoe instructions are never longer than this.  */
#define MAXLEN 62

/* Number of elements in the opcode table.  */
#define NOPCODES (sizeof votstrs / sizeof votstrs[0])

extern char *reg_names[];

static unsigned char *print_insn_arg ();

/* Print the Tahoe instruction at address MEMADDR in debugged memory,
   on STREAM.  Returns length of the instruction, in bytes.  */

int
print_insn (memaddr, stream)
     CORE_ADDR memaddr;
     FILE *stream;
{
  unsigned char buffer[MAXLEN];
  register int i;
  register unsigned char *p;
  register char *d;

  read_memory (memaddr, buffer, MAXLEN);

  for (i = 0; i < NOPCODES; i++)
    if (votstrs[i].detail.code == buffer[0]
	|| votstrs[i].detail.code == *(unsigned short *)buffer)
      break;

  /* Handle undefined instructions.  */
  if (i == NOPCODES)
    {
      fprintf (stream, "0%o", buffer[0]);
      return 1;
    }

  fprintf (stream, "%s", votstrs[i].name);

  /* Point at first byte of argument data,
     and at descriptor for first argument.  */
  p = buffer + 1 + (votstrs[i].detail.code >= 0x100);
  d = votstrs[i].detail.args;

  if (*d)
    fputc ('\t', stream);

  while (*d)
    {
      p = print_insn_arg (d, p, memaddr + (p - buffer), stream);
      d += 2;
      if (*d)
	fprintf (stream, ",");
    }
  return p - buffer;
}
/*******************************************************************/
static unsigned char *
print_insn_arg (d, p, addr, stream)
     char *d;
     register char *p;
     CORE_ADDR addr;
     FILE *stream;
{
  int temp1 = 0;
  register int regnum = *p & 0xf;
  float floatlitbuf;

  if (*d == 'b')
    {
      if (d[1] == 'b')
	fprintf (stream, "0x%x", addr + *p++ + 1);
      else
	{

	  temp1 = *p;
	  temp1 <<= 8;
	  temp1 |= *(p + 1);
	  fprintf (stream, "0x%x", addr + temp1 + 2);
	  p += 2;
	}
    }
  else
    switch ((*p++ >> 4) & 0xf)
      {
      case 0:
      case 1:
      case 2:
      case 3:			/* Liter>al(short immediate byte) mode */
	if (d[1] == 'd' || d[1] == 'f' || d[1] == 'g' || d[1] == 'h')
	  {
	    *(int *)&floatlitbuf = 0x4000 + ((p[-1] & 0x3f) << 4);
	    fprintf (stream, "$%f", floatlitbuf);
	  }
	else
	  fprintf (stream, "$%d", p[-1] & 0x3f);
	break;

      case 4:			/* Indexed */
	p = (char *) print_insn_arg (d, p, addr + 1, stream);
	fprintf (stream, "[%s]", reg_names[regnum]);
	break;

      case 5:			/* Register */
	fprintf (stream, reg_names[regnum]);
	break;

      case 7:			/* Autodecrement */
	fputc ('-', stream);
      case 6:			/* Register deferred */
	fprintf (stream, "(%s)", reg_names[regnum]);
	break;

      case 9:	                /* Absolute Address & Autoincrement deferred */
	fputc ('*', stream);
	if (regnum == PC_REGNUM)
	  {
	    temp1 = *p;
	    temp1 <<= 8;
	    temp1 |= *(p +1);

	    fputc ('$', stream);
	    print_address (temp1, stream);
	    p += 4;
	    break;
	  }
      case 8:			/*Immediate & Autoincrement SP */
        if (regnum == 8)         /*88 is Immediate Byte Mode*/
	  fprintf (stream, "$%d", *p++);

	else if (regnum == 9)        /*89 is Immediate Word Mode*/
	  {
	    temp1 = *p;
	    temp1 <<= 8; 
	    temp1 |= *(p +1);
	    fprintf (stream, "$%d", temp1);
	    p += 2;
	  }  

	else if (regnum == PC_REGNUM)    /*8F is Immediate Long Mode*/
	  {
	    temp1 = *p;
	    temp1 <<=8;
	    temp1 |= *(p +1);
	    temp1 <<=8;
	    temp1 |= *(p +2);
	    temp1 <<= 8;
	    temp1 |= *(p +3);
	    fprintf (stream, "$%d", temp1);
	    p += 4;
	  }

	else                            /*8E is Autoincrement SP Mode*/
	      fprintf (stream, "(%s)+", reg_names[regnum]);
	break;

      case 11:			/* Register + Byte Displacement Deferred Mode*/
	fputc ('*', stream);
      case 10:			/* Register + Byte Displacement Mode*/
	if (regnum == PC_REGNUM)
	  print_address (addr + *p + 2, stream);
	else
	  fprintf (stream, "%d(%s)", *p, reg_names[regnum]);
	p += 1;
	break;

      case 13:			/* Register + Word Displacement Deferred Mode*/
	fputc ('*', stream);
      case 12:			/* Register + Word Displacement Mode*/
	temp1 = *p;
	temp1 <<= 8;
	temp1 |= *(p +1);
	if (regnum == PC_REGNUM)
	  print_address (addr + temp1 + 3, stream);
	else
	  fprintf (stream, "%d(%s)", temp1, reg_names[regnum]);
	p += 2;
	break;

      case 15:			/* Register + Long Displacement Deferred Mode*/
	fputc ('*', stream);
      case 14:			/* Register + Long Displacement Mode*/
	temp1 = *p;
	temp1 <<= 8;
	temp1 |= *(p +1);
	temp1 <<= 8;
	temp1 |= *(p +2);
	temp1 <<= 8;
	temp1 |= *(p +3);
	if (regnum == PC_REGNUM)
	  print_address (addr + temp1 + 5, stream);
	else
	  fprintf (stream, "%d(%s)", temp1, reg_names[regnum]);
	p += 4;
      }

  return (unsigned char *) p;
}













