/* Print IBM RS/6000 instructions for GNU software.
   Copyright 1991 Free Software Foundation, Inc.
   Contributed by IBM Corporation.

This file is part of GDB and the GNU binutils.

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

#include "defs.h"
#include "opcode/rs6k.h"

/* Print the rs6k instruction at address MEMADDR in debugged memory,
   on STREAM.  Returns length of the instruction, in bytes.  */

int
print_insn (memaddr, stream)
  CORE_ADDR memaddr;
  FILE *stream;
{
	int  pop, eop, probable_eop;	/* primary and extended opcodes */
	int  min, max;
	int  best = -1;			/* found best opcode index	*/
	int  oldbest = -1;
	unsigned int the_insn;

	read_memory (memaddr, &the_insn, sizeof (the_insn));
	pop = (unsigned)(the_insn >> 26);
	min = 0, max = NOPCODES-1;

	while (min < max) {
	  best = (min + max) / 2;

	  /* see if we are running in loops */
	  if (best == oldbest)
	    goto not_found;
	  oldbest = best;

	  if (pop < rs6k_ops [best].p_opcode)
	    max = best;

	  else if (pop > rs6k_ops [best].p_opcode)
	    min = best;

	  else {
	    /* Opcode matched, check extended opcode. */

	    if (rs6k_ops [best].e_opcode == -1) {
	      /* there is no valid extended opcode, what we've got is
		 just fine. */
	      goto insn_found;
	    }

	    /* Largest possible value of extended opcode. */
	    probable_eop = ((the_insn) >> 1) & 0x3ff;

	    eop = probable_eop & eopMask [rs6k_ops [best].format];

	    if (eop < rs6k_ops [best].e_opcode) {

	      while (pop == rs6k_ops [best].p_opcode) {
		if (eop == rs6k_ops [best].e_opcode)	/* found it! */
		  goto insn_found;
	        --best;
	        eop = probable_eop & eopMask [rs6k_ops [best].format];
	      }
	      goto not_found;
	    }

	    else if (eop > rs6k_ops [best].e_opcode) {

	      while (pop == rs6k_ops [best].p_opcode) {
		if (eop == rs6k_ops [best].e_opcode)	/* found it! */
		  goto insn_found;
	        ++best;
	        eop = probable_eop & eopMask [rs6k_ops [best].format];
	      }
	      goto not_found;
	    }

	    else /*  eop == rs6k_ops [best].e_opcode */
	      goto insn_found;
	  }
	}	

	best = min;
	if (pop == rs6k_ops [best].p_opcode &&
           (rs6k_ops [best].e_opcode == -1 || rs6k_ops [best].e_opcode == eop))
	    goto insn_found;

	else
	  goto not_found;


insn_found:
	print_operator (stream, memaddr, the_insn, best);
	return 4;

not_found:
	fprintf (stream, "0x%08x", the_insn);
	return 4;
}



/* condition code names */
static char *cond_code [] = {
  "lt", "gt", "eq", "so", "ge", "le", "ne", "ns", "nl", "ng", "z", "nz" };


print_operator (stream, memaddr, insn_word, insn_no)
FILE	*stream;
long	memaddr;
long	insn_word;
int	insn_no;
{
  char buf [20];
  char *qq = buf;
  char *pp = rs6k_ops[insn_no].opr_ext;
  int tmp;
  int nocomma = 0;			/* true if no comma needed */

  if (pp) {
    while (*pp) {

      switch ( *pp ) {
	case '.':
	  if (insn_word & 0x1)
	    *qq++ = '.';
	  break;

	case 'l':
	  if (insn_word & 0x1)
	   *qq++ = 'l';
	  break;

	case 't':
	  if ((insn_word & 0x03e00000) == 0x01800000)
	   *qq++ = 't';
	  break;

	case 'f':
	  if ((insn_word & 0x03e00000) == 0x00800000)
	   *qq++ = 'f';
	  break;

	case 'a':
	  if (insn_word & 0x2)
	   *qq++ = 'a';
	  break;

	case 'o':
	  if (insn_word & 0x4000)
	   *qq++ = 'o';
	  break;

	case '1':		/* exception #1 for bb/bc ambiguity */
	  tmp = (insn_word >> 21) & 0x1f;	/* extract BO	*/
	  if (tmp != 0xc && tmp != 0x4) {
	    /* you can't use `bb' now. switch to `bc' */
	    *(qq-1) = 'c';
	    ++insn_no;
	    pp = rs6k_ops[insn_no].opr_ext;
	    continue;
	  }
	  break;

	default:
	  abort ();
      }
      ++pp;
    }
  }
  *qq = '\0';

  fprintf (stream, "%s%s\t", rs6k_ops[insn_no].operator, buf);

  /* parse the operand now. */
  pp = rs6k_ops[insn_no].oprnd_format;

  while (1) {
    switch (*pp) {
      case TO	:
	fprintf (stream, "%d", (insn_word >> 21) & 0x1f);
	break;

      case RT	:
      case RS	:
	fprintf (stream, "r%d", (insn_word >> 21) & 0x1f);
	break;

      case LI	:
	tmp  = (insn_word >> 16) & 0x1f;
	if (tmp > 11) {
	  fprintf (stream, "{unknown cond code: 0x%x}", insn_word);
	  tmp = 0;
	}
	fprintf (stream, "%s", cond_code [tmp]);
	break;

      case A2	:
      case TA14	:
	tmp = (insn_word & 0xfffc);
	if (tmp & 0x8000)		/* fix sign extension	*/
	  tmp -= 0x10000;

	if ((insn_word & 0x2) == 0)	/* if AA not set	*/
	  tmp += memaddr;

        print_address (tmp, stream);
	break;

      case TA24	:
	tmp = insn_word & 0x03fffffc;
	if (tmp & 0x2000000)
	  tmp -= 0x4000000;
	
	if ((insn_word & 0x2) == 0)		/* if no AA bit set */
	  tmp += memaddr;

        print_address (tmp, stream);
	break;

      case LEV	:			/* for svc only */
	if (insn_word & 0x2) {		/* SA is set	*/
	  nocomma = 1;
	}
	else
          fprintf (stream, "%d", (insn_word >> 5) & 0x7f);
	break;

      case FL1	:			/* for svc only */
	if (insn_word & 0x2) {		/* SA is set	*/
	  nocomma = 1;
	}
	else
          fprintf (stream, "%d", (insn_word >> 12) & 0xf);
	break;

      case FL2	:			/* for svc only	*/
	nocomma = 0;
	if (insn_word & 0x2)		/* SA is set	*/
	  fprintf (stream, "%d", (insn_word >> 2) & 0x3fff);
	else
          fprintf (stream, "%d", (insn_word >> 2) & 0x7);
	break;

      case RA	:
	if (nocomma) {
	  fprintf (stream, "r%d)", (insn_word >> 16) & 0x1f);
	  nocomma = 0;
	}
	else
	  fprintf (stream, "r%d", (insn_word >> 16) & 0x1f);
	break;

      case RB	:
	fprintf (stream, "r%d", (insn_word >> 11) & 0x1f);
	break;

      case SI	:
	tmp = insn_word & 0xffff;
	if (tmp & 0x8000)
	  tmp -= 0x10000;
	fprintf (stream, "%d", tmp);
	break;

      case UI	:
	fprintf (stream, "%d", insn_word & 0xffff);
	break;

      case BF	:
	fprintf (stream, "%d", (insn_word >> 23) & 0x7);
	break;

      case BFA	:
	fprintf (stream, "%d", (insn_word >> 18) & 0x7);
	break;

      case BT	:
	fprintf (stream, "%d", (insn_word >> 21) & 0x1f);
	break;

      case BA	:
	fprintf (stream, "%d", (insn_word >> 16) & 0x1f);
	break;

      case BB	:
	fprintf (stream, "%d", (insn_word >> 11) & 0x1f);
	break;

      case BO	:
	fprintf (stream, "%d", (insn_word >> 21) & 0x1f);
	break;

      case BI	:
	fprintf (stream, "%d", (insn_word >> 16) & 0x1f);
	break;

      case SH	:
	fprintf (stream, "%d", (insn_word >> 11) & 0x1f);
	break;

      case MB	:
	fprintf (stream, "0x%x", (insn_word >> 6) & 0x1f);
	break;

      case ME	:
	fprintf (stream, "0x%x", (insn_word >> 1) & 0x1f);
	break;

      case SPR	:
	fprintf (stream, "%d", (insn_word >> 16) & 0x1f);
	break;

      case DIS	:
	nocomma = 1;
	tmp = insn_word & 0xffff;
	if (tmp & 0x8000)
	  tmp -= 0x10000;
	fprintf (stream, "%d(", tmp);
	break;

      case FXM	:
	fprintf (stream, "0x%x", (insn_word >> 12) & 0xff);
	break;

      case FRT	:
      case FRS	:
	fprintf (stream, "f%d", (insn_word >> 21) & 0x1f);
	break;

      case FRA	:
	fprintf (stream, "f%d", (insn_word >> 16) & 0x1f);
	break;

      case FRB	:
	fprintf (stream, "f%d", (insn_word >> 11) & 0x1f);
	break;

      case FRC	:
	fprintf (stream, "f%d", (insn_word >> 6) & 0x1f);
	break;

      case FLM	:
	fprintf (stream, "0x%x", (insn_word >> 17) & 0xff);
	break;

      case NB	:
	fprintf (stream, "%d", (insn_word >> 11) & 0x1f);
	break;

      case I	:
	fprintf (stream, "%d", (insn_word >> 12) & 0xf);
	break;

      default	:
	fprintf (stream,
		 "{Internal error: Unknown operand format identifier %d}",
		 *pp);
    }
    ++pp;

    if (*pp == '\0')
      break;
    else if (!nocomma)
      fputc(',', stream);
  }
}
