/* Execution of byte code produced by bytecomp.el.
   Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include "lisp.h"
#include "buffer.h"

Lisp_Object Qbytecode;

/*  Byte codes: */

#define Bvarref 010
#define Bvarset 020
#define Bvarbind 030
#define Bcall 040
#define Bunbind 050

#define Bnth 070
#define Bsymbolp 071
#define Bconsp 072
#define Bstringp 073
#define Blistp 074
#define Beq 075
#define Bmemq 076
#define Bnot 077
#define Bcar 0100
#define Bcdr 0101
#define Bcons 0102
#define Blist1 0103
#define Blist2 0104
#define Blist3 0105
#define Blist4 0106
#define Blength 0107
#define Baref 0110
#define Baset 0111
#define Bsymbol_value 0112
#define Bsymbol_function 0113
#define Bset 0114
#define Bfset 0115
#define Bget 0116
#define Bsubstring 0117
#define Bconcat2 0120
#define Bconcat3 0121
#define Bconcat4 0122
#define Bsub1 0123
#define Badd1 0124
#define Beqlsign 0125
#define Bgtr 0126
#define Blss 0127
#define Bleq 0130
#define Bgeq 0131
#define Bdiff 0132
#define Bnegate 0133
#define Bplus 0134
#define Bmax 0135
#define Bmin 0136

#define Bpoint 0140
#define Bmark 0141 /* no longer generated as of v18 */
#define Bgoto_char 0142
#define Binsert 0143
#define Bpoint_max 0144
#define Bpoint_min 0145
#define Bchar_after 0146
#define Bfollowing_char 0147
#define Bpreceding_char 0150
#define Bcurrent_column 0151
#define Bindent_to 0152
#define Bscan_buffer 0153 /* No longer generated as of v18 */
#define Beolp 0154
#define Beobp 0155
#define Bbolp 0156
#define Bbobp 0157
#define Bcurrent_buffer 0160
#define Bset_buffer 0161
#define Bread_char 0162
#define Bset_mark 0163 /* this loser is no longer generated as of v18 */
#define Binteractive_p 0164 /* Needed since interactive-p takes unevalled args */

#define Bconstant2 0201
#define Bgoto 0202
#define Bgotoifnil 0203
#define Bgotoifnonnil 0204
#define Bgotoifnilelsepop 0205
#define Bgotoifnonnilelsepop 0206
#define Breturn 0207
#define Bdiscard 0210
#define Bdup 0211

#define Bsave_excursion 0212
#define Bsave_window_excursion 0213
#define Bsave_restriction 0214
#define Bcatch 0215

#define Bunwind_protect 0216
#define Bcondition_case 0217
#define Btemp_output_buffer_setup 0220
#define Btemp_output_buffer_show 0221

#define Bconstant 0300
#define CONSTANTLIM 0100

/* Fetch the next byte from the bytecode stream */

#define FETCH ((unsigned char *)XSTRING (bytestr)->data)[pc++]

/* Fetch two bytes from the bytecode stream
 and make a 16-bit number out of them */

#define FETCH2 (op = FETCH, op + (FETCH << 8))

/* Push x onto the execution stack. */

#define PUSH(x) (*++stackp = (x))

/* Pop a value off the execution stack.  */

#define POP (*stackp--)

/* Discard n values from the execution stack.  */

#define DISCARD(n) (stackp -= (n))

/* Get the value which is at the top of the execution stack, but don't pop it. */

#define TOP (*stackp)


DEFUN ("byte-code", Fbyte_code, Sbyte_code, 3, 3, 0,
  "")
  (bytestr, vector, maxdepth)
     Lisp_Object bytestr, vector, maxdepth;
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  int count = specpdl_ptr - specpdl;
  register int pc = 0;
  register int op;
  Lisp_Object *stack;
  register Lisp_Object *stackp;
  Lisp_Object *stacke;
  register Lisp_Object v1, v2;
  register Lisp_Object *vectorp = XVECTOR (vector)->contents;

  CHECK_STRING (bytestr, 0);
  if (XTYPE (vector) != Lisp_Vector)
    vector = wrong_type_argument (Qvectorp, vector);
  CHECK_NUMBER (maxdepth, 2);

  stackp = (Lisp_Object *) alloca (XFASTINT (maxdepth) * sizeof (Lisp_Object));
  bzero (stackp, XFASTINT (maxdepth) * sizeof (Lisp_Object));
  GCPRO3 (bytestr, vector, *stackp);
  gcpro3.nvars = XFASTINT (maxdepth);

  --stackp;
  stack = stackp;
  stacke = stackp + XFASTINT (maxdepth);

  while (1)
    {
      if (stackp > stacke)
	error ("Stack overflow in byte code (byte compiler bug), pc = %d", pc);
      if (stackp < stack)
	error ("Stack underflow in byte code (byte compiler bug), pc = %d", pc);
      switch (op = FETCH)
	{
	case Bvarref+6:
	  op = FETCH;
	  goto varref;

	case Bvarref+7:
	  op = FETCH2;
	  goto varref;

	case Bvarref: case Bvarref+1: case Bvarref+2: case Bvarref+3:
	case Bvarref+4: case Bvarref+5:
	  op = op - Bvarref;
	varref:
	  v1 = vectorp[op];
	  if (XTYPE (v1) != Lisp_Symbol)
	    v2 = Fsymbol_value (v1);
	  else
	    {
	      v2 = XSYMBOL (v1)->value;
#ifdef SWITCH_ENUM_BUG
	      switch ((int) XTYPE (v2))
#else
	      switch (XTYPE (v2))
#endif
		{
		case Lisp_Symbol:
		  if (!EQ (v2, Qunbound))
		    break;
		case Lisp_Intfwd:
		case Lisp_Boolfwd:
		case Lisp_Objfwd:
		case Lisp_Buffer_Local_Value:
		case Lisp_Some_Buffer_Local_Value:
		case Lisp_Buffer_Objfwd:
		case Lisp_Void:
		  v2 = Fsymbol_value (v1);
		}
	    }
	  PUSH (v2);
	  break;

	case Bvarset+6:
	  op = FETCH;
	  goto varset;

	case Bvarset+7:
	  op = FETCH2;
	  goto varset;

	case Bvarset: case Bvarset+1: case Bvarset+2: case Bvarset+3:
	case Bvarset+4: case Bvarset+5:
	  op -= Bvarset;
	varset:
	  Fset (vectorp[op], POP);
	  break;

	case Bvarbind+6:
	  op = FETCH;
	  goto varbind;

	case Bvarbind+7:
	  op = FETCH2;
	  goto varbind;

	case Bvarbind: case Bvarbind+1: case Bvarbind+2: case Bvarbind+3:
	case Bvarbind+4: case Bvarbind+5:
	  op -= Bvarbind;
	varbind:
	  specbind (vectorp[op], POP);
	  break;

	case Bcall+6:
	  op = FETCH;
	  goto docall;

	case Bcall+7:
	  op = FETCH2;
	  goto docall;

	case Bcall: case Bcall+1: case Bcall+2: case Bcall+3:
	case Bcall+4: case Bcall+5:
	  op -= Bcall;
	docall:
	  DISCARD(op);
	  /* Remove protection from the args we are giving to Ffuncall.
	     FFuncall will protect them, and double protection would
	     cause disasters.  */
	  gcpro3.nvars = &TOP - stack - 1;
	  TOP = Ffuncall (op + 1, &TOP);
	  gcpro3.nvars = XFASTINT (maxdepth);
	  break;

	case Bunbind+6:
	  op = FETCH;
	  goto dounbind;

	case Bunbind+7:
	  op = FETCH2;
	  goto dounbind;

	case Bunbind: case Bunbind+1: case Bunbind+2: case Bunbind+3:
	case Bunbind+4: case Bunbind+5:
	  op -= Bunbind;
	dounbind:
	  unbind_to (specpdl_ptr - specpdl - op);
	  break;

	case Bgoto:
	  QUIT;
	  op = FETCH2;    /* pc = FETCH2 loses since FETCH2 contains pc++ */
	  /* A loop always uses a plain goto to jump back.
	     So this makes sure we consider GC in each loop.  */
	  if (op < pc && consing_since_gc > gc_cons_threshold)
	    Fgarbage_collect ();
	  pc = op;
	  break;

	case Bgotoifnil:
	  QUIT;
	  op = FETCH2;
	  if (NULL (POP))
	    pc = op;
	  break;

	case Bgotoifnonnil:
	  QUIT;
	  op = FETCH2;
	  if (!NULL (POP))
	    pc = op;
	  break;

	case Bgotoifnilelsepop:
	  QUIT;
	  op = FETCH2;
	  if (NULL (TOP))
	    pc = op;
	  else DISCARD(1);
	  break;

	case Bgotoifnonnilelsepop:
	  QUIT;
	  op = FETCH2;
	  if (!NULL (TOP))
	    pc = op;
	  else DISCARD(1);
	  break;

	case Breturn:
	  v1 = POP;
	  goto exit;

	case Bdiscard:
	  DISCARD(1);
	  break;

	case Bdup:
	  v1 = TOP;
	  PUSH (v1);
	  break;

	case Bconstant2:
	  PUSH (vectorp[FETCH2]);
	  break;

	case Bsave_excursion:
	  record_unwind_protect (save_excursion_restore, save_excursion_save ());
	  break;

	case Bsave_window_excursion:
	  TOP = Fsave_window_excursion (TOP);
	  break;

	case Bsave_restriction:
	  record_unwind_protect (save_restriction_restore, save_restriction_save ());
	  break;

	case Bcatch:
	  v1 = POP;
	  TOP = internal_catch (TOP, Feval, v1);
	  break;

	case Bunwind_protect:
	  record_unwind_protect (0, POP);
	  (specpdl_ptr - 1)->symbol = Qnil;
	  break;

	case Bcondition_case:
	  v1 = POP;
	  v1 = Fcons (POP, v1);
	  TOP = Fcondition_case (Fcons (TOP, v1));
	  break;

	case Btemp_output_buffer_setup:
	  temp_output_buffer_setup (XSTRING (TOP)->data);
	  TOP = Vstandard_output;
	  break;

	case Btemp_output_buffer_show:
	  v1 = POP;
	  temp_output_buffer_show (TOP);
	  TOP = v1;
	  /* pop binding of standard-output */
	  unbind_to (specpdl_ptr - specpdl - 1);
	  break;

	case Bnth:
	  v1 = POP;
	  v2 = TOP;
	  CHECK_NUMBER (v2, 0);
	  op = XINT (v2);
	  immediate_quit = 1;
	  while (--op >= 0)
	    {
	      if (CONSP (v1))
		v1 = XCONS (v1)->cdr;
	      else if (!NULL (v1))
		{
		  immediate_quit = 0;
		  v1 = wrong_type_argument (Qlistp, v1);
		  immediate_quit = 1;
		  op++;
		}
	    }
	  immediate_quit = 0;
	  goto docar;

	case Bsymbolp:
	  TOP = XTYPE (TOP) == Lisp_Symbol ? Qt : Qnil;
	  break;

	case Bconsp:
	  TOP = CONSP (TOP) ? Qt : Qnil;
	  break;

	case Bstringp:
	  TOP = XTYPE (TOP) == Lisp_String ? Qt : Qnil;
	  break;

	case Blistp:
	  TOP = CONSP (TOP) || NULL (TOP) ? Qt : Qnil;
	  break;

	case Beq:
	  v1 = POP;
	  TOP = EQ (v1, TOP) ? Qt : Qnil;
	  break;

	case Bmemq:
	  v1 = POP;
	  TOP = Fmemq (TOP, v1);
	  break;

	case Bnot:
	  TOP = NULL (TOP) ? Qt : Qnil;
	  break;

	case Bcar:
	  v1 = TOP;
	docar:
	  if (CONSP (v1)) TOP = XCONS (v1)->car;
	  else if (NULL (v1)) TOP = Qnil;
	  else Fcar (wrong_type_argument (Qlistp, v1));
	  break;

	case Bcdr:
	  v1 = TOP;
	  if (CONSP (v1)) TOP = XCONS (v1)->cdr;
	  else if (NULL (v1)) TOP = Qnil;
	  else Fcdr (wrong_type_argument (Qlistp, v1));
	  break;

	case Bcons:
	  v1 = POP;
	  TOP = Fcons (TOP, v1);
	  break;

	case Blist1:
	  TOP = Fcons (TOP, Qnil);
	  break;

	case Blist2:
	  v1 = POP;
	  TOP = Fcons (TOP, Fcons (v1, Qnil));
	  break;

	case Blist3:
	  DISCARD(2);
	  TOP = Flist (3, &TOP);
	  break;

	case Blist4:
	  DISCARD(3);
	  TOP = Flist (4, &TOP);
	  break;

	case Blength:
	  TOP = Flength (TOP);
	  break;

	case Baref:
	  v1 = POP;
	  TOP = Faref (TOP, v1);
	  break;

	case Baset:
	  v2 = POP; v1 = POP;
	  TOP = Faset (TOP, v1, v2);
	  break;

	case Bsymbol_value:
	  TOP = Fsymbol_value (TOP);
	  break;

	case Bsymbol_function:
	  TOP = Fsymbol_function (TOP);
	  break;

	case Bset:
	  v1 = POP;
	  TOP = Fset (TOP, v1);
	  break;

	case Bfset:
	  v1 = POP;
	  TOP = Ffset (TOP, v1);
	  break;

	case Bget:
	  v1 = POP;
	  TOP = Fget (TOP, v1);
	  break;

	case Bsubstring:
	  v2 = POP; v1 = POP;
	  TOP = Fsubstring (TOP, v1, v2);
	  break;

	case Bconcat2:
	  DISCARD(1);
	  TOP = Fconcat (2, &TOP);
	  break;

	case Bconcat3:
	  DISCARD(2);
	  TOP = Fconcat (3, &TOP);
	  break;

	case Bconcat4:
	  DISCARD(3);
	  TOP = Fconcat (4, &TOP);
	  break;

	case Bsub1:
	  v1 = TOP;
	  if (XTYPE (v1) == Lisp_Int)
	    {
	      XSETINT (v1, XINT (v1) - 1);
	      TOP = v1;
	    }
	  else
	    TOP = Fsub1 (v1);
	  break;

	case Badd1:
	  v1 = TOP;
	  if (XTYPE (v1) == Lisp_Int)
	    {
	      XSETINT (v1, XINT (v1) + 1);
	      TOP = v1;
	    }
	  else
	    TOP = Fadd1 (v1);
	  break;

	case Beqlsign:
	  v2 = POP; v1 = TOP;
	  CHECK_NUMBER_COERCE_MARKER (v1, 0);
	  CHECK_NUMBER_COERCE_MARKER (v2, 0);
	  TOP = XINT (v1) == XINT (v2) ? Qt : Qnil;
	  break;

	case Bgtr:
	  v1 = POP;
	  TOP = Fgtr (TOP, v1);
	  break;

	case Blss:
	  v1 = POP;
	  TOP = Flss (TOP, v1);
	  break;

	case Bleq:
	  v1 = POP;
	  TOP = Fleq (TOP, v1);
	  break;

	case Bgeq:
	  v1 = POP;
	  TOP = Fgeq (TOP, v1);
	  break;

	case Bdiff:
	  DISCARD(1);
	  TOP = Fminus (2, &TOP);
	  break;

	case Bnegate:
	  v1 = TOP;
	  if (XTYPE (v1) == Lisp_Int)
	    {
	      XSETINT (v1, - XINT (v1));
	      TOP = v1;
	    }
	  else
	    TOP = Fminus (1, &TOP);
	  break;

	case Bplus:
	  DISCARD(1);
	  TOP = Fplus (2, &TOP);
	  break;

	case Bmax:
	  DISCARD(1);
	  TOP = Fmax (2, &TOP);
	  break;

	case Bmin:
	  DISCARD(1);
	  TOP = Fmin (2, &TOP);
	  break;

	case Bpoint:
	  XFASTINT (v1) = point;
	  PUSH (v1);
	  break;

	case Bmark:		/* this loser is no longer generated as of v18 */
	  PUSH (Fmarker_position (current_buffer->mark));
	  break;

	case Bgoto_char:
	  TOP = Fgoto_char (TOP);
	  break;

	case Binsert:
	  TOP = Finsert (1, &TOP);
	  break;

	case Bpoint_max:
	  XFASTINT (v1) = ZV;
	  PUSH (v1);
	  break;

	case Bpoint_min:
	  XFASTINT (v1) = BEGV;
	  PUSH (v1);
	  break;

	case Bchar_after:
	  TOP = Fchar_after (TOP);
	  break;

	case Bfollowing_char:
	  XFASTINT (v1) = PT == ZV ? 0 : FETCH_CHAR (point);
	  PUSH (v1);
	  break;

	case Bpreceding_char:
	  XFASTINT (v1) = point == BEGV ? 0 : FETCH_CHAR (point - 1);
	  PUSH (v1);
	  break;

	case Bcurrent_column:
	  XFASTINT (v1) = current_column ();
	  PUSH (v1);
	  break;

	case Bindent_to:
	  TOP = Findent_to (TOP, Qnil);
	  break;

	case Bscan_buffer:
	  /* Get an appropriate error.  */
	  Fsymbol_function (intern ("scan-buffer"));
	  break;

	case Beolp:
	  PUSH (Feolp ());
	  break;

	case Beobp:
	  PUSH (Feobp ());
	  break;

	case Bbolp:
	  PUSH (Fbolp ());
	  break;

	case Bbobp:
	  PUSH (Fbobp ());
	  break;

	case Bcurrent_buffer:
	  PUSH (Fcurrent_buffer ());
	  break;

	case Bset_buffer:
	  TOP = Fset_buffer (TOP);
	  break;

	case Bread_char:
	  PUSH (Fread_char ());
	  QUIT;
	  break;

	case Bset_mark:		/* this loser is no longer generated as of v18 */
	  /* TOP = Fset_mark (TOP); */
	  TOP = Fset_marker (current_buffer->mark, TOP, Fcurrent_buffer ());
	  break;

	case Binteractive_p:
	  PUSH (Finteractive_p ());
	  break;

	default:
	  if ((op -= Bconstant) < (unsigned)CONSTANTLIM)
	    PUSH (vectorp[op]);
	}
    }

 exit:
  UNGCPRO;
  /* Binds and unbinds are supposed to be compiled balanced.  */
  if (specpdl_ptr - specpdl != count)
    abort ();
  return v1;
}

syms_of_bytecode ()
{
  Qbytecode = intern ("byte-code");
  staticpro (&Qbytecode);

  defsubr (&Sbyte_code);
}

