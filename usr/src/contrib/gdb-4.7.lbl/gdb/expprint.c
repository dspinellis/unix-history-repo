/* Print in infix form a struct expression.
   Copyright (C) 1986, 1989, 1991 Free Software Foundation, Inc.

This file is part of GDB.

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
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "value.h"
#include "language.h"
#include "parser-defs.h"

/* Prototypes for local functions */

static void
print_subexp PARAMS ((struct expression *, int *, FILE *, enum precedence));

static void
print_simple_m2_func PARAMS ((char *, struct expression *, int *, FILE *));

void
print_expression (exp, stream)
     struct expression *exp;
     FILE *stream;
{
  int pc = 0;
  print_subexp (exp, &pc, stream, PREC_NULL);
}

/* Print the subexpression of EXP that starts in position POS, on STREAM.
   PREC is the precedence of the surrounding operator;
   if the precedence of the main operator of this subexpression is less,
   parentheses are needed here.  */

static void
print_subexp (exp, pos, stream, prec)
     register struct expression *exp;
     register int *pos;
     FILE *stream;
     enum precedence prec;
{
  register unsigned tem;
  register const struct op_print *op_print_tab;
  register int pc;
  unsigned nargs;
  register char *op_str;
  int assign_modify = 0;
  enum exp_opcode opcode;
  enum precedence myprec;
  /* Set to 1 for a right-associative operator.  */
  int assoc;
  value val;

  op_print_tab = exp->language_defn->la_op_print_tab;
  pc = (*pos)++;
  opcode = exp->elts[pc].opcode;
  switch (opcode)
    {
    /* Common ops */

    case OP_SCOPE:
      myprec = PREC_PREFIX;
      assoc = 0;
      (*pos) += 2;
      print_subexp (exp, pos, stream,
		    (enum precedence) ((int) myprec + assoc));
      fputs_filtered (" :: ", stream);
      nargs = strlen (&exp->elts[pc + 2].string);
      (*pos) += 1 + (nargs + sizeof (union exp_element)) / sizeof (union exp_element);

      fputs_filtered (&exp->elts[pc + 2].string, stream);
      return;

    case OP_LONG:
      (*pos) += 3;
      value_print (value_from_longest (exp->elts[pc + 1].type,
				    exp->elts[pc + 2].longconst),
		   stream, 0, Val_no_prettyprint);
      return;

    case OP_DOUBLE:
      (*pos) += 3;
      value_print (value_from_double (exp->elts[pc + 1].type,
				      exp->elts[pc + 2].doubleconst),
		   stream, 0, Val_no_prettyprint);
      return;

    case OP_VAR_VALUE:
      (*pos) += 2;
      fputs_filtered (SYMBOL_NAME (exp->elts[pc + 1].symbol), stream);
      return;

    case OP_LAST:
      (*pos) += 2;
      fprintf_filtered (stream, "$%d",
			longest_to_int (exp->elts[pc + 1].longconst));
      return;

    case OP_REGISTER:
      (*pos) += 2;
      fprintf_filtered (stream, "$%s",
	       reg_names[longest_to_int (exp->elts[pc + 1].longconst)]);
      return;

    case OP_INTERNALVAR:
      (*pos) += 2;
      fprintf_filtered (stream, "$%s",
	       internalvar_name (exp->elts[pc + 1].internalvar));
      return;

    case OP_FUNCALL:
      (*pos) += 2;
      nargs = longest_to_int (exp->elts[pc + 1].longconst);
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fputs_filtered (" (", stream);
      for (tem = 0; tem < nargs; tem++)
	{
	  if (tem != 0)
	    fputs_filtered (", ", stream);
	  print_subexp (exp, pos, stream, PREC_ABOVE_COMMA);
	}
      fputs_filtered (")", stream);
      return;

    case OP_STRING:
      nargs = strlen (&exp->elts[pc + 1].string);
      (*pos) += 2 + (nargs + sizeof (union exp_element)) / sizeof (union exp_element);
      fputs_filtered ("\"", stream);
      for (tem = 0; tem < nargs; tem++)
	printchar ((&exp->elts[pc + 1].string)[tem], stream, '"');
      fputs_filtered ("\"", stream);
      return;

    case TERNOP_COND:
      if ((int) prec > (int) PREC_COMMA)
	fputs_filtered ("(", stream);
      /* Print the subexpressions, forcing parentheses
	 around any binary operations within them.
	 This is more parentheses than are strictly necessary,
	 but it looks clearer.  */
      print_subexp (exp, pos, stream, PREC_HYPER);
      fputs_filtered (" ? ", stream);
      print_subexp (exp, pos, stream, PREC_HYPER);
      fputs_filtered (" : ", stream);
      print_subexp (exp, pos, stream, PREC_HYPER);
      if ((int) prec > (int) PREC_COMMA)
	fputs_filtered (")", stream);
      return;

    case STRUCTOP_STRUCT:
      tem = strlen (&exp->elts[pc + 1].string);
      (*pos) += 2 + (tem + sizeof (union exp_element)) / sizeof (union exp_element);
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fputs_filtered (".", stream);
      fputs_filtered (&exp->elts[pc + 1].string, stream);
      return;

    /* Will not occur for Modula-2 */
    case STRUCTOP_PTR:
      tem = strlen (&exp->elts[pc + 1].string);
      (*pos) += 2 + (tem + sizeof (union exp_element)) / sizeof (union exp_element);
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fputs_filtered ("->", stream);
      fputs_filtered (&exp->elts[pc + 1].string, stream);
      return;

    case BINOP_SUBSCRIPT:
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fputs_filtered ("[", stream);
      print_subexp (exp, pos, stream, PREC_ABOVE_COMMA);
      fputs_filtered ("]", stream);
      return;

    case UNOP_POSTINCREMENT:
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fputs_filtered ("++", stream);
      return;

    case UNOP_POSTDECREMENT:
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fputs_filtered ("--", stream);
      return;

    case UNOP_CAST:
      (*pos) += 2;
      if ((int) prec > (int) PREC_PREFIX)
        fputs_filtered ("(", stream);
      fputs_filtered ("(", stream);
      type_print (exp->elts[pc + 1].type, "", stream, 0);
      fputs_filtered (") ", stream);
      print_subexp (exp, pos, stream, PREC_PREFIX);
      if ((int) prec > (int) PREC_PREFIX)
        fputs_filtered (")", stream);
      return;

    case UNOP_MEMVAL:
      (*pos) += 2;
      if ((int) prec > (int) PREC_PREFIX)
        fputs_filtered ("(", stream);
      if (exp->elts[pc + 1].type->code == TYPE_CODE_FUNC &&
	  exp->elts[pc + 3].opcode == OP_LONG) {
	/* We have a minimal symbol fn, probably.  It's encoded
	   as a UNOP_MEMVAL (function-type) of an OP_LONG (int, address).
	   Swallow the OP_LONG (including both its opcodes); ignore
	   its type; print the value in the type of the MEMVAL.  */
	(*pos) += 4;
	val = value_at_lazy (exp->elts[pc + 1].type,
			     (CORE_ADDR) exp->elts[pc + 5].longconst);
	value_print (val, stream, 0, Val_no_prettyprint);
      } else {
	fputs_filtered ("{", stream);
	type_print (exp->elts[pc + 1].type, "", stream, 0);
	fputs_filtered ("} ", stream);
        print_subexp (exp, pos, stream, PREC_PREFIX);
      }
      if ((int) prec > (int) PREC_PREFIX)
        fputs_filtered (")", stream);
      return;

    case BINOP_ASSIGN_MODIFY:
      opcode = exp->elts[pc + 1].opcode;
      (*pos) += 2;
      myprec = PREC_ASSIGN;
      assoc = 1;
      assign_modify = 1;
      op_str = "???";
      for (tem = 0; op_print_tab[tem].opcode != OP_NULL; tem++)
	if (op_print_tab[tem].opcode == opcode)
	  {
	    op_str = op_print_tab[tem].string;
	    break;
	  }
      break;

    /* C++ ops */

    case OP_THIS:
      ++(*pos);
      fputs_filtered ("this", stream);
      return;

    /* Modula-2 ops */

    case BINOP_MULTI_SUBSCRIPT:
      (*pos) += 2;
      nargs = longest_to_int (exp->elts[pc + 1].longconst);
      print_subexp (exp, pos, stream, PREC_SUFFIX);
      fprintf (stream, " [");
      for (tem = 0; tem < nargs; tem++)
	{
	  if (tem != 0)
	    fprintf (stream, ", ");
	  print_subexp (exp, pos, stream, PREC_ABOVE_COMMA);
	}
      fprintf (stream, "]");
      return;

    case BINOP_VAL:
      (*pos)+=2;
      fprintf(stream,"VAL(");
      type_print(exp->elts[pc+1].type,"",stream,0);
      fprintf(stream,",");
      print_subexp(exp,pos,stream,PREC_PREFIX);
      fprintf(stream,")");
      return;

    case UNOP_CAP:
      print_simple_m2_func("CAP",exp,pos,stream);
      return;

    case UNOP_CHR:
      print_simple_m2_func("CHR",exp,pos,stream);
      return;

    case UNOP_ORD:
      print_simple_m2_func("ORD",exp,pos,stream);
      return;
      
    case UNOP_ABS:
      print_simple_m2_func("ABS",exp,pos,stream);
      return;

    case UNOP_FLOAT:
      print_simple_m2_func("FLOAT",exp,pos,stream);
      return;

    case UNOP_HIGH:
      print_simple_m2_func("HIGH",exp,pos,stream);
      return;

    case UNOP_MAX:
      print_simple_m2_func("MAX",exp,pos,stream);
      return;

    case UNOP_MIN:
      print_simple_m2_func("MIN",exp,pos,stream);
      return;

    case UNOP_ODD:
      print_simple_m2_func("ODD",exp,pos,stream);
      return;

    case UNOP_TRUNC:
      print_simple_m2_func("TRUNC",exp,pos,stream);
      return;
      
    case BINOP_INCL:
    case BINOP_EXCL:
      error("print_subexp:  Not implemented.");

    /* Default ops */

    default:
      op_str = "???";
      for (tem = 0; op_print_tab[tem].opcode != OP_NULL; tem++)
	if (op_print_tab[tem].opcode == opcode)
	  {
	    op_str = op_print_tab[tem].string;
	    myprec = op_print_tab[tem].precedence;
	    assoc = op_print_tab[tem].right_assoc;
	    break;
	  }
   }

  if ((int) myprec < (int) prec)
    fputs_filtered ("(", stream);
  if ((int) opcode > (int) BINOP_END)
    {
      /* Unary prefix operator.  */
      fputs_filtered (op_str, stream);
      print_subexp (exp, pos, stream, PREC_PREFIX);
    }
  else
    {
      /* Binary operator.  */
      /* Print left operand.
	 If operator is right-associative,
	 increment precedence for this operand.  */
      print_subexp (exp, pos, stream,
		    (enum precedence) ((int) myprec + assoc));
      /* Print the operator itself.  */
      if (assign_modify)
	fprintf_filtered (stream, " %s= ", op_str);
      else if (op_str[0] == ',')
	fprintf_filtered (stream, "%s ", op_str);
      else
	fprintf_filtered (stream, " %s ", op_str);
      /* Print right operand.
	 If operator is left-associative,
	 increment precedence for this operand.  */
      print_subexp (exp, pos, stream,
		    (enum precedence) ((int) myprec + !assoc));
    }

  if ((int) myprec < (int) prec)
    fputs_filtered (")", stream);
}

/* Print out something of the form <s>(<arg>).
   This is used to print out some builtin Modula-2
   functions.
   FIXME:  There is probably some way to get the precedence
   rules to do this (print a unary operand with parens around it).  */
static void
print_simple_m2_func(s,exp,pos,stream)
   char *s;
   register struct expression *exp;
   register int *pos;
   FILE *stream;
{
   fprintf(stream,"%s(",s);
   print_subexp(exp,pos,stream,PREC_PREFIX);
   fprintf(stream,")");
}
   
/* Return the operator corresponding to opcode OP as
   a string.   NULL indicates that the opcode was not found in the
   current language table.  */
char *
op_string(op)
   enum exp_opcode op;
{
  int tem;
  register const struct op_print *op_print_tab;

  op_print_tab = current_language->la_op_print_tab;
  for (tem = 0; op_print_tab[tem].opcode != OP_NULL; tem++)
    if (op_print_tab[tem].opcode == op)
      return op_print_tab[tem].string;
  return NULL;
}
