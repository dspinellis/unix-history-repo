/* parse.c
 *
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 *  Items are left on the stack when execution finishes.
 *
 *  pop() can be used to remove items from the stack.
 *  Please remember to free() any item you pop off the stack.
 *
 *
 * HISTORY
 *
 * K. Knox, 28-Mar-85 18:26:50, Created first version.
 *
 */

#include <stdio.h>
#include <iptokens.h>

FILE *fp;
long filepos = 0;


#define len_IP_header 40

#define token_id          0340
#define token_mask        0037
#define token_short_op    0200
#define token_long_op     0240
#define token_short_seq   0300
#define token_long_seq    0340

parse(fpin)
  FILE *fpin;
  {
  /* variables */
  int c, len;
  char  string[len_IP_header+1],
       *ptr;
  int last = 0;
  int seq,
      hlen;

  fp = fpin;

  /* get the header */
  for (hlen = 0, ptr = string; hlen < len_IP_header; hlen++)
    if ( (*ptr++ = getc(fp)) == ' ' )
	break;

  *ptr = '\0';

  header(string);

  /* Analyze every token in the file. */
  while ((c = getc(fp)) != EOF)
    {
    filepos = ftell(fp)-1;
    switch (c & token_id)
      {
      case token_short_op:        /* Only allowed short op is "nop". */
        len = c & token_mask;
        (void) operator(len);
	last = 0;
        break;
      case token_long_op:
        len = getc(fp)+((c & token_mask) << 8);
        if (operator(len)) return;      /* Quit when you hit endBlock. */
	last = 0;
        break;
      case token_short_seq:
        len = getc(fp);
	seq = c & token_mask;
        sequence(seq, len, last);
	if (seq != sequenceContinued) last = seq;
        break;
      case token_long_seq:
        len = getc(fp) << 16;
        len = len+(getc(fp) << 8);
        len = len+getc(fp);
	seq = c & token_mask;
        sequence(seq, len, last);
	if (seq != sequenceContinued) last = seq;
        break;
      default:
        shortnum((c << 8)+getc(fp)-INTEGER_ZERO);
	last = 0;
        break;
      }
    }
  /* Shouldn't get here if file ends properly! */
  fprintf(stderr, "(%d) Unexpected EOF\n", filepos);
  exit(2);
  }


#ifndef debug
operator(op)
  int op;
  {
  switch (op)
    {
    case OP_makevec:                 op_makevec();                break;
    case OP_makeveclu:               op_makeveclu();             break;
    case OP_rotate:                  op_rotate();                 break;
    case OP_scale:                   op_scale();                  break;
    case OP_scale2:                  op_scale2();                 break;
    case OP_concat:                  op_concat();                 break;
    case OP_beginBlock:              op_beginblock();             break;
    case OP_endBlock:                op_endblock();               return(1);
    case OP_beginBody:		     op_beginbody();		  break;
    case OP_endBody:		     op_endbody();		  break;
    default:                         op_unknown(op);              break;
    }
  return (0);
  }

sequence(type, len, last)
  int type, len, last;
  {
  switch (type)
    {
    case sequenceComment:               seq_comment(len);           break;
    case sequenceIdentifier:            seq_identifier(len);        break;
    case sequenceInteger:               seq_integer(len);           break;
    case sequenceLargeVector:           seq_largevector(len);       break;
    case sequenceRational:              seq_rational(len);          break;
    case sequenceString:                seq_string(len);            break;
    default:                            seq_unknown(type, len);     break;
    }
  }

#else
operator(op)
  int op;
  {
  switch (op)
    {
    case OP_makevec:
      fprintf(stderr, "op_makevec\n");
      op_makevec();
      break;
    case OP_do:
      fprintf(stderr, "op_do\n");
      op_do();
      break;
    case OP_pop:
      fprintf(stderr, "op_pop\n");
      op_pop();
      break;
    case OP_copy:
      fprintf(stderr, "op_copy\n");
      op_copy();
      break;
    case OP_dup:
      fprintf(stderr, "op_dup\n");
      op_dup();
      break;
    case OP_roll:
      fprintf(stderr, "op_roll\n");
      op_roll();
      break;
    case OP_exch:
      fprintf(stderr, "op_exch\n");
      op_exch();
      break;
    case OP_nop:
      fprintf(stderr, "op_nop\n");
      op_nop();
      break;
    case OP_translate:
      fprintf(stderr, "op_translate\n");
      op_translate();
      break;
    case OP_rotate:
      fprintf(stderr, "op_rotate\n");
      op_rotate();
      break;
    case OP_scale:
      fprintf(stderr, "op_scale\n");
      op_scale();
      break;
    case OP_scale2:
      fprintf(stderr, "op_scale2\n");
      op_scale2();
      break;
    case OP_concat:
      fprintf(stderr, "op_concat\n");
      op_concat();
      break;
    case OP_makepixelarray:
      fprintf(stderr, "op_makepixelarray\n");
      op_makepixelarray();
      break;
    case OP_extractpixelarray:
      fprintf(stderr, "op_extractpixelarray\n");
      op_extractpixelarray();
      break;
    case OP_finddecompressor:
      fprintf(stderr, "op_finddecompressor\n");
      op_finddecompressor();
      break;
    case OP_makegray:
      fprintf(stderr, "op_makegray\n");
      op_makegray();
      break;
    case OP_findcolor:
      fprintf(stderr, "op_findcolor\n");
      op_findcolor();
      break;
    case OP_findcoloroperator:
      fprintf(stderr, "op_findcoloroperator\n");
      op_findcoloroperator();
      break;
    case OP_findcolormodeloperator:
      fprintf(stderr, "op_findcolormodeloperator\n");
      op_findcolormodeloperator();
      break;
    case OP_beginBlock:
      fprintf(stderr, "op_beginblock\n");
      op_beginblock();
      break;
    case OP_endBlock:
      fprintf(stderr, "op_endblock\n");
      op_endblock();
      return(1);
    case OP_beginBody:		     ;				  break;
    case OP_endBody:		     ;				  break;
    default:
      fprintf(stderr, "op_unknown\n");
      op_unknown(op);
      break;
    }
  return (0);
  }

sequence(type, len, last)
  int type, len, last;
  {
  switch (type)
    {
    case sequenceAdaptivePixelVector:
      fprintf(stderr, "seq_adaptivepixel\n");
      seq_adaptivepixel(len);
      break;
    case sequenceComment:
      fprintf(stderr, "seq_comment\n");
      seq_comment(len);
      break;
    case sequenceCompressedPixelVector:
      fprintf(stderr, "seq_compressedpixel\n");
      seq_compressedpixel(len);
      break;
    case sequenceContinued:
      fprintf(stderr, "seq_continued\n");
      break;
    case sequenceIdentifier:
      fprintf(stderr, "seq_identifier\n");
      seq_identifier(len);
      break;
    case sequenceInsertFile:
      fprintf(stderr, "seq_insertfile\n");
      seq_insertfile(len);
      break;
    case sequenceInteger:
      fprintf(stderr, "seq_integer\n");
      seq_integer(len);
      break;
    case sequenceLargeVector:
      fprintf(stderr, "seq_largevector\n");
      seq_largevector(len);
      break;
    case sequencePackedPixelVector:
      fprintf(stderr, "seq_packedpixel\n");
      seq_packedpixel(len);
      break;
    case sequenceRational:
      fprintf(stderr, "seq_rational\n");
      seq_rational(len);
      break;
    case sequenceString:
      fprintf(stderr, "seq_string\n");
      seq_string(len);
      break;
    default:
      fprintf(stderr, "seq_unknown\n");
      seq_unknown(type, len);     
      break;
    }
  }

#endif

