/* enumerate.c
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 *  Define the functions used in parse.c.
 *
 *
 *
 */

#include <stdio.h>

#define octal 0
#define hex 1
#define decimal 2
#define character 3

extern long filepos;
extern FILE *fp;
int verbose = 0;

header(string)
  char *string;
  {
  printf("(%u) header: %s\n", filepos, string);
  }

op_makevec()
  {
  printf("(%u) makevec\n", filepos);
  }

op_do()
  {
  printf("(%u) do\n", filepos);
  }

op_pop()
  {
  printf("(%u) pop\n", filepos);
  }

op_copy()
  {
  printf("(%u) copy\n", filepos);
  }

op_dup()
  {
  printf("(%u) dup\n", filepos);
  }

op_roll()
  {
  printf("(%u) roll\n", filepos);
  }

op_exch()
  {
  printf("(%u) exch\n", filepos);
  }

op_nop()
  {
  printf("(%u) nop\n", filepos);
  }

op_translate()
  {
  printf("(%u) translate\n", filepos);
  }

op_rotate()
  {
  printf("(%u) rotate\n", filepos);
  }

op_scale()
  {
  printf("(%u) scale\n", filepos);
  }

op_scale2()
  {
  printf("(%u) scale2\n", filepos);
  }

op_concat()
  {
  printf("(%u) concat\n", filepos);
  }

op_makepixelarray()
  {
  printf("(%u) makepixelarray\n", filepos);
  }

op_extractpixelarray()
  {
  printf("(%u) extractpixelarray\n", filepos);
  }

op_finddecompressor()
  {
  printf("(%u) finddecompressor\n", filepos);
  }

op_makegray()
  {
  printf("(%u) makegray\n", filepos);
  }

op_findcolor()
  {
  printf("(%u) findcolor\n", filepos);
  }

op_findcoloroperator()
  {
  printf("(%u) findcoloroperator\n", filepos);
  }

op_findcolormodeloperator()
  {
  printf("(%u) findcolormodeloperator\n", filepos);
  }

op_beginblock()
  {
  printf("(%u) beginblock\n", filepos);
  }

op_endblock()
  {
  printf("(%u) endblock\n", filepos);
  }

op_unknown(op)
  int op;
  {
  printf("(%u) unknown operator: %u\n", filepos, op);
  }

seq_adaptivepixel(len)
  int len;
  {
  printf("(%u) sequence adaptive pixel vector, %u bytes:\n", filepos, len);
  printbytes(len, decimal, verbose);
  }

seq_comment(len)
  int len;
  {
  printf("(%u) sequence comment, %u bytes:\n", filepos, len);
  printbytes(len, character, 1);
  }

seq_compressedpixel(len)
  int len;
  {
  printf("(%u) sequence compressed pixel vector, %u bytes:\n", filepos, len);
  printbytes(len, decimal, verbose);
  }

seq_continued(len, last)
  int len, last;
  {
  printf("(%u) sequence continued, %u bytes:\n", filepos, len);
  printbytes(len, decimal, verbose);
  }

seq_identifier(len)
  int len;
  {
  printf("(%u) sequence identifier, %u bytes:\n", filepos, len);
  printbytes(len, character, 1);
  }

seq_insertfile(len)
  int len;
  {
  printf("(%u) sequence insert file, %u bytes:\n", filepos, len);
  printbytes(len, character, 1);
  }

seq_integer(len)
  int len;
  {
  printf("(%u) sequence integer, %u bytes:\n", filepos, len);
  printbytes(len, decimal, 1);
  }

seq_largevector(len)
  int len;
  {
  printf("(%u) sequence large vector, %u bytes:\n", filepos, len);
  printbytes(len, decimal, verbose);
  }

seq_packedpixel(len)
  int len;
  {
  printf("(%u) sequence packed pixel vector, %u bytes:\n", filepos, len);
  printbytes(len, decimal, verbose);
  }

seq_rational(len)
  int len;
  {
  printf("(%u) sequence rational, %u bytes:\n", filepos, len);
  printbytes(len, decimal, 1);
  }

seq_string(len)
  int len;
  {
  printf("(%u) sequence string, %u bytes:\n", filepos, len);
  printbytes(len, character, 1);
  }

seq_unknown(type, len)
  int type, len;
  {
  printf("(%u) unknown sequence, %u bytes:\n", filepos, len);
  printbytes(len, decimal, verbose);
  }

shortnum(number)
  int number;
  {
  printf("(%u) %d\n", filepos, number);
  }

printbytes(len, format, v)
  int len;
  {
  int n, c;
  if (v == 0) { fseek(fp, len, 1); return; }
  for (n=0; n < len; n++)
    {
    c = getc(fp);
    switch (format)
      {
      case octal:      printf("%o ", c);   break;
      case hex:        printf("%x ", c);   break;
      case decimal:    printf("%u ", c);   break;
      case character:  printf("%c ", c);   break;
      }
    if ((n % 8) == 7) printf("\n");
    }
  printf("\n");
  }

/* Change Log
 *
 * K. Knox, 28-Mar-85 15:01:49, Created first version.
 *
 *
 *
 */
