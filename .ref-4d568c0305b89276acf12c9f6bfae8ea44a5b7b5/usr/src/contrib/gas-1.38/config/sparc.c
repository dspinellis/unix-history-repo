/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1993 by Chris Torek at Lawrence Berkeley Laboratory.
 */

#ifndef lint
static char sccsid[] = "@(#)sparc.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/* sparc.c -- Assemble for the SPARC
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


#include <stdio.h>
#include <ctype.h>

#include "sparc-opcode.h"
#include "as.h"
#include "frags.h"
#include "struc-symbol.h"
#include "flonum.h"
#include "expr.h"
#include "hash.h"
#include "md.h"
#include "sparc.h"
#include "write.h"
#include "read.h"
#include "symbols.h"

void md_begin();
void md_end();
void md_number_to_chars();
void md_assemble();
char *md_atof();
void md_convert_frag();
void md_create_short_jump();
void md_create_long_jump();
int  md_estimate_size_before_relax();
void md_number_to_imm();
void md_number_to_disp();
void md_number_to_field();
void md_ri_to_chars();
void emit_relocations();
static void sparc_ip();

const relax_typeS md_relax_table[] = { 0 };

/* handle of the OPCODE hash table */
static struct hash_control *op_hash = NULL;

static void s_seg(), s_proc(), s_data1(), s_reserve(), s_common();
extern void s_globl(), s_long(), s_short(), s_space(), cons();

const pseudo_typeS
md_pseudo_table[] = {
    { "common",     s_common,   0 },
    { "global",     s_globl,    0 },
    { "half",       cons,       2 },
    { "proc",       s_proc,     0 },
    { "reserve",    s_reserve,  0 },
    { "seg",        s_seg,      0 },
    { "skip",       s_space,    0 },
    { "word",       cons,       4 },
    { NULL,         0,          0 },
};

int md_short_jump_size = 4;
int md_long_jump_size = 4;
int omagic  =  (0x103 << 16) | OMAGIC;  /* Magic number for header */

/* This array holds the chars that always start a comment.  If the
    pre-processor is disabled, these aren't very useful */
char comment_chars[] = "!";	/* JF removed '|' from comment_chars */

/* This array holds the chars that only start a comment at the beginning of
   a line.  If the line seems to have the form '# 123 filename'
   .line and .file directives will appear in the pre-processed output */
/* Note that input_file.c hand checks for '#' at the beginning of the
   first line of the input file.  This is because the compiler outputs
   #NO_APP at the beginning of its output. */
/* Also note that '/*' will always start a comment */
char line_comment_chars[] = "#";

/* Chars that can be used to separate mant from exp in floating point nums */
char EXP_CHARS[] = "eE";

/* Chars that mean this number is a floating point constant */
/* As in 0f12.456 */
/* or    0d1.2345e12 */
char FLT_CHARS[] = "rRsSfFdDxXpP";

/* Also be aware that MAXIMUM_NUMBER_OF_CHARS_FOR_FLOAT may have to be
   changed in read.c .  Ideally it shouldn't have to know about it at all,
   but nothing is ideal around here.
 */
int size_reloc_info = sizeof(struct reloc_info_sparc);

static unsigned char octal[256];
#define isoctal(c)  octal[c]
static unsigned char toHex[256];

/*
 *  anull bit - causes the branch delay slot instructions to not be executed 
 */
#define ANNUL       (1 << 29)

struct sparc_it {
    char    *error;
    unsigned long opcode;
    struct nlist *nlistp;
    expressionS exp;
    int pcrel;
    enum reloc_type reloc;
} the_insn, set_insn;

#ifdef __STDC__
static void print_insn(struct sparc_it *insn);
static int getExpression(char *str);
#else
static void print_insn();
static int getExpression();
#endif
static char *expr_end;
static int special_case;

#define SPECIAL_CASE_SET    1

/*
 * sort of like s_lcomm
 *
 */
static void
s_reserve()
{
    char *name;
    char c;
    char *p;
    int temp;
    symbolS *symbolP;

    name = input_line_pointer;
    c = get_symbol_end();
    p = input_line_pointer;
    *p = c;
    SKIP_WHITESPACE();
    if ( * input_line_pointer != ',' ) {
	as_warn("Expected comma after name");
	ignore_rest_of_line();
	return;
    }
    input_line_pointer ++;
    if ((temp = get_absolute_expression()) < 0) {
	as_warn("BSS length (%d.) <0! Ignored.", temp);
	ignore_rest_of_line();
	return;
    }
    *p = 0;
    symbolP = symbol_find_or_make(name);
    *p = c;
    if (strncmp(input_line_pointer, ",\"bss\"", 6) != 0) {
	as_warn("bad .reserve segment: `%s'", input_line_pointer);
	return;
    }
    input_line_pointer += 6;
    if (symbolP->sy_other == 0 
        && symbolP->sy_desc  == 0
	&& ((symbolP->sy_type  == N_BSS
	&& symbolP->sy_value == local_bss_counter)
	|| ((symbolP->sy_type & N_TYPE) == N_UNDF
	&& symbolP->sy_value == 0))) {
	    symbolP->sy_value = local_bss_counter;
	    symbolP->sy_type  = N_BSS;
	    symbolP->sy_frag  = & bss_address_frag;
	    local_bss_counter += temp;
    } else {
	as_warn( "Ignoring attempt to re-define symbol from %d. to %d.",
	    symbolP->sy_value, local_bss_counter );
    }
    demand_empty_rest_of_line();
    return;
}

static void
s_common()
{
    register char *name;
    register char c;
    register char *p;
    register int temp;
    register symbolS *	symbolP;

    name = input_line_pointer;
    c = get_symbol_end();
    /* just after name is now '\0' */
    p = input_line_pointer;
    *p = c;
    SKIP_WHITESPACE();
    if ( * input_line_pointer != ',' ) {
	as_warn("Expected comma after symbol-name");
	ignore_rest_of_line();
	return;
    }
    input_line_pointer ++; /* skip ',' */
    if ( (temp = get_absolute_expression ()) < 0 ) {
	as_warn(".COMMon length (%d.) <0! Ignored.", temp);
	ignore_rest_of_line();
	return;
    }
    *p = 0;
    symbolP = symbol_find_or_make (name);
    *p = c;
    if (   (symbolP->sy_type & N_TYPE) != N_UNDF ||
        symbolP->sy_other != 0 || symbolP->sy_desc != 0) {
	as_warn( "Ignoring attempt to re-define symbol");
	ignore_rest_of_line();
	return;
    }
    if (symbolP->sy_value) {
	if (symbolP->sy_value != temp) {
	    as_warn( "Length of .comm \"%s\" is already %d. Not changed to %d.",
		symbolP->sy_name, symbolP->sy_value, temp);
	}
    } else {
	symbolP->sy_value = temp;
	symbolP->sy_type |= N_EXT;
    }
    know(symbolP->sy_frag == &zero_address_frag);
    if (strncmp(input_line_pointer, ",\"bss\"", 6) != 0) {
	p=input_line_pointer;
	while(*p && *p!='\n')
		p++;
	c= *p;
	*p='\0';
	as_warn("bad .common segment: `%s'", input_line_pointer);
	*p=c;
	return;
    }
    input_line_pointer += 6;
    demand_empty_rest_of_line();
    return;
}

static void
s_seg()
{

    if (strncmp(input_line_pointer, "\"text\"", 6) == 0) {
	input_line_pointer += 6;
	s_text();
	return;
    }
    if (strncmp(input_line_pointer, "\"data\"", 6) == 0) {
	input_line_pointer += 6;
	s_data();
	return;
    }
    if (strncmp(input_line_pointer, "\"data1\"", 7) == 0) {
	input_line_pointer += 7;
	s_data1();
	return;
    }
    as_warn("Unknown segment type");
    demand_empty_rest_of_line();
    return;
}

static void
s_data1()
{
    subseg_new(SEG_DATA, 1);
    demand_empty_rest_of_line();
    return;
}

static void
s_proc()
{
    extern char is_end_of_line[];

    while (!is_end_of_line[*input_line_pointer]) {
	++input_line_pointer;
    }
    ++input_line_pointer;
    return;
}

/* This function is called once, at assembler startup time.  It should
   set up all the tables, etc. that the MD part of the assembler will need.  */
void
md_begin()
{
  register char *retval = NULL;
  int lose = 0;
  register unsigned int i = 0;

  op_hash = hash_new();
  if (op_hash == NULL)
    as_fatal("Virtual memory exhausted");

  while (i < NUMOPCODES)
    {
      const char *name = sparc_opcodes[i].name;
      retval = hash_insert(op_hash, name, &sparc_opcodes[i]);
      if(retval != NULL && *retval != '\0')
	{
	  fprintf (stderr, "internal error: can't hash `%s': %s\n",
		   sparc_opcodes[i].name, retval);
	  lose = 1;
	}
      do
	{
	  if (sparc_opcodes[i].match & sparc_opcodes[i].lose)
	    {
	      fprintf (stderr, "internal error: losing opcode: `%s' \"%s\"\n",
		       sparc_opcodes[i].name, sparc_opcodes[i].args);
	      lose = 1;
	    }
	  ++i;
	} while (i < NUMOPCODES
		 && !strcmp(sparc_opcodes[i].name, name));
    }

  if (lose)
    as_fatal ("Broken assembler.  No assembly attempted.");

  for (i = '0'; i < '8'; ++i)
    octal[i] = 1;
  for (i = '0'; i <= '9'; ++i)
    toHex[i] = i - '0';
  for (i = 'a'; i <= 'f'; ++i)
    toHex[i] = i + 10 - 'a';
  for (i = 'A'; i <= 'F'; ++i)
    toHex[i] = i + 10 - 'A';
}

void
md_end()
{
    return;
}

void
md_assemble(str)
    char *str;
{
    char *toP;
    int rsd;

    assert(str);
    sparc_ip(str);
    toP = frag_more(4);
    /* put out the opcode */
    md_number_to_chars(toP, the_insn.opcode, 4);

    /* put out the symbol-dependent stuff */
    if (the_insn.reloc != NO_RELOC) {
	fix_new(
	    frag_now,                           /* which frag */
	    (toP - frag_now->fr_literal), /* where */
	    4,                                  /* size */
	    the_insn.exp.X_add_symbol,
	    the_insn.exp.X_subtract_symbol,
	    the_insn.exp.X_add_number,
	    the_insn.pcrel,
	    the_insn.reloc
	);
    }
    switch (special_case) {

    case SPECIAL_CASE_SET:
	special_case = 0;
	assert(the_insn.reloc == RELOC_HI22);
	if (the_insn.exp.X_seg == SEG_ABSOLUTE &&
	    the_insn.exp.X_add_symbol == 0 &&
	    the_insn.exp.X_subtract_symbol == 0 &&
	    (the_insn.exp.X_add_number & 0x3ff) == 0)
		return;
	toP = frag_more(4);
	rsd = (the_insn.opcode >> 25) & 0x1f;
	the_insn.opcode = 0x80102000 | (rsd << 25) | (rsd << 14);
	md_number_to_chars(toP, the_insn.opcode, 4);
	fix_new(
	    frag_now,                           /* which frag */
	    (toP - frag_now->fr_literal),       /* where */
	    4,                                  /* size */
	    the_insn.exp.X_add_symbol,
	    the_insn.exp.X_subtract_symbol,
	    the_insn.exp.X_add_number,
	    the_insn.pcrel,
	    RELOC_LO10
	);
	return;

    case 0:
	return;

    default:
	abort();
    }
}

static void
sparc_ip(str)
    char *str;
{
    char *s;
    const char *args;
    char c;
    unsigned long i;
    struct sparc_opcode *insn;
    char *argsStart;
    unsigned long   opcode;
    unsigned int mask;
    int match = FALSE;
    int comma = 0;

    for (s = str; islower(*s) || (*s >= '0' && *s <= '3'); ++s)
	;
    switch (*s) {

    case '\0':
	break;

    case ',':
	comma = 1;

	/*FALLTHROUGH*/

    case ' ':
	*s++ = '\0';
	break;

    default:
	    as_warn("Unknown opcode: `%s'", str);
	    exit(1);
    }
    if ((insn = (struct sparc_opcode *) hash_find(op_hash, str)) == NULL) {
	as_warn("Unknown opcode: `%s'", str);
	return;
    }
    if (comma) {
	*--s = ',';
    }
    argsStart = s;
    for (;;) {
	opcode = insn->match;
	bzero(&the_insn, sizeof(the_insn));
	the_insn.reloc = NO_RELOC;

	/*
	 * Build the opcode, checking as we go to make
	 * sure that the operands match
	 */
	for (args = insn->args; ; ++args) {
	    switch (*args) {

	    case '\0':  /* end of args */
		if (*s == '\0') {
		    match = TRUE;
		}
		break;

	    case '+':
		if (*s == '+') {
		    ++s;
		    continue;
		}
		if (*s == '-') {
		    continue;
		}
		break;

	    case '[':   /* these must match exactly */
	    case ']':
	    case ',':
	    case ' ':
		if (*s++ == *args)
		    continue;
		break;

	    case '#':   /* must be at least one digit */
		if (isdigit(*s++)) {
		    while (isdigit(*s)) {
			++s;
		    }
		    continue;
		}
		break;

	    case 'C':   /* coprocessor state register */
		if (strncmp(s, "%csr", 4) == 0) {
		    s += 4;
		    continue;
		}
		break;

	    case 'b':    /* next operand is a coprocessor register */
	    case 'c':
	    case 'D':
	        if (*s++ == '%' && *s++ == 'c' && isdigit(*s)) {
		    mask = *s++;
		    if (isdigit(*s)) {
			mask = 10 * (mask - '0') + (*s++ - '0');
			if (mask >= 32) {
			    break;
			}
		    } else {
			mask -= '0';
		    }
		    switch (*args) {

		    case 'b':
			opcode |= mask << 14;
			continue;

		    case 'c':
			opcode |= mask;
			continue;

		    case 'D':
			opcode |= mask << 25;
			continue;
		    }
		}
		break;

	    case 'r':   /* next operand must be a register */
	    case 'R':
	    case '1':
	    case '2':
	    case 'd':
		if (*s++ == '%') {
		    switch (c = *s++) {

		    case 'f':   /* frame pointer */
		        if (*s++ == 'p') {
			    mask = 0x1e;
			    break;
			}
			goto error;

		    case 'g':   /* global register */
			if (isoctal(c = *s++)) {
			    mask = c - '0';
			    break;
			}
			goto error;

		    case 'i':   /* in register */
			if (isoctal(c = *s++)) {
			    mask = c - '0' + 24;
			    break;
			}
			goto error;

		    case 'l':   /* local register */
			if (isoctal(c = *s++)) {
			    mask= (c - '0' + 16) ;
			    break;
			}
			goto error;

		    case 'o':   /* out register */
			if (isoctal(c = *s++)) {
			    mask= (c - '0' + 8) ;
			    break;
			}
			goto error;

		    case 's':   /* stack pointer */
		        if (*s++ == 'p') {
			    mask= 0xe;
			    break;
			}
			goto error;

		    case 'r': /* any register */
		        if (!isdigit(c = *s++)) {
			    goto error;
			}
			/* FALLTHROUGH */
		    case '0': case '1': case '2': case '3': case '4':
		    case '5': case '6': case '7': case '8': case '9':
			if (isdigit(*s)) {
			    if ((c = 10 * (c - '0') + (*s++ - '0')) >= 32) {
				goto error;
			    }
			} else {
			    c -= '0';
			}
			mask= c;
			break;

		    default:
			goto error;
		    }
		    /*
		     * Got the register, now figure out where
		     * it goes in the opcode.
		     */
		    switch (*args) {

		    case '1':
			opcode |= mask << 14;
			continue;

		    case '2':
			opcode |= mask;
			continue;

		    case 'd':
			opcode |= mask << 25;
			continue;

		    case 'r':
			opcode |= (mask << 25) | (mask << 14);
			continue;

		    case 'R':
			opcode |= (mask << 25) | mask;
			continue;
		    }
		}
		break;

	    case 'e':    /* next operand is a floating point register */
	    case 'f':
	    case 'g':
	        if (*s++ == '%' && *s++ == 'f' && isdigit(*s)) {
		    mask = *s++;
		    if (isdigit(*s)) {
			mask = 10 * (mask - '0') + (*s++ - '0');
			if (mask >= 32) {
			    break;
			}
		    } else {
			mask -= '0';
		    }
		    switch (*args) {

		    case 'e':
			opcode |= mask << 14;
			continue;

		    case 'f':
			opcode |= mask;
			continue;

		    case 'g':
			opcode |= mask << 25;
			continue;
		    }
		}
		break;

	    case 'F':
		if (strncmp(s, "%fsr", 4) == 0) {
		    s += 4;
		    continue;
		}
		break;

	    case 'h':       /* high 22 bits */
	        the_insn.reloc = RELOC_HI22;
		goto immediate;

	    case 'l':   /* 22 bit PC relative immediate */
	        the_insn.reloc = RELOC_WDISP22;
		the_insn.pcrel = 1;
		goto immediate;

	    case 'L':   /* 30 bit immediate */
	        the_insn.reloc = RELOC_WDISP30;
		the_insn.pcrel = 1;
		goto immediate;

	    case 'i':   /* 13 bit immediate */
	        the_insn.reloc = RELOC_BASE13;

		/*FALLTHROUGH*/

	    immediate:
		if(*s==' ')
		  s++;
		if (*s == '%') {
		    if ((c = s[1]) == 'h' && s[2] == 'i') {
			the_insn.reloc = RELOC_HI22;
			s+=3;
		    } else if (c == 'l' && s[2] == 'o') {
			the_insn.reloc = RELOC_LO10;
			s+=3;
		    } else
		    	break;
		}
		/* Note that if the getExpression() fails, we will still have
		   created U entries in the symbol table for the 'symbols'
		   in the input string.  Try not to create U symbols for
		   registers, etc. */
		{
			/* This stuff checks to see if the expression ends
			   in +%reg If it does, it removes the register from
			   the expression, and re-sets 's' to point to the
			   right place */

			char *s1;

			for(s1=s;*s1 && *s1!=','&& *s1!=']';s1++)
				;

			if(s1!=s && isdigit(s1[-1])) {
				if(s1[-2]=='%' && s1[-3]=='+') {
					s1-=3;
					*s1='\0';
					(void)getExpression(s);
					*s1='+';
					s=s1;
					continue;
				} else if(index("goli0123456789",s1[-2]) && s1[-3]=='%' && s1[-4]=='+') {
					s1-=4;
					*s1='\0';
					(void)getExpression(s);
					*s1='+';
					s=s1;
					continue;
				}
			}
		}
		(void)getExpression(s);
		s = expr_end;
		continue;

	    case 'a':
		if (*s++ == 'a') {
		    opcode |= ANNUL;
		    continue;
		}
		break;

	    case 'A':       /* alternate space */
		if (isdigit(*s)) {
		    long num;

		    num=0;
		    while (isdigit(*s)) {
			num= num*10 + *s-'0';
			++s;
		    }
		    opcode |= num<<5;
		    continue;
		}
		break;
		/* abort(); */

	    case 'p':
		if (strncmp(s, "%psr", 4) == 0) {
		    s += 4;
		    continue;
		}
		break;

	    case 'q':   /* floating point queue */
		if (strncmp(s, "%fq", 3) == 0) {
		    s += 3;
		    continue;
		}
		break;

	    case 'Q':   /* coprocessor queue */
		if (strncmp(s, "%cq", 3) == 0) {
		    s += 3;
		    continue;
		}
		break;

	    case 'S':
		if (strcmp(str, "set") == 0) {
		    special_case = SPECIAL_CASE_SET;
		    continue;
		}
		break;

	    case 't':
		if (strncmp(s, "%tbr", 4) != 0)
		    break;
		s += 4;
		continue;

	    case 'w':
		if (strncmp(s, "%wim", 4) != 0)
		    break;
		s += 4;
		continue;

	    case 'y':
		if (strncmp(s, "%y", 2) != 0)
		    break;
		s += 2;
		continue;

	    default:
		abort();
	    }
	    break;
	}
	error:
	if (match == FALSE)
	  {
	    /* Args don't match.  */
	    if (&insn[1] - sparc_opcodes < NUMOPCODES
		&& !strcmp(insn->name, insn[1].name))
	      {
		++insn;
		s = argsStart;
		continue;
	      }
	    else
	      {
		as_warn("Illegal operands");
		return;
	      }
	  }
	break;
    }

    the_insn.opcode = opcode;
    return;
}

static int
getExpression(str)
    char *str;
{
    char *save_in;
    segT seg;

    save_in = input_line_pointer;
    input_line_pointer = str;
    switch (seg = expression(&the_insn.exp)) {

    case SEG_ABSOLUTE:
    case SEG_TEXT:
    case SEG_DATA:
    case SEG_BSS:
    case SEG_UNKNOWN:
    case SEG_DIFFERENCE:
    case SEG_BIG:
    case SEG_NONE:
	break;

    default:
	the_insn.error = "bad segment";
	expr_end = input_line_pointer;
	input_line_pointer=save_in;
	return 1;
    }
    expr_end = input_line_pointer;
    input_line_pointer = save_in;
    return 0;
}


/*
    This is identical to the md_atof in m68k.c.  I think this is right,
    but I'm not sure.

   Turn a string in input_line_pointer into a floating point constant of type
   type, and store the appropriate bytes in *litP.  The number of LITTLENUMS
   emitted is stored in *sizeP .  An error message is returned, or NULL on OK.
 */

/* Equal to MAX_PRECISION in atof-ieee.c */
#define MAX_LITTLENUMS 6

char *
md_atof(type,litP,sizeP)
    char type;
    char *litP;
    int *sizeP;
{
    int	prec;
    LITTLENUM_TYPE words[MAX_LITTLENUMS];
    LITTLENUM_TYPE *wordP;
    char	*t;
    char	*atof_ieee();

    switch(type) {

    case 'f':
    case 'F':
    case 's':
    case 'S':
	prec = 2;
	break;

    case 'd':
    case 'D':
    case 'r':
    case 'R':
	prec = 4;
	break;

    case 'x':
    case 'X':
	prec = 6;
	break;

    case 'p':
    case 'P':
	prec = 6;
	break;

    default:
	*sizeP=0;
	return "Bad call to MD_ATOF()";
    }
    t=atof_ieee(input_line_pointer,type,words);
    if(t)
	input_line_pointer=t;
    *sizeP=prec * sizeof(LITTLENUM_TYPE);
    for(wordP=words;prec--;) {
	md_number_to_chars(litP,(long)(*wordP++),sizeof(LITTLENUM_TYPE));
	litP+=sizeof(LITTLENUM_TYPE);
    }
    return "";	/* Someone should teach Dean about null pointers */
}

/*
 * Write out big-endian.
 */
void
md_number_to_chars(buf,val,n)
    char *buf;
    long val;
    int n;
{

    switch(n) {

    case 4:
	*buf++ = val >> 24;
	*buf++ = val >> 16;
    case 2:
	*buf++ = val >> 8;
    case 1:
	*buf = val;
	break;

    default:
	abort();
    }
    return;
}

void
md_number_to_imm(buf,val,n, fixP, seg_type)
    char *buf;
    long val;
    int n;
    fixS *fixP;
    int seg_type;
{
    if (seg_type != N_TEXT || fixP->fx_r_type == NO_RELOC) {
	switch (n) {
	case 1:
		*buf = val;
		break;
	case 2:
		*buf++ = (val>>8);
		*buf = val;
		break;
	case 4:
		*buf++ = (val>>24);
		*buf++ = (val>>16);
		*buf++ = (val>>8);
		*buf = val;
		break;
	default:
		abort();
	}
	return;
    }

    assert(n == 4);
    assert(fixP->fx_r_type < NO_RELOC);

    /*
     * This is a hack.  There should be a better way to
     * handle this.
     */
    if (fixP->fx_r_type == RELOC_WDISP30 && fixP->fx_addsy) {
	    val += fixP->fx_where + fixP->fx_frag->fr_address;
    }

    switch (fixP->fx_r_type) {

    case RELOC_32:
	buf[0] = val >> 24;
	buf[1] = val >> 16;
	buf[2] = val >> 8;
	buf[3] = val;
	break;

#if 0
    case RELOC_8:         /* These don't seem to ever be needed. */
    case RELOC_16:
    case RELOC_DISP8:
    case RELOC_DISP16:
    case RELOC_DISP32:
#endif
    case RELOC_WDISP30:
	val = (val >>= 2) + 1;
	buf[0] |= (val >> 24) & 0x3f;
	buf[1]= (val >> 16);
	buf[2] = val >> 8;
	buf[3] = val;
	break;

    case RELOC_HI22:
	if(!fixP->fx_addsy) {
	  buf[1] |= (val >> 26) & 0x3f;
	  buf[2] = val >> 18;
	  buf[3] = val >> 10;
	} else {
	  buf[2]=0;
	  buf[3]=0;
	}
	break;
#if 0
    case RELOC_22:
    case RELOC_13:
#endif
    case RELOC_LO10:
	if(!fixP->fx_addsy) {
	  buf[2] |= (val >> 8) & 0x03;
	  buf[3] = val;
	} else
	  buf[3]=0;
	break;
#if 0
    case RELOC_SFA_BASE:
    case RELOC_SFA_OFF13:
    case RELOC_BASE10:
#endif
    case RELOC_BASE13:
	buf[2] |= (val >> 8) & 0x1f;
	buf[3] = val;
	break;

    case RELOC_WDISP22:
	val = (val >>= 2) + 1;
	/* FALLTHROUGH */
    case RELOC_BASE22:
	buf[1] |= (val >> 16) & 0x3f;
	buf[2] = val >> 8;
	buf[3] = val;
	break;

#if 0
    case RELOC_PC10: 
    case RELOC_PC22: 
    case RELOC_JMP_TBL:
    case RELOC_SEGOFF16:
    case RELOC_GLOB_DAT:
    case RELOC_JMP_SLOT: 
    case RELOC_RELATIVE:
#endif

    case NO_RELOC:
    default:
	as_warn("bad relocation type: 0x%02x", fixP->fx_r_type);
	break;
    }
    return;
}

/* should never be called for sparc */
void
md_create_short_jump(ptr, from_addr, to_addr, frag, to_symbol)
    char *ptr;
    long from_addr, to_addr;
{
    fprintf(stderr, "sparc_create_short_jmp\n");
    abort();
}

/* should never be called for sparc */
void
md_number_to_disp(buf,val,n)
    char	*buf;
    long	val;
{
    fprintf(stderr, "md_number_to_disp\n");
    abort();
}

/* should never be called for sparc */
void
md_number_to_field(buf,val,fix)
    char *buf;
    long val;
    void *fix;
{
    fprintf(stderr, "sparc_number_to_field\n");
    abort();
}

/* the bit-field entries in the relocation_info struct plays hell 
   with the byte-order problems of cross-assembly.  So as a hack,
   I added this mach. dependent ri twiddler.  Ugly, but it gets
   you there. -KWK */
/* on sparc: first 4 bytes are normal unsigned long address, next three
   bytes are index, most sig. byte first.  Byte 7 is broken up with
   bit 7 as external, bits 6 & 5 unused, and the lower
   five bits as relocation type.  Next 4 bytes are long int addend. */
/* Thanx and a tip of the hat to Michael Bloom, mb@ttidca.tti.com */
void
md_ri_to_chars(ri_p, ri)
     struct reloc_info_sparc *ri_p, ri;
{
  unsigned char the_bytes[sizeof(*ri_p)];
  
  /* this is easy */
  md_number_to_chars(the_bytes, ri.r_address, sizeof(ri.r_address));
  /* now the fun stuff */
  the_bytes[4] = (ri.r_index >> 16) & 0x0ff;
  the_bytes[5] = (ri.r_index >> 8) & 0x0ff;
  the_bytes[6] = ri.r_index & 0x0ff;
  the_bytes[7] = ((ri.r_extern << 7)  & 0x80) | (0 & 0x60) | (ri.r_type & 0x1F);
  /* Also easy */
  md_number_to_chars(&the_bytes[8], ri.r_addend, sizeof(ri.r_addend));
  /* now put it back where you found it, Junior... */
  bcopy (the_bytes, (char *)ri_p, sizeof(*ri_p));
}

/* should never be called for sparc */
void
md_convert_frag(fragP)
    register fragS *fragP;
{
    fprintf(stderr, "sparc_convert_frag\n");
    abort();
}

/* should never be called for sparc */
void
md_create_long_jump(ptr, from_addr, to_addr, frag, to_symbol)
    char	*ptr;
    long	from_addr,
	        to_addr;
    fragS	*frag;
    symbolS	*to_symbol;
{
    fprintf(stderr, "sparc_create_long_jump\n");
    abort();
}

/* should never be called for sparc */
int
md_estimate_size_before_relax(fragP, segtype)
    register fragS *fragP;
{
    fprintf(stderr, "sparc_estimate_size_before_relax\n");
    abort();
    return 0;
}

#if 0
/* for debugging only */
static void
print_insn(insn)
    struct sparc_it *insn;
{
    char *Reloc[] = {
    "RELOC_8",
    "RELOC_16",
    "RELOC_32",
    "RELOC_DISP8",
    "RELOC_DISP16",
    "RELOC_DISP32",
    "RELOC_WDISP30",
    "RELOC_WDISP22",
    "RELOC_HI22",
    "RELOC_22",
    "RELOC_13",
    "RELOC_LO10",
    "RELOC_SFA_BASE",
    "RELOC_SFA_OFF13",
    "RELOC_BASE10",
    "RELOC_BASE13",
    "RELOC_BASE22",
    "RELOC_PC10",
    "RELOC_PC22",
    "RELOC_JMP_TBL",
    "RELOC_SEGOFF16",
    "RELOC_GLOB_DAT",
    "RELOC_JMP_SLOT",
    "RELOC_RELATIVE",
    "NO_RELOC"
    };

    if (insn->error) {
	fprintf(stderr, "ERROR: %s\n");
    }
    fprintf(stderr, "opcode=0x%08x\n", insn->opcode);
    fprintf(stderr, "reloc = %s\n", Reloc[insn->reloc]);
    fprintf(stderr, "exp =  {\n");
    fprintf(stderr, "\t\tX_add_symbol = %s\n",
	insn->exp.X_add_symbol ?
	(insn->exp.X_add_symbol->sy_name ? 
	insn->exp.X_add_symbol->sy_name : "???") : "0");
    fprintf(stderr, "\t\tX_sub_symbol = %s\n",
	insn->exp.X_subtract_symbol ?
	    (insn->exp.X_subtract_symbol->sy_name ? 
	        insn->exp.X_subtract_symbol->sy_name : "???") : "0");
    fprintf(stderr, "\t\tX_add_number = %d\n",
	insn->exp.X_add_number);
    fprintf(stderr, "}\n");
    return;
}
#endif

/*
 * Sparc relocations are completely different, so it needs
 * this machine dependent routine to emit them.
 */
void
emit_relocations(fixP, segment_address_in_file)
    register fixS *fixP;
    relax_addressT segment_address_in_file;
{
    struct reloc_info_sparc ri;
    register symbolS *symbolP;
    extern char *next_object_file_charP;
    long add_number;

    bzero((char *) &ri, sizeof(ri));
    for (; fixP; fixP = fixP->fx_next) {

	if (fixP->fx_r_type >= NO_RELOC) {
	    fprintf(stderr, "fixP->fx_r_type = %d\n", fixP->fx_r_type);
	    abort();
	}

	if ((symbolP = fixP->fx_addsy) != NULL) {
	    ri.r_address = fixP->fx_frag->fr_address +
	        fixP->fx_where - segment_address_in_file;
	    if ((symbolP->sy_type & N_TYPE) == N_UNDF) {
		ri.r_extern = 1;
		ri.r_index = symbolP->sy_number;
	    } else {
		ri.r_extern = 0;
		ri.r_index = symbolP->sy_type & N_TYPE;
	    }
	    if (symbolP && symbolP->sy_frag) {
		ri.r_addend = symbolP->sy_frag->fr_address;
	    }
	    ri.r_type = fixP->fx_r_type;
	    if (fixP->fx_pcrel) {
/*		ri.r_addend -= fixP->fx_where;          */
		ri.r_addend -= ri.r_address;            
	    } else {
		ri.r_addend = fixP->fx_addnumber;
	    }

/*	    md_ri_to_chars((char *) &ri, ri);        */
	    append(&next_object_file_charP, (char *)& ri, sizeof(ri));
	}
    }
    return;
}

int
md_parse_option(argP,cntP,vecP)
    char **argP;
    int *cntP;
    char ***vecP;
{
    return 1;
}

