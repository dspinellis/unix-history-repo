/*
 *  Interpress utilities
 *
 *  Written for Xerox Corporation by William LeFebvre
 *  24-May-1984
 *
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 * HISTORY
 * 15-Jan-86  lee at Xerox, WRC
 *	Removed vax dependencies.
 *
 *	29-apr-85  ed flint	add conditional compilation for vax-11 c (vms)
 */

/*
 *  Subroutines to help build interpress files:
 *
 *  operator interface level - these routines call routines at the literal
 *			       interface level to provide an easy way to
 *			       write operators along with their parameters.
 */

# include "iptokens.h"
# include "literal.h"
# include "operator.h"

char *index();

op_i(opcode, i)

int opcode;
long i;

{
    AppendInteger(i);
    AppendOp(opcode);
}

op_n(opcode, n)

int opcode;
double n;

{
    AppendNumber(n);
    AppendOp(opcode);
}

op_ii(opcode, i1, i2)

int opcode;
long i1, i2;

{
    AppendInteger(i1);
    AppendInteger(i2);
    AppendOp(opcode);
}

op_ni(opcode, n, i)

int opcode;
double n;
long i;

{
    AppendNumber(n);
    AppendInteger(i);
    AppendOp(opcode);
}

op_nn(opcode, n1, n2)

int opcode;
double n1, n2;

{
    AppendNumber(n1);
    AppendNumber(n2);
    AppendOp(opcode);
}

op_nnnn(opcode, n1, n2, n3, n4)

int opcode;
double n1, n2, n3, n4;

{
    AppendNumber(n1);
    AppendNumber(n2);
    AppendNumber(n3);
    AppendNumber(n4);
    AppendOp(opcode);
}

op_nnnnnn(opcode, n1, n2, n3, n4, n5, n6)

int opcode;
double n1, n2, n3, n4, n5, n6;

{
    AppendNumber(n1);
    AppendNumber(n2);
    AppendNumber(n3);
    AppendNumber(n4);
    AppendNumber(n5);
    AppendNumber(n6);
    AppendOp(opcode);
}

/*
 *  Here are some extra and useful goodies 
 */

SetupFont(name, size, frame_index)

char *name;
double size;
int frame_index;

{
    int i = 1;
    char *slashp;
    char *namep = name;

    /* make heirarchical name vector using slash as separator */
#ifdef vax11c
    while((slashp = strchr(namep, '/')) != 0)
#else
    while((slashp = index(namep, '/')) != 0)
#endif

    {
	*slashp = '\0';
	AppendIdentifier(namep);
	namep = slashp + 1;
	*slashp = '/';
	i++;
    }
    AppendIdentifier(namep);
    AppendInteger((long) i);
    AppendOp(OP_makevec);

    /* find the font */
    AppendOp(OP_findfont);

    /* build a scaling transform */
    Scale(size);

    /* apply the transform to the font */
    AppendOp(OP_modifyfont);

    /* set the frame index of choice */
    AppendInteger((long) frame_index);
    AppendOp(OP_fset);
}

ShowString(string)

char *string;

{
    AppendString(string);
    AppendOp(OP_show);
}
