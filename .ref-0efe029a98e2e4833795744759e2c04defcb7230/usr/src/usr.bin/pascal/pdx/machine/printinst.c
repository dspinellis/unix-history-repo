/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)printinst.c	5.2 (Berkeley) %G%";
#endif not lint
/*
 * decode and print the instructions
 */

#include "defs.h"
#include "machine.h"
#include "process.h"
#include "pxops.h"
#include "optab.h"
#include "object.h"
#include "process/process.rep"
#include "process/pxinfo.h"

LOCAL ADDRESS printop(), docase();

/*
 * print instructions within the given address range
 */

printinst(lowaddr, highaddr)
ADDRESS lowaddr;
ADDRESS highaddr;
{
    register ADDRESS addr;

    for (addr = lowaddr; addr <= highaddr; ) {
	addr = printop(addr);
    }
}

/*
 * print the opcode at the given address, return the address
 * of the next instruction
 */

LOCAL ADDRESS printop(addr)
register ADDRESS addr;
{
    int i;
    PXOP op;
    OPTAB *o;
    char subop;
    short arg;
    long longarg;
    union {
	short i;
	struct { char c1, c2; } opword;
    } u;

    iread(&u.i, addr, sizeof(u.i));
    op = (PXOP) u.opword.c1;
    subop = u.opword.c2;
    o = &optab[op];
    printf("%5d   %s", addr, o->opname);
    addr += sizeof(u);
    for (i = 0; o->argtype[i] != 0; i++) {
	if (i == 0) {
	    putchar('\t');
	} else {
	    putchar(',');
	}
	switch(o->argtype[i]) {
	    case ADDR4:
	    case LWORD:
#ifdef tahoe
		addr = (ADDRESS)(((int)addr + 3) & ~3);
#endif
		iread(&longarg, addr, sizeof(longarg));
		printf("%d", longarg);
		addr += sizeof(long);
		break;

	    case SUBOP:
		printf("%d", subop);
		break;

	    case ADDR2:
	    case DISP:
	    case PSUBOP:
	    case VLEN:
	    case HWORD:
		if (i != 0 || subop == 0) {
		    iread(&arg, addr, sizeof(arg));
		    addr += sizeof(short);
		} else {
		    arg = subop;
		}
		printf("%d", arg);
		break;

	    case STRING: {
		char c;

		putchar('\'');
		while (subop > 0) {
		    iread(&c, addr, sizeof(c));
		    if (c == '\0') {
			break;
		    }
		    putchar(c);
		    subop--;
		    addr++;
		}
		addr++;
		putchar('\'');
#ifdef tahoe
		addr = (ADDRESS)(((int)addr + 3) & ~3);
#else
		if ((addr&1) != 0) {
		    addr++;
		}
#endif
		break;
	    }

	    default:
		panic("bad argtype %d", o->argtype[i]);
		/*NOTREACHED*/
	}
    }
    switch(op) {
	case O_CON:
	    addr += arg;
#ifdef tahoe
	    addr = (ADDRESS)(((int)addr + 3) & ~3);
#endif
	    break;

	case O_CASE1OP:
	    addr = docase(addr, 1, subop);
	    break;

	case O_CASE2OP:
	    addr = docase(addr, 2, subop);
	    break;

	case O_CASE4OP:
	    addr = docase(addr, 4, subop);
	    break;
    }
    putchar('\n');
    return(addr);
}

/*
 * print out the destinations and cases
 */

LOCAL ADDRESS docase(addr, size, n)
ADDRESS addr;
int size;
int n;
{
    register int i;
    char c;
    short arg;
    long longarg;

    iread(&arg, addr, sizeof(arg));
    printf("\n\t%5d", arg);
    addr += 2;
    for (i = 1; i < n; i++) {
	iread(&arg, addr, sizeof(arg));
	printf(", %5d", arg);
	addr += 2;
    }
    printf("\n\t");
    for (i = 0; i < n; i++) {
	switch(size) {
	    case 1:
		iread(&c, addr, sizeof(c));
		printf("%5d", c);
		break;

	    case 2:
		iread(&arg, addr, sizeof(arg));
		printf("%5d", arg);
		break;

	    case 4:
#ifdef tahoe
		addr = (ADDRESS)(((int)addr + 3) & ~3);
#endif
		iread(&longarg, addr, sizeof(longarg));
		printf("%5d", longarg);
		break;
	}
	addr += size;
	if (i < n - 1) {
	    printf(", ");
	}
    }
#ifdef tahoe
    addr = (ADDRESS)(((int)addr + 3) & ~3);
#else
    if ((addr&01) == 01) {
	addr++;
    }
#endif
    return(addr);
}
