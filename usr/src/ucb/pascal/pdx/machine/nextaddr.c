/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)nextaddr.c	5.2 (Berkeley) 4/7/87";
#endif not lint

/*
 * Calculate the next address that will be executed from the current one.
 *
 * If the next address depends on runtime data (e.g. a conditional
 * branch will depend on the value on top of the stack),
 * we must execute up to the given address with "stepto".
 *
 * If the second argument is TRUE, we treat a CALL instruction as
 * straight line rather than following it as a branch.
 */

#include "defs.h"
#include "machine.h"
#include "process.h"
#include "breakpoint.h"
#include "sym.h"
#include "pxops.h"
#include "optab.h"
#include "mappings.h"
#include "runtime.h"
#include "process/pxinfo.h"
#include "process/process.rep"

#ifdef tahoe
#define EVEN 3
#else
#define EVEN 1
#endif

LOCAL ADDRESS docase(), dofor();

ADDRESS nextaddr(beginaddr, isnext)
ADDRESS beginaddr;
BOOLEAN isnext;
{
    register PXOP op;
    ADDRESS addr;
    short offset;
    int nextbyte;
    SYM *s;
    union {
	short word;
	char byte[2];
    } o;

#ifdef tahoe
    doret(process);
#endif
    addr = beginaddr;
    iread(&o.word, addr, sizeof(o.word));
    op = (PXOP) o.byte[0];
    nextbyte = o.byte[1];
    addr += sizeof(short);
    switch(op) {

    /*
     * The version of px we are using assumes that the instruction
     * at the entry point of a function is a TRA4 to the beginning
     * of the block.
     */
	case O_CALL: {
	    ADDRESS eaddr;

	    if (isnext) {
		addr += sizeof(int);
#ifdef tahoe
	        addr = (ADDRESS)(((int)addr + EVEN) & ~EVEN);
#endif
	    } else {
#ifdef tahoe
		addr = (ADDRESS)(((int)addr + EVEN) & ~EVEN);
#endif
		iread(&eaddr, addr, sizeof(eaddr));
		addr = eaddr + sizeof(short);
#ifdef tahoe
		addr = (ADDRESS)(((int)addr + EVEN) & ~EVEN);
#endif
		iread(&addr, addr, sizeof(addr));
		stepto(addr);
		if (linelookup(addr) == 0) {
		    bpact();
		    addr = pc;
		}
		if (ss_lines && trcond()) {
		    s = whatblock(addr);
		    if (s == NIL) {
			panic("bad call addr");
		    }
		    printentry(s);
		}
	    }
	    break;
	}

	case O_FCALL: {
	    ADDRESS eaddr;
	    ADDRESS *fparam;

	    if (!isnext) {
		stepto(addr - sizeof(short));
#ifdef tahoe
		doret(process);
#endif
		dread(&fparam, process->sp + sizeof(ADDRESS), sizeof(fparam));
		dread(&eaddr, fparam, sizeof(eaddr));
		addr = eaddr - ENDOFF;
		stepto(addr);
#ifdef tahoe
		doret(process);
#endif
		if (linelookup(addr) == 0) {
		    bpact();
		    addr = pc;
		}
		if (ss_lines && trcond()) {
		    s = whatblock(addr);
		    if (s == NIL) {
			panic("bad call addr");
		    }
		    printentry(s);
		}
	    }
	    break;
	}

	case O_END:
	    if ((addr - sizeof(short)) == lastaddr()) {
		stepto(addr - sizeof(short));
		endprogram();
	    } else {
		addr = return_addr();
		s = whatblock(pc);
		stepto(addr);
		if (ss_lines && trcond()) {
		    printexit(s);
		}
		if (linelookup(addr) == 0) {
		    bpact();
		    addr = pc;
		}
	    }
	    break;

	case O_TRA4:
	case O_GOTO:
#ifdef tahoe
	    addr = (ADDRESS)(((int)addr + EVEN) & ~EVEN);
#endif
	    iread(&addr, addr, sizeof(addr));
	    break;

	case O_TRA:
	    iread(&offset, addr, sizeof(offset));
	    addr += offset;
	    break;

	case O_CON: {
	    short consize;

	    if (nextbyte == 0) {
#ifdef tahoe
	        addr = (ADDRESS)(((int)addr + EVEN) & ~EVEN);
#endif
		iread(&consize, addr, sizeof(consize));
		addr += sizeof(consize);
	    } else {
		consize = nextbyte;
	    }
	    addr += consize;
	    break;
	}

	case O_CASE1OP:
	    addr = docase(nextbyte, 1, addr);
	    break;

	case O_CASE2OP:
	    addr = docase(nextbyte, 2, addr);
	    break;

	case O_CASE4OP:
	    addr = docase(nextbyte, 4, addr);
	    break;

	case O_FOR1U:
	    addr = dofor(2, addr, nextbyte, 1);
	    break;

	case O_FOR2U:
	    addr = dofor(2, addr, nextbyte, 1);
	    break;

	case O_FOR4U:
	    addr = dofor(4, addr, nextbyte, 1);
	    break;

	case O_FOR1D:
	    addr = dofor(2, addr, nextbyte, -1);
	    break;

	case O_FOR2D:
	    addr = dofor(2, addr, nextbyte, -1);
	    break;

	case O_FOR4D:
	    addr = dofor(4, addr, nextbyte, -1);
	    break;

	case O_IF:
	    stepto(addr - sizeof(short));
#ifdef tahoe
	    doret(process);
	    dread(&offset, process->sp+sizeof(int)-sizeof(offset), sizeof(offset));
#else
	    dread(&offset, process->sp, sizeof(offset));
#endif
	    if (offset == 0) {
		iread(&offset, addr, sizeof(offset));
		addr += offset;
	    } else {
		addr += sizeof(offset);
	    }
	    break;

	default: {
	    int i;

	    for (i = 0; optab[op].argtype[i] != 0; i++) {
		switch(optab[op].argtype[i]) {
		    case ADDR4:
		    case LWORD:
			addr += 4;
#ifdef tahoe
		        addr = (ADDRESS)(((int)addr + EVEN) & ~EVEN);
#endif
			break;

		    case SUBOP:
			break;

		    case ADDR2:
		    case HWORD:
		    case PSUBOP:
		    case DISP:
		    case VLEN:
			if (i != 0 || nextbyte == 0) {
			    addr += sizeof(short);
			}
			break;

		    case STRING: {
			char c;

			while (nextbyte > 0) {
			    iread(&c, addr, 1);
			    if (c == '\0') {
				break;
			    }
			    nextbyte--;
			    addr++;
			}
			addr++;
		        addr = (ADDRESS)(((int)addr + EVEN) & ~EVEN);
			break;
		    }

		    default:
			panic("bad argtype");
			/*NOTREACHED*/
		}
	    }
	    break;
	}
    }
    return addr;
}

/*
 * Find the next address that will be executed after the
 * case statement at the given address.
 */

LOCAL ADDRESS docase(ncases, size, addr)
int ncases;
int size;
ADDRESS addr;
{
    register ADDRESS i;
    ADDRESS firstval, lastval, jmptable;
    short offset;
    long swtval, caseval;

    stepto(addr - 2);
#ifdef tahoe
    doret(process);
#endif
    if (ncases == 0) {
	iread(&ncases, addr, sizeof(ncases));
	addr += sizeof(short);
    }
    jmptable = addr;
    firstval = jmptable + ncases*sizeof(short);
#ifdef tahoe
    if (size == 4) {
	firstval = (ADDRESS)(((int)firstval + EVEN) & ~EVEN);
    }
#endif
    lastval = firstval + ncases*size;
#ifdef tahoe
    if (size <= 4) {
	dread(&swtval, process->sp, 4);
#else
    if (size <= 2) {
	dread(&swtval, process->sp, 2);
#endif
    } else {
	dread(&swtval, process->sp, size);
    }
    for (i = firstval; i < lastval; i += size) {
	caseval = 0;
#ifdef tahoe
	iread((char *)&caseval + sizeof caseval - size, i, size);
	if (swtval == caseval)
#else
	iread(&caseval, i, size);
	if (cmp(&swtval, &caseval, size) == 0)
#endif
	{
	    i = ((i - firstval) / size) * sizeof(offset);
	    iread(&offset, jmptable + i, sizeof(offset));
	    addr = jmptable + offset;
	    return addr;
	}
    }
    return((lastval+1)&~1);
}

LOCAL ADDRESS dofor(size, addr, subop, incr)
int size;
ADDRESS addr;
short subop;
int incr;
{
    register PROCESS *p;
    long i, limit;
    ADDRESS valaddr;

    stepto(addr - sizeof(short));
    p = process;
#ifdef tahoe
    doret(p);
#endif
    i = limit = 0;
    if (subop == 0) {
	dread(&subop, addr, sizeof (short));
	addr += sizeof (short);
    }
    dread(&valaddr, p->sp, sizeof(valaddr));
#ifdef tahoe
    dread((char *)&i + sizeof i - size, valaddr, size);
#else
    dread(&i, valaddr, size);
#endif
    dread(&limit, p->sp + sizeof(valaddr), sizeof limit);
    i += incr;

/*
 * It is very slow to go through the loop again and again.
 * If it is desired to just skip to the end, the next 4 lines
 * should be skipped.
 */
    if ((incr > 0 && i < limit) || (incr < 0 && i > limit)) {
	return(addr + subop);
    } else {
	return(addr);
    }
}

/*
 * Determine whether or not the given address corresponds to the
 * end of a procedure.
 */

BOOLEAN isendofproc(addr)
ADDRESS addr;
{
    PXOP op;

    iread(&op, addr, sizeof(op));
    return (op == O_END);
}
