/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)nextaddr.c 1.1 %G%";

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

	addr = beginaddr;
	iread(&o.word, addr, sizeof(o.word));
	op = (PXOP) o.byte[0];
	nextbyte = o.byte[1];
	addr += sizeof(short);
	switch(op) {

#	if (isvaxpx)
	/*
	 * The version of px on the VAX assumes that the instruction
	 * at the entry point of a function is a TRA4 to the beginning
	 * of the block.
	 */
#	endif
		case O_CALL: {
			ADDRESS eaddr;

			if (isnext) {
				addr += sizeof(int);
			} else {
#				if (isvaxpx)
					iread(&eaddr, addr, sizeof(eaddr));
					addr = eaddr + sizeof(short);
					iread(&addr, addr, sizeof(addr));
#				else
					iread(&offset, addr, sizeof(offset));
					addr += offset;
#				endif
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

#	if (isvaxpx)
		case O_FCALL: {
			ADDRESS eaddr;
			ADDRESS *fparam;

			if (!isnext) {
				stepto(addr - sizeof(short));
				dread(&fparam, process->sp + sizeof(ADDRESS), sizeof(fparam));
				dread(&eaddr, fparam, sizeof(eaddr));
				addr = eaddr - ENDOFF;
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
#	endif

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

#	if (isvaxpx)
		case O_TRA4:
		case O_GOTO:
			iread(&addr, addr, sizeof(addr));
			break;
#	endif

		case O_TRA:
			iread(&offset, addr, sizeof(offset));
			addr += offset;
			break;

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
			dread(&offset, process->sp, sizeof(offset));
			if (offset == 0) {
				iread(&offset, addr, sizeof(offset));
				addr += offset;
			} else {
				addr += sizeof(offset);
			}
			break;

		default: {
#	if (isvaxpx)
			int i;

			for (i = 0; optab[op].argtype[i] != 0; i++) {
				switch(optab[op].argtype[i]) {
					case ADDR4:
					case LWORD:
						addr += 4;
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
						if ((addr&1) != 0) {
							addr++;
						}
						break;
					}

					default:
						panic("bad argtype");
						/*NOTREACHED*/
				}
			}
#	else
			int oplen;

			oplen = optab[op].nargs;
			if (oplen < 0) {
				oplen = (-oplen) - 1;
			} else  if (oplen > 0 && nextbyte != 0) {
				oplen--;
			}
			oplen *= sizeof(int);
			switch (op) {
				case O_BEG:
				case O_NODUMP:
					oplen += 10;
					break;

				case O_CON:
					oplen += ((nextbyte + 1)&~1);
					break;
			}
			addr += oplen;
#	endif
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
	if (ncases == 0) {
		iread(&ncases, addr, sizeof(ncases));
		addr += sizeof(short);
	}
	jmptable = addr;
	firstval = jmptable + ncases*sizeof(short);
	lastval = firstval + ncases*size;
	if (size <= 2) {
		dread(&swtval, process->sp, 2);
	} else {
		dread(&swtval, process->sp, size);
	}
	for (i = firstval; i < lastval; i += size) {
		iread(&caseval, i, size);
		if (cmp(&swtval, &caseval, size) == 0) {
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
	long i, limit, lower;
	ADDRESS valaddr;
	short offset;

	stepto(addr - sizeof(short));
	p = process;
	i = limit = 0;
	if (subop == 0) {
		addr += size;
	}
	dread(&valaddr, p->sp, sizeof(valaddr));
	dread(&i, valaddr, size);
	dread(&limit, p->sp + sizeof(valaddr), size);
	i += (incr << (8*(sizeof(i) - size)));
	addr += size;
/*
 * It is very slow to go through the loop again and again.
 * So for the time being, we just skip to the end.
 *
	if ((incr > 0 && i < limit) || (incr < 0 && i > limit)) {
		iread(&offset, addr, sizeof(offset));
		return(addr + offset);
	} else {
 */
		return(addr + sizeof(short));
/*
	}
 */
}
