/*
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)tahoe.c	5.8 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 * Target machine dependent stuff.
 */

#include "defs.h"
#include "machine.h"
#include "process.h"
#include "runtime.h"
#include "events.h"
#include "main.h"
#include "symbols.h"
#include "source.h"
#include "mappings.h"
#include "object.h"
#include "keywords.h"
#include "ops.h"
#include "eval.h"
#include <signal.h>

#ifndef public
typedef unsigned int Address;
typedef unsigned char Byte;
typedef unsigned int Word;

#define NREG 16

#define FRP 13
#define STKP 14
#define PROGCTR 15

#define CODESTART 0
#define FUNCOFFSET 2

#define BITSPERBYTE 8
#define BITSPERWORD (BITSPERBYTE * sizeof(Word))

/*
 * This magic macro enables us to look at the process' registers
 * in its user structure.
 */

#define regloc(reg)	(ctob(UPAGES) + (sizeof(Word) * (reg)))

#define nargspassed(frame) (((argn(-1, frame)&0xffff)-4)/4)

#define	SYSBASE	0xc0000000		/* base of system address space */
#define	physaddr(a)	((a) &~ 0xc0000000)

#include "source.h"
#include "symbols.h"
#include <sys/param.h>
#include <machine/psl.h>
#include <sys/user.h>
#undef DELETE /* XXX */
#include <sys/vm.h>
#include <machine/reg.h>
#include <machine/pte.h>

Address pc;
Address prtaddr;

#endif

/*
 * Indices into u. for use in collecting registers values.
 */
public int rloc[] =
    { R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, FP, SP, PC };

private Address printop();

Optab	*ioptab[256];	/* index by opcode to optab */
/*
 * Initialize the opcode lookup table.
 */
public optab_init()
{
	register Optab *p;

	for (p = optab; p->iname; p++)
		ioptab[p->val & 0xff] = p;
}

/*
 * Decode and print the instructions within the given address range.
 */
public printinst(lowaddr, highaddr)
	Address lowaddr, highaddr;
{
	register Address addr;

	for (addr = lowaddr; addr <= highaddr; )
		addr = printop(addr);
	prtaddr = addr;
}

/*
 * Another approach:  print n instructions starting at the given address.
 */
public printninst(count, addr)
	int count;
	Address addr;
{
	register Integer i;
	register Address newaddr;

	if (count <= 0)
		error("non-positive repetition count");
	for (newaddr = addr, i = 0; i < count; i++)
		newaddr = printop(newaddr);
	prtaddr = newaddr;
}

/*
 * Hacked version of adb's instruction decoder.
 */
private Address printop(addr)
	Address addr;
{
	register Optab *op;
	Opcode ins;
	unsigned char mode;
	int argtype, amode, argno, argval, r;
	String reg;
	Boolean indexf;
	short offset;

	argval = 0;
	indexf = false;
	printf("%08x  ", addr);
	iread(&ins, addr, sizeof(ins));
	addr += 1;
	op = ioptab[ins];
	printf("%s", op->iname);
	for (argno = 0; argno < op->numargs; argno++) {
		if (indexf == true)
			indexf = false;
		else
			printf(argno == 0 ? "\t" : ",");
		argtype = op->argtype[argno];
		if (is_branch_disp(argtype))
			mode = 0xAF + (typelen(argtype) << 5);
		else
			iread(&mode, addr, sizeof(mode)), addr += 1;
		reg = regname[regnm(mode)];
		amode = addrmode(mode);
		switch (amode) {

		case LITSHORT: case LITUPTO31:
		case LITUPTO47: case LITUPTO63:
			if (ins == O_KCALL && mode >= 0 && mode < SYSSIZE &&
			   systab[mode])
				printf("$%s", systab[mode]);
			else
				printf("$%x", mode);
			argval = mode;
			break;

		case INDEX:
			printf("[%s]", reg);
			indexf = true;
			argno--;
			break;

		case REG:
			printf("%s", reg);
			break;

		case REGDEF:
			printf("(%s)", reg);
			break;

		case AUTODEC:
			printf("-(%s)", reg);
			break;

		case AUTOINC:
			r = mode & 0xf;
			if (r == 0xf || r == 8 || r == 9) {
				int size = (mode&03) + 1;

				/* immediate mode */
				printf("$");
				argval = printdisp(addr, size,
				    regname[PROGCTR], amode);
				addr += size;
			} else
				printf("(%s)+", reg);
			break;

		case AUTOINCDEF:
			if ((mode&0xf) == 0xf) {
				printf("*$");
				argval = printdisp(addr, 4, reg, amode);
				addr += 4;
			} else
				printf("*(%s)+", reg);
			break;

		case BYTEDISP:
			argval = printdisp(addr, 1, reg, amode);
			addr += 1;
			break;

		case BYTEDISPDEF:
			printf("*");
			argval = printdisp(addr, 1, reg, amode);
			addr += 1;
			break;

		case WORDDISP:
			argval = printdisp(addr, 2, reg, amode);
			addr += 2;
			break;

		case WORDDISPDEF:
			printf("*");
			argval = printdisp(addr, 2, reg, amode);
			addr += 2;
			break;

		case LONGDISP:
			argval = printdisp(addr, 4, reg, amode);
			addr += 4;
			break;

		case LONGDISPDEF:
			printf("*");
			argval = printdisp(addr, 4, reg, amode);
			addr += 4;
			break;
		}
	}
	if (ins == O_CASEL)
		for (argno = 0; argno <= argval; argno++) {
			iread(&offset, addr, sizeof(offset));
			printf("\n\t\t%d", offset);
			addr += 2;
		}
	printf("\n");
	return (addr);
}

/*
 * Print the displacement of an instruction that uses displacement
 * addressing.
 */
private int printdisp(addr, nbytes, reg, mode)
	Address addr;
	int nbytes;
	char *reg;
	int mode;
{
	char byte;
	short hword;
	int argval;
	Symbol f;

	switch (nbytes) {

	case 1:
		iread(&byte, addr, sizeof(byte));
		argval = byte;
		break;

	case 2:
		iread(&hword, addr, sizeof(hword));
		argval = hword;
		break;

	case 4:
		iread(&argval, addr, sizeof(argval));
		break;
	}
	if (reg == regname[PROGCTR] && mode >= BYTEDISP)
		argval += addr + nbytes;
	if (reg == regname[PROGCTR]) {
		f = whatblock((Address) argval + 2);
		if (codeloc(f) == argval + 2)
			printf("%s", symname(f));
		else
			printf("%x", argval);
	} else {
		if (varIsSet("$hexoffsets")) {
			if (argval < 0)
				printf("-%x(%s)", -(argval), reg);
			else
				printf("%x(%s)", argval, reg);
		} else
			printf("%d(%s)", argval, reg);
	}
	return (argval);
}

/*
 * Print the contents of the addresses within the given range
 * according to the given format.
 */
typedef struct {
	String	name;
	String	printfstring;
	int	length;
} Format;

private Format fmt[] = {
	{ "d", " %d", sizeof(short) },
	{ "D", " %ld", sizeof(long) },
	{ "o", " %o", sizeof(short) },
	{ "O", " %lo", sizeof(long) },
	{ "x", " %04x", sizeof(short) },
	{ "X", " %08x", sizeof(long) },
	{ "b", " \\%o", sizeof(char) },
	{ "c", " '%c'", sizeof(char) },
	{ "s", "%c", sizeof(char) },
	{ "f", " %f", sizeof(float) },
	{ "g", " %g", sizeof(double) },
	{ nil, nil, 0 }
};

private Format *findformat(s)
	String s;
{
	register Format *f;

	for (f = &fmt[0]; f->name != nil && !streq(f->name, s); f++)
		;
	if (f->name == nil)
		error("bad print format \"%s\"", s);
	return (f);
}

public Address printdata(lowaddr, highaddr, format)
	Address lowaddr;
	Address highaddr;
	String format;
{
	register int n;
	register Address addr;
	register Format *f;
	int value;

	if (lowaddr > highaddr)
		error("first address larger than second");
	f = findformat(format);
	n = 0;
	value = 0;
	for (addr = lowaddr; addr <= highaddr; addr += f->length) {
		if (n == 0)
			printf("%08x: ", addr);
		dread(&value, addr, f->length);
		printf(f->printfstring, value);
		++n;
		if (n >= (16 div f->length)) {
			putchar('\n');
			n = 0;
		}
	}
	if (n != 0)
		putchar('\n');
	prtaddr = addr;
	return (addr);
}

/*
 * The other approach is to print n items starting with a given address.
 */

public printndata(count, startaddr, format)
int count;
Address startaddr;
String format;
{
	register int i, n;
	register Address addr;
	register Format *f;
	register Boolean isstring;
	char c;
	union {
		char	charv;
		short	shortv;
		int	intv;
		float	floatv;
		double	doublev;
	} value;

	if (count <= 0)
		error("non-positive repetition count");
	f = findformat(format);
	isstring = (Boolean) streq(f->name, "s");
	n = 0;
	addr = startaddr;
	value.intv = 0;
	for (i = 0; i < count; i++) {
		if (n == 0)
			printf("%08x: ", addr);
		if (isstring) {
			putchar('"');
			dread(&c, addr, sizeof(char));
			while (c != '\0') {
				printchar(c);
				++addr;
				dread(&c, addr, sizeof(char));
			}
			putchar('"');
			putchar('\n');
			n = 0;
			addr += sizeof(String);
			continue;
		}
		dread(&value, addr, f->length);
		printf(f->printfstring, value);
		++n;
		if (n >= (16 div f->length)) {
			putchar('\n');
			n = 0;
		}
		addr += f->length;
	}
	if (n != 0)
		putchar('\n');
	prtaddr = addr;
}

/*
 * Print out a value according to the given format.
 */
public printvalue(v, format)
	long v;
	String format;
{
	Format *f;
	char *p, *q;

	f = findformat(format);
	if (streq(f->name, "s")) {
		putchar('"');
		for (p = (char *) &v, q = p + sizeof(v); p < q; ++p)
			printchar(*p);
		putchar('"');
	} else
		printf(f->printfstring, v);
	putchar('\n');
}

/*
 * Print out an execution time error.
 * Assumes the source position of the error has been calculated.
 *
 * Have to check if the -r option was specified; if so then
 * the object file information hasn't been read in yet.
 */
public printerror()
{
	extern Integer sys_nsig;
	extern String sys_siglist[];
	integer err;

	if (isfinished(process)) {
		err = exitcode(process);
		if (err) {
			printf("\"%s\" terminated abnormally (exit code %d)\n",
			    objname, err);
			erecover();
		} else
			printf("\"%s\" terminated normally\n", objname);
	}
	if (runfirst) {
		fprintf(stderr, "Entering debugger ...\n");
		init();
	}
	err = errnum(process);
	putchar('\n');
	printsig(err);
	putchar(' ');
	printloc();
	putchar('\n');
	if (curline > 0)
		printlines(curline, curline);
	else
		printinst(pc, pc);
	erecover();
}

/*
 * Print out a signal.
 */
private String illinames[] = {
	"reserved addressing fault",
	"privileged instruction fault",
	"reserved operand fault"
};
#define	NILLINAMES	(sizeof (illinames) / sizeof (illinames[0]))

private String fpenames[] = {
	nil,
	"integer overflow trap",
	"integer divide by zero trap",
	"floating point divide by zero trap",
	"floating point overflow trap",
	"floating point underflow trap",
};
#define	NFPENAMES	(sizeof (fpenames) / sizeof (fpenames[0]))

public printsig(signo)
integer signo;
{
	integer code;

	if (signo < 0 or signo > sys_nsig)
		printf("[signal %d]", signo);
	else
		printf("%s", sys_siglist[signo]);
	code = errcode(process);
	if (signo == SIGILL)
		if (code >= 0 && code < NILLINAMES)
			printf(" (%s)", illinames[code]);
	if (signo == SIGFPE)
		if (code > 0 and code < NFPENAMES)
			printf(" (%s)", fpenames[code]);
}

/*
 * Note the termination of the program.  We do this so as to avoid
 * having the process exit, which would make the values of variables
 * inaccessible.  We do want to flush all output buffers here,
 * otherwise it'll never get done.
 */
public endprogram()
{
	Integer exitcode;

	stepto(nextaddr(pc, true));
	printnews();
	exitcode = argn(1, nil);
	if (exitcode != 0)
		printf("\nexecution completed (exit code %d)\n", exitcode);
	else
		printf("\nexecution completed\n");
	getsrcpos();
	erecover();
}

private Address getcall();
/*
 * Single step the machine a source line (or instruction if "inst_tracing"
 * is true).  If "isnext" is true, skip over procedure calls.
 */
public dostep(isnext)
	Boolean isnext;
{
	register Address addr;
	register Lineno line;
	String filename;
	Address startaddr;

	startaddr = pc;
	addr = nextaddr(pc, isnext);
	if (!inst_tracing && nlhdr.nlines != 0) {
		line = linelookup(addr);
		for (; line == 0; line = linelookup(addr))
			addr = nextaddr(addr, isnext);
		curline = line;
	} else
		curline = 0;
	stepto(addr);
	filename = srcfilename(addr);
	setsource(filename);
}

private Address findnextaddr();
/*
 * Compute the next address that will be executed from the given one.
 * If "isnext" is true then consider a procedure call as straight line code.
 *
 * We must unfortunately do much of the same work that is necessary
 * to print instructions.  In addition we have to deal with branches.
 * Unconditional branches we just follow, for conditional branches
 * we continue execution to the current location and then single step
 * the machine.  We assume that the last argument in an instruction
 * that branches is the branch address (or relative offset).
 */
public Address nextaddr(startaddr, isnext)
	Address startaddr;
	boolean isnext;
{
	Address addr;

	addr = usignal(process);
	if (addr == 0 or addr == 1)
		addr = findnextaddr(startaddr, isnext);
	return (addr);
}

/*
 * Determine if it's ok to skip function f entered by instruction ins.
 * If so, we're going to compute the return address and step to it.
 */
private boolean skipfunc(ins, f)
	Opcode ins;
	Symbol f;
{

	return ((boolean) (!inst_tracing && nlhdr.nlines != 0 &&
		nosource(curfunc) && canskip(curfunc)));
}

private Address findnextaddr(startaddr, isnext)
	Address startaddr;
	Boolean isnext;
{
	register Address addr;
	Optab *op;
	Opcode ins;
	unsigned char mode;
	int argtype, amode, argno, argval, nib;
	String r;
	Boolean indexf;
	enum { KNOWN, SEQUENTIAL, BRANCH } addrstatus;

	argval = 0;
	indexf = false;
	addr = startaddr;
	iread(&ins, addr, sizeof(ins));
	switch (ins) {

	case O_CALLF:
	case O_CALLS:
		addrstatus = KNOWN;
		stepto(addr);
		pstep(process, DEFSIG);
		addr = reg(PROGCTR);
		pc = addr;
		setcurfunc(whatblock(pc));
		if (not isbperr()) {
			printstatus();
			/* NOTREACHED */
		}
		bpact();
		if (isnext or skipfunc(ins, curfunc)) {
			addrstatus = KNOWN;
			addr = return_addr();
			stepto(addr);
			bpact();
		} else
			callnews(/* iscall = */ true);
		break;

	case O_RET:
		addrstatus = KNOWN;
		stepto(addr);
		callnews(/* iscall = */ false);
		pstep(process, DEFSIG);
		addr = reg(PROGCTR);
		pc = addr;
		if (not isbperr())
			printstatus();
		bpact();
		break;

	case O_BRB:
	case O_BRW:
	case O_JMP:
	case O_BBSSI:
	case O_BCC:
	case O_BCS:
	case O_BEQL:
	case O_BGEQ:
	case O_BGTR:
	case O_BGTRU:
	case O_BLEQ:
	case O_BLEQU:
	case O_BLSS:
	case O_BNEQ:
	case O_BVC:
	case O_BVS:
	case O_CASEL:
	case O_AOBLSS:
	case O_AOBLEQ:
		addrstatus = KNOWN;
		stepto(addr);
		pstep(process, DEFSIG);
		addr = reg(PROGCTR);
		pc = addr;
		if (not isbperr())
			printstatus();
		break;

	default:
		addrstatus = SEQUENTIAL;
		break;
	}
	if (addrstatus == KNOWN)
		return (addr);
	addr += 1;
	op = ioptab[ins];
	for (argno = 0; argno < op->numargs; argno++) {
		if (indexf == true)
			indexf = false;
		argtype = op->argtype[argno];
		if (is_branch_disp(argtype))
			mode = 0xAF + (typelen(argtype) << 5);
		else
			iread(&mode, addr, sizeof(mode)), addr += 1;
		r = regname[regnm(mode)];
		amode = addrmode(mode);
		switch (amode) {

		case LITSHORT:
		case LITUPTO31:
		case LITUPTO47:
		case LITUPTO63:
			argval = mode;
			break;

		case INDEX:
			indexf = true;
			--argno;
			break;

		case REG:
		case REGDEF:
		case AUTODEC:
			break;

		case AUTOINC:
			nib = mode & 0xf;
			if (nib == 0xf || nib == 8 || nib == 9) {
				int size = (mode&03)+1;

				argval = getdisp(addr, size,
				    regname[PROGCTR], amode);
				addr += size;
			}
			break;

		case AUTOINCDEF:
			if ((mode&0xf) != 0xf)
				break;
			argval = getdisp(addr, 4, r, amode);
			addr += 4;
			break;

		case BYTEDISP:
		case BYTEDISPDEF:
			argval = getdisp(addr, 1, r, amode);
			addr += 1;
			break;

		case WORDDISP:
		case WORDDISPDEF:
			argval = getdisp(addr, 2, r, amode);
			addr += 2;
			break;

		case LONGDISP:
		case LONGDISPDEF:
			argval = getdisp(addr, 4, r, amode);
			addr += 4;
			break;
		}
	}
	if (ins == O_CALLF or ins == O_CALLS)
		argval += 2;
	if (addrstatus == BRANCH)
		addr = argval;
	return (addr);
}

/*
 * Get the displacement of an instruction that uses displacement addressing.
 */
private int getdisp(addr, nbytes, reg, mode)
	Address addr;
	int nbytes;
	String reg;
	int mode;
{
	char byte;
	short hword;
	int argval;

	switch (nbytes) {

	case 1:
		iread(&byte, addr, sizeof(byte));
		argval = byte;
		break;

	case 2:
		iread(&hword, addr, sizeof(hword));
		argval = hword;
		break;

	case 4:
		iread(&argval, addr, sizeof(argval));
		break;
	}
	if (reg == regname[PROGCTR] && mode >= BYTEDISP)
		argval += addr + nbytes;
	return (argval);
}

#define BP_OP	   	O_BPT	   /* breakpoint trap */
#define BP_ERRNO	SIGTRAP	 /* signal received at a breakpoint */

/*
 * Setting a breakpoint at a location consists of saving
 * the word at the location and poking a BP_OP there.
 *
 * We save the locations and words on a list for use in unsetting.
 */
typedef struct Savelist *Savelist;

struct Savelist {
	Address location;
	Byte save;
	Byte refcount;
	Savelist link;
};

private Savelist savelist;

/*
 * Set a breakpoint at the given address.  Only save the word there
 * if it's not already a breakpoint.
 */
public setbp(addr)
	Address addr;
{
	Byte w, save;
	register Savelist newsave, s;

	for (s = savelist; s != nil; s = s->link)
		if (s->location == addr) {
			s->refcount++;
			return;
		}
	iread(&save, addr, sizeof(save));
	newsave = new(Savelist);
	newsave->location = addr;
	newsave->save = save;
	newsave->refcount = 1;
	newsave->link = savelist;
	savelist = newsave;
	w = BP_OP;
	iwrite(&w, addr, sizeof(w));
}

/*
 * Unset a breakpoint; unfortunately we have to search the SAVELIST
 * to find the saved value.  The assumption is that the SAVELIST will
 * usually be quite small.
 */
public unsetbp(addr)
Address addr;
{
	register Savelist s, prev;

	prev = nil;
	for (s = savelist; s != nil; s = s->link) {
		if (s->location == addr) {
			iwrite(&s->save, addr, sizeof(s->save));
			s->refcount--;
			if (s->refcount == 0) {
				if (prev == nil)
					savelist = s->link;
				else
					prev->link = s->link;
				dispose(s);
			}
			return;
		}
		prev = s;
	}
	panic("unsetbp: couldn't find address %d", addr);
}

/*
 * Enter a procedure by creating and executing a call instruction.
 */

#define CALLSIZE 7	/* size of call instruction */

public beginproc(p, argc)
	Symbol p;
	Integer argc;
{
	char save[CALLSIZE];
	struct {
		Opcode op;
		unsigned char numargs;
		unsigned char mode;
		char addr[sizeof(long)];	/* unaligned long */
	} call;
	long dest;

	if (4*argc+4 > 256)
		error("too many parameters (max %d)", 256/4 - 1);
	pc = 2;
	iread(save, pc, sizeof(save));
	call.op = O_CALLF;
	call.numargs = 4*argc+4;
	call.mode = 0xef;			/* longword relative */
	dest = codeloc(p) - 2 - (pc + CALLSIZE);
	mov(&dest, call.addr, sizeof(call.addr));
	iwrite(&call, pc, sizeof(call));
	setreg(PROGCTR, pc);
	pstep(process, DEFSIG);
	iwrite(save, pc, sizeof(save));
	pc = reg(PROGCTR);
	if (not isbperr())
		printstatus();
}

/*
 * Special variables for debugging the kernel.
 */

public integer masterpcbb;
public integer slr;
public struct pte *sbr;
private struct pcb pcb;

public getpcb ()
{
    fseek(corefile, masterpcbb & ~0xc0000000, 0);
    get(corefile, pcb);
    printf("p0br %lx p0lr %lx p2br %lx p2lr %lx\n",
	pcb.pcb_p0br, pcb.pcb_p0lr, pcb.pcb_p2br, pcb.pcb_p2lr
    );
    setreg(0, pcb.pcb_r0);
    setreg(1, pcb.pcb_r1);
    setreg(2, pcb.pcb_r2);
    setreg(3, pcb.pcb_r3);
    setreg(4, pcb.pcb_r4);
    setreg(5, pcb.pcb_r5);
    setreg(6, pcb.pcb_r6);
    setreg(7, pcb.pcb_r7);
    setreg(8, pcb.pcb_r8);
    setreg(9, pcb.pcb_r9);
    setreg(10, pcb.pcb_r10);
    setreg(11, pcb.pcb_r11);
    setreg(12, pcb.pcb_r12);
    setreg(FRP, pcb.pcb_fp);
    setreg(STKP, pcb.pcb_ksp);
    setreg(PROGCTR, pcb.pcb_pc);
}

public copyregs (savreg, reg)
Word savreg[], reg[];
{
    reg[0] = savreg[R0];
    reg[1] = savreg[R1];
    reg[2] = savreg[R2];
    reg[3] = savreg[R3];
    reg[4] = savreg[R4];
    reg[5] = savreg[R5];
    reg[6] = savreg[R6];
    reg[7] = savreg[R7];
    reg[8] = savreg[R8];
    reg[9] = savreg[R9];
    reg[10] = savreg[R10];
    reg[11] = savreg[R11];
    reg[12] = savreg[R12];
    reg[FRP] = savreg[FP];
    reg[STKP] = savreg[SP];
    reg[PROGCTR] = savreg[PC];
}

/*
 * Map a virtual address to a physical address.
 */

public Address vmap (addr)
Address addr;
{
	int oldaddr = addr, v;
	struct pte pte;

	addr &= ~0xc0000000;
	v = btop(addr);
	switch (oldaddr&0xc0000000) {

	case 0xc0000000:
		/*
		 * In system space get system pte.  If
		 * valid or reclaimable then physical address
		 * is combination of its page number and the page
		 * offset of the original address.
		 */
		if (v >= slr)
			goto oor;
		addr = ((long)(sbr+v)) &~ 0xc0000000;
		goto simple;

	case 0x80000000:
		/*
		 * In p2 spce must not be in shadow region.
		 */
		if (v < pcb.pcb_p2lr)
			goto oor;
		addr = (long)(pcb.pcb_p2br+v);
		break;

	case 0x40000000:
		/*
		 * In p1 space everything is verboten (for now).
		 */
		goto oor;

	case 0x00000000:
		/*
		 * In p0 space must not be off end of region.
		 */
		if (v >= pcb.pcb_p0lr)
			goto oor;
		addr = (long)(pcb.pcb_p0br+v);
		break;
	oor:
		error("address out of segment");
	}
	/*
	 * For p0/p1/p2 address, user-level page table should
	 * be in kernel vm.  Do second-level indirect by recursing.
	 */
	if ((addr & 0xc0000000) != 0xc0000000)
		error("bad p0br, p1br, or p2br in pcb");
	addr = vmap(addr);
simple:
	/*
	 * Addr is now address of the pte of the page we
	 * are interested in; get the pte and paste up the
	 * physical address.
	 */
	fseek(corefile, addr, 0);
	if (fread(&pte, sizeof (pte), 1, corefile) != 1)
		error("page table botch");
	/* SHOULD CHECK NOT I/O ADDRESS; NEED CPU TYPE! */
	if (pte.pg_v == 0 && (pte.pg_fod || pte.pg_pfnum == 0))
		error("page not valid/reclaimable");
	return ((long)(ptob(pte.pg_pfnum) + (oldaddr & PGOFSET)));
}

/*
 * Extract a bit field from an integer.
 */

public integer extractField (s)
Symbol s;
{
    integer nbytes, nbits, n, r, off, len;

    off = s->symvalue.field.offset;
    len = s->symvalue.field.length;
    nbytes = size(s);
    n = 0;
    if (nbytes > sizeof(n)) {
	printf("[bad size in extractField -- word assumed]\n");
	nbytes = sizeof(n);
    }
    popn(nbytes, ((char *) &n) + (sizeof(Word) - nbytes));
    nbits = nbytes * BITSPERBYTE;
    r = n >> (nbits - ((off mod nbits) + len));
    r &= ((1 << len) - 1);
    return r;
}

/*
 * Change the length of a value in memory according to a given difference
 * in the lengths of its new and old types.
 */

public loophole (oldlen, newlen)
integer oldlen, newlen;
{
    integer i, n;
    Stack *oldsp;

    n = newlen - oldlen;
    oldsp = sp - oldlen;
    if (n > 0) {
	for (i = oldlen - 1; i >= 0; i--) {
	    oldsp[n + i] = oldsp[i];
	}
	for (i = 0; i < n; i++) {
	    oldsp[i] = '\0';
	}
    } else {
	for (i = 0; i < newlen; i++) {
	    oldsp[i] = oldsp[i - n];
	}
    }
    sp += n;
}
