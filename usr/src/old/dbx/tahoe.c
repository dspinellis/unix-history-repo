/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tahoe.c	5.1 (Berkeley) %G%";
#endif not lint

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
#include <signal.h>

#ifndef public
typedef unsigned int Address;
typedef unsigned char Byte;
typedef unsigned int Word;

#define NREG 16

#define FRP 13
#define STKP 14
#define PROGCTR 15

#define BITSPERBYTE 8
#define BITSPERWORD (BITSPERBYTE * sizeof(Word))

#define nargspassed(frame) ((frame->removed-4)/4)

#include "source.h"
#include "symbols.h"

Address pc;
Address prtaddr;

#endif

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
	"priviliged instruction fault",
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

error("Can't do a \"call\" right now...sorry");	/* XXX */
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
