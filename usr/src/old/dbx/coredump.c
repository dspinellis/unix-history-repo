/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)@(#)coredump.c 1.1 %G%";

/*
 * Deal with the core dump anachronism.
 *
 * If I understood this code, I'd try to make it readable.
 */

#include "defs.h"
#include "coredump.h"
#include "machine.h"
#include "object.h"
#include "main.h"
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/psl.h>
#include <sys/pte.h>
#include <sys/user.h>
#include <sys/vm.h>
#include <sys/reg.h>
#include <a.out.h>

#ifndef public
#define coredump_readin(m, r, s) coredump_xreadin(&(m), r, &(s))

#include "machine.h"
#endif

#define MAXSTKADDR (0x80000000 - ctob(UPAGES))	/* highest stack address */

typedef struct {
    Address begin;
    Address end;
    Address seekaddr;
} Map;

private Map datamap, stkmap;
private File objfile;
private struct exec hdr;

/*
 * Read the user area information from the core dump.
 */

public coredump_xreadin(mask, reg, signo)
int *mask;
Word reg[];
int *signo;
{
    register struct user *up;
    register Word *savreg;
    union {
	struct user u;
	char dummy[ctob(UPAGES)];
    } ustruct;

    objfile = fopen(objname, "r");
    if (objfile == nil) {
	fatal("can't read \"%s\"", objname);
    }
    get(objfile, hdr);
    up = &(ustruct.u);
    fread(up, ctob(UPAGES), 1, corefile);
    savreg = (Word *) &(ustruct.dummy[ctob(UPAGES)]);
    *mask = savreg[PS];
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
    reg[ARGP] = savreg[AP];
    reg[FRP] = savreg[FP];
    reg[STKP] = savreg[SP];
    reg[PROGCTR] = savreg[PC];
    *signo = up->u_arg[0];
    datamap.seekaddr = ctob(UPAGES);
    stkmap.begin = MAXSTKADDR - ctob(up->u_ssize);
    stkmap.end = MAXSTKADDR;
    stkmap.seekaddr = datamap.seekaddr + ctob(up->u_dsize);
    switch (hdr.a_magic) {
	case OMAGIC:
	    datamap.begin = 0;
	    datamap.end = ctob(up->u_tsize) + ctob(up->u_dsize);
	    break;

	case NMAGIC:
	case ZMAGIC:
	    datamap.begin = (Address) ptob(btop(ctob(up->u_tsize) - 1) + 1);
	    datamap.end = datamap.begin + ctob(up->u_dsize);
	    break;

	default:
	    fatal("bad magic number 0x%x", hdr.a_magic);
    }
    /*
     * Core dump not from this object file?
     */
    if (hdr.a_magic != 0 and up->u_exdata.ux_mag  != 0 and
      hdr.a_magic != up->u_exdata.ux_mag) {
	warning("core dump ignored");
	coredump = false;
	fclose(coredump);
	fclose(objfile);
	start(nil, nil, nil);
    }
}

public coredump_close()
{
    fclose(objfile);
}

public coredump_readtext(buff, addr, nbytes)
char *buff;
Address addr;
int nbytes;
{
    if (hdr.a_magic == OMAGIC) {
	coredump_readdata(buff, addr, nbytes);
    } else {
	fseek(objfile, N_TXTOFF(hdr) + addr, 0);
	fread(buff, nbytes, sizeof(Byte), objfile);
    }
}

public coredump_readdata(buff, addr, nbytes)
char *buff;
Address addr;
int nbytes;
{
    if (addr < datamap.begin) {
	error("data address 0x%x too low (lb = 0x%x)", addr, datamap.begin);
    } else if (addr > stkmap.end) {
	error("data address 0x%x too high (ub = 0x%x)", addr, stkmap.end);
    } else if (addr < stkmap.begin) {
	fseek(corefile, datamap.seekaddr + addr - datamap.begin, 0);
	fread(buff, nbytes, sizeof(Byte), corefile);
    } else {
	fseek(corefile, stkmap.seekaddr + addr - stkmap.begin, 0);
	fread(buff, nbytes, sizeof(Byte), corefile);
    }
}
