/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)coredump.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * Deal with the core dump anachronism.
 */

#include "defs.h"
#include "coredump.h"
#include "machine.h"
#include "object.h"
#include "main.h"
#include <a.out.h>

#ifndef public
#define coredump_readin(m, r, s) coredump_xreadin(&(m), r, &(s))

#include "machine.h"
#endif

typedef struct {
    Address begin;
    Address end;
    Address seekaddr;
} Map;

private Map datamap, stkmap;
private File objfile;
private struct exec hdr;

public coredump_getkerinfo ()
{
    Symbol s;

    s = lookup(identname("Sysmap", true));
    if (s == nil) {
	panic("can't find 'Sysmap'");
    }
    sbr = (struct pte *) (s->symvalue.offset);
    s = lookup(identname("Syssize", true));
    if (s == nil) {
	panic("can't find 'Syssize'");
    }
    slr = (integer) (s->symvalue.offset);
    printf("sbr %lx slr %lx\n", sbr, slr);
    s = lookup(identname("masterpaddr", true));
    if (s == nil) {
	panic("can't find 'masterpaddr'");
    }
    fseek(
	corefile,
	datamap.seekaddr + s->symvalue.offset&0x7fffffff - datamap.begin,
	0
    );
    get(corefile, masterpcbb);
    masterpcbb = (masterpcbb&PG_PFNUM)*NBPG;
    getpcb();
}

/*
 * Read the user area information from the core dump.
 */

public coredump_xreadin(mask, reg, signo)
int *mask;
Word reg[];
short *signo;
{
    register struct user *up;
    register Word *savreg;
    union {
	struct user u;
	char dummy[ctob(UPAGES)];
    } ustruct;
    Symbol s;

    objfile = fopen(objname, "r");
    if (objfile == nil) {
	fatal("can't read \"%s\"", objname);
    }
    get(objfile, hdr);
    if (vaddrs) {
	datamap.begin = 0;
	datamap.end = 0xffffffff;
	stkmap.begin = 0xffffffff;
	stkmap.end = 0xffffffff;
    } else {
	up = &(ustruct.u);
	fread(up, ctob(UPAGES), 1, corefile);
#	if vax || tahoe
	    savreg = (Word *) &(ustruct.dummy[ctob(UPAGES)]);
#	else ifdef mc68000
	    savreg = (Word *) (
		&ustruct.dummy[ctob(UPAGES) - 10] - (NREG * sizeof(Word))
	    );
#	endif
#       ifdef IRIS
	    *mask = savreg[RPS];
#       else
	    *mask = savreg[PS];
#       endif
	copyregs(savreg, reg);
	*signo = up->u_arg[0];
	datamap.seekaddr = ctob(UPAGES);
	stkmap.begin = USRSTACK - ctob(up->u_ssize);
	stkmap.end = USRSTACK;
	stkmap.seekaddr = datamap.seekaddr + ctob(up->u_dsize);
	switch (hdr.a_magic) {
	    case OMAGIC:
		datamap.begin = CODESTART;
		datamap.end = CODESTART + ctob(up->u_tsize) + ctob(up->u_dsize);
		break;

	    case NMAGIC:
	    case ZMAGIC:
		datamap.begin = (Address)
		    ptob(btop(ctob(up->u_tsize) - 1) + 1) + CODESTART;
		datamap.end = datamap.begin + ctob(up->u_dsize);
		break;

	    default:
		fatal("bad magic number 0x%x", hdr.a_magic);
	}
#ifdef UXMAG
	/*
	 * Core dump not from this object file?
	 */
	if (hdr.a_magic != 0 and up->u_exdata.ux_mag  != 0 and
	  hdr.a_magic != up->u_exdata.ux_mag) {
	    warning("core dump ignored");
	    coredump = false;
	    fclose(corefile);
	    fclose(objfile);
	    start(nil, nil, nil);
	}
#endif
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
    if (hdr.a_magic == OMAGIC or vaddrs) {
	coredump_readdata(buff, addr, nbytes);
    } else {
	fseek(objfile, N_TXTOFF(hdr) + addr - CODESTART, 0);
	fread(buff, nbytes, sizeof(Byte), objfile);
    }
}

public coredump_readdata(buff, addr, nbytes)
char *buff;
Address addr;
int nbytes;
{
    Address a;

    a = addr;
    if (a < datamap.begin) {
	if (hdr.a_magic == OMAGIC) {
	    error("[data address 0x%x too low (lb = 0x%x)]", a, datamap.begin);
	} else {
	    coredump_readtext(buff, a, nbytes);
	}
    } else if (a > stkmap.end) {
	error("data address 0x%x too high (ub = 0x%x)", a, stkmap.end);
    } else {
	if (vaddrs) {
	    vreadfromfile(corefile, a, buff, nbytes);
	} else {
	    readfromfile(corefile, a, buff, nbytes);
	}
    }
}

/*
 * Read a block of data from a memory image, mapping virtual addresses.
 * Have to watch out for page boundaries.
 */

private vreadfromfile (corefile, v, buff, nbytes)
File corefile;
Address v;
char *buff;
integer nbytes;
{
    Address a;
    integer i, remainder, pagesize;
    char *bufp;

    a = v;
    pagesize = (integer) ptob(1);
    remainder = pagesize - (a mod pagesize);
    if (remainder >= nbytes) {
	readfromfile(corefile, vmap(a), buff, nbytes);
    } else {
	readfromfile(corefile, vmap(a), buff, remainder);
	a += remainder;
	i = nbytes - remainder;
	bufp = buff + remainder;
	while (i > pagesize) {
	    readfromfile(corefile, vmap(a), bufp, pagesize);
	    a += pagesize;
	    bufp += pagesize;
	    i -= pagesize;
	}
	readfromfile(corefile, vmap(a), bufp, i);
    }
}

private readfromfile (f, a, buff, nbytes)
File f;
Address a;
char *buff;
integer nbytes;
{
    integer fileaddr;

    if (a < stkmap.begin) {
	fileaddr = datamap.seekaddr + a - datamap.begin;
    } else {
	fileaddr = stkmap.seekaddr + a - stkmap.begin;
    }
    fseek(f, fileaddr, 0);
    fread(buff, nbytes, sizeof(Byte), f);
}
