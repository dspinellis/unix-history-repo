/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Lawrence Berkeley Laboratory,
 * Berkeley, CA.  The name of the University may not be used to
 * endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char rcsid[] =
    "@(#) $Header: kernel.c,v 1.2 93/02/19 15:24:35 mccanne Exp $ (LBL)";
#endif

#include <sys/param.h>
#include <ctype.h>
#include "defs.h"
#include "symtab.h"

#ifdef KERNELDEBUG
#include "kernel.h"

int
is_a_vmunix(name)
	char *name;
{
	register char *cp;
	
	if (name && (cp = strstr(name, "vmunix")) &&
	    (cp == name || cp[-1] == '/'))
		return (1);
	return (0);
}

/*
 * Lookup the address of a symbol and report a meaningful message 
 * if not found.
 */
CORE_ADDR
ksym_lookup(name)
        char *name;
{
        struct minimal_symbol *sym;

        if ((sym = lookup_minimal_symbol(name, NULL)) == NULL)
                error("Kernel symbol `%s' not found.", name);

        return (sym->address);
}

/*
 * Read the panic string and print it out if set.
 */
static void
panicinfo()
{
	CORE_ADDR addr;
	register char *cp;
	char buf[256];

        /* print out the panic string if there is one */
        if (target_read_memory(ksym_lookup("panicstr"), &addr, 4) || 
	    addr == 0 || target_read_memory(addr, buf, sizeof(buf)))
                return;

        for (cp = buf; cp < &buf[sizeof(buf)] && *cp; cp++)
                if (!isascii(*cp) || (!isprint(*cp) && !isspace(*cp)))
                        *cp = '?';
        *cp = '\0';
        if (buf[0] != '\0')
                printf("panic: %s\n", buf);
}

/*
 * Print info about the kernel crashdump.
 */
void
kerninfo()
{
        printf("sp=%x pc=%x psr=%x\n", 
	       read_register(SP_REGNUM),
	       read_register(PC_REGNUM),
	       read_register(PS_REGNUM));
	panicinfo();
}

/*
 * XXX this needs serious work to really work right.
 */
void
set_procaddr_com(arg, from_tty)
	char *arg;
	int from_tty;
{
	u_int paddr, uaddr;
	
	if (!kernel_debugging)
		error("Not kernel debugging.");

	if (arg == 0)
		(void)set_procaddr(0);
	else {
		paddr = (u_int)parse_and_eval_address(arg);
		if (set_procaddr(paddr))
			error("invalid proc address");
		flush_cached_frames();
		set_current_frame(create_new_frame(read_register(FP_REGNUM),
						   read_pc()));
		select_frame(get_current_frame(), 0);
	}
}

void
_initialize_kernel()
{
	add_com("process-address", class_obscure, set_procaddr_com, 
"The process with proc structure at ADDR becomes the\n\
\"current\" process context for kernel debugging.");
	add_com_alias("paddr", "process-address", class_obscure, 0);
}

#endif
