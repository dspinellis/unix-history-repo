/* Copyright (c) 1993 Carlos Leandro and Rui Salgueiro
 *	Dep. Matematica Universidade de Coimbra, Portugal, Europe
 */
/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char	sccsid[] = "@(#)main.c	1.4 (Coimbra) 93/06/03";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <ctype.h>

#include <arpa/nameser.h>
#include <resolv.h>

#define	ADDR 1
#define	NAME 2
#define	UNDEF -1

int	h_errno;
void	herror();

char	*progname;
char	*use = "%s [-d] [-a addr | -n name | string]  \n";
int	name_or_addr = UNDEF;

main(argc, argv)
int	argc;
char	*argv[];
{ 
	char	**ch, c, *net_name_or_addr, *s;
	struct netent *net_entry;
	register unsigned long	net2, nn;
	unsigned	netbr[4];

	progname = argv[0];
	net_name_or_addr = NULL;

	while (--argc > 0 && (*++argv)[0] == '-') {
		for (s = argv[0] + 1; *s != '\0'; s++) {
			switch (*s) {
			case 'd':
				_res.options |= RES_DEBUG;
				break;
			case 'a':
				if (--argc > 0) {
					name_or_addr = ADDR;
					net_name_or_addr = *++argv;
				} else 
					terminate();
				break;
			case 'n':
				if (--argc > 0) {
					name_or_addr = NAME;
					net_name_or_addr = *++argv;
				} else 
					terminate();
				break;
			default: 
				terminate();
				break;
			}
		}
	}

	if (name_or_addr == UNDEF) {
		if (argc > 0) {
			c = **argv;
			if (isalpha(c)) {
				name_or_addr = NAME;
			} else if (isdigit(c)) {
				name_or_addr = ADDR;
			} else {
				terminate();
			}
			net_name_or_addr = *argv++;
			--argc;
		} else 
			terminate();
	}

	if (argc > 0) {
		fprintf(stderr, "extra arguments ignored:");
		while (argc-- > 0 )
			fprintf(stderr, " %s", *argv++);
		fprintf(stderr, "\n");
	}

/*	fprintf(stderr, "name_or_addr = %d, net_name_or_addr = %s\n",
	    name_or_addr , net_name_or_addr);
*/
	switch (name_or_addr) {
	case ADDR:
		net_entry = getnetbyaddr(inet_network(net_name_or_addr ),
		    AF_INET);
		break;
	case NAME:
		net_entry = getnetbyname(net_name_or_addr );
		break;
	default: /* can't happen */
		terminate();
	}

	if (net_entry) {
		printf(" Official name: %s \n", net_entry->n_name);
		printf(" Net addr. type: %d \n", net_entry->n_addrtype);
		printf(" Network :  %u ----> ", net2 = (unsigned long)net_entry->n_net);
		for (nn = 4; net2; netbr[--nn] = net2 & 0xff, net2 >>= 8)
			;
		printf("  ");
		switch (nn) {
		case 0: 
			printf("%u.%u.%u.%u", netbr[0], netbr[1], netbr[2], netbr[3]);
			break;
		case 1: 
			printf("%u.%u.%u", netbr[1], netbr[2], netbr[3]);
			break;
		case 2: 
			printf("%u.%u", netbr[2], netbr[3]);
			break;
		case 3: 
			printf("%u", netbr[3]);
			break;
		}
		printf("  \n");
		printf(" Aliases : \n");
		for (ch = net_entry->n_aliases; *ch != NULL; ch++)
			printf("             %s \n", *ch);
		return 0;
	} else {
		herror(progname);
		return 2;
	}
}


terminate()
{
	fprintf(stderr, use, progname);
	exit (1);
}


#ifdef sun
char	*h_errlist[] = {
	"Error 0",
	"Unknown host", 			/* 1 HOST_NOT_FOUND */
	"Host name lookup failure", 		/* 2 TRY_AGAIN */
	"Unknown server error", 		/* 3 NO_RECOVERY */
	"No address associated with name", 	/* 4 NO_ADDRESS */
};

int	h_nerr = { 
	sizeof(h_errlist) / sizeof(h_errlist[0]) };

extern int	h_errno;

/*
 * herror --
 *	print the error indicated by the h_errno value.
 */
void
herror(s)
char	*s;
{
	fprintf(stderr, "%s: %s\n", s,
	    (u_int) h_errno < h_nerr ?  h_errlist[h_errno] : "Unknown error");
}

#endif
