/*-
 * Copyright (c) 1983, 1985
 *	The Regents of the University of California.  All rights reserved.
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

#ifndef lint
static char sccsid[] = "@(#)get_addrs.c	5.1 (Berkeley) 6/6/85";
#endif not lint

#include "talk_ctl.h"

struct	hostent *gethostbyname();
struct	servent *getservbyname();

get_addrs(my_machine_name, his_machine_name)
	char *my_machine_name;
	char *his_machine_name;
{
	struct hostent *hp;
	struct servent *sp;

	msg.pid = getpid();
	/* look up the address of the local host */
	hp = gethostbyname(my_machine_name);
	if (hp == (struct hostent *) 0) {
		printf("This machine doesn't exist. Boy, am I confused!\n");
		exit(-1);
	}
	bcopy(hp->h_addr, (char *)&my_machine_addr, hp->h_length);
	/* if he is on the same machine, then simply copy */
	if (bcmp((char *)&his_machine_name, (char *)&my_machine_name,
	    sizeof(his_machine_name)) == 0)
		bcopy((char *)&my_machine_addr, (char *)&his_machine_addr,
		    sizeof(his_machine_name));
	else {
		/* look up the address of the recipient's machine */
		hp = gethostbyname(his_machine_name);
		if (hp == (struct hostent *) 0 ) {
			printf("%s is an unknown host\n", his_machine_name);
			exit(-1);
		}
		bcopy(hp->h_addr, (char *) &his_machine_addr, hp->h_length);
	}
	/* find the daemon portal */
	sp = getservbyname("talk", "udp");
	if (sp == 0) {
		p_error("This machine doesn't support talk");
		exit(-1);
	}
	daemon_port = sp->s_port;
}
