/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
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
 *
 *	@(#)conf.h	5.17 (Berkeley) 6/1/90
 */

/*
**  CONF.H -- All user-configurable parameters for sendmail
*/

/*
**  Table sizes, etc....
**	There shouldn't be much need to change these....
*/

# define MAXLINE	1024		/* max line length */
# define MAXNAME	256		/* max length of a name */
# define MAXFIELD	4096		/* max total length of a hdr field */
# define MAXPV		40		/* max # of parms to mailers */
# define MAXHOP		17		/* max value of HopCount */
# define MAXATOM	100		/* max atoms per address */
# define MAXMAILERS	25		/* maximum mailers known to system */
# define MAXRWSETS	30		/* max # of sets of rewriting rules */
# define MAXPRIORITIES	25		/* max values for Precedence: field */
# define MAXTRUST	30		/* maximum number of trusted users */
# define MAXUSERENVIRON	40		/* max # of items in user environ */
# define QUEUESIZE	600		/* max # of jobs per queue run */
# define MAXMXHOSTS	10		/* max # of MX records */

/*
**  Compilation options.
**
**	#define these if they are available; comment them out otherwise.
*/

/* # define DBM		1	/* use DBM library (requires -ldbm) */
/* # define NDBM	1	/* new DBM library available (requires DBM) */
# define LOG		1	/* enable logging */
# define SMTP		1	/* enable user and server SMTP */
# define QUEUE		1	/* enable queueing */
# define UGLYUUCP	1	/* output ugly UUCP From lines */
# define DAEMON		1	/* include the daemon (requires IPC & SMTP) */
# define SETPROCTITLE	1	/* munge argv to display current status */
# define NAMED_BIND	1	/* use Berkeley Internet Domain Server */

	/*
	 * Use query type of ANY if possible (NO_WILDCARD_MX), which will
	 * find types CNAME, A, and MX, and will cause all existing records
	 * to be cached by our local server.  If there is (might be) a
	 * wildcard MX record in the local domain or its parents that are
	 * searched, we can't use ANY; it would cause fully-qualified names
	 * to match as names in a local domain.
	 */
/* # define NO_WILDCARD_MX	1 */
