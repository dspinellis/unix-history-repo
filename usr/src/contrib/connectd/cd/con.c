/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Bill Jolitz.
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
static char sccsid[] = "@(#)con.c	5.2 (Berkeley) 5/29/93";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <stdio.h>

char string[80] ; char *seq(), *find(), *passuntil() ;
fd_set fdset;
struct timeval tv_10th = { 0, 100000 } ; /* 1/10th of a second */
# define TIMEOUT	600	/* 10ths of a second */
main (argc, argv) char *argv[]; {
	int n,m,r,rv; char *cp ;

fprintf(stderr,"%s\n", argv[1]) ;
	FD_ZERO(&fdset) ;
	/* flush out any line noise */
	seq ("\r", 0) ;
	sleep (1) ;
	ioctl (0, TIOCFLUSH, &n) ;

	if (strcmp (argv[1], "drop") == 0) {
		(void) select(0,0,0,0,&tv_10th);
		seq("+", 5) ;
		(void) select(0,0,0,0,&tv_10th);
		seq("+", 5) ;
		(void) select(0,0,0,0,&tv_10th);
		seq("+", 5) ;
		ioctl (0, TIOCFLUSH, &n) ;
		(void) select(0,0,0,0,&tv_10th);
		if(!find("\r\nOK\r\n", 30)) ;
		ioctl (0, TIOCFLUSH, &n) ;
		seq("ATH0\r", 5) ;
		/*if(!find("\r\nOK\r\n", 10)) ;*/
		cp = passuntil("\n",TIMEOUT);
		fprintf(stderr, "|%s| ", cp) ;
		cp = passuntil("\n",TIMEOUT);
		fprintf(stderr, "|%s| ", cp) ;
		if (strncmp(cp, "OK", 2) == 0) rv = 0;
		else	rv = 1;
	} else {
	system ("stty raw -echo 1200") ;
		if ((seq ("ATDT", 10) == 0)
		&& (seq (argv[1], 30) == 0) && (seq ("\r", 10) == 0) ) ;
		else write(2,"Cannot sync with hayes\n", 23) ;
		/*if(!find("\r\nCONNECT", TIMEOUT))
			passuntil("\r\n",20);*/
		cp = passuntil("\n",TIMEOUT);
		fprintf(stderr, "|%s| ", cp) ;
		cp = passuntil("\n",TIMEOUT);
		fprintf(stderr, "|%s| ", cp) ;
		if (strncmp(cp, "CONNECT", 7) == 0) rv = 0;
		else	rv = 1;
fprintf(stderr,"rv = %d\n", rv) ;
	}
	write (2, "\r\n", 2) ;
	return (rv) ;
}

char *seq (s,t) char *s; {
	char c; int n;

	do {
		write (1, s, 1) ;
		n = 0 ;
	loop:
		FD_SET(0,&fdset) ;
		if (select(1,&fdset,0,0,&tv_10th) > 0) {
			n = read (0, &c, 1) ;
			write (2, &c, 1) ;
		}
		else if (!t)  return ((char *) -1) ;
		else  { t-- ; write (2, "*", 1); goto loop ; }
		if (n != 1 || c != *s) return (s) ;
	}	while (*++s != '\0') ;
	return ( (char *) 0) ;
}

char buf2[80];
char *find (s, n) char *s; {
	int m,r; char *sp = buf2;

	m = n ;
	while (m > 0) {
		FD_SET(0,&fdset) ;
		if ((r=select(1,&fdset,0,0,&tv_10th)) > 0) {
			ioctl (0, FIONREAD, &n) ;
			read (0, sp, n) ;
			write (2, sp, n) ;
			sp += n;
		} else	m-- ;
		if (strcmp(s, buf2) == 0) return ( (char *) 0) ;
	}
	return ( (char *) -1) ;
}

char *passuntil (s, n) char *s; {
	int m,r; char *sp = buf2;

	m = n ;
	while (m > 0) {
		FD_SET(0,&fdset) ;
		if ((r=select(1,&fdset,0,0,&tv_10th)) > 0) {
			ioctl (0, FIONREAD, &n) ;
			n = read (0, sp, n) ;
			write (2, sp, n) ;
			sp += n; *sp = '\0' ;
			do if(index(s,sp[-n])) return (buf2) ; while (--n) ;
		} else	m-- ;
	}
	return ( (char *) -1) ;
}
