/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/pathnames.h,v 3.4 1991/12/14 20:45:46 christos Exp $ */
/*
 * pathnames.h: Location of things to find
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
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
#ifndef _h_pathnames
#define _h_pathnames

#ifdef CMUCS
#define _PATH_LOCAL		"/usr/cs/bin"
#else
#define _PATH_LOCAL		"/usr/local/bin"
#endif

#define _PATH_USRBIN		"/usr/bin"
#define _PATH_USRUCB		"/usr/ucb"
#define _PATH_USRBSD		"/usr/bsd"
#define _PATH_BIN		"/bin"

#if defined(convex) || defined(__convex__) || defined(stellar)
# define _PATH_DOTLOGIN		"/etc/login"
# define _PATH_DOTLOGOUT	"/etc/logout"
# define _PATH_DOTCSHRC		"/etc/cshrc"
#endif /* convex || __convex__ || stellar */

#if defined(sgi) || defined(OREO) || defined(cray)
# define _PATH_DOTLOGIN		"/etc/cshrc"
#endif /* sgi || OREO || cray */

#if defined(NeXT)
# define _PATH_DOTLOGIN		"/etc/login.std"
# define _PATH_DOTLOGOUT	"/etc/logout.std"
# define _PATH_DOTCSHRC		"/etc/cshrc.std"
#endif /* NeXT */

#ifndef _PATH_DOTLOGIN
# define _PATH_DOTCSHRC		"/etc/csh.cshrc"
# define _PATH_DOTLOGIN		"/etc/csh.login"
# define _PATH_DOTLOGOUT	"/etc/csh.logout"
#endif

#define _PATH_DEVNULL		"/dev/null"

#define _PATH_BSHELL		"/bin/sh"
#ifdef notdef
# define _PATH_CSHELL 		"/bin/csh"
#endif
#ifndef _PATH_TCSHELL
#define _PATH_TCSHELL		"/usr/local/bin/tcsh"
#endif

#define _PATH_LOGIN		"/bin/login"
#ifdef NEWGRP
# define _PATH_BIN_NEWGRP	"/bin/newgrp"
# define _PATH_USRBIN_NEWGRP	"/usr/bin/newgrp"
#endif

#endif /* _h_pathnames */
