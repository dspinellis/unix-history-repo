.\" Copyright (c) 1983, 1993
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" Redistribution and use in source and binary forms, with or without
.\" modification, are permitted provided that the following conditions
.\" are met:
.\" 1. Redistributions of source code must retain the above copyright
.\"    notice, this list of conditions and the following disclaimer.
.\" 2. Redistributions in binary form must reproduce the above copyright
.\"    notice, this list of conditions and the following disclaimer in the
.\"    documentation and/or other materials provided with the distribution.
.\" 3. All advertising materials mentioning features or use of this software
.\"    must display the following acknowledgement:
.\"	This product includes software developed by the University of
.\"	California, Berkeley and its contributors.
.\" 4. Neither the name of the University nor the names of its contributors
.\"    may be used to endorse or promote products derived from this software
.\"    without specific prior written permission.
.\"
.\" THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
.\" ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
.\" IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
.\" ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
.\" FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
.\" DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
.\" OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
.\" HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.\" LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
.\" OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
.\" SUCH DAMAGE.
.\"
.\"	@(#)a.t	8.2 (Berkeley) 5/16/94
.\"
.de L0
.nr l1 0
.nr l2 0
.nr l3 0
.nr l4 0
..
.de L1
.nr l1 +1
.nr l2 0
.nr l3 0
.nr l4 0
.sp 0.5
\\n(l1	\fB\\$1\fP
..
.de L2
.nr l2 +1
.nr l3 0
.nr l4 0
.sp 0.25
\\n(l1.\\n(l2	\fB\\$1\fP
..
.de L3
..
.de L4
..
.de Nm
.br
 		\\$1	\\$3
..
.bp
.Sh 1 "Summary of facilities
.sp 1
.ta \w'8.8\ \ 'u +0.25i +\w'gettimeofday\ \ 'u
.so Toc
.pn 2
.bp
.de L0
.nr l1 0
.nr l2 0
.nr l3 0
.nr l4 0
.br
 		\fB\\$1\fP	\\$2
..
.de L1
.nr l1 +1
.nr l2 0
.nr l3 0
.nr l4 0
.sp 0.5
 	\\n(l1	\fB\\$1\fP	\\$2
..
.de L2
.nr l2 +1
.nr l3 0
.nr l4 0
.sp 0.25
 	\\n(l1.\\n(l2	\fB\\$1\fP	\\$2
..
.de L3
.nr l3 +1
.nr l4 0
.br
 	\\n(l1.\\n(l2.\\n(l3	\\$1	\\$2
..
.de L4
.nr l4 +1
.br
 	\\n(l1.\\n(l2.\\n(l3.\\n(l4	\\$1	\\$2
..
.de Nm
..
.ce 1
\s+4\fBContents\fP\s0
.sp 2
.ta \w'8.8.8.88\ \ 'uR +\w'\ \ \ 'u 6iR
.so Toc
.sy mv toc Toc
