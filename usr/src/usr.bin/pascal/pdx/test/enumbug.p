(*
 * Copyright (c) 1980, 1993
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
 *
 *	@(#)enumbug.p	8.1 (Berkeley) 6/6/93
 *)

(*
 * From peter@UCBERNIE Wed Apr 13 15:22:29 1983
 * Date: 13 Apr 83 14:04:39 PST (Wed)
 * From: peter@UCBERNIE (peter b. kessler)
 * Subject: try ``pi -t t.p ; obj'' and then ``print variant.b''
 * Message-Id: <8304132204.AA00797@UCBERNIE.ARPA>
 * Received: by UCBERNIE.ARPA (3.320/3.12)
 * 	id AA00797; 13 Apr 83 14:04:39 PST (Wed)
 * Received: from UCBERNIE.ARPA by UCBARPA.ARPA (3.332/3.19)
 * 	id AA01057; 13 Apr 83 15:22:23 PST (Wed)
 * To: linton@UCBERNIE
 * Status: R
 *)

program t(output);
    var
	variant : record case boolean of
			true: (b:boolean);
			false: (i:integer);
		    end;
    begin
	variant.i := -1;
	writeln(variant.b);
    end.

