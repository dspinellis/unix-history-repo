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
 *	@(#)fproc.p	8.1 (Berkeley) 6/6/93
 *)

program fproc(output);
    var
    i :integer;

    procedure print(function frtn :integer);
	begin
	    write(frtn:3,'   formal routine =');
	end;

    procedure lvl1(function form: integer);
	label	1;
	var
	loc :integer;

	function eval :integer;
	    begin
		if loc = 8 then begin
			writeln(' non-local jump');
			goto 1;
		end;
		eval := loc;
	    end;

    begin
	loc := i;
	i := i - 1;
	if (loc = 4) or (loc = 8) then
		lvl1(eval)
	else if loc > 0 then
		lvl1(form);
1:	write('Stack frame:',loc:3,'   formal print =');
	print(form);
	writeln(form:3);
    end;

    function geval :integer;
	begin
	    geval := i;
	end;

    begin
	writeln('This should print levels 0-3, with formal values of 4.');
	writeln('Level 4 should jump to level 8.');
	writeln('Finally levels 8-10 should print with formal values of -1.');
	i := 10;
	lvl1(geval);
    end.
