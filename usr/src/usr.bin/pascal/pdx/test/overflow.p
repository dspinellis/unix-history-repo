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
 *	@(#)overflow.p	8.1 (Berkeley) 6/6/93
 *)

program fpexceptions(input,output);
    type
	fperrorkind = ( fperrorfirst,
			overflow,underflow,divideby0,domain,
			fperrolast );
    var
	request : fperrorkind;
    procedure genoverflow;
	var
	    i : integer;
	    r : real;
	begin
	    r := 2.0;
	    for i := 1 to 1000 do begin
		r := r * r;
	    end;
	    writeln('this machine handles more than 2^1000');
	end;
    procedure genunderflow;
	var
	    i : integer;
	    r : real;
	begin
	    r := 0.5;
	    for i := 1 to 1000 do begin
		r := r * r;
	    end;
	    writeln('this machine handles more than 2^-1000');
	end;
    procedure gendivideby0;
	var
	    r : real;
	begin
	    r := 17.0;
	    r := r - r;		{should be 0.0}
	    r := 17.0 / r;
	    writeln('i wonder what r is?', r);
	end;
    procedure gendomain;
	var
	    r : real;
	begin
	    r := -17.0;
	    r := sqrt(r);
	    writeln('i wonder what r is?', r);
	end;
    begin
	write('which do you want (');
	for request := succ(fperrorfirst) to pred(fperrolast) do begin
		{this isn't standard pascal.}
	    write( ' ', request);
	end;
	write(' ): ');
	    {neither is this, but it sure is convenient.}
	readln(request);
	if request in [overflow,underflow,divideby0,domain] then begin
	    writeln('one ', request, ' coming right down');
	    case request of
		overflow:	genoverflow;
		underflow:	genunderflow;
		divideby0:	gendivideby0;
		domain:	gendomain;
	    end;
	end else begin
		{default:}
	    writeln('oh, never mind');
	end;
    end.
	    
