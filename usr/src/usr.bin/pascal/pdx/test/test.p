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
 *	@(#)test.p	8.1 (Berkeley) 6/6/93
 *)

program blah(input, output);
const
	CONSTANT = 3;
	FLTCON = 3.14;
type
	Index = 1..10;
	intarray = array[Index] of 0..100;
	rec = record
		x : integer;
		y : char;
		z : integer;
	end;
var
	i : integer;
	x : real;
	a : array[1..10] of intarray;
	r : rec;
	p : ^rec;
	b : boolean;
	unused : integer;

function first(var p : integer) : integer;
begin
	i := p;
	r.x := 4;
	r.y := 'y';
	r.z := 6;
	b := true;
	first := p;
end;

procedure start;
var	q : integer;
begin
	q := -5;
	q := first(q);
	a[i][1] := q;
end;

procedure init;
var	i, j : integer;
begin
	b := false;
	for i := 1 to 10 do begin
		for j := 1 to 10 do begin
			a[i][j] := 10*(i-1) + j;
		end;
	end;
	start;
end;

begin
	init;
	a[1][1] := 3;
	a[10][1] := 2;
	a[1][10] := 1;
	writeln('garbage');
	readln(i);
end.
