(*
 * Copyright (c) 1979, 1993
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
 *	@(#)unixio.i	8.1 (Berkeley) 6/6/93
 *)

const
sccsid = '@(#)unixio.i 8.1 6/6/93';

type
fileptr = record
	cnt :integer
	end;

function TELL(
var	fptr :text)
{returns} :fileptr;

var
filesize, headsize, tailsize :integer;
result :fileptr;

begin
tailsize := 0;
while not eof(fptr) do begin
	get(fptr);
	tailsize := tailsize + 1
	end;
filesize := 0;
reset(fptr);
while not eof(fptr) do begin
	get(fptr);
	filesize := filesize + 1
	end;
reset(fptr);
for headsize := 1 to filesize - tailsize do
	get(fptr);
result.cnt := headsize;
TELL := result
end;

procedure SEEK(
 var	fptr :text;
 var	cnt :fileptr);

var
i :integer;

begin
reset(fptr);
for i := 1 to cnt.cnt do
	get(fptr)
end;

procedure APPEND(
 var	fptr :text);

var
tmp :text;

begin
rewrite(tmp);
reset(fptr);
while not eof(fptr) do begin
	if eoln(fptr) then
		writeln(tmp)
	else
		write(tmp, fptr^);
	get(fptr)
	end;
reset(tmp);
rewrite(fptr);
while not eof(tmp) do begin
	if eoln(tmp) then
		writeln(fptr)
	else
		write(fptr, tmp^);
	get(tmp)
	end
end;
