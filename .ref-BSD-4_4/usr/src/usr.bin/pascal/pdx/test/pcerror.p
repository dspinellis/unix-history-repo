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
 *	@(#)pcerror.p	8.1 (Berkeley) 6/6/93
 *)

program errs(input,output,junk,locked);

label	99;

type
etype = (ECHR, EHALT, ENILPTR, ECASE, EOUTOFMEM, ECTSNG,
ESTLIM, EARGV, EPACK, EUNPACK, EASRT, ELLIMIT, ETRASHHEAP, EPASTEOF, 
EREFINAF, ENOFILE, ENAMESIZE, EBADINUM, EBADFNUM, ENUMNTFD, ENAMRNG,
EFMTSIZE, ESEEK, ECREATE, EREMOVE, EOPEN, EREADIT, EWRITEIT, ESQRT,
ELN, ERANGE, ESUBSC, EGOTO, ECLOSE, EWRITE, ECTLWR, ECTUPR, xxx);
biggie = array[1..15000] of integer;

var
ch :char;
chs :alfa;
ch1 :array[1..10] of char;
ptr, ptr1 :^char;
ptr2 :^biggie;
junk, locked, other :file of char;
variant :record
	case boolean of
	true:(val :1..100);
	false:(name :etype)
	end;
s :set of 1..4;
i :integer;
r :real;
err :etype;

begin
writeln('Want a list of error names?');
readln(ch);
if ch = 'y' then begin
	for err:=ECHR to pred(xxx) do begin
		write(err:15);
		if ord(err) mod 5 = 4 then
			writeln;
		end;
	writeln;
	end;
writeln('enter an error name');
readln(err);
if err in [ESEEK, EGOTO, ECLOSE, EWRITE] then begin
	writeln(err, ': error cannot be simulated');
	goto 99;
	end;
case err of
ECHR: ch:=chr(128);
EHALT: halt;
ENILPTR: ch:=ptr^;
ECASE: case 4 of 1:; end;
EOUTOFMEM: while true do begin
		new(ptr2);
		writeln('alloc successful');
		end;
ECTLWR: begin
	i:=0;
	s:=[i..2];
	end;
ECTUPR: begin
	i:=5;
	s:=[1..i];
	end;
ECTSNG: begin
	i:=0;
	s:=[i];
	end;
ESTLIM: stlimit(0);
EARGV: argv(100,chs);
EPACK: pack(ch1,2,chs);
EUNPACK: unpack(chs,ch1,2);
EASRT: assert(false);
ELLIMIT: begin
	 linelimit(output,1);
	 writeln('This only should print');
	 writeln;
	 writeln('ERROR');
	 end;
ETRASHHEAP: begin
	    new(ptr);
	    ptr1:=ptr;
	    dispose(ptr1);
	    dispose(ptr);
	    end;
EPASTEOF: begin
	  rewrite(junk);
	  reset(junk);
	  get(junk);
	  get(junk);
	  write(junk^);
	  end;
EREFINAF: ch:=junk^;
ENOFILE: ch:=other^;
ENAMESIZE: rewrite(junk,
'thisisaverylongandconvolutedfilenamewhichexceedsalllimitsofreasonablenessandgoodtaste');
EBADINUM: begin
	  writeln('Enter a letter');
	  read(i);
	  end;
EBADFNUM: begin
	  writeln('Enter a letter');
	  read(r);
	  end;
ENUMNTFD: begin
	  writeln('Enter your name');
	  read(err);
	  end;
ENAMRNG:  begin
	  variant.val:=100;
	  writeln(variant.name);
	  end;
EFMTSIZE: begin
	  i:=-1;
	  writeln(1.0:i);
	  end;
ECREATE: rewrite(locked);
EREMOVE: remove('none');
EOPEN: reset(locked);
EREADIT: read(output,ch);
EWRITEIT: write(input,ch);
ESQRT: r:=sqrt(-1.0);
ELN: r:=ln(0);
ERANGE: ch:=succ(chr(127));
ESUBSC: ch:=ch1[127 + 1];
end;
writeln('*** ERROR NOT DETECTED ***');
99:end.
