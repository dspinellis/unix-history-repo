(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pcerror.p	5.1 (Berkeley) %G%
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
