(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pxref.p	5.5 (Berkeley) %G%
 *)

{$t-,p-,b2,w+}
program xref(input, output);
label
    99, 100;
const
    alfasize = 18;
    linesize = 10;
    namesize = 64;
    linelength = 133;
    maxlineno = 30000;
    charclassize = 127;
    p = 1000;
    nk = 36;
    blanks = '  ';
type
    alfa = 
      array[1..alfasize] of 
	char;
    index = 0..p;
    linptr = 0..linelength;
    linebuf = array[1..linelength] of char;
    ref = ^item;
    filename = array [1..namesize] of char;
    charclasses = (digit, letter, separator, illegal);
    charclasstype = array[0..charclassize] of charclasses;
    word = 
      record
	key: alfa;
	first, last: ref;
	fol: index
      end;
    item =   packed
      record
	lno: 0..maxlineno;
	next: ref
      end;
var
    i, top: index;
    formfeed :char;
    scr: alfa;
    list: boolean;
    k, k1: integer;
    n: integer;
    c1, c2: integer;
    inputfile : filename;
    lineptr :linptr;
    line :linebuf;
    charclass :charclasstype;
    id: 
      record
	case boolean of
	  false:(
	    a: alfa
	  );
	  true:(
	    ord: integer
	  )
      end;
    a: array [1..alfasize] of char;
    t: array [index] of word;
    key: array [1..nk] of alfa;
    empty: alfa;

    function nokey(x: alfa): Boolean;
    var
	i, j, k: integer;
    begin
	i := 1;
	j := nk;
	repeat
	    k := (i + j) div 2;
	    if key[k] <= x then 
		i := k + 1;
	    if key[k] >= x then 
		j := k - 1
	until i > j;
	nokey := key[k] <> x
    end { nokey };

    procedure search;
    var
	h, d: index;
	x: ref;
	f: Boolean;
    begin
	h := id.ord div 4096 mod p;
	f := false;
	d := 1;
	c2 := c2 + 1;
	new(x);
	x^.lno := n;
	x^.next := nil;
	repeat
	    if t[h].key = id.a then begin
		f := true;
		t[h].last^.next := x;
		t[h].last := x
	    end else if t[h].key = empty then begin
		f := true;
		c1 := c1 + 1;
		t[h].key := id.a;
		t[h].first := x;
		t[h].last := x;
		t[h].fol := top;
		top := h
	    end else begin
		h := (h + d) mod p;
		d := d + 2;
		if d >= p then begin
		    writeln;
		    writeln(' **** table full');
		    goto 99
		end
	    end
	until f
    end { search };

    procedure printword(w: word);
    var
	l: integer;
	x: ref;
    begin
	write(' ', w.key);
	x := w.first;
	l := 0;
	repeat
	    if l = linesize then begin
		l := 0;
		writeln;
		write(' ', empty)
	    end;
	    l := l + 1;
	    write(x^.lno: 6);
	    x := x^.next
	until x = nil;
	writeln
    end { printword };

    procedure printtable;
    var
	i, j, m: index;
    begin
	i := top;
	while i <> p do begin
	    m := i;
	    j := t[i].fol;
	    while j <> p do begin
		if t[j].key < t[m].key then 
		    m := j;
		j := t[j].fol
	    end;
	    printword(t[m]);
	    if m <> i then begin
		t[m].key := t[i].key;
		t[m].first := t[i].first;
		t[m].last := t[i].last
	    end;
	    i := t[i].fol
	end
    end { printtable };

    procedure readinput(var inpfile :filename);
    var
    inp :file of char;
    
    procedure lwriteln;
    begin
	if list then begin
	    { write sans trailing blanks }
	    if lineptr > 0 then
		writeln(line: lineptr)
	    else
		writeln;
	end;
	get(inp);
	lineptr:=0
    end { lwriteln };

    procedure newline;
    begin
	n:=n+1;
	if n = maxlineno then begin
	    writeln(' text too long');
	    goto 99
	end;
	if inp^ = formfeed then begin
	    if list then
		page(output);
	    get(inp)
	end;
	if list then
	    if not eoln(inp) then
		write(n:6,'  ')
    end { newline };

    begin
	reset(inp,inpfile);
	while not eof(inp) do begin
	    newline;
	    if inp^ = '#' then begin
		while inp^ <> '"' do begin
		    lineptr:=lineptr+1;
		    read(inp,line[lineptr])
		end;
		lineptr:=lineptr+1;
		read(inp,line[lineptr]);
		k:=0;
		inputfile:=blanks;
		repeat
		    k:=k+1;
		    if k <= namesize then
			inputfile[k]:=inp^;
		    lineptr:=lineptr+1;
		    read(inp,line[lineptr])
		until inp^ = '"';
		while not eoln(inp) do begin
		    lineptr:=lineptr+1;
		    read(inp,line[lineptr])
		end;
		id.a := '#include';
		search;
		lwriteln;
		readinput(inputfile);
	    end else begin
		while not eoln(inp) do begin
		    if (inp^ = ' ') or (inp^ = tab) then begin
			lineptr:=lineptr+1;
			read(inp,line[lineptr])
		    end else if charclass[ord(inp^)] = letter then begin
		        k := 0;
			a:=blanks;
		        repeat
			    k := k + 1;
			    if k <= alfasize then
			        a[k] := inp^;
			    lineptr:=lineptr+1;
			    read(inp,line[lineptr])
		        until (charclass[ord(inp^)] <> letter) and
			      (charclass[ord(inp^)] <> digit);
		        pack(a, 1, id.a);
		        if nokey(id.a) then 
			    search
		    end else if charclass[ord(inp^)] = digit then 
		        repeat
			    lineptr:=lineptr+1;
			    read(inp,line[lineptr])
		        until charclass[ord(inp^)] <> digit
		    else if inp^='''' then begin
		        repeat
			    lineptr:=lineptr+1;
			    read(inp,line[lineptr])
		        until inp^ = '''';
			lineptr:=lineptr+1;
			read(inp,line[lineptr])
		    end else if inp^ = '{' then begin
		        repeat
			    lineptr:=lineptr+1;
			    read(inp,line[lineptr]);
			    while eoln(inp) do begin
			        lwriteln;
				newline
			    end
		        until inp^ = '}';
			lineptr:=lineptr+1;
			read(inp,line[lineptr])
		    end else if inp^ = '(' then begin
			lineptr:=lineptr+1;
			read(inp,line[lineptr]);
		        if inp^ = '*' then begin
			    lineptr:=lineptr+1;
			    read(inp,line[lineptr]);
			    repeat
			        while inp^ <> '*' do
				    if eoln(inp) then begin
				        lwriteln;
					newline
				    end else begin
					lineptr:=lineptr+1;
					read(inp,line[lineptr])
			            end;
				lineptr:=lineptr+1;
				read(inp,line[lineptr])
			    until inp^ = ')';
			    lineptr:=lineptr+1;
			    read(inp,line[lineptr])
		        end
		    end else begin
			lineptr:=lineptr+1;
			read(inp,line[lineptr]);
		    end
		end; { scan of token }
		lwriteln;
	    end; { scan of line }
	end; { while not eof }
    end; {readinput }

begin { xref }
    empty := blanks;
    list := true;
    if argc = 3 then begin
	argv(1, scr);
	if (scr[1] <> '-') or (scr[2] <> ' ') then begin
	    writeln('usage: pxref [ - ] file');
	    goto 100
	end;
	list := false
    end;
    if (argc < 2) or (argc > 3) then begin
	writeln('usage: pxref [ - ] file');
	goto 100
    end;
    for i := 0 to p - 1 do 
	t[i].key := empty;
    c1 := 0;
    c2 := 0;
    key[1] := 'and';
    key[2] := 'array';
    key[3] := 'assert';
    key[4] := 'begin';
    key[5] := 'case';
    key[6] := 'const';
    key[7] := 'div';
    key[8] := 'do';
    key[9] := 'downto';
    key[10] := 'else';
    key[11] := 'end';
    key[12] := 'file';
    key[13] := 'for';
    key[14] := 'function';
    key[15] := 'hex';
    key[16] := 'if';
    key[17] := 'in';
    key[18] := 'mod';
    key[19] := 'nil';
    key[20] := 'not';
    key[21] := 'oct';
    key[22] := 'of';
    key[23] := 'or';
    key[24] := 'packed';
    key[25] := 'procedure';
    key[26] := 'program';
    key[27] := 'record';
    key[28] := 'repeat';
    key[29] := 'set';
    key[30] := 'then';
    key[31] := 'to';
    key[32] := 'type';
    key[33] := 'until';
    key[34] := 'var';
    key[35] := 'while';
    key[36] := 'with';
    for k:= 0 to charclassize do
	charclass[k]:=illegal;
    for k:=ord('a') to ord('z') do
	charclass[k]:=letter;
    for k:=ord('A') to ord('Z') do
	charclass[k]:=letter;
    for k:=ord('0') to ord('9') do
	charclass[k]:=digit;
    charclass[ord('_')]:=letter;
    charclass[ord(' ')]:=separator;
    charclass[ord(tab)]:=separator;
    n := 0;
    lineptr:=0;
    line:=blanks;
    top := p;
    k1 := alfasize;
    formfeed:=chr(12);
    if list then
        argv(1,inputfile)
    else
        argv(2,inputfile);
    readinput(inputfile);
99:
    if list then begin
	page(output);
        writeln;
        end;
    printtable;
    writeln;
    writeln(c1, ' identifiers', c2, ' occurrences');
100:
    {nil}
end { xref }.
