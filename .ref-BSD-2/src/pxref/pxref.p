{$p-,t-,b2}
program xref(input, output);
label
    99, 100;
const
    p = 797;
    nk = 36;
    empty = '          ';
type
    index = 0..p;
    ref = ^item;
    word = 
      record
	key: alfa;
	first, last: ref;
	fol: index
      end;
    item =   packed
      record
	lno: 0..9999;
	next: ref
      end;
var
    i, top: index;
    scr: alfa;
    list: boolean;
    k, k1: integer;
    n: integer;
    c1, c2: integer;
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
    a: array [1..10] of char;
    t: array [index] of word;
    key: array [1..nk] of alfa;

    function letter(ch: char): Boolean;
    begin
	letter := (ch >= 'a') and (ch <= 'z') or (ch >= 'A') and (ch <= 'Z')
    end { letter };

    function digit(ch: char): Boolean;
    begin
	digit := (ch >= '0') and (ch <= '9')
    end { digit };

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

    procedure newline;
    begin
	if n < 9999 then begin
	    n := n + 1;
	    if list then 
		write(n: 6, '  ')
	end else begin
	    writeln(' text too long');
	    goto 99
	end
    end { newline };

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
		h := h + d;
		d := d + 2;
		if h >= p then 
		    h := h - p;
		if d = p then begin
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
	    if l = 20 then begin
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

    procedure openinput(i: integer);
    var
	filename: array [1..64] of char;
    begin
	argv(i, filename);
	reset(input, filename)
    end { openinput };

    procedure lwriteln;
    begin
	if list then 
	    writeln
    end { lwriteln };

    procedure lwrite(c: char);
    begin
	if list then 
	    write(c)
    end { lwrite };

begin { xref }
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
    if list then 
	openinput(1)
    else 
	openinput(2);
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
    n := 0;
    top := p;
    k1 := 10;
    while not eof(input) do begin
	if not eoln(input) then 
	    newline
	else 
	    n := n + 1;
	if input^ = '#' then begin
	    while not eoln(input) do begin
		lwrite(input^);
		get(input)
	    end;
	    id.a := '#include';
	    search
	end else 
	    while not eoln(input) do begin
		if (input^ = ' ') or (input^ = tab) then begin
		    lwrite(input^);
		    get(input)
		end else if letter(input^) then begin
		    k := 0;
		    repeat
			lwrite(input^);
			if k < 10 then begin
			    k := k + 1;
			    a[k] := input^
			end;
			get(input)
		    until not (letter(input^) or digit(input^));
		    if k >= k1 then 
			k1 := k
		    else 
			repeat
			    a[k1] := ' ';
			    k1 := k1 - 1
			until k1 = k;
		    pack(a, 1, id.a);
		    if nokey(id.a) then 
			search
		end else if digit(input^) then 
		    repeat
			lwrite(input^);
			get(input)
		    until not digit(input^)
		else if input^ = '''' then begin
		    repeat
			lwrite(input^);
			get(input)
		    until input^ = '''';
		    lwrite('''');
		    get(input)
		end else if input^ = '{' then begin
		    repeat
			lwrite(input^);
			get(input);
			while eoln(input) do begin
			    lwriteln;
			    get(input);
			    newline
			end
		    until input^ = '}';
		    lwrite('}');
		    get(input)
		end else if input^ = '(' then begin
		    lwrite('(');
		    get(input);
		    if input^ = '*' then begin
			lwrite('*');
			get(input);
			repeat
			    while input^ <> '*' do begin
				if eoln(input) then begin
				    lwriteln;
				    newline
				end else 
				    lwrite(input^);
				get(input)
			    end;
			    lwrite('*');
			    get(input)
			until input^ = ')';
			lwrite(')');
			get(input)
		    end
		end else begin
		    lwrite(input^);
		    get(input)
		end
	    end;
	lwriteln;
	get(input)
    end;
99:
    if list then 
	page(output);
    printtable;
    lwriteln;
    writeln(c1, ' identifiers', c2, ' occurrences');
100:
    {nil}
end { xref }.
