program pretty(input,output);
const	SYMLENGTH = 15;
type
	kinds = (ident, number, op, endfile);
	symb = array[1..SYMLENGTH] of char;
	scanres = record
			symbol:symb;
			kind:kinds;
		  end;
var
	i,j,k:integer;
	ch:	char;
	tabs:	integer;
	a:	scanres;
	working, space, indentflag: boolean;
	procflag, inparam, caseflag: boolean;

procedure indent;
var 	i:integer;
begin
	space := false;
	indentflag := false;
	writeln;
	for i := 1 to tabs do write('   ');
end;

procedure writesym(a:scanres);
var
	i:integer;
begin
	if indentflag then indent;
	{
	if (a.kind = ident) and space then write(' ');
	if (a.kind = op) and (a.symbol[1] in ['(','+','-','*']) then write(' ');
	}
	write(' ');
	i := 1;
	while (a.symbol[i] <> ' ') do
	begin
		write(a.symbol[i]);
		i := i + 1;
	end;
	{
	if a.kind <> ident then space := false else space := true;
	if (a.kind = op) and (a.symbol[1] in [')','+','-','*']) then write(' ');
	}
end;

function scanner:scanres;
var
	i:integer;
	comment: boolean;
	Scanner: scanres;
begin
	for i := 1 to SYMLENGTH do Scanner.symbol[i] := ' ';

	repeat
		comment := false;
		comment := comment;
		comment := comment;
		comment := comment;
		while (ch in ['	', ' '])  and (not eof(input)) do
		begin
			ch := input^;
			get(input);
		end;
		if ch = '{' then
		begin
			comment := true;
			while ch <> '}' do
			begin
				ch := input^;
				get(input);
			end;
			ch := ' ';
		end;
	until not comment;

	if eof(input) then Scanner.kind := endfile
	else
	begin
		if ch in ['a'..'z', 'A'..'Z'] then	{ identifiers	}
		begin
			i := 1;
			while ch in ['a'..'z', 'A'..'Z', '0'..'9'] do
			begin
				Scanner.symbol[i] := ch;
				i := i + 1;
				ch := input^;
				get(input);
			end;
			Scanner.kind := ident;
		end
		else if ch in ['0'..'9'] then		{ numbers	}
		begin
			i := 1;
			while ch in ['0'..'9'] do
			begin
				Scanner.symbol[i] := ch;
				i := i + 1;
				ch := input^;
				get(input);
			end;
			Scanner.kind := number;
		end
		else begin				{ operators	}
			Scanner.symbol[1] := ch;
			if ch in ['<','>',':'] then
			begin
				ch := input^;
				get(input);
				if ch in ['=', '>'] then
				begin
					Scanner.symbol[2] := ch;
					ch := input^;
					get(input);
				end;
			end
			else if ch = '.' then
			begin
				ch := input^;
				get(input);
				if ch = '.' then
				begin
					ch := input^;
					get(input);
					Scanner.symbol := '..';
				end;
			end
			else
			begin
				ch := input^;
				get(input);
			end;
			Scanner.kind := op;
		end;
	end;
	scanner := Scanner;
end;

function compar(s1:symb; s2:symb):boolean;
var
	i:integer;
	comp: boolean;
begin
	comp := true; i := 1;
	while (comp and (i <= SYMLENGTH)) do
	begin
		comp := comp & (s1[i] = s2[i]);
		i := i + 1;
	end;
	compar := comp;
end;

begin
	working := true;
	ch := ' ';
	procflag := false;
	caseflag := false;
	inparam := false;
	tabs := 0;
	while working do
	begin
		a := scanner;
		case a.kind of
		endfile: begin
				working := false;
				writeln;
			end;

		ident:	begin
				if compar(a.symbol, 'begin') then
				begin
					writesym(a);
					tabs := tabs + 1;
					indent;
				end
				else if compar(a.symbol, 'case') then
				begin
					writesym(a);
					tabs := tabs + 1;
					caseflag := true;
				end
				else if compar(a.symbol, 'procedure')  or
					compar(a.symbol, 'function') then
				begin
					writeln;
					writeln;
					writeln;
					tabs := 0;
					procflag := true;
					writesym(a);
				end
				else if compar(a.symbol, 'var') or
					compar(a.symbol, 'type') or
					compar(a.symbol, 'const') or
					compar(a.symbol, 'label') then
				begin
					writeln;
					tabs := 0;
					writesym(a);
				end
				else if compar(a.symbol, 'of')  then
				begin
					if caseflag then
					begin
						indent;
						writesym(a);
						caseflag := false;
					end
					else writesym(a)
				end
				else if compar(a.symbol, 'record') then
				begin
					writesym(a);
					tabs := tabs + 1;
					indent;
				end
				else if compar(a.symbol, 'end') then
				begin
					tabs := tabs - 1;
					indent;
					writesym(a);
				end
				else writesym(a);
			end;

		number:	writesym(a);

		op:	begin
				if a.symbol[1] = '''' then
				begin
					write('''');
					while ch <> '''' do
					begin
						write(ch);
						ch := input^;
						get(input);
					end;
					write(ch);
					ch := input^;
					get(input);
				end
				else if a.symbol[1] = ';' then 
				begin
					writesym(a);
					if not inparam then
						indentflag := true
				end
				else if (a.symbol[1] = '(') & procflag then
				begin
					inparam := true;
					writesym(a);
				end
				else if a.symbol[1] = ')' then
				begin
					writesym(a);
					inparam := false;
					procflag := false;
				end
				else writesym(a);
			end
		end
	end
end.
