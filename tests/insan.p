program insane(input, output);
label
	1;
type
	alfa = packed array[1..10] of char;
	face = (front, back, top, bottom, left, right);
	pair = (one2, three4, five6);
	color = (red, blue, green, white);
	blockno = 1..4;
var
	nosolutions: Boolean;
	index, halfindex: integer;
	pointr: integer;
	data: array[blockno, face] of alfa;
	sum: array[blockno, pair, color] of integer;
	halfsolution: array[blockno, 1..30] of pair;

function word(alf: alfa): color;
begin
	if alf = 'red' then
		word := red else
	if alf = 'blue' then
		word := blue else
	if alf = 'green' then
		word := green else
		word := white;
end;

procedure readin;
var
	hue: alfa;
	ch: char;
	cube: blockno;
	position: face;

procedure tone;
begin
	case ch of
	'r': hue := 'red';
	'w': hue := 'white';
	'g': hue := 'green';
	'b': hue := 'blue';
	end;
end;

begin
	for cube := 1 to 4 do
	begin
		for position := front to right do
		begin
			read(ch);
			tone;
			data[cube, position] := hue;
		end;
		readln;
	end;
end;

procedure sumcolors;
var
	cube: blockno;
	side: face;
function facepair(aface: face): pair;
begin
	case aface of
	front, back: facepair := one2;
	top, bottom: facepair := three4;
	left, right: facepair := five6
	end;
end;

procedure initializesum;
var
	cube: blockno;
	side: face;
	technicolor: color;
begin
	for cube := 1 to 4 do
		for side :=  front to right do
			for technicolor := red to white do
				sum[cube, facepair(side), technicolor] := 0;
end;

begin
	initializesum;
	for cube := 1 to 4 do
		for side := front to right do
			sum[cube, facepair(side), word(data[cube,side])] :=
			sum[cube, facepair(side), word(data[cube,side])] + 1;
end;

procedure find2222;
var
	subtotals: array[red..white] of integer;
	pair1, pair2, pair3, pair4: pair;

function two222(pair1, pair2, pair3, pair4: pair): Boolean;
var
	hue: color;
begin
	for hue := red to white do
		subtotals[hue] :=
			sum[1, pair1, hue]+
			sum[2, pair2, hue]+
			sum[3, pair3, hue]+
			sum[4, pair4, hue];
	if (subtotals[red]=2) and
	   (subtotals[blue]=2) and
	   (subtotals[green]=2) and
	   (subtotals[white]=2) then
		two222 := true else
		two222 := false;
end;

procedure listsolution;
begin
	halfsolution[1, halfindex] := pair1;
	halfsolution[2, halfindex] := pair2;
	halfsolution[3, halfindex] := pair3;
	halfsolution[4, halfindex] := pair4;
	halfindex := halfindex + 1;
end;

begin
	halfindex := 1;
	for pair1 := one2 to five6 do
	for pair2 := one2 to five6 do
	for pair3 := one2 to five6 do
	for pair4 := one2 to five6 do
		if two222(pair1, pair2, pair3, pair4) then
			listsolution;
	if halfindex <= 2 then
	begin
		nosolutions := true;
		goto 1;
	end;
end;

procedure simultaneous;
var
	done: Boolean;
begin
	nosolutions := false;
	pointr := 0;
	done := false;
	repeat
		pointr := pointr + 1;
		repeat
			index := succ(pointr);
			if (halfsolution[1, pointr]<>halfsolution[1,index]) and
			   (halfsolution[2, pointr]<>halfsolution[2,index]) and
			   (halfsolution[3, pointr]<>halfsolution[3,index]) and
			   (halfsolution[4, pointr]<>halfsolution[4,index]) then
				done := true else
				index := index + 1;
		until done or (index = pred(halfindex));
	until done or (pointr = halfindex);
	if pointr = halfindex then
	begin
		nosolutions := true;
		goto 1;
	end;
end;

procedure rearrange;
var
	box: blockno;
	a, b: pair;

procedure put(a, b: pair);
var
	old1, new1, old2, new2: face;
	save1, save2: alfa;

procedure oldpair(c: pair);
begin
	case c of
	one2:
		begin
			old1 := front;
			old2 := back;
		end;
	three4:
		begin
			old1 := top;
			old2 := bottom;
		end;
	five6:
		begin
			old1 := left;
			old2 := right;
		end
	end;
end;
procedure newpair(d: pair);
begin
	oldpair(b);
	new1 := old1;
	new2 := old2;
end;

begin
	newpair(b);
	oldpair(a);
	save1 := data[box, new1];
	data[box, new1] := data[box, old1];
	data[box, old1] := save1;
	save2 := data[box, new2];
	data[box, new2] := data[box, old2];
	data[box, old2] := save2;
end;

begin
	for box := 1 to 4 do
	begin
		a := halfsolution[box, pointr];
		b := halfsolution[box, index];
		if (a=one2) and (b=five6) then
			put(five6, three4) else
		begin
			if a = three4 then
			begin
				if b = one2 then
				begin
					put(one2, five6);
					put(three4, one2);
					put(five6, three4);
				end else
				begin
					put(three4, one2);
					put(five6, three4);
				end
			end else
			if b = one2 then
			begin
				put(one2, three4);
				put(five6, one2);
			end else
				put(five6, one2);
		end;
	end;
end;

procedure correct;
var
	list: array[1..8] of integer;
	done: Boolean;
	side: face;
	counter: integer;

procedure check;
var
	delux: array[red..white] of integer;
	kolor: color;
	counter: integer;
begin
	done := true;
	for kolor := red to white do
		for counter := 1 to 4 do
			delux[kolor] := 0;
	for counter := 1 to 4 do
	begin
		delux[word(data[counter,side])] :=
		delux[word(data[counter,side])] + 1;
		if delux[word(data[counter,side])] >= 2 then
			done := false;
	end;
end;

procedure rotate;
var
	save: alfa;
	opposite: face;
begin
	if side = back then
		opposite := front else
	if side = front then
		opposite := back else
	if side = top then
		opposite := bottom else
	if side = bottom then
		opposite := top;
	save := data[list[counter], side];
	data[list[counter], side] := data[list[counter], opposite];
	data[list[counter], opposite] := save;
end;

begin
	list[1] := 4;
	list[2] := 3;
	list[3] := 4;
	list[4] := 2;
	list[5] := 4;
	list[6] := 3;
	list[7] := 4;
	list[8] := 3;
	for side := back to top do
	begin
		counter := 0;
		check;
		while not done do
		begin
			counter := counter + 1;
			rotate;
			check;
		end;
	end
end;

procedure printout;
var
	space: integer;
	cube: integer;
	side: face;
begin
	if nosolutions then
		writeln('no solutions') else
	begin
		writeln('solution to instant insanity');
		for cube := 1 to 4 do
		begin
			write(cube, '   ');
			for side := front to bottom do
				write(data[cube, side]);
			writeln;
		end;
	end;
end;

begin
	reset(input, 'insan.d');
	readin;
	sumcolors;
	find2222;
	simultaneous;
	rearrange;
	correct;
1:
	printout;
end.
{
wbggrb
wbrgrr
wbgwrg
wrgwbr
}
