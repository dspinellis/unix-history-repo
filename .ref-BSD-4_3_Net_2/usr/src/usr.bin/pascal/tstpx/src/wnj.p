program testset (output);

{ try to figure out why sets don't work }

const
	firstel = -1; lastel = 80;
type
	eltype = firstel..lastel;
	settype = set of eltype;
var
	tset: settype;

procedure elements (someset: settype);
	{ print the elements of a set}
	var i: eltype;

	begin
		writeln;
		writeln ('in elements');
		for i := firstel to lastel do
			if i in someset then writeln ('member:',i)
	end; {elements}

begin
	tset := [];		elements (tset);
	tset := [23,45];	elements (tset);

	if 23 in tset then writeln ('ok'); {to eliminate the possibility that
						the problem arises from passing
						a set as a parameter}

	tset := tset + [firstel]; elements (tset);
	tset := tset + [lastel];elements (tset);
	tset := [];		elements (tset)
	end.
