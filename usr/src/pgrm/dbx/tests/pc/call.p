(*
 * Test program for dbx call command.
 *)

program calltest (input, output);

type String = array [1..4] of char;
var global : integer;

procedure p1 ;
begin
    writeln(output, 'in p1');
    global := 4;
end;

procedure p2 (frommain : integer);
begin
    writeln(output, 'in p2(', frommain:1, ')');
    global := 9;
end;

procedure p3 (s : String; i : integer);
begin
    writeln(output, 'in p3(', s, ', ', i:1, ')');
    global := 10;
end;

procedure startup ;
var
    mainlocal : integer;
begin
    global := 2;
    mainlocal := 19;
    p1;
    p2(mainlocal);
    p3('test', 3);
end;

begin
    startup;
end.
