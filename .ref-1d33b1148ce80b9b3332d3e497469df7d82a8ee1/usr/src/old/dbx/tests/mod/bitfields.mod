module main;

import io;

type
    uint = @align 1 cardinal;
    dotrecord = record
	cost : @size 24 uint;
	dtype : @size 3 uint;
	dirToCenter : @size 3 uint;
	pad : @size 1 uint;
	pin : @size 1 uint;
	traceback : @size 3 uint;
	traceforward : @size 3 uint;
	expanded : @size 1 uint;
	underDir : @size 3 uint;
	underOffset : @size 4 uint;
	start : @size 1 uint;
	target : @size 1 uint;
	owner : @size 6 uint;
	segment : @size 7 uint;
	intrinsicCost : @size 3 uint;
    end;

procedure p1 ();
var junk : dotrecord;
begin
    junk.owner := 63;
    junk.segment := 1;
    junk.intrinsicCost := 1;
    io.Writef(io.output, "owner = %d, segment = %d, intrinsicCost = %d\n",
	junk.owner, junk.segment, junk.intrinsicCost);
    p2();
end p1;

procedure p2 ();
var x : record
	first : integer;
	second : integer;
	a : @size 8 @align 1 integer;
	b : @size 8 @align 1 integer;
	c : integer;
    end;
begin
    x.first := 0;
    x.second := 0;
    x.a := 2;
    x.b := 10;
    x.c := 1;
    io.Writef(io.output, "done\n");
end p2;

begin
    p1();
end main.
