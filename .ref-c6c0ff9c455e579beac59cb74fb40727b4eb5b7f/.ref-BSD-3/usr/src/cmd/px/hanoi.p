program hanoi(input,output);
const print = 0;
	DISK = 14;
var num: array[1..3] of integer;
	ow: integer;
    cnt: integer;
    disk: integer;
procedure mov(n,f,t : integer);
var o: integer;
begin
	if n = 1 then begin
		num[f] := num[f] - 1;
		num[t] := num[t] - 1;
		if print = 1 then writeln('Move ',f, ' to ', t);
		cnt := cnt + 1;
		end
	else begin
		o := 6 - (f+t);
		mov(n-1,f,o);
		mov(1,f,t);
		mov(n-1,o,t);
		end
end;

begin
	disk := DISK;
	writeln('Number: ');
	read(disk);
	num[1] := disk;
	cnt := 0;
	if print = 1 then writeln('start ',disk);
	ow := wallclock;
	mov(disk,1,3);
	writeln('For ',disk:3,' ',cnt:10,' steps, time',wallclock-ow);
end.
