module main;
import io;

var globalf : io.File;

procedure p (var f : io.File);
begin
    f := io.terminal;
    io.Writef(f, 'this is a test');
    io.Writef(f, '\n');
end p;

begin
    globalf := io.terminal;
    p(io.output);
end main.
