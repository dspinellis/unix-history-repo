program filetest(input, output, testfile);
var testfile : text;
begin
    writeln('opening testfile');
    rewrite(testfile);
    writeln(testfile, 'all done');
end.
