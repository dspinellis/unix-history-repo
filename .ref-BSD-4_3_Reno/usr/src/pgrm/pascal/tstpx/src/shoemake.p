program test(input, output);
type
   alpha    = array[1..10] of char;
   lineType = array[1..80] of char;

procedure test1(name : alpha);		{If this parameter is integer, the pro-
					 gram will not bomb }
   var					{If any of these local variables are
					 deleted, the program will run OK}
      linenum : integer;
      line   : lineType; 		{If this is a simple type, the program
					 will not bomb.}
      index  : integer;
      chars  : integer;
      infile : text;

   begin				{If other statements are put before this
					 reset, they are executed, and the pro-
					 gram doesn't bomb until the reset is
					 executed.}

      reset(infile, '/dev/null');
   end;
      
begin
   rewrite(output,'/dev/null');		{If this rewrite statement is not here,
					 the program will not bomb}
   test1('/dev/null');
end.
