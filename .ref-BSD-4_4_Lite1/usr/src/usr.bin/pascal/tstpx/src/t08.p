program summing(output);
var
	s1, s2p, s2n, s3, s4p, s4n,
	lrp, lrn, rlp, rln: real;
	i: integer;
begin
	s1 := 0;
	s2p := 0;
	s2n := 0;
	s3 := 0;
	s4p := 0;
	s4n := 0;
	for i := 1 to 500 do
	begin
		lrp := 1/(2*i-1);
		lrn := 1/(2*i);
		rlp := 1/(10001-2*i);
		rln := 1/(10002-2*i);
		s1 := s1 + lrp - lrn;
		s2p := s2p + lrp;
		s2n := s2n + lrn;
		s3 := s3 + rlp - rln;
		s4p := s4p + rlp;
		s4n := s4n + rln;
	end;
	writeln(s1, s2p-s2n);
	writeln(s3, s4p-s4n);
end.
