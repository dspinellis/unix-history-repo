/^\.TH/{
	s/ I / 1 /
	s/ IL / 1 local /
	s/ IC / 1C /
	s/ ICL / 1C local /
	s/ ID / 1D /
	s/ IG / 1G /
	s/ IGL / 1G local /
	s/ IM / 1M /
	s/ II / 2 /
	s/ III / 3Z /
	s/ IIIA / 3A /
	s/ IIIF / 3F /
	s/ IIIS / 3S /
	s/ IIIZ / 3Z /
	s/ IV / 4 /
	s/ IVH / 9 /
	s/ IVT / 7 /
	s/ V / 5 /
	s/ VI / 1 /
	s/ VII / 3 /
	s/ VIII / 8 /
	/local/{
		s///
		s/$/ local/
	}
}
s/(I)/(1)/g
s/(IC)/(1)/g
s/(ID)/(1)/g
s/(IG)/(1)/g
s/(IM)/(1)/g
s/(II)/(2)/g
s/(III)/(3)/g
s/(IIIA)/(3)/g
s/(IIIF)/(3)/g
s/(IIIS)/(3)/g
s/(IIIZ)/(3)/g
s/(IV)/(4)/g
s/(IVH)/(9)/g
s/(IVT)/(7)/g
s/(V)/(5)/g
s/(VI)/(1)/g
s/(VII)/(3)/g
s/(VIII)/(8)/g
/SEE/,/SH/s/  *(/(/g
${
	/BUG/d
}
