# Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984.
#
# Test the B interpreter.
#
WS	= generate hanoi pi xref

examples: ../bin/b ../lib/bint
	@DoExamples bint
smallexamples: ../bin/b ../lib/bsmall
	@DoExamples bsmall
clean:
	-@for ws in $(WS); do cd $$ws; rm -f Out .b_temp .Bed_sugg; cd ..; done
