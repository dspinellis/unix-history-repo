# (c) Copyright 1986, Xerox Corporation
#
#
test: ../mp2res
	../mp2res -o happyFamily.res happyFamily.macPaint
	../mp2res -o happyFamily2.res -b 500 happyFamily.macPaint

../mp2res:
	(cd ..; make)

clean:
	rm -f *.res
