all:	as as2

cmp:	as as2
	cmp as /bin/as
	cmp as2 /lib/as2
	rm as as2 a.out

cp:	as as2
	cp as /bin/as
	cp as2 /lib/as2
	rm as as2 a.out

as:
	as /usr/include/sys.s as1?.s
	ld -n -s a.out -o as

as2:
	as /usr/include/sys.s as2?.s
	ld -n -s a.out -o as2
