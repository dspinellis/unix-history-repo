CFLAGS = -O -s

all:	mkey inv hunt refer
	:

cp:	all
	cp refer /bin
	cp inv hunt mkey /usr/lib/refer
	rm mkey inv hunt refer *.o

cmp:	all
	cmp refer /bin/refer
	cmp inv /usr/lib/refer/inv
	cmp hunt /usr/lib/refer/hunt
	cmp mkey /usr/lib/refer/mkey
	rm mkey inv hunt refer *.o

mkey: mkey1.o mkey2.o mkey3.o deliv2.o
	cc -s -n mkey?.o deliv2.o -o mkey
inv: inv1.o inv2.o inv3.o inv5.o inv6.o deliv2.o
	cc -s -n inv?.o deliv2.o -o inv
hunt: hunt1.o hunt2.o hunt3.o hunt5.o hunt6.o hunt7.o glue5.o
hunt: refer3.o hunt9.o shell.o deliv2.o hunt8.o glue4.o
	cc -s -i hunt?.o refer3.o glue5.o glue4.o shell.o deliv2.o -o hunt
deliv: deliv1.o deliv2.o
	cc -s -n deliv?.o  -o deliv
refpart: refer0.o refer1.o refer2.o refer3.o refer4.o refer5.o
refpart: refer6.o refer7.o refer8.o deliv2.o glue4.o
	cc -s -n refer?.o deliv2.o glue4.o  -o refpart
refer0.o: refer..c
refer1.o: refer..c
refer2.o: refer..c
refer3.o: refer..c
refer4.o: refer..c
refer6.o: refer..c
refer: glue1.o refer1.o refer2.o refer4.o refer5.o refer6.o mkey3.o
refer: refer7.o refer8.o hunt2.o hunt3.o deliv2.o hunt5.o hunt6.o hunt8.o
refer: glue3.o hunt7.o hunt9.o glue2.o glue4.o glue5.o refer0.o shell.o
	cc -s -i glue?.o refer[01245678].o hunt[2356789].o mkey3.o shell.o deliv2.o -o refer
whatabout: what1.o what2.o what3.o what4.o shell.o mkey3.o
	cc -s -n what?.o shell.o mkey3.o  -o whatabout
