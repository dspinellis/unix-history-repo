CC= cc
CFLAGS= -g

all:		file2dn tdirname edb2dbm edbcat /usr/lib/libgdbm.a

edb2dbm:	edb2dbm.o
	$(CC) -o $@ $? -lgdbm

edbcat:		edbcat.o
	$(CC) -o $@ $? -lgdbm

tdirname:	tdirname.o
	$(CC) -o $@ $?

file2dn:	file2dn.o
	$(CC) -o $@ $?

tree2dbm:

syncedb:

synctree:

install:	all
	install -c -m 755 edb2dbm $(SBINDIR)edb2dbm
	install -c -m 755 edbcat $(SBINDIR)edbcat
	install -c -m 755 tree2dbm $(SBINDIR)tree2dbm
	install -c -m 755 syncedb $(SBINDIR)syncedb
	install -c -m 755 synctree $(SBINDIR)synctree
	install -c -m 755 tdirname $(SBINDIR)tdirname
	install -c -m 755 file2dn $(SBINDIR)file2dn

clean:
	rm -f *.o a.out edb2dbm edbcat tdirname file2dn
