TOP = /cs/research/darpa/ron/jon/isodex/bin/

all:	Xsun clients 

Xsun:	server/*.o tsap/*.o
	make -f MakeAServer
	mv Xsun bin

clients:  client/*.o tsap/*.o
	cd xinit; make T=$(TOP); 
	cd xterm; make T=$(TOP); 
	cd uwm; make T=$(TOP); 
	cd xclock; make T=$(TOP); 
	cd xmag; make T=$(TOP); 
	cd xhost; make T=$(TOP); 
	cd xedit; make T=$(TOP); 
	cd xwininfo; make T=$(TOP); 
	cd xev; make T=$(TOP); 

client/*.o:	client/*.c
	cd client; make

server/*.o:	server/*.[ch]
	cd server; make

# coz we dont have latest ISODE handy here
# remove following if you do
tsap/*.o:	tsap/*.c
	cd tsap; make

clean: 
	rm -f */*.o \
	$(TOP)/Xsun \
	$(TOP)/xinit \
	$(TOP)/xterm \
	$(TOP)/uwm \
	$(TOP)/xclock \
	$(TOP)/xmag \
	$(TOP)/xhost \
	$(TOP)/xedit \
	$(TOP)/xwininfo 
