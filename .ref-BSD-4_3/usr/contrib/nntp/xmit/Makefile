
NNTPSRC = nntpxmit.c rfc822.c remote.c get_tcp_conn.c getdate.y llist.c
NNTPOBJ = nntpxmit.o rfc822.o remote.o get_tcp_conn.o getdate.o llist.o

get_tcp_conn.o: get_tcp_conn.c get_tcp_conn.h
 
remote.o: remote.c response_codes.h nntpxmit.h
 
nntpxmit.o: nntpxmit.c nntpxmit.h get_tcp_conn.h header.h

nntpxmit: $(NNTPOBJ)
	$(CC) $(LDFLAGS) $(NNTPOBJ) -o nntpxmit
