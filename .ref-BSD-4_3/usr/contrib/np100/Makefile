OBJS = npload npdump

all: $(OBJS)

install: all
	for j in $(OBJS); do install -c -s $$j $(DESTDIR)/etc; done;
	cp np.image $(DESTDIR)/etc

clean:
	rm -f $(OBJS)

npload: npload.c
	cc -o npload npload.c

npdump: npdump.c
	cc -o npdump npdump.c
