# make the various documents

ROFF=nroff

install: install.mn tmac.n
	$(ROFF) tmac.n install.mn

howto: howto.mn tmac.n
	$(ROFF) tmac.n howto.mn

manner: manner.mn tmac.n
	$(ROFF) tmac.n manner.mn

standard: standard.mn tmac.n
	$(ROFF) tmac.n standard.mn

copyright: copyright.mn tmac.n
	$(ROFF) tmac.n copyright.mn
