#
#	since vtroff is a cshell script, the semi-colon is needed
#	to get a cshell to execute the vtroff commands.
#
#	Makefile 1.1 %G% 15:30:05

HEADER = header.me
FILES = abstract.me intro.me profiling.me gathering.me \
      postp.tbl.eqn present.tbl refs.me
MES = abstract.me intro.me profiling.me gathering.me \
      postp.me present.me refs.me

all: $(FILES)
	vtroff -t -me $(HEADER) $(FILES) >/tmp/peter.vpr-t;
	vpr -t /tmp/peter.vpr-t

Wall: $(FILES)
	vtroff -W -l14 -t -me Wheader.me $(FILES) > ccc.vpr-t-W;

vpr-t: $(FILES)
	vtroff -t -me $(HEADER) $(FILES) >/tmp/peter.vpr-t;

equations.eqn: equations.me
	eqn < equations.me > equations.eqn

postp.tbl.eqn: postp.me
	tbl postp.me | eqn > postp.tbl.eqn

present.tbl: present.me
	tbl < present.me > present.tbl

sources: $(HEADER) $(MES)

$(HEADER) $(MES):
	sccs get $@

echo:
	@echo ${HEADER} ${MES}

clean:
	rm *.vpr-t
	touch $(MES)
