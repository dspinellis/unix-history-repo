#
#	since vtroff is a cshell script, the semi-colon is needed
#	to get a cshell to execute the vtroff commands.
#
TROFF=	vtroff
HEADER = header.me
FILES = abstract.me intro.me profiling.me gathering.me \
      postp.tbl.eqn present.tbl refs.me
MES = abstract.me intro.me profiling.me gathering.me \
      postp.me present.me refs.me

print: vpr-t
	vpr -t vpr-t

troff:	${FILES}
	${TROFF} -t -me ${HEADER} ${FILES};

vpr-t: $(FILES)
	${TROFF} -t -me $(HEADER) $(FILES) >vpr-t;

equations.eqn: equations.me
	eqn < equations.me > equations.eqn

postp.tbl.eqn: postp.me
	tbl postp.me | eqn > postp.tbl.eqn

present.tbl: present.me
	tbl < present.me > present.tbl

sources: $(HEADER) $(MES)

clean:
	rm -f vpr-t present.tbl postp.tbl.eqn equations.eqn
