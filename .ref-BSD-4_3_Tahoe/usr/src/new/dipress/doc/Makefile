#
# User adjustable parameters
TROFF=iptroff
EQN=ieqn -Tip
TBL=itbl -Tip
PIC=ipic -Tip
#
##############

FILES=setup.me intro.me guide.me program.me intertext.me install-unix.me install-vms.me acknowledge.me appendixA.me examples.me char-chart.me preamble.me

doc.ip: body.ip cover.ip
	ipfe -o doc.ip -s cover.ip body.ip

body.ip cover.ip: doc-.ip
	sh Split-pages.sh

doc-.ip: $(FILES)
	${TBL} ${FILES} | ${EQN} | ${PIC} | ${TROFF} -o doc-.ip -me

clean:
	rm -f doc.ip doc-.ip cover.ip body.ip Split-pages.sh
