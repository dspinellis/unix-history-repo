# (c) Copyright 1986, Xerox Corporation
#
# Do test cases

test: pic.ip res.ip

pic.ip: pic.me
	${PIC} pic.me | ${TROFF} -o pic.ip -me

res.ip:
	${TROFF} -o res-.ip -me
	ipfe -s -o res.ip res-.ip

clean:
	rm -f pic.ip res-.ip res.ip
