# (c) Copyright 1986, Xerox Corporation
#
# Do test cases

test:
	graph -a -g 1 -y 0 9000 < data1 > data1.plot
	../plot2ip < data1.plot > data1.ip
	graph -a -g 1 -y 0 9000 < data2 > data2.plot
	../plot2ip < data2.plot -o data2.ip
	graph -a -g 1 -y 0 9000 < data3 > data3.plot
	../plot2ip -o data3.ip < data3.plot

clean:
	rm -f *.plot *.ip
