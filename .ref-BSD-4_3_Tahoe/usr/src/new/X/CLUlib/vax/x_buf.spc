
x_buf = cluster is init,
		   get, send_data, send_array, flush,
		   receive, receive_data, events,
		   get_lp0, get_lp1, get_lp2, get_lp3,
		   get_sp0, get_sp1, get_sp2, get_sp3, get_sp4, get_sp5,
		   get_bp10, get_bp11

rep = null

init = proc (addr: _wordvec) signals (error(string))
	end init

get = proc () returns (oreq, ereq)
	end get

send_data = proc (b: _bytevec, start, z: int)
	end send_data

send_array = proc (a: array[char], start, z: int)
	end send_array

flush = proc ()
	end flush

receive = proc () signals (error(string))
	end receive

receive_data = proc (b: _bytevec)
	end receive_data

events = proc (need: bool)
	end events

get_lp0 = proc () returns (int)
	end get_lp0

get_lp1 = proc () returns (int)
	end get_lp1

get_lp2 = proc () returns (int)
	end get_lp2

get_lp3 = proc () returns (int)
	end get_lp3

get_sp0 = proc () returns (int)
	end get_sp0

get_sp1 = proc () returns (int)
	end get_sp1

get_sp2 = proc () returns (int)
	end get_sp2

get_sp3 = proc () returns (int)
	end get_sp3

get_sp4 = proc () returns (int)
	end get_sp4

get_sp5 = proc () returns (int)
	end get_sp5

get_bp10 = proc () returns (int)
	end get_bp10

get_bp11 = proc () returns (int)
	end get_bp11

end x_buf
