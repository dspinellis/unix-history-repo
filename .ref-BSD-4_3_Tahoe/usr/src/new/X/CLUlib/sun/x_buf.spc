
x_buf = cluster is init, setup,
		   set_s0, set_s01, set_s01l1, set_s0123,
		   set_l0, set_l01, set_l0s23,
		   set_s45l3, set_s4567, set_l23, set_l2s67,
		   send_data, send_array, flush,
		   receive, receive_data, events,
		   get_lp0, get_lp1, get_lp2, get_lp3,
		   get_sp0, get_sp1, get_sp2, get_sp3, get_sp4, get_sp5,
		   get_bp10, get_bp11

rep = null

init = proc (addr: _wordvec) signals (error(string))
	end init

setup = proc (code, func, mask, win: int)
	end setup

set_s0 = proc (s0: int)
	end set_s0

set_s01 = proc (s0, s1: int)
	end set_s01

set_s01l1 = proc (s0, s1, l1: int)
	end set_s01l1

set_s0123 = proc (s0, s1, s2, s3: int)
	end set_s0123

set_l0 = proc (l0: int)
	end set_l0

set_l01 = proc (l0, l1: int)
	end set_l01

set_l0s23 = proc (l0, s2, s3: int)
	end set_l0s23

set_s45l3 = proc (s4, s5, l3: int)
	end set_s45l3

set_s4567 = proc (s4, s5, s6, s7: int)
	end set_s4567

set_l23 = proc (l2, l3: int)
	end set_l23

set_l2s67 = proc (l2, s6, s7: int)
	end set_l2s67

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
