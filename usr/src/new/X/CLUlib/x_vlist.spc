x_vlist = cluster is create, store, fetch, size, equal

rep = null

create = proc (n: int) returns (cvt) signals (toobig)
	end create

store = proc (list: cvt, i: int, x, y, flags: int) signals (bounds)
	end store

fetch = proc (list: cvt, i: int) returns (int, int, int) signals (bounds)
	end fetch

size = proc (list: cvt) returns (int)
	end size

equal = proc (list1, list2: cvt) returns (bool)
	end equal

end x_vlist
