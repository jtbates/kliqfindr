all: kliqfind

kliqfind:
	gfortran kliqfind.f -o kliqfind

clean:
	rm kliqfind
