all: kliqfind

kliqfind:
	gfortran kliqfind.f -o kliqfind

clean-output:
	rm kliqfind.cmd kliqfind.log kliqfind.out kliqfind.plc kliqfind.uci

clean:
	rm kliqfind
