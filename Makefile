all: kliqfind

kliqfind:
	gfortran kliqfind.f -o kliqfind

test-sammy1:
	cd tests/sammy1/doskliq_output; \
	( echo "\"../test.cmd\""; echo "\"../test.list\"" ) | ../../../kliqfind

clean-output:
	rm kliqfind.cmd kliqfind.log kliqfind.out kliqfind.plc kliqfind.uci

clean:
	rm kliqfind
