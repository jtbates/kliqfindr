all: doskliq kliqfindr

kliqfindr:
	gfortran -c kliqfindr.f
	gfortran -shared -o kliqfindr.so kliqfindr.o

doskliq:
	gfortran doskliq.f -o doskliq

test-sammy1:
	cd ../tests/sammy1/doskliq_output; \
	( echo "\"../test.cmd\""; echo "\"../test.list\"" ) | ../../../src/doskliq

clean-output:
	rm kliqfind.cmd kliqfind.log kliqfind.out kliqfind.plc kliqfind.uci

clean:
	rm doskliq
	rm kliqfindr.o kliqfindr.so
