all: doskliq

doskliq:
	gfortran doskliq.f -o doskliq

test-sammy1:
	cd ../tests/sammy1/doskliq_output; \
	( echo "\"../test.cmd\""; echo "\"../test.list\"" ) | ../../../src/doskliq

clean-output:
	rm kliqfind.cmd kliqfind.log kliqfind.out kliqfind.plc kliqfind.uci

clean:
	rm doskliq
