.PHONY: test
test: compiler.ss
	echo \(test-all\) | chez -q compiler.ss

.PHONY: stst
stst: startup.c stst.s
	gcc -m32 -Wall -g3 -ggdb3 -fomit-frame-pointer \
		-fno-asynchronous-unwind-tables \
		-O0 startup.c stst.s -o stst

.PHONY: clean
clean:
	rm -f stst stst.s stst.out startup.s
