cc: cc.c
	gcc -std=gnu11 -Werror -Wall cc.c -o cc

tests/%: tests/%.c cc
	./cc $<
	gcc -m32 a.s -o $@

ccself: cc
	./cc cc.s cc.c
	gcc cc.s -o ccself

selfhost: ccself

selftest: ccself tests/triangular.c
	./ccself tests/triangular.s tests/triangular.c
	gcc tests/triangular.s -o tests/triangular; ./tests/triangular 5; [ $$? -eq 15 ]

clean:
	rm -f {cc,ccself,tests/triangular}{,.s} a.s

.PHONY: selfhost selftest clean