CHICKEN_C = csc

MODULES = sexc modules templates utils
OBJ = $(MODULES:%=%.o)

# chicken flags
CFLAGS = -compile-syntax

sexc: main.o $(OBJ)
	$(CHICKEN_C) $^ -o $@

main.o: main.scm
	$(CHICKEN_C) $< -c -o $@

%.o: %.scm
	$(CHICKEN_C) $< -e -c -o $@ $(CFLAGS)

sex-tests: $(OBJ) tests/run.scm
	$(CHICKEN_C) tests/run.scm -c -o sex-tests.o
	$(CHICKEN_C) $(OBJ) test.o -o sex-tests

clean:
	rm -f $(OBJ) sexc sex-tests
