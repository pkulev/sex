CHICKEN_C = csc

MODULES = sexc main modules templates utils
OBJ = $(MODULES:%=%.o)

# chicken flags
CFLAGS = -compile-syntax

sexc: $(OBJ)
	$(CHICKEN_C) $^ -o $@

main.o: main.scm
	$(CHICKEN_C) $< -c -o $@

%.o: %.scm
	$(CHICKEN_C) $< -e -c -o $@ $(CFLAGS)

clean:
	rm -f $(OBJ) sexc
