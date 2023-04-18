# Define compiler and compiler flags
# Define compiler and compiler flags
OCAMLC = ocamlc
OCAMLFLAGS = -g -w +a-4-9-42-44-45-48

# Define source files and targets
SOURCES = demo1.ml demo2.ml demo3.ml demo4.ml demo5.ml demo6.ml
TARGETS = $(SOURCES:.ml=)

# Build rule
%: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ unix.cma $<

# Build all demos
all: $(TARGETS)

# Clean rule
clean:
	rm -f $(TARGETS) *.cmi *.cmo

# Run rule
run: all
	@for target in $(TARGETS); do \
		echo "Running $$target"; \
		./$$target; \
	done

.PHONY: all clean run
