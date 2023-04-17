# Define compiler and compiler flags
OCAMLC = ocamlc
OCAMLFLAGS = -g -w +a-4-9-42-44-45-48

# Define the name of your OCaml source file Pavage-carres-dynamique.ml
SOURCE = Pavage-carres-dynamique.ml

# Define the name of the output executable
TARGET = Pavage-carres-dynamique

# Build rule
$(TARGET): $(SOURCE)
	$(OCAMLC) $(OCAMLFLAGS) -o $(TARGET) unix.cma $(SOURCE)


# Clean rule
clean:
	rm -f $(TARGET) *.cmi *.cmo

# Default rule
all: $(TARGET)

# Run rule
run: $(TARGET)
	./$(TARGET)
