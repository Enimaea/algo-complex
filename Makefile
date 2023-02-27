# Define compiler and compiler flags
OCAMLC = ocamlc
OCAMLFLAGS = -g -w +a-4-9-42-44-45-48

# Define the name of your OCaml source file
SOURCE = Pavage-carres.ml

# Define the name of the output executable
TARGET = Pavage-carres

# Build rule
$(TARGET): $(SOURCE)
	$(OCAMLC) $(OCAMLFLAGS) -o $(TARGET) $(SOURCE)

# Clean rule
clean:
	rm -f $(TARGET) *.cmi *.cmo

# Default rule
all: $(TARGET)

# Run rule
run: $(TARGET)
	./$(TARGET)
