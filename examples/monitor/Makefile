TARGET=monitor_meta.out

.PHONY: all lib script run clean

all: lib

script:
	ocamlfind -toolchain metaocaml ocamlc -o $(TARGET) -I ../../lib -linkpkg \
	-package str,archimedes \
	stream.cma util.ml  monitor.ml && \
	./$(TARGET)

run: main.ml
	echo 'let _ = Fl_dynload.load_packages ["archimedes"] in' > lead.ml && \
	cat lead.ml main.ml > main2.ml && \
	ocamlfind ocamlopt -o main.out -linkpkg -ccopt -L"/opt/X11/lib" \
	-package findlib.dynload,str,archimedes \
	util.ml main2.ml && \
	rm -f main.ml main2.ml lead.ml && \
	./main.out

lib:
	ocamlfind -toolchain metaocaml ocamlc -a -o $(TARGET:.out=.cma) -I ../../lib -linkpkg \
	-package archimedes \
	stream.cma util.ml monitor.ml

clean:
	rm -f *.cm[ixoa] *.[oa] *.out