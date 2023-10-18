.PHONY: build build-pi clean

build: clean
	cd build && \
	cmake .. -DCMAKE_TOOLCHAIN_FILE=../env_intelmac.cmake && \
	cmake --build . -j4

clean:
	rm -rf build/
	mkdir build
