.PHONY: build-pi clean

build-pi: clean
	cd build && \
	cmake .. -DCMAKE_TOOLCHAIN_FILE=../env_armv6l.cmake && \
	cmake --build .

clean:
	rm -rf build/
	mkdir build
