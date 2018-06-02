all: benchmark

benchmark: build/Benchmark
	build/Benchmark --output build/report.html

build/Benchmark: build/ src/Benchmark.hs
	cd src && ghc -O --make -odir ../build -hidir ../build -o ../build/Benchmark Benchmark.hs

build/:
	mkdir -p build
