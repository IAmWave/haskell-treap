all: test benchmark

benchmark: build/Benchmark
	build/Benchmark --output build/report.html

build/Benchmark: build/ src/Benchmark.hs src/Treap.hs
	cd src && ghc -O --make -odir ../build -hidir ../build -o ../build/Benchmark Benchmark.hs

test: build/TreapTest
	build/TreapTest

build/TreapTest: build/ src/TreapTest.hs src/Treap.hs
	cd src && ghc -O --make -odir ../build -hidir ../build -o ../build/TreapTest TreapTest.hs

build/:
	mkdir -p build
