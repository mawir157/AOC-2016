figlet Advent of Code -w 80 -c
figlet 2023 -w 80 -c

if [ -d Haskell ]; then
	figlet Haskell -w 80 -c
	cd Haskell
	./run.sh $1
	cd ..
fi

if [ -d Go ]; then
	figlet Go -w 80 -c
	cd Go
	./run.sh $1
	cd ..
fi

if [ -d C++ ]; then
	figlet C++ -w 80 -c
	cd C++
	./run.sh $1
	cd ..
fi

if [ -d Rust ]; then
	figlet Rust -w 80 -c
	cd Rust
	./run.sh $1
	cd ..
fi
