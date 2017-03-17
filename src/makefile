

example.pdf: ./Paper/Fourier.hs
	stack ghc -- $< -e "writeExample"


all: Backend/InductiveGraph/*.hs makefile
	stack ghc -- ./Paper/Proto2.hs -e "writePdfs"
