PROGRAM = Main
OBJS = Main.o ObjRead.o
FLAGS = -O3 
SRCS = Main.hs ObjRead.hs BufTypes.hs Camera.hs State.hs Utils.hs Menu.hs\
			 Config.hs HalfEdge.hs LoopSubdiv.hs

test: Main
	./Main

profile: MainP
	./MainP +RTS -p

Main: $(SRCS)
	ghc Main.hs $(FLAGS)

MainP: $(SRCS)
	ghc Main.hs -o MainP $(FLAGS) -rtsopts -prof -auto-all

clean: 
	-rm Main MainP *.o

# all: build
# 
# clean:
# 	rm -f ${PROGRAM} ${OBJS} ${patsubst %.o,%.hi,${OBJS}}
# 
# build: ${PROGRAM}
# 
# ${PROGRAM}: ${OBJS}
# 	ghc -o ${PROGRAM} ${OBJS}
# 
# %.o: %.hs
# 	ghc -c -o $@ $<
