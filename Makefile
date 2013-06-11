PROGRAM = Main
OBJS = Main.o ObjRead.o

test: Main
	./Main

Main: Main.hs ObjRead.hs BufTypes.hs Camera.hs State.hs Utils.hs Menu.hs Config.hs
	ghc Main.hs

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
