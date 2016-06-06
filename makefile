build: FORCE
	stack build

install: FORCE
	stack install

pedantic: FORCE
	stack clean
	stack build \
    --fast \
    --jobs=8 \
    --ghc-options="\
        -fforce-recomp \
        -O0 \
        -Wall \
        -fwarn-unused-imports \
        -fwarn-incomplete-patterns \
        -fwarn-unused-do-bind \
				-fno-warn-type-defaults \
        -fno-warn-name-shadowing \
        -fno-warn-overlapping-patterns \
        -fno-warn-orphans"

test: FORCE
	stack test

love:
	@echo "not war"

FORCE:


