build: FORCE
	stack build

install: FORCE
	stack install

pedantic: FORCE
	stack clean
	stack build \
    --pedantic \
    --fast \
    --jobs=8 \
    --ghc-options="\
        -fforce-recomp \
        -O0 \
        -Wall \
        -Werror \
        -fwarn-unused-imports \
        -fwarn-incomplete-patterns \
        -fwarn-unused-do-bind \
        -fno-warn-name-shadowing \
        -fno-warn-overlapping-patterns \
        -fno-warn-orphans" \
    --test \
    --test-arguments="\
      --dry-run \
      "\

love:
	@echo "not war"

FORCE:


