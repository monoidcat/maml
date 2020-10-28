NAME := maml
GHCID = cabal exec ghcid -- --allow-eval --command='cabal repl lib:$(NAME)'
SRC := src/ \
		hie.yaml \
		$(NAME).cabal

default: build

build: $(SRC) app/
	@cabal build

hie.yaml:
	@gen-hie > hie.yaml

lib: $(SRC)
	@cabal build lib:$(NAME)

parse: $(SRC) app/
	@cabal run exe:$(NAME) -- parse test/example.maml

ast: $(SRC) app/
	@cabal run exe:$(NAME) -- ast test/example.maml

/app: $(SRC) app/
	@GHCID --setup=':load Main' --command='cabal repl exe:$(NAME)'

/lib: $(SRC)
	@GHCID

/test: $(SRC)
	@GHCID --test=':!runhaskell test/Spec.hs'

clean:
	@cabal clean
	@rm -rf ./dist ./dist-newstyle

.PHONY: default build lib parse ast clean /lib /app /test
