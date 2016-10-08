# http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk \
	'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

FORCE:

lint:
	-hlint app src

watch: ## build
	stack build --file-watch 

build:
	stack build --ghc-options -threaded --ghc-options -eventlog --ghc-options -rtsopts
	#stack build --ghc-options -threaded --ghc-options -rtsopts

run-profile-mem:
	stack exec hcell-exe -- +RTS -ls -N7
	threadscope hcell-exe.eventlog

run-profile:
	# stack build --executable-profiling --library-profiling 
	stack exec hcell-exe -- +RTS
	#hp2ps -e8in -c hcell-exe.hp
run:
	stack exec hcell-exe -- +RTS -N7

test: ## test
	echo test

clean: ## clean all the things
	-sh clean.sh

work: ## open all files in editor
	emacs -nw app/*.hs src/*.hs Makefile *.cabal

setup:
	touch battle-plan.org
	mkdir -p design

add: clean ## add files to the git repo
	git add -A :/

commit: ## git commit -a
	git commit -a


