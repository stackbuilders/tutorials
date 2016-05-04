watch: build
	stack build --exec 'tutorials watch'

build:
	stack build --exec 'tutorials build'

rebuild:
	stack build --exec 'tutorials rebuild'
