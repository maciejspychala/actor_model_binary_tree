all:
	erlc tree.erl
	erl -noshell -s tree start -s init stop
