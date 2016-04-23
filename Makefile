COOKIE = middleware
NAME = node$$$$@127.0.0.1

SRC_PATH = ./src/
BIN_PATH = ./bin/

all: compile

clean:
	rm $(BIN_PATH)*.beam

compile:
	mkdir -p $(BIN_PATH)
	erlc -o $(BIN_PATH) $(SRC_PATH)*.erl

agent: all
	cd $(BIN_PATH) && \
	erl -noshell -name $(NAME) -setcookie $(COOKIE) \
	    -run agent -run init stop  &

monitor: all
	cd $(BIN_PATH) && \
	erl -name monitor$$$$ -setcookie $(COOKIE) \
	    -run monitor

