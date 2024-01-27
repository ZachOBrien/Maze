FROM racket/racket:8.6-full

RUN mkdir Maze/

COPY . Maze/

WORKDIR Maze/

RUN raco make xtest

CMD ["./xtest"]
