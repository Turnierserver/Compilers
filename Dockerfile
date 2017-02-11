FROM hseeberger/scala-sbt

ADD . /compiler
WORKDIR /compiler

RUN sbt compile
CMD sbt run
