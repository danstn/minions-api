FROM haskell:7.10

RUN apt-get update -y
RUN apt-get install g++ -y
RUN apt-get install libpq-dev -y
RUN cabal update

# Add .cabal file
ADD ./minionsApi.cabal /opt/minionsApi/minionsApi.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
RUN cd /opt/minionsApi && cabal install --only-dependencies -j4

# Add and Install Application Code
ADD ./src /opt/minionsApi/src
ADD ./run.sh /opt/minionsApi/run.sh
ADD ./Setup.hs /opt/minionsApi/Setup.hs

RUN cd /opt/minionsApi && \
   cabal install && \
   cabal configure && \
   cabal clean &&  \
   cabal build

# Add installed cabal executables to PATH
ENV PATH /root/.cabal/bin:$PATH

# Default Command for Container
WORKDIR /opt/minionsApi
EXPOSE 8081
CMD ./run.sh
