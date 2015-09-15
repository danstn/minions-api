FROM ubuntu

RUN apt-get update -y
RUN apt-get install curl -y
RUN apt-get install libpq-dev -y

ADD ./artifacts/minionsApi /opt/

WORKDIR ./opt/

ENV ENV Production
EXPOSE 8081

CMD ./minionsApi
