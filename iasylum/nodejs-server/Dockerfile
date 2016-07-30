FROM igorhvr/bedlam

RUN curl -sL https://deb.nodesource.com/setup | bash -
RUN apt-get update
RUN apt-get -y install nodejs
RUN apt-get -y install build-essential

RUN yes | npm install websocket
RUN yes | npm install express
RUN yes | npm install segfault-handler

