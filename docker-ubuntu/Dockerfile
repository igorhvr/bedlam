FROM ubuntu:18.04
MAINTAINER Igor Hjelmstrom Vinhas Ribeiro <igorhvr@iasylum.net>
# Erase apt-get caches, then run update operation.

ENV TERM xterm
ENV DEBIAN_FRONTEND noninteractive

RUN echo "2019-08-26" > /last_bedlam_base_image_update
RUN cd /var/lib/apt ; rm -rf * 
RUN apt-get -y update --fix-missing
RUN apt-get install -y apt-utils software-properties-common dialog
RUN apt-get install -y git
RUN cd / ; mkdir base ; cd base ; git clone --depth 32 https://github.com/igorhvr/bedlam.git ; chmod -R 755 /base
RUN apt-get install -y zsh
RUN apt-get install -y curl wget
RUN curl -L https://github.com/igorhvr/oh-my-zsh/raw/master/tools/install.sh | /bin/bash
RUN apt-get -y install screen
ADD .screenrc /root/.screenrc
RUN apt-get -y install mosh
RUN apt-get -y install htop
RUN apt-get -y install emacs nano vim w3m
RUN apt-get -y install telnet
RUN apt-get -y install w3m
RUN apt-get -y install openssh-server sudo gnupg
RUN apt-get -y install rsync
RUN apt-get -y install zip
RUN apt-get -y install unzip dtrx
RUN apt-get -y install psmisc ncdu
RUN apt-get -y install less
RUN apt-get -y install ntpdate ntp
RUN apt-get -y install sysstat
RUN apt-get -y install tor netcat-openbsd
RUN apt-get -y install secure-delete

RUN apt-get -y install openjdk-11-jdk

ENV JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64

RUN cd /tmp ; wget https://github.com/ajermakovics/jvm-mon/releases/download/0.3/jvm-mon-0.3.tar.gz ; tar -zxvf jvm-mon-0.3.tar.gz ; cd jvm-mon-0.3 ; rsync -avz -P ./ /usr/local/ ; cd /

RUN echo 'root:root' | chpasswd
RUN sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
ADD id_rsa.pub /root/.ssh/authorized_keys
RUN chmod -R 700 /root/.ssh/

RUN echo '#!/bin/sh\n/etc/init.d/ssh start\n/base/bedlam/schejure/sj\nsleep infinity' > /start-network-services ; chmod 755 /start-network-services
RUN echo '#!/bin/sh\n/base/bedlam/sisc/sisc-1.16.6/sisc $*' > /sisc ; chmod 755 /sisc
ADD data/ /data

ENV LANG C.UTF-8

CMD ["/bin/zsh"]

# ssh
EXPOSE 22 

# Sisc Repl
EXPOSE 3000 

# Beanshell httpd service
EXPOSE 3001 

# Beanshell raw
EXPOSE 3002 

# Clojure nrepl
EXPOSE 6000 

# Clojure tty transport
EXPOSE 6001 
