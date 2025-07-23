FROM debian:latest
MAINTAINER Igor Hjelmstrom Vinhas Ribeiro <igorhvr@iasylum.net>

ENV TERM xterm
ENV DEBIAN_FRONTEND noninteractive

RUN echo "2025-07-17" > /last_bedlam_base_image_update
RUN cd /var/lib/apt ; rm -rf * 
RUN rm -rf /var/lib/apt/lists/*
RUN apt-get clean
RUN apt-get -y update --fix-missing
RUN apt-get -y install man-db
RUN apt-get install -y apt-utils software-properties-common dialog
RUN apt-get install -y inetutils-ping net-tools iproute2 iptables
RUN apt-get install -y git

# Set timezone.
ENV TZ=Etc/UTC
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

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
RUN apt-get -y install openssh-server sudo gnupg
RUN apt-get -y install rsync
RUN apt-get -y install zip unzip gzip tar
RUN apt-get -y install psmisc ncdu procps
RUN apt-get -y install less
RUN apt-get -y install ntpdate ntp
RUN apt-get -y install sysstat
RUN apt-get -y install tor netcat-openbsd ncat socat tcpdump traceroute dnsutils
RUN apt-get -y install secure-delete
RUN apt-get -y install strace lsof
RUN apt-get -y install make

RUN apt-get -y install man-db tar gzip ca-certificates tzdata

WORKDIR /opt
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
    && curl \
        -L \
        -o openjdk.tar.gz \
        https://download.java.net/java/GA/jdk11/13/GPL/openjdk-11.0.1_linux-x64_bin.tar.gz \
    && mkdir jdk \
    && tar zxf openjdk.tar.gz -C jdk --strip-components=1 \
    && rm -rf openjdk.tar.gz \
    && ln -sf /opt/jdk/bin/* /usr/local/bin/ \
    && java  --version \
    && javac --version \
&& jlink --version
WORKDIR /

ENV JAVA_HOME /opt/jdk

RUN cd /tmp ; wget https://github.com/ajermakovics/jvm-mon/releases/download/0.3/jvm-mon-0.3.tar.gz ; tar -zxvf jvm-mon-0.3.tar.gz ; cd jvm-mon-0.3 ; rsync -avz -P ./ /usr/local/ ; cd /

RUN mkdir -m 0755 /nix && groupadd -r nixbld && chown root /nix && for n in $(seq 1 10); do useradd -c "Nix build user $n" -d /var/empty -g nixbld -G nixbld -M -N -r -s "$(command -v nologin)" "nixbld$n"; done
ENV USER=root
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN set -o pipefail && curl -L https://nixos.org/nix/install | bash
RUN . "$HOME/.nix-profile/etc/profile.d/nix.sh" && nix-env --timeout 60 --connect-timeout 60 -i dtrx uv
RUN echo "" >> /root/.bashrc ; echo 'if [ -e /root/.nix-profile/etc/profile.d/nix.sh ]; then . /root/.nix-profile/etc/profile.d/nix.sh; fi' >> /root/.bashrc

RUN curl -fsSL https://deb.nodesource.com/setup_24.x | sudo -E bash - && apt-get install -y nodejs

RUN echo 'root:root' | chpasswd
RUN sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
ADD id_rsa.pub /root/.ssh/authorized_keys
RUN chmod -R 700 /root/.ssh/

RUN useradd -m toor && \
    echo "toor:root" | chpasswd && \
    adduser toor sudo
ADD id_rsa.pub /home/toor/.ssh/authorized_keys
RUN chmod -R 700 /home/toor/.ssh/ ; chown -R toor:toor /home/toor/
RUN echo 'toor ALL=(ALL:ALL) NOPASSWD:ALL' | sudo EDITOR='tee -a' visudo

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
