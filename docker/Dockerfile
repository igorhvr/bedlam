FROM java:openjdk-8
MAINTAINER Igor Hjelmstrom Vinhas Ribeiro <igorhvr@iasylum.net>
RUN yes y | apt-get install git zsh
RUN curl -L https://github.com/igorhvr/oh-my-zsh/raw/master/tools/install.sh | /bin/bash
RUN yes y | apt-get install screen mosh htop
RUN cd / ; mkdir base ; cd base ; git clone https://github.com/igorhvr/bedlam.git ; chmod -R 755 /base
CMD ["bash"]
