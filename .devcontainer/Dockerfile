# github codespaces currently only supports linux containers 8/31/2020

ARG OTP_VERSION=22
FROM erlang:${OTP_VERSION}

# https://github.com/microsoft/vscode-remote-release/issues/22
# add user
ARG USER=sr-erlang
RUN useradd -m $USER
ENV HOME /home/$USER
# env vars
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

# install packages
RUN apt-get update && \
    apt-get install -y curl

# [Optional] Add sudo support
# RUN apt-get install -y sudo \
#     && echo $USER ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USER && \
#     chmod 0440 /etc/sudoers.d/$USER

USER $USER

COPY start.sh /start.sh

ENTRYPOINT ["/start.sh"]
