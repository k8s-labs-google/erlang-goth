# github codespaces currently only supports linux containers 8/31/2020

FROM erlang:22-alpine

ARG USER=sr-erlang
ARG USER_UID=1000
ARG USER_GID=$USER_UID

ENV HOME /home/$USER

RUN apk update && \
    apk add sudo \
    bash

RUN useradd -s /bin/bash --uid $USER_UID --gid $USER_GID -m $USERNAME

# RUN adduser -D --home /home/$USER --shell /bin/bash --uid 1000 $USER && \
#     echo "$USER ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/$USER && \
#     chmod 0440 /etc/sudoers.d/$USER

# [Optional] Add sudo support
# RUN apt-get install -y sudo \
#     && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME && \
#     chmod 0440 /etc/sudoers.d/$USERNAME

USER $USER

COPY start.sh /start.sh

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

ENTRYPOINT ["/start.sh"]
