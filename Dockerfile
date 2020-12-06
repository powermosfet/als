FROM archlinux:latest
RUN mkdir -p /opt/als/
ARG BINARY_PATH
WORKDIR /opt/als
RUN pacman -Syu --noconfirm && pacman -S --noconfirm \
  ca-certificates
COPY "$BINARY_PATH" /opt/als

ENV PORT=8080

CMD ["/opt/als/als"]
