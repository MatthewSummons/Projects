FROM handsonsecurity/seed-ubuntu:large-multi

RUN useradd -g sudo -d /home/user -m user
RUN echo 'user:user' | chpasswd && head /dev/urandom | tr -dc A-Za-z0-9 | head -c 16 > /home/user/flag && echo "" >> /home/user/flag 