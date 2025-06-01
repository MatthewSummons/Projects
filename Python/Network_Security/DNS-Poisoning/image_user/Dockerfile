FROM handsonsecurity/seed-ubuntu:large

COPY resolv.conf /etc/resolv.conf.override

COPY start.sh /
RUN  chmod +x /start.sh 

CMD [ "/start.sh"]


