FROM handsonsecurity/seed-server:bind

COPY named.conf           /etc/bind/
COPY named.conf.options   /etc/bind/

CMD service named start && tail -f /dev/null 
