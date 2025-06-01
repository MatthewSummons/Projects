FROM handsonsecurity/seed-server:bind

COPY named.conf  zone_attacker32.com  zone_example.com  /etc/bind/

CMD service named start && tail -f /dev/null
