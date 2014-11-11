### httprecap (work in progress)

#### usage

    Usage: httpre play [options] <pcap_file>

    --ip=<ip>      Destination ip.
    --port=<port>  Destination port.

#### pcap file

    tcpdump -i eno1 -s 65535 dst net 38.74.184.131 -w bid_requests.pcap

#### system tuning

    ulimit -n 64000
    echo 1 > /proc/sys/net/ipv4/tcp_tw_reuse

#### todo

  - connection pool (--connections)
  - verbose mode (--verbose)
  - tests

