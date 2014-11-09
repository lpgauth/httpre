-include_lib("pcapfile/include/pcapfile.hrl").
-include_lib("pkt/include/pkt.hrl").

-define(DEFAULT_DEST_IP, "127.0.0.1").
-define(DEFAULT_DEST_PORT, "80").
-define(DEFAULT_VERBOSE, false).

-define(DOCOPT_OPT,
"Usage: httpre play [options] <pcap_file>

--ip=<ip>      Destination ip.
--port=<port>  Destination port."
).
