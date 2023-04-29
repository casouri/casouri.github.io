#lang pollen

◊define-meta[date]{<2023-02-22 Wed 12:49>}
◊define-meta[uuid]{72d54088-b2f2-11ed-a42d-df77846b42d9}
◊define-meta[tags]{Programming}
◊define-meta[lang]{en}

◊meta{
  ◊title{Secure LAN Clipboard Sharing}
}

I recently upgraded my ◊sc{pc} and started to use it regularly again. When setting it up, I run into times when it would be nice to be able to copy something from my laptop to the ◊sc{pc}. That prompted me to search for a ◊sc{lan} clipboard sharing program.

I did find a nice one on GitHub written by ascclemens, called ◊fnref["lan_clipboard"]{lan_clipboard}. It consists of a server and a client, where the server synchronizes the clipboard between multiple clients. Better yet, it uses ◊sc{tls} and certificates to secure the communication between server and clients.

I could’ve figure out how to generate a set of certificates that it accepts and just use it, but decided to write one myself, because reasons. And I ended up with ccbc (Clueless ClipBoard Cluster).

◊section{P2P}

I didn’t like having a designated server, so I made ccbc ◊scom{p2p}, where every node broadcasts changes in its clipboard to every other node. Because by the nature of clipboard sharing, there is only one active user moving between hosts, so at any short span of time there should be only one node having changes in its clipboard. And there should only be a handful of nodes. So broadcasting makes perfect sense.

There is also absolutely no synchronization or consensus. Nodes just broadcast their clipboard content when there is a change. They don’t try to agree on what should be in the clipboard and doesn’t try to make every host to have the same content in their clipboard.

ccbc also uses ◊sc{tls}. Even better, it can generate certificates for you, so you don’t need to figure out how to cast the right openssl spell. And it can fire up a ◊sc{http} server that gives out certificates to anyone connected. This step is completely insecure, but we are in ◊sc{lan}, so I’m not really worried about it.

I want ccbc to be able to recover from disconnections, and auto-detect new peers in the cluster. So here is how it keeps track of peers: Every node has an unique ◊em{name} (not difficult to manager for a handful of devices owned by the same person). And every node has a ◊sc{peer list}, which is a map from unique names of each node to its current address (and port).

When a node Alice joins a cluster, it needs to know at least one peer’s address, let’s call it Bob. Alice will connect to Bob and request for its peer list. Bob will send Alice its peer list and add Alice to its peer list. Alice will then connect to every other peer in the peer list. Peers will send their peer list to each other periodically so under a simple network topology, everyone should get to know each other eventually.

When a peer lost connection to another peer, it will keep trying to connect to it. When a peer disconnects and rejoins the network under another address, the new address will override the old one in other peers’ peer list. And naturally the other peers will stop trying to connect to the old address. This is why we require unique names and don’t use the address to identify a peer.

◊section{Copying files}

The library I used to get the content of system clipboard already supports both text and images, but wouldn’t it be cool if ccbc could copy files, too? It isn’t hard, either. On Linux (◊scom{X11}), if you copy a file and pull the clipboard content as text (which is what the library does), you’ll get the paths of those files. I could modify the clipboard library to identify files and pull them as files, but really, file paths are good enough, because ccbc can easily tell whether the text it pulled from the clipboard is ordinary text or a list of file paths: it just interpret the content as paths and check if they exists.

If the clipboard content are file paths, ccbc will broadcast them as files to its peers. And if a Peer wants to paste those files, it downloads those files from the peer that owns those files. For directories I cheated: I pack them into tars, transfer them, and unpacks the tar. I don’t expect to use this feature on huge files anyway, there’s always rsync.

Ideally you want to invoke a command which pastes the files you just copied on another machine to the current directory. So I added a subcommand to the ccbc command which creates a client and connects to the local ccbc daemon and instructs it to download the files from the remote peer. Better yet, the local daemon will stream the download progress to the client.

That’s about it, check it out here: ◊link["https://sr.ht/~casouri/ccbc/"]{ccbc}.

◊fndef["lan_clipboard"]{Sadly it is now deleted from GitHub. I found a Wayback Machine snapshot here: ◊link["https://web.archive.org/web/20180627103011/https://github.com/jkcclemens/lan_clipboard"]{lan_clipboard}}.