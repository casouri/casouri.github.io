#lang pollen

◊define-meta[date]{<2022-03-20 Sun 20:26>}
◊define-meta[uuid]{84e3d816-a8c6-11ec-bcbd-af8069bc8361}
◊define-meta[tags]{Programming}
◊define-meta[lang]{en}

◊meta{
  ◊title{NAT traversal: STUN, TURN, ICE, what do they actually do?}
}

When searching for ◊sc{nat} traversal I found all these protocols but no one can tell me what do they essentially do to traverse ◊sc{nat}, surely not by magic, but what? Turns out it’s conceptually very simple.

What ◊sc{nat} traversal does is not really “punching holes” on the ◊sc{nat}, or delivering message through some tunnel, or some arcane magic, but to simply find the public address:port that can can reach ◊em{me}.

If I’m behind a ◊sc{nat} or even multiple ◊sc{nat}’s, my packets are relayed by these ◊sc{nat}’s and they appears on the public Internet at the out-most ◊sc{nat}’s address and port. And reply packets going to that address:port are relayed back to me. So essentially I got a public address:port that can reach me on the public Internet. The purpose of ◊sc{nat} traversal is to find that public address:port. That’s basically what the initial/classic ◊sc{stun} (◊link["https://datatracker.ietf.org/doc/html/rfc3489"]{◊scom{rfc 3489}}) does.

The truly host-to-host traversal is just that, finding the public address:port.
That’s what classic ◊sc{stun} does. You send a ◊sc{stun} server a message, the ◊sc{stun} server looks at the source ◊sc{ip} address and port of the ◊sc{ip} packet, and reply that back to you. Voilà, you know you public ◊code{address:port}!

Sometimes having that address:port is not enough, because ◊sc{nat} poses ◊fnref["extra"]{extra restrictions}. Then we have to resort to having a public-visible relay server in the middle, which is what ◊sc{turn} (◊link["https://datatracker.ietf.org/doc/html/rfc5766"]{◊scom{rfc 5766}}) does. But any host-to-host traversal is just finding public address:port. There is no extra magic there.

◊fndef["extra"]{
  Some ◊sc{nat} wouldn’t let a packet from an external host through if the host inside never sent a packet to that external host before. There are many ways a ◊sc{nat} could make your life difficult, check out “full cone”, “restricted cone”, “symmetric ◊sc{nat}”, etc.
}

But ◊sc{stun}nd ◊sc{turn} still isn’t enough. For one, one can usually find multiple address that could possibly work, but then which one to use? Eg, maybe a host has an ◊sc{ip} assigned by a ◊sc{vpn}, if the other host is also in the ◊sc{vpn}, we should use this ◊sc{ip} over the others; similarly, if the other host is in the same ◊sc{lan}, we should use the local ◊sc{ip}; even over ◊sc{nat}, there could be multiple ◊sc{ip}’s that can reach us.

◊sc{ice} fills that gap. It gathers a bunch of ◊code{address:port}’s that possibly works (through ◊sc{stun} messages with stun servers), sorts them by preference, and tries them one-by-one according to some algorithm, and reports to you the best one. If none works, it tries to establish a relay through ◊sc{turn}.

And here is where the new ◊sc{stun} comes in. People threw away the algorithm for finding ◊code{address:port} in classic ◊sc{stun}, and kept and extended the ◊sc{stun} message format. Now ◊sc{ice} runs a more thorough algorithm that uses ◊sc{stun} messages to communicate with ◊sc{stun} servers. And the new ◊sc{stun} (◊link["https://datatracker.ietf.org/doc/html/rfc5389"]{◊scom{rfc 5389}}) just defines the ◊sc{stun} message format. There is a even newer version (◊link["https://datatracker.ietf.org/doc/html/rfc8489"]{◊scom{rfc 8489}}) that updated ◊scom{rfc 5389} slightly, but with no fundamental changes.

Similarly, ◊sc{turn} is updated in ◊link["https://datatracker.ietf.org/doc/html/rfc8656"]{◊scom{rfc 8656}} and now is a method used by ◊sc{ice} rather than a standalone solution.
