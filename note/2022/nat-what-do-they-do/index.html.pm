#lang pollen

◊define-meta[date]{<2022-03-20 Sun 20:26>}
◊define-meta[uuid]{84e3d816-a8c6-11ec-bcbd-af8069bc8361}
◊define-meta[tags]{Programming}
◊define-meta[lang]{en}

◊meta{
  ◊title{NAT traversal: STUN, TURN, ICE, what do they actually do?}
}

When searching for NAT traversal I found all these protocols but no one can tell me what do they essentially do to traverse NAT, surely not by magic, but what? Turns out it’s conceptually very simple.

What NAT traversal does is not really “punching holes” on the NAT, or delivering message through some tunnel, or some arcane magic, but to simply find the public address:port that can can reach ◊em{me}.

If I’m behind a NAT or even multiple NAT’s, what happens is that my packets get relayed by my NAT’s and they appears on the public Internet at the out-most NAT’s address with a port assigned to me. And reply packets going to that address:port are relayed back to me. So essentially I got a public address:port that can reach me on the public Internet. The purpose of NAT traversal is to find that public address:port. That’s basically what the initial/classic STUN (◊link["https://datatracker.ietf.org/doc/html/rfc3489"]{RFC 3489}) does.

The truly host-to-host traversal is just that, finding the public address:port. Sometimes having that address:port is not enough, because NAT poses ◊fnref["extra"]{extra restrictions}. Then we have to resort to having a public-visible relay server in the middle, which is what TURN (◊link["https://datatracker.ietf.org/doc/html/rfc5766"]{RFC 5766}) does. But any host-to-host traversal is just finding public address:port. There is no extra magic there.

How does the classic STUN works is quite simple. You send a STUN server a message, the STUN server looks at the source IP address and port of the IP packet, and reply that back to you. Voilà, you know you public address:port!

That leaves us with ICE, what does it do? Basically, people realize that there are so many situations that are wildly different, so there is no one single method that guarantees to work everywhere. It is best to try a bunch of ways and pick the one that works and works best.

ICE does basically that. It gathers a bunch of address:port’s that possibly works (through STUN) and tries them one-by-one according to some algorithm, and reports to you the best one. If none works, it tries to establish a relay through TURN.

And here is where the new STUN comes in. People threw away the algorithm for finding address:port in classic STUN, and kept and extended the STUN message format. Now ICE runs a more thorough algorithm that uses STUN messages to communicate with STUN servers. And the new STUN (◊link["https://datatracker.ietf.org/doc/html/rfc5389"]{RFC 5389}) just defines the STUN message format. There is a even newer version (◊link["https://datatracker.ietf.org/doc/html/rfc8489"]{RFC 8489}) that updated RFC 5389 slightly, but with no fundamental changes.

Similarly, TURN is updated in◊link["https://datatracker.ietf.org/doc/html/rfc8656"]{RFC 8656} and now is a method used by ICE rather than a standalone solution.

◊fndef["extra"]{
  Some NAT wouldn’t let a packet from an external host through if the host inside never sent a packet to that external host before. There are many ways a NAT could make your life difficult, check out “full cone”, “restricted cone”, “symmetric NAT”, etc.
}
