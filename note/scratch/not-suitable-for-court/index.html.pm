#lang pollen

◊define-meta[date]{<2023-02-15 Wed 20:24>}
◊define-meta[uuid]{bd59dc72-adb1-11ed-ba6d-ff815e049953}
◊define-meta[tags]{Misc}
◊define-meta[lang]{en}

◊meta{
  ◊title{“This Information is Not Suitable for Use in Court”}
}

I learned from an ◊fnref["article"]{article} today that US Navy Observatory has a website that tells you the time of sunrise and sunset, etc, but under the disclaimer that this information is not suitable for use in court. It’s interesting but not hard to understand: we all know that legal matters require credibility and accountability.

These two things, in general, means a real person is behind and it is their credibility as a person that the credibility of the information is based on. Their credibility in turn comes from other people: their colleagues, their teacher, their license which are endorsed by people or institutions made by human. The point is, in a society of human, credibility ultimately comes from other humans.

This observation applies to everywhere. Take the Internet for an example, the international ◊fnref["pki"]{public key infrastructure} protects the authenticity of web pages served though ◊sc{https}: every browser vendor ships their browser with a set of root certificates that are trusted. Then, when a web server shows you their certificate, and the certificated can be traced back to one of the trusted root certificates, you know it’s legit. But how does the web browser vendors know what root certificates to include? “Every trust chain ends in meatspace.” The vendors attend committees and conferences, there they meet people and get to know them, and these people figure out what root certificates goes into the browsers.

Not only browsers, the whole internet is build upon connections between human. The internet is made of numerous interconnected networks (◊link["https://en.wikipedia.org/wiki/Autonomous_system_(Internet)"]{◊sc{as}}) managed by different entities. Your internet provider manages the network your house is in, and they have a connection to Google’s network. In very simplistic term, to participate in the internet, you only need to know someone that has connections to other ◊sc{as}’s, get yourself some ◊sc{ip} addresses, connect your router to their router, and start advertising your ◊sc{ip} addresses. Other routers on the internet will learn your address thought their connections to other routers, and they will send packets addressed to your address to your router.

It is entirely possible for someone on the internet to go rogue and start advertising other people’s address and ◊fnref["bgp"]{routers on the internet will just take it}. The whole system depends on one ◊sc{as} trusting its neighbors and its neighbor trusting their neighbors, again, through trust formed in meatspace.

The point is, I guess, “don’t be fooled by the machine”. The machine, or technology, doesn’t have initiative by itself, it is always controlled, directly or indirectly, by human. You are not trusting your browser or its root certificates, you are trusting Mozilla, Google and Apple; you are trusting the human at Mozilla, Google, and Apple to do their job and have good intentions.

If we go a bit further, it seems that nothing really matters without human. Everything in our society revolves around human, and whatever we interact with, we are ultimately interacting with other human.

If all human on Earth except you disappears tomorrow, and you have ample resources to live, is such life worth living? If in addition you gained knowledge of the ultimate questions, but have no one to share it to, does it matter anymore (beyond the brief enjoyment of acquiring that knowledge)? If human developed some awesome technology, then proceeds to disappear (and there is no alien to come discover it), does this awesome technology matter anymore?

My current stance is that it wouldn’t matter anymore.

◊; A technology should be evaluated by how would affect human, rather than its properties on paper. Modern car is an amazing technology, but its core value is that they can transport ◊em{human} at certain speed, plus the nice benefit of comfort, safety, enjoyment, etc.

◊; Skip the following section if you don’t want to read yet another person’s “insight” on ChatGPT.

◊; Now, this ChatGPT thing. It surely isn’t sentient. Even if you believe true sentience can be achieved by (sophisticated/well enough) imitation, it is far from it. It also makes factual mistakes. But it is amazingly good at imitating human. I’ll give it a passing grade on Turning test at this point, and it’s easy to imagine it ◊fnref["turning"]{absolutely passing the Turning test not to far in the future}. After all, it can already imitate emotion, imagination, opinion, etc.

◊; So instead of thinking whether ◊sc{ai} will replace human’s job, or using ChatGPT as search engines, it is more interesting to me that we now have the technology to imitate human.

◊; Firstly, ChatGPT can produce very valuable things, if the criterion of value is “how well it imitates human”.

◊; Secondly, being able to imitate human means other human could consider it a human. Human are magical creatures, when enough of them believe in imaginary things, those things became reality. Human believe in nations and money, and they are darn sure real.

◊; Finally, I’ll note that personally, when I interact with something that feels like human to me, I’m starting to treat it as a human: I treat it nicely like I would to other human, and saying bad things to it would make me feel bad. I can’t control it, that’s just how the primal part of my brain works.


◊fndef["article"]{◊link["https://ansuz.sooke.bc.ca/entry/23"]{◊em{What Colour are your bits?}} Talks about intellectual property and its difficulty in the digital world.}

◊fndef["pki"]{◊link["https://smallstep.com/blog/everything-pki/"]{◊em{Everything you should know about certificates and PKI but are too afraid to ask}}, if you are a programmer and want to know what’s going on under the hood.}

◊fndef["bgp"]{There is talk about “secure ◊sc{bgp}”, but it’s far from becoming reality.}

◊; ◊fndef["turning"]{If you limit the topics, eg, excluding math. But one can argue that a sufficiently dumb person is fully capable of messing up simple arithmetics}
