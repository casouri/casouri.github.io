#lang pollen

◊define-meta[date]{<2025-07-21 Mon 23:23>}
◊define-meta[uuid]{bd59dc72-adb1-11ed-ba6d-ff815e049953}
◊define-meta[tags]{Misc}
◊define-meta[lang]{en}

◊meta{
  ◊title{“This Information is Not Suitable for Use in Court”}
}

I learned from an ◊fnref["article"]{article} ◊fnref["today"]{today} that US Navy Observatory has a website that tells you the time of sunrise and sunset, etc, but under the disclaimer that this information is not suitable for use in court. It’s kind of interesting; basically, legal matters require credibility and accountability.

◊fndef["article"]{◊link["https://ansuz.sooke.bc.ca/entry/23"]{◊em{What Colour are your bits?}} Talks about intellectual property and its difficulty in the digital world.}

◊fndef["today"]{I wrote the original draft in <2023-02-15 Wed 20:24>, so “today” means that date.}

These two things, in general, means a real person is behind and it is their credibility as a person that the credibility of the information is based on. Their credibility in turn comes from other people: their colleagues, their teacher, their license which are endorsed by people or institutions made by human. The point is, in a society of human, credibility ultimately comes from other humans.

This observation applies in so many more places. Take the Internet for an example, the international ◊fnref["pki"]{public key infrastructure} protects the authenticity of web pages served though ◊sc{https}: every browser vendor ships their browser with a set of root certificates that are trusted. Then, when a web server shows you their certificate, and the certificated can be traced back to one of the trusted root certificates, you know it’s legit. But how does the web browser vendors know what root certificates to include? “Every trust chain ends in meatspace”—said in the previously linked article. The vendors attend committees and conferences, there they meet people and get to know them, and these people figure out what root certificates goes into the browsers.

Not only browsers, the whole internet is build upon connections between human. The internet is made of numerous interconnected networks (◊link["https://en.wikipedia.org/wiki/Autonomous_system_(Internet)"]{◊sc{as}}) managed by different entities. Your internet provider manages the network your house is in, and they have a connection to Google’s network. In very simplistic term, to participate in the internet, you only need to know someone that has connections to other ◊sc{as}’s, get yourself some ◊sc{ip} addresses, connect your router to their router, and start advertising your ◊sc{ip} addresses. Other routers on the internet will learn your address thought their connections to other routers, and they will send packets addressed to your address to your router.

◊fndef["pki"]{◊link["https://smallstep.com/blog/everything-pki/"]{◊em{Everything you should know about certificates and PKI but are too afraid to ask}}, if you are a programmer and want to know what’s going on under the hood.}

It is entirely possible for someone on the internet to go rogue and start advertising other people’s address and ◊fnref["bgp"]{routers on the internet will just take it}, and that has actually happend. The whole system depends on one ◊sc{as} trusting its neighbors and its neighbor trusting their neighbors, again, through trust formed in meatspace.

◊fndef["bgp"]{There is talk about “secure ◊sc{bgp}”, but it’s far from becoming reality.}

The point is, I guess, look pass the machine. The machine, or whatever technology, doesn’t have agency by itself, it is always created and controlled by human. You are not trusting your browser or its root certificates, you are trusting Mozilla, Google and Apple; you are trusting the human at Mozilla, Google, and Apple to do their job and have good intentions. Really, human is the only actor in this society, military are made of human, lawmakers are human, lawenforcement are made of human, so if everyone in a society think something needs to change, it will change. If all the people think someone is innocent or guilty but shouldn’t be punished, they can just walk; if all the people think a law is stupid, it’ll be changed. It’s that easy. Things that seems unchangeable can be easily changed if enough people’s mind are swayed.

Maybe a tangent but often we hear about this technologism that says if we just keep advancing technology we’ll solve this or that problem. If we just build more homes we’ll make housing affordable. Or the reverse where people says this or that technology is bad and is ruining our lifes. But a lot of time the problem is not the technology, a lot of time it’s a societal problem.

Actually, I don’t know what I’m trying to say because these are just some random thoughts emerged from the two linked articles. But at least I shared some cool facts about internet ◊smile{}

◊; If we go a bit further, it seems that nothing really matters without human. Everything in our society revolves around human, and whatever we interact with, we are ultimately interacting with other human.

◊; If all human on Earth except you disappears tomorrow, and you have ample resources to live, is such life worth living? If in addition you gained knowledge of the ultimate questions, but have no one to share it to, does it matter anymore (beyond the brief enjoyment of acquiring that knowledge)? If human developed some awesome technology, then proceeds to disappear (and there is no alien to come discover it), does this awesome technology matter anymore?

◊; My current stance is that it wouldn’t matter anymore.
