#lang pollen

◊define-meta[date]{<2023-02-23 Thu 00:37>}
◊define-meta[uuid]{4a6f4716-b355-11ed-8033-5f84d0436494}
◊define-meta[tags]{Programming}
◊define-meta[lang]{en}

◊meta{
  ◊title{Bonjour Crash Course}
}

Bonjour is Apple’s implementation of zeroconf networking. With Bonjour, you can plug in a printer into the local network and expect it to show up on computers in the network, without manually configuring anything. Linux’s implementation is Avahi.

I recently needed to use Bonjour for some project and read some documentation. This is an article summarizing some concepts one needs to know in order to use a Bonjour library. This article assumes some basic network knowledge (◊sc{tpc/ip}, ◊sc{dhcp}, ◊sc{dns}, multicast, unicast, network layers, etc).

Everything in this article is based on Apple’s documentation at ◊link["https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/NetServices/Introduction.html#//apple_ref/doc/uid/TP40002445-SW1"]{◊em{Bonjour Overview}}. (If you want to read it, I recommend starting with the “Bonjour Operations” section.)

Bonjour operates in the link-local network and provides three operations:

◊ol{
  ◊li{Registering services}
  ◊li{Discovering available services}
  ◊li{Resolving a service instance name to an address and port}
}

◊section{Registering a service}

When registering/publishing a service, you (or rather the library) creates a mDNS (multicast ◊sc{dns}) responder with three records: a service (◊sc{srv}) record, a pointer (◊sc{ptr}) record, and a text (◊sc{txt}) record. The text record is for providing additional information and is usually empty.

◊subsection{Service records}

A service record maps the service name to the host name and the port of the service. It uses host name rather than an ◊sc{ip} address so that the service can be on multiple ◊sc{ip} addresses at the same time (eg, on both IPv4 and IPv6).

The full name of a service is made of three parts, the ◊em{service instance name}, the ◊em{service type}, and the ◊em{domain}, in the form of:

◊bcode{<service instance name>.<service type>.<domain>}

The ◊em{service instance name} is a human-readable string showed to end-users, encoded in ◊om{utf-8}, and can be up to ◊om{63} bytes long.

The ◊em{service type} is made of the ◊em{service type} and the ◊em{transport protocol}, in the form of ◊code{_type._protocol.}. Eg, ◊code{_ftp._tcp.}. The underscore prefix is to distinguish from domain names. Bonjour basically uses the format described in ◊link["https://www.ietf.org/rfc/rfc2782.txt"]{◊scom{rfc 2782}}.

Technically, both the type and the protocol are standardized. If you want to add a service type, you need to register it with ◊link["https://www.iana.org/form/ports-services"]{◊sc{iana}}.

The ◊em{domain name} is just like an Internet domain name, eg, ◊code{www.apple.com.}. In addition, there is a pseudo domain, ◊code{local.}, which refers to the link-local network. (So you have Bonjour to thank when you ssh to ◊sc{lan} hosts with ◊code{<host>.local}.)

Service instance name, service type, and domain name together make up the full name of a service instance. For example,

◊bcode{Alice’s Music library._music._tcp.local.}

◊subsection{Pointer records}

A pointer record basically maps service types to full service names. Ie, it maps

◊bcode{<service type>.<domain>}

to

◊bcode{<service instance name>.<service type>.<domain>}

This way you can search for a type of service and get a list of available service instances.

◊subsection{Publishing (advertising)}

When publishing a service, a host will first make sure the intended service instance name is not taken by someone else, by broadcasting request to that service instance name: if there is a response, the name is taken. If someone else has taken it, the host will append a number to the service instance name and increment the number until it gets a name that no one is using.

If you use a library, this part is taken care for you. But it’s good to know how does Bonjour avoid name conflicts.

◊section{Discovering services}

To discover service instances, you first request ◊sc{ptr} records by mDNS, and get back a list of service instance names. …And that’s it. The host will save those names, and resolve a service name into actual address and port every time it needs to use the service.

◊section{Resolving service names}

By the discovery step, we collected some service instance names that are available for us in the local network. The next step is to pick one, resolve it into an actual address and connect to it.

The host will send out a mDNS request for the service instance name, and get back a host name and a port. It then sends out a mDNS request for the host name and get an ◊sc{ip} address. Now it can connect to the address on the port and start using the service.