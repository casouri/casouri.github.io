#lang pollen

◊define-meta[date]{<2024-07-20 Sat>}
◊define-meta[uuid]{9b2c35da-4583-11ef-a0d5-53181c23211b}
◊define-meta[tags]{Programming}
◊define-meta[lang]{en}

◊meta{
  ◊title{Peer-to-peer Connection with WebRTC in Rust Using webrtc-rs}
}

This is a guide for using ◊link["https://webrtc.rs"]{webrtc-rs} to create ◊om{p2p} connections that can go through ◊sc{nat} in Rust. This should be useful for anyone that wants to create a ◊om{p2p}/distributed program. I assume the reader knows about ◊fnref["stun"]{◊sc{stun}, ◊sc{ice}}, websocket. I’ll brefly explain how WebRTC works. Reader also needs to know ◊sc{pem}, ◊sc{der}, X.509, and ◊sc{pki} in general, for the security side of things.

I’m not an expert on WebRTC, just some dude that needs ◊om{p2p} and spent some time figuring out how to use webrt-rs for it; so if you spot some mistake, please do correct me!

◊fndef["stun"]{Yuo can refer to this post: ◊link["../2022/nat-what-do-they-do/index.html"]{◊em{NAT traversal: STUN, TURN, ICE, what do they actually do?}}}

◊section{Overall structure}

There are several parts in the system. First there is a bunch of ◊om{p2p} programs that want to connect to each other, let’s call them peers. Then there needs to be a public-facing server that every peer can connect to. You’ll need to write this server and host it yourself. When peer A and B wants to connect to each other, they both send a request to the public server (let’s call it S); S will relay information between A and B, until A and B successfully establish a ◊fnref["udp"]{◊sc{udp} “connection”}. Finally, you need some ◊sc{stun} servers and maybe even ◊sc{turn} servers. There are plenty of free public ◊sc{stun} servers (Google, Cloudflare, etc hosts a bunch of them). On the other hand, free public ◊sc{turn} server is basically unheard of, since they’re so easy to abuse.

◊fndef["udp"]{Most of the time people use ◊sc{udp} for ◊sc{nat} traversal, it’s rare to see ◊sc{tcp} connections: it’s more difficult to establish through ◊sc{nat}, and only used when the firewall blocks ◊sc{udp}.}


◊section{WebRTC}

The main purpose of WebRTC is for video conferencing and VoIP on browsers; transfering arbitrary data requires much less hassle. So we really don’t need most of the WebRTC that deals with video codec and media channels. On top of that, WebRTC isn’t really a single protocol, but rather a bunch of revived protocols plus a spec defining how to use these protocols together. The Rust crate, webrtc-rs, implements each underlining protocol (◊sc{sctp}, ◊sc{dtls}, ◊sc{stun}, ◊sc{ice}, ...) in separate crates, plus a webrtc glue layer. So it’s possible to only use the underlining crates and ignore the WebRTC layer altogether.

Technically, WebRTC already has what we want—◊link["https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API/Using_data_channels"]{data channel}. It’s convenient if you’re using WebRTC in browsers with the Javascript ◊sc{api}. Ironically, the data channel spec added limits like message size on top of the underlying protocol. webrtc-rs acknowledges this and provides a custom “detach” feature to get a pure socket out of the datachannel. Either way, we need to add some chunking/multiplexing layer on top so now we’re two layers on top of ◊sc{sctp} with all the added chunking and metadata overhead. So for us, it’s simpler and more flexible to use the underlying protocol (◊sc{sctp}) directly instead of going through WebRTC.

The stack of WebRTC looks roughly like this:

◊quick-table{
  Protocol | Description
  WebRTC | Application
  ◊sc{sctp} | Congestion and flow control
  ◊sc{dtls} | Security
  ◊sc{ice} | ◊sc{nat} traversal
  ◊sc{udp} | Transport
}

Technically, ◊sc{dtls} (think of ◊sc{tls} for ◊sc{udp}) should run on top of ◊sc{sctp} (think of ◊sc{tcp} Pro Max), right? But WebRTC uses them the other way around. Probably because ◊sc{sctp} provides a much nicer abstraction than ◊sc{dtls}? Anyway, the designers explained it in detail here: ◊link["https://www.rfc-editor.org/rfc/rfc8831.html"]{◊scom{rfc 8831}}.

Now, here’s how WebRTC establish a connection between two peers A and B:

◊ol{
  ◊li{A creates a local ◊fnref["sdp"]{◊sc{sdp}} (called offer), send it to B through a ◊fnref["third-party"]{third-party channel}.}
  ◊li{B receives A’s ◊sc{sdp}, sets it as the remote ◊sc{sdp}, and sends B’s local ◊sc{sdp} (called answer) to A. Meanwhile, B starts gathering ◊sc{ice} candidate according to the information in A’s ◊sc{sdp}.}
  ◊li{A receives B’s ◊sc{sdp}, sets it to its remote ◊sc{sdp}, and start gathering ◊sc{ice} candidates according to B’s ◊sc{sdp}.}
  ◊li{While A and B gather ◊sc{ice} candidates, they’ll send the candidates they gathered to each other through the signaling server, ◊fnref["trickle"]{and try to establish a (◊sc{udp}) connection.}}
  ◊li{Once the connection is established, A and B setup a ◊sc{dtls} connection over it, then a ◊sc{sctp} connection over the ◊sc{dtls} connection.}
  }

◊fndef["sdp"]{◊sc{sdp} (Session Description Protocol) is basically a text packet with a bunch of metadata used for establishing the connection, including media codec, ◊sc{ice} information, fingerprints, etc. See ◊link["https://webrtchacks.com/sdp-anatomy/"]{◊em{SDP Anatomy}} for more. There’s no need to know the details, because we’re going to use our own kind of ◊sc{sdp}.}

◊fndef["third-party"]{WebRTC doesn’t specify this third-party, it can be copy-pasting in Message app between two users, email, pidegon, whatever. A common setup is to use a public “signaling server”, that’s our server S.}

◊fndef["trickle"]{This is called “trickle ◊sc{ice}”. The alternative is to first gather all the candidates, then try to establish ◊sc{ice} connection. Trickle ◊sc{ice} is much faster and is pretty much the standard practice now.}

◊subsection{Authentication}

For authentication, A and B each generates a self-signed key, and hash it to get a fingerprint, then put the fingerprint in their ◊sc{sdp}. Then they do the ◊sc{ice} exchange, and gets each other’s fingerprint from the ◊sc{sdp}. When setting up the ◊sc{dtls} connection, they accpet any key that the other end provides. But after the handshake completes, they verify that the other end’s key matches the fingerprint

The implication here is that A and B must trust the signaling server to deliver their ◊sc{sdp} securely.

The format of the fingerprint is specified in ◊link["https://datatracker.ietf.org/doc/html/rfc8122"]{◊scom{rfc 8122} section ◊om{5}}: “A certificate fingerprint is a secure one-way hash of the Distinguished Encoding Rules (◊sc{der}) form of the certificate.”

Technically many hash functions can be used, but webrtc-rs only supports ◊om{sha-256}; maybe all the browsers and libraries decide to only use ◊om{sha-256}?

For reference, here is how does webrtc-rs hash and validate the fingerprint: ◊link["https://github.com/webrtc-rs/webrtc/blob/4bb9614c56131accc47e903eed086b46c9a2e57c/webrtc/src/dtls_transport/mod.rs#L548"]{◊code{validate_fingerprint}}.

Here’s an example ◊sc{sdp} that contains two fingerprints. (◊code{a} means attribute. See ◊link["https://datatracker.ietf.org/doc/html/rfc8866#name-attributes-a"]{◊scom{rfc 8866}}.) The fingerprint is produced by first hasing the ◊sc{der}, then print out each byte in hex, and join them together with colon.

◊bcode{
m=image 54111 TCP/TLS t38
c=IN IP4 192.0.2.2
a=setup:passive
a=connection:new
a=fingerprint:SHA-256 \
12:DF:3E:5D:49:6B:19:E5:7C:AB:4A:AD:B9:B1:3F:82:18:3B:54:02:12:DF: \
3E:5D:49:6B:19:E5:7C:AB:4A:AD
a=fingerprint:SHA-1 \
4A:AD:B9:B1:3F:82:18:3B:54:02:12:DF:3E:5D:49:6B:19:E5:7C:AB
}

◊section{Code}

Knowing how WebRTC works is one thing, knowing how to conjure the right module and function in the library is another thing. It doesn’t help that webrtc-rs is relatively thin on documentation. So this section contains code snippets taken directly from working code, plus reference to ◊link["https://github.com/casouri/collab-mode"]{my program} and webrtc-rs.

◊subsection{Signaling server}

This is not a part of WebRTC, but for completeness I’ll brefly explain how I wrote my signaling server. There are many articles online about signaling servers too.

For my signaling server, I used websocket since it allows the client to receive streams from the server, plus it provides a nice text/binary message abstraction, making it nicer to use than ◊sc{tcp}. I used ◊link["https://crates.io/crates/tokio-tungstenite"]{tokio-tungstenite} for websocket.

When a client (say, A) wants to accept connections from other peers, it sends a ◊code{Bind} message to the signaling server S, along with an id. Then, another client (say, B) can send a ◊code{Connect} message to S that asks to connect to A by its id. B’s ◊code{Connect} message would contain its ◊sc{sdp}. S relays B’s ◊code{Connect} message to A, then A sends its ◊sc{sdp} via a ◊code{Connect} message to B (relayed by S). Then A and B would start sending each other ◊sc{ice} candidates via ◊code{Candidate} message through S. Finally, A and B establish e2e connection and don’t need S anymore.

My signaling code is in ◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/signaling.rs"]{◊code{/src/signaling.rs}} and ◊link["https://github.com/casouri/collab-mode/tree/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/signaling"]{◊code{/src/signaling}}.

◊subsection{Cargo.toml}

Here’s the relevant crates I used and their version:

◊bcode-hl['toml]{
sha2 = "0.10.8"
pem = "3.0.4"
# Make sure the version of webrtc-util matches the one that's used by
# webrtc-ice, webrtc-sctp, and webrtc-dtls.
webrtc-ice = "0.10.0"
webrtc-util = "0.8.0"
webrtc-sctp = "0.9.0"
webrtc-dtls = "0.8.0"
# Used by webrtc.
bytes = "1.4.0"
# This is the version used by webrtc-dtls.
rcgen = { version = "0.11.1", features = ["pem", "x509-parser"]}
# This is the version used by webrtc-dtls.
rustls = "0.21.10"
}

◊subsection{ICE}

◊link["https://docs.rs/webrtc-ice/latest/webrtc_ice/agent/index.html"]{webrtc_ice documentation}.

Suppose we have two peer A and B; A wants to accept connection from B. Then A is the server in this situation, and B is the client. In the same time, both A and B are clients of the signaling server S. To avoid confusion, let’s call A the ◊om{p2p} server, B the ◊om{p2p} client, and call A & B the signaling client.

To start establishing an ◊sc{ice} connection, we need to create an agent (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/webrpc/ice.rs#L146"]{source}):

◊bcode-hl['rust]{
  use std::sync::Arc;
  use webrtc_ice::agent::agent_config::AgentConfig;
  use webrtc_ice::agent::Agent;
  use webrtc_ice::network_type::NetworkType;
  use webrtc_ice::udp_network::{EphemeralUDP, UDPNetwork};
  use webrtc_ice::url::Url;
  
  let mut config = AgentConfig::default();
  // "Controlling" should be true for the initiator (p2p client), false
  // for the acceptor (p2p server).
  config.is_controlling = false;
  config.network_types = vec![NetworkType::Udp4];
  config.udp_network = UDPNetwork::Ephemeral(EphemeralUDP::default());
  // A list of public STUN servers.
  config.urls = vec![
      Url::parse_url("stun:stun1.l.google.com:19302").unwrap(),
      Url::parse_url("stun:stun2.l.google.com:19302").unwrap(),
      Url::parse_url("stun:stun3.l.google.com:19302").unwrap(),
      Url::parse_url("stun:stun4.l.google.com:19302").unwrap(),
      Url::parse_url("stun:stun.nextcloud.com:443").unwrap(),
      Url::parse_url("stun:stun.relay.metered.ca:80").unwrap(),
  ];
  let agent = Arc::new(Agent::new(config));
}

If we were to use WebRTC’s glue layer, we would create a ◊sc{sdp} and set two ◊sc{ice} attributes in it: ◊code{ufrag} and ◊code{pwd}. But since we aren’t using WebRTC’s glue layer, we just need to get ◊code{ufrag} and ◊code{pwd} from the ◊sc{ice} agent, serialize it, and send it through the signaling server. This will be our version of the ◊sc{sdp}.

Our “◊sc{sdp}-at-home” also needs to include the fingerprint. Technically this fingerprint can be in any format you wish, but I decided to just follow WebRTC’s spec—hash the ◊sc{der} version of the public key. Here’s my hash function (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/config_man.rs#L47"]{source}):

◊bcode-hl['rust]{
  use sha2::{Digest, Sha256};
  /// Hash the binary DER file and return the hash in fingerprint
  /// format: each byte in uppercase hex, separated by colons.
  pub fn hash_der(der: &[u8]) -> String {
    let hash = Sha256::digest(der);
    // Separate each byte with colon like webrtc does.
    let bytes: Vec<String> = hash.iter().map(|x| format!("{x:02x}")).collect();
    bytes.join(":").to_uppercase()
  }

  let fingerprint = hash_der(key_der);
  let (ufrag, pwd) = agent.get_local_user_credentials().await;
  // Then serialize them and send them over the signaling server.
}

My hash function is mostly the same as ◊link["https://github.com/webrtc-rs/webrtc/blob/62f2550799efe2dd36cdc950ad3f334b120c75bb/webrtc/src/dtls_transport/mod.rs#L548"]{the hash function in webrtc-rs}.

Now assume both A and B have their own local ◊sc{sdp} (◊code{ufrag}, ◊code{pwd}, and ◊code{fingerprint}), and received each other’s ◊sc{sdp}. The next step is to exchange ◊sc{ice} candidates.

To send out candidates, we register a callback function on ◊code{agent}, like so (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/webrpc/ice.rs#L202"]{source}):

◊bcode-hl['rust]{
agent.on_candidate(Box::new(move |candidate| {
    if let Some(candidate) = candidate {
        let candidate = candidate.marshal();

        tokio::spawn(async move {
            // Send out candidate through the signaling server.
        });
    }
    Box::pin(async {})
}))

// And start gathering candidates, once the agent got a candidate,
// it’ll invoke the on_candidate callback and our code will send
// it out.
agent.gather_candidates()?;
}

On the other side, we want to receive ◊sc{ice} candidates from the signaling server and feed them into ◊code{agent} (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/webrpc/ice.rs#L230"]{source}):

◊bcode-hl['rust]{
while let Some(candidate) = (receive candidate from signaling server) {
    let candidate = unmarshal_candidate(&candidate)?;
    let candidate: Arc<dyn Candidate + Send + Sync> = Arc::new(candidate);
    agent.add_remote_candidate(&candidate)?;
}
}

While gathering and exchanging candidate run in the background, we block on ◊code{agent.accept()} (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/webrpc/ice.rs#L67"]{source}) or ◊code{agent.dial()} (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/webrpc/ice.rs#L110"]{source}) to get our connection:

◊bcode-hl['rust]{
  // For p2p server A:
  let ice_conn = agent.accept(cancel_rx, ufrag, pwd);
  // For p2p client B:
  let ice_conn = agent.dial(cancel_rx, ufrag, pwd);
}

◊section{DTLS}

◊link["https://docs.rs/webrtc-dtls/latest/webrtc_dtls/index.html"]{webrtc_dtls documentation}.

Now we need to setup a ◊sc{dtls} connection from the ◊sc{ice} connection, and verify the fingerprint. ◊; The most difficult part is actually finding the right type that webrtc-dtls accepts and avoiding rcgen’s traps.

To create a ◊sc{dtls} connection, we need to pass it the key we used to generate the fingerprint earlier. Suppose variable ◊code{key_der: u8[]} contains the key in ◊sc{der} format, we create the certificate that webrtc_dtls accepts (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/config_man.rs#L31"]{source}):

◊bcode-hl['rust]{
let dtls_cert = webrtc_dtls::crypto::Certificate {
    certificate: vec![rustls::Certificate(key_der)],
    private_key: webrtc_dtls::crypto::CryptoPrivateKey::from_key_pair(
        &rcgen::KeyPair::from_der(key_der).unwrap(),
    )
        .unwrap(),
};
}

Then create the ◊sc{dtls} connection. For ◊om{p2p} server, do this (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/webrpc.rs#L98"]{source}):

◊bcode-hl['rust]{
let config = webrtc_dtls::config::Config {
    certificates: vec![dtls_cert],
    client_auth: webrtc_dtls::config::ClientAuthType::RequireAnyClientCert,
    // We accept any certificate, and then verifies the provided
    // certificate with the cert we got from signaling server.
    insecure_skip_verify: true,
    ..Default::default()
};

// Pass false for p2p server.
let dtls_conn = DTLSConn::new(ice_conn, config, false, None).await?;
}

For ◊om{p2p} client, do this (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/webrpc.rs#L117"]{source}):

◊bcode-hl['rust]{
let config = webrtc_dtls::config::Config {
    certificates: vec![dtls_cert],
    // We accept any certificate, and then verifies the provided
    // certificate with the cert we got from signaling server.
    insecure_skip_verify: true,
    ..Default::default()
};

// Pass true for p2p client. 
let dtls_conn = DTLSConn::new(ice_conn, config, true, None).await?;
}

Next, on both ◊om{p2p} server and ◊om{p2p} client, verify the peer certificate of the ◊sc{dtls} connection matches the fingerprint we received from the signaling server (we got it along with ◊code{ufrag} and ◊code{pwd} in the ◊sc{sdp}) (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/webrpc.rs#L84"]{source}).

◊bcode-hl['rust]{
let certs = dtls_conn.connection_state().await.peer_certificates;
if certs.is_empty() {
    // Throw error.
}
// hash_der is shown in the previous section.
let peer_cert_hash_from_dtls = hash_der(&certs[0]);;
if peer_cert_hash_from_dtls != cert_hash_from_signaling_server {
    // Throw error.
}
}

◊section{SCTP}

◊link["https://docs.rs/webrtc-sctp/0.10.0/webrtc_sctp/association/index.html"]{webrtc_sctp documentation}.

We’re getting there! The final step is to setup ◊sc{sctp} connection (◊link["https://github.com/casouri/collab-mode/blob/e06588294bec25b0b1a6d22ee33cdf4c8c8fd252/src/webrpc.rs#L135"]{source}):

◊bcode-hl['rust]{
use webrtc_sctp::association;

let assoc_config = association::Config {
    net_conn: dtls_conn,
    name: "whatever".to_string(),
};

// For p2p server:
let assoc_server = association::Association::server(assoc_config).await?;
sctp_connection.accept_stream().await

// For p2p client:
let assoc_client = association::Association::client(assoc_config).await?;
// The stream identifier can be anything (here I used 1).
sctp_conn.open_stream(1, PayloadProtocolIdentifier::Binary).await?
}

Technically, ◊sc{sctp} support both message and stream, but webrtc-rs only implemented the stream. Fortunately, it also implemented concurrent streams. So instead of chunking messages myself, I simply create a new stream for each message and rely on ◊sc{sctp} to do the chunking. The streams are technically used for long-lived data transfer, but because it’s implemented by simple stream-id tags on chunks, creating and destroying streams doesn’t add much overhead. Just be aware that a new stream created by yourself also arrives as a new stream to the ◊code{accept} method, so there needs to be a way to differentiate between streams opened locally and remotely. I use odd/even stream id to differentiate.

◊section{Conclusion}

That’s it! Now we have a binary stream between two peers. While setting everything up, it helps to go one layer at a time, verify it works, and add the next layer. It also helps to first set it up without authentication, then add the key verification step.

◊section{Appendix A, a rcgen pitfall}

Because I fell into this trap using rcgen and spent two whole nights scratching my head, I want to call it out so readers can avoid it.

Say that you want to generate a certificate and pass it around your program. The intuitive way is to create a ◊code{rcgen::Certificate}, pass it around, and call ◊code{rcgen::Certificate::serialize_der} every time you need a ◊sc{der}, right? But actually, every time you call ◊code{serialize_der}, rather than just serializing the certificate, it ◊em{generates} a new certificate. Put it another way, every time you call ◊code{serialize_der}, it returns a different value.

So the correct way to generate a certificate and pass it around is to create a ◊code{rcgen::Certificate}, call ◊code{serialize_der} to get the ◊sc{der}, and pass the ◊sc{der} around. If you need to use the certificate in another format, just parse the ◊sc{der}.

Here’s an GitHub issue discussing it: ◊link["https://github.com/rustls/rcgen/issues/62"]{Issue#62}.