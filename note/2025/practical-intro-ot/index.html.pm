#lang pollen

◊;define-meta[date]{<2023-05-01 Mon 12:26>}
◊define-meta[date]{<2025-11-26 Wed>}
◊define-meta[uuid]{df4b6d54-e6e2-11ed-96f5-bf97b2144f64}
◊define-meta[tags]{Programming}
◊define-meta[lang]{en}

◊meta{
  ◊title{Practical Intro to Operational Transformation}
  ◊subtitle{Can’t you just give me the best algorithm?}
}

Unfortunately, there isn’t a single best algorithm that you can just learn and implement. There are many different algorithms, each with their own trade-offs and subtleties, none of which I found completely satisfactory. In this article, I hope to just introduce basic ◊sc{ot} concepts in simple terms, and draw a rough picture of how it generally works, and point readers to materials for further reading. I also included a simple ◊sc{ot} algorithm that should give the reader something concrete to look at.

A huge disclaimer upfront: I’m not an expert, only someone that read all of papers and blog posts that I can find, so take what I say with a grain of salt. There’s a frustrating lack of materials of ◊sc{ot} online (which is part of what prompted this article), so I can’t say I’ve grasped the whole picture regarding ◊sc{ot}. If there’s any mistake, or anything I missed, please do let me know. Thanks!

◊section{What’s OT & CRDT}

Nowadays you can’t talk about ◊sc{ot} (operational transformation) without mentioning ◊sc{crdt} (conflict-free replicated data types). Broadly speaking, they are the two approaches to collaborative editing. ◊sc{ot} is the first to be conceived and researched on since the ◊om{90}’s. ◊sc{crdt} is a database/distributed system concept that was brought into collaborative editing in around ◊om{2006}. I’ll briefly introduce both and give them a comparison, before delving into ◊sc{ot}.

◊sc{ot} works by transforming operations like insert, delete, update, etc. Consider this example:

◊ol{
  ◊li{Two users A, B work on a document containing only “x”;}
  ◊li{A inserts “a” before “x” (insert “a” at position 0), propagates its operation to B;}
  ◊li{Not yet aware of A’s modification, B deletes “x” (delete at position 0), propagates its operation to A.}
}

Now if A applies B’s operation verbatim, it would delete the character at position 0, which is “a” rather than the intended “x”. The correct way is to transform B’s deletion so that it deletes at position 1, which then deletes “x”.

This is a trivial example, but things get very complicated very fast once you mix more concurrent operations and have ◊om{3+} users.

The problem, in essence, is that B’s operation is based on document state “x”, so A can’t apply it verbatim on document “ax”. Once we transform the operation to be based on document “ax”, the operation can apply cleanly.

◊sc{crdt} is a data type plus a set of operations, such that operations can be transmitted in different order and eventually still get the same result, as long as everybody eventually receives all the operations. Then, you don’t need a central synchronization, a distributed gossip protocol can ensure that every node in the system eventually reaches the same state. (I omitted some details for brevity.)

Turns out you can design a ◊sc{crdt} that represents a document and a set of operations that covers all text editing operations (on plain text). You model the document as a series of characters with unique ids, and have two operations: insert after the character with id x, and hide character with id x.

Take the same example above. A’s operation is now “insert after the beginning of the document”, B’s operation is now “hide character with id 1 (which is character x)”. Note that operations are not position-based anymore. When A applies B’s operation, it first finds the position of “the character with id 1”, and deletes it.

You’ll notice that it only has “hide” and no “delete”, because ◊sc{crdt} handles delete as hide. ◊sc{crdt} can’t delete a character since other operations might need that character as an anchor. (The hidden characters are called tombstones.)

◊section{OT vs CRDT}

Both ◊sc{ot} and ◊sc{crdt} are viable approaches, with their own trade-offs.

Collaborative editing is a distributed systems problem, and as everything else in distributed systems, you can simplify your system by either centralizing things (so make it less distributed), or you limit the number of things you can do (limit the functionality). ◊sc{crdt} and ◊sc{ot} moves around these axes of trade-offs.

The advantage of ◊sc{crdt} is that it’s relatively simple to make fully distributed. There are many ◊sc{ot} algorithms that claims to be distributed, but they usually have some catch: vector timestamps, requirement of global synchronization, bad space/time complexity that scales with number of nodes in the system, no mention of handling join/leave, etc. (Not to say there are no truly distributed ◊sc{ot} algorithms, there are.)

People often credit ◊sc{crdt} for being simpler than ◊sc{ot}, which it is, ◊em{on paper}. The papers and algorithms only talk about the concept. But if you are to actually implement it, and do it efficiently, things get complicated.

Recall that we described an ◊sc{crdt} operation as “find the position of the character with ◊om{id 1}”, a lot of complexity is hidden in the “find”. How do you find the character with ◊om{id 1}? Are you going to scan the whole document every time?

Also, to use ◊sc{crdt} in the real-world for an editor, you need to translate “delete character with ◊om{id 1}” into something an editor can actually apply, namely, delete character at position ◊em{x}. And once you find the position of the character with ◊om{id 1} in the ◊sc{crdt} data structure, you’ll need to subtract the hidden characters from that position, because the editor’s document doesn’t contain those. Same for the reverse direction, you need to translate an editor operation made by the user into a ◊sc{crdt} operation.

Also, since ◊sc{crdt} lives in a different world than what the user and editor see, operations that ◊sc{crdt} produce might not be “intuitive” to the user, even though the document ends up in a consistent state. A big part of collaborative editing is capturing and preserving “user intent”, and it’s harder to do when your core operations are slightly different from what the user actually does, and you see a different document internally than what the user sees.

There are ◊fnref["tricks"]{tricks} that we can do to speed up the translation, but it’s still a lot of complexity. On the other hand, a basic ◊sc{ot} algorithm is much easier to implement.

Even though ◊sc{ot} ◊sc{do} is simple, ◊sc{ot} ◊sc{undo} is very complicated and inefficient. We’ll expand on this later sections. Undo in ◊sc{crdt} is simple and can be handled as normal operations.

I hope I’ve demonstrated that neither ◊sc{ot} nor ◊sc{crdt} is strictly superior to the other. Some further comparison can be found in Appendix A.

◊section{Intro to OT}

An ◊sc{ot} algorithm is made of three parts, a set of basic operations; the transformation function that transforms operations against each other; and a control algorithm that determines which operation to transform against which.

Usually there’s several editors collaborating on a document, each editor is commonly called a site. What we want to achieve is eventual consistency, that is, the document on each site should end up the same; but we also want to preserve user intent—if the algorithm messes up the sequence of text and makes the document unreadable, even if all the sites have the same garbled document, it’s not of much use for users.

To nobody’s suprise, user intent is fuzzy and hard to pin down formally. Nevertheless, ◊sc{ot} researchers over the years came up with formal concepts and properties that formalizes the problem and can be used to prove the correctness of algorithms. Most notably, ◊scom{tp1} and ◊scom{tp2}.

◊section{TP1 and TP2}

◊sc{TP} stands for transformation property. They are properties that a transformation function has to satisfy in order to preserve consistency and user intent. ◊scom{tp1} is basic and easily satisfied. ◊scom{tp2} is rarely satisfied, most algorithms use a control algorithm to rearrange operations such that ◊scom{tp2} is never encountered.

◊scom{tp1} says: given two concurrent operations ◊code◊{o1} and ◊code{o2} created on the ◊em{same} state, you can get the same document state through two routes. First route: transform ◊code{o2} against ◊code{o1} and get ◊code{o2’}, and apply it on top of ◊code{o1}, so history is ◊code{[o1, o2’]}. Second route: transform ◊code{o1} against ◊code{o2} and get ◊code{o1’}, and apply it on top of ◊code{o2}, so history is ◊code{[o2, o1’]}.

◊scom{tp2} involves three operations, given any operation ◊code{o3}, you should be able to transform it against ◊code{[o1, o2’]}, or against ◊code{[o2, o1’]}, and the result ◊code{o3’} should be the same. This is very hard to achive. But luckily, you only need ◊scom{tp2} if your algorithm needs to transform operations that are not created on the same context. Most algorithms simply arrange the operations so they never need to do this transformation. Instead, they pre-transform the operations so that they’re based on the same context, then transform them against each other. Indeed, how to correctly and efficiently track and store the context of each operation, and find the operation with the right context, etc, would be the job of the control algorithm.

◊;section{State matrix}

◊section{Control algorithm}

The principle of a control algorithm is simple, suppose there are two editors, and you’re the control algorithm on ◊om{editor 2}, user has made a series of edits ◊code{o1,...,o3}, and in comes an op ◊code{o8}, from ◊om{editor 2}. Op ◊code{o8} is made on a document state different from the document state currently in your editor, so it can’t be applied verbatim. Let’s suppose our transformation function satisfies ◊scom{tp1} but not ◊scom{tp2}, which is typical, how do you transform ◊code{o8} so that it avoids ◊scom{tp2} and the result can apply cleanly on the current document?

Note that a document state is simply made of all the ops applied to it. So we can define the document state, aka the context of any op generated on that state, as a set of ops. So our current document state is ◊code{{o1, o2, o3}}. Conveniently, the three ops are generated by the user sequentially, so ◊code{o1}’s context is ◊code{{}}, ◊code{o2}’s context is ◊code{{o1}}, and ◊code{o3}’s context is ◊code{{o1, o2}}, each can be directly applied in the order of ◊code{o1}, ◊code{o2}, ◊code{o3}.

Now if we look back at ◊code{o8}, how we transform it depends on its context. Suppose by the time ◊om{editor 2} generates ◊code{o8}, it has already received o1, then the context of ◊code{o8} is ◊code{{o1}}. We just need to transform it in some way such that ◊code{o8}’s context becomes ◊code{{o1, o2, o3}} to apply it on our document. And with our current history, there is a valid path to do it: first transform ◊code{o8} against ◊code{o2}, since both have context ◊code{{o1}}, to get ◊code{o8’}. Note that now ◊code{o8’} has context ◊code{{o1, o2}}. Then transform ◊code{o8’} against ◊code{o3} and get ◊code{o8”} which has context ◊code{{o1, o2, o3}}, and it can be applied to our document. Remember, because our transformation function doesn’t satisfy ◊scom{tp2}, we can only transform an op against another when they have the same context.

So that’s the rough idea, in real algorithms, there are many ways to play with the context. You can enforce a global sequence so that the context is implied by the position of an op in the sequence. Or you might find yourself with an op with a context that you can’t transform (maybe ◊code{o8}’s context is ◊code{{o1, o3}}, how do you transform that?)

For concrete algorithms, I recommend reading ◊fnref["wave"]{Google Wave’s implementation} and the ◊fnref["COT"]{◊sc{cot} paper}, and maybe the ◊fnref["UNO"]{◊sc{uno}} paper for a blend of ◊sc{ot} and ◊sc{crdt}. I also came up with an algorithm myself, it’s basically a blend of Google Wave and ◊sc{cot}, plus tombstones (see below).

◊section{Undo}

On the surface, undo with ◊sc{ot} seems simple: for an op ◊code{o}, you just generate the inverse of it ◊code{I(o)}, and treat it as a concurrent operation after ◊code{o}, transform it against all ops after ◊code{o}, and apply it to the document. But this won’t work. Suppose you insert "AAA", undo it, then redo the undo. Now the history consists of ◊code{[ins(0, "AAA"), del(0, "AAA"), ins(0, "AAA")]}. Let’s try to undo the first insertion—make an inverse of it, which is ◊code{del(0, "AAA")}; transform it against the second op, now you get ◊code{ins(0, "")}; then you transform it against the third op, of course, nothing changes, and you still get ◊code{ins(0, "")}. That’s wrong! We expect to get ◊code{del(0, "AAA")}. Our transformation function doesn’t know that the third op undoes the second op, so after the content in our op was removed in the first transformation, it didn’t get added back during the second transformation.

What I just described is basically ◊scom{ip2}: transforming an op ◊code{o1} against another op ◊code{o2} followed by its inverse ◊code{I(o2)} should give you ◊code{o1} back.

In total, there are three inverse properties ◊scom{ip1}, ◊scom{ip2}, ◊scom{ip3}. ◊scom{ip1} is basic and like ◊scom{tp1}, is always satisfied. It says applying an op followed by its inverse gives you the same document.

◊scom{ip3} says given two concurrent ops ◊code{o1} and ◊code{o2}, if you transform ◊code{o2} against ◊code{o1} to get ◊code{o2’} and apply to the document, then generate an inverse of ◊code{o1} and transform it against ◊code{o2’} to get ◊code{I(o1)’}. This should be the same as transforming o1 against ◊code{o2} to get ◊code{o1’}, then taking the inverse of it to get ◊code{I(o1’)}.

Again, ◊scom{ip2} and ◊scom{ip3} are generally avoided with control algorithms. Earlier ◊sc{ot} algorithms try to associate undo with their original op and skips them in pairs when transforming the inverse op. Later ◊sc{ot} algorithms like ◊sc{cot} use increasingly complicated and expensive procedure to generate inverse operations.

But if you’re willing to introduce tombstones into the algorithm, suddenly the control algorithm becomes easy: with tombstone, ◊scom{ip1}, ◊scom{ip2}, and ◊scom{ip3} can all be satisfied, you can just use the naive algorithm described at the beginning of this section. But obviously tombstones come with significant drawbacks like increased storage, need to convert between internal position and editor position, etc.

If you want to learn the proper way to do undo with ◊sc{ot}, I recommend reading the ◊fnref["COT"]{◊sc{cot} paper}. If I remember correctly, its undo is exponential time.

All in all, I don’t think undo is “solved”. Even Google Doc has cracks. Try this: open the same doc in two tabs, in one tab type AABB, in the other insert XX between AA and BB. Now undo in both tabs. Redo in the first tab, you get AABB back. Now redo in the second tab, you get XXAABB.

◊section{A simple OT algorithm}

This is a very simple OT algorithm, in fact, simplicity is the whole point here. I tried to come up with an algorithm that’s as simple to implement as possible. As a result, it doesn’t support rich text, nor is it distributed, and it uses tombstones. But it’s as simple as you can get.

For transform function, we just use the most basic string-based insert and delete. Each site has an assigned site id, which is used to break ties.

There needs to be a central server that receives ops from each site, transforms them and broadcasts the op to each site. Each op’s context is represented by a sequence number in a global sequence, its context is basically all the ops before it combined. We make sure all the ops are generated from/transformed to have this linear, continuous context. This is key to simplify the control algorithm.

The server’s job is simple, it maintains the global history, and receives ops from each site. The ops sent from each site doesn’t always have the latest context, if the current global history is ◊code{[o1,...,o9]}, a site might send an op with context ◊code{[o1,...,o6]}. That’s fine, the server just transforms it against ◊code{o7,...,o9}, stores it in the global history, and broadcast it out. The important thing is that a client’s op’s context is always a prefix of the global history. Eg, a context like ◊code{[o1, o2, o5, o6]} would never happen.

On the editor side, we maintain a history made of two segments, the global history ◊code{L1} and local history ◊code{L2}. And the current editor document state is at the end of ◊code{L1 + L2}. ◊code{L1} contains all the ops sent from the server, and ◊code{L2} contains all the ops the user created that haven’t been sent to the server yet. If user keeps adding ops, we put them at the end of ◊code{L2}. The ops we receive from the server always have continuous global sequence numbers, and the context of a new op from the server is always all the ops in ◊code{L1}. That means whenever an op comes from the server, we just need to transform it against all the ops in ◊code{L2} sequentially, and it’ll have the correct context to be applied to the current local document. Also the untransformed server op is appended to the end of ◊code{L1}.

To send ops to server, we always send the whole ◊code{L2} to server together, because ops in ◊code{L2} have continuous context. We send ◊code{L2} to server, server transforms them against the global history it has, and broadcast it out. Note that the server can always transform the ◊code{L2} we send out, because ◊code{L2}’s context is ◊code{L1}, which is always a prefix of the global history that the server maintains. Then, we received ◊code{L2’} back from server, which is the transformed version of ◊code{L2}, and append it to the end of ◊code{L1}. We also remove the ops from ◊code{L2} since they’re now in ◊code{L1}. Waiting for this ◊sc{ack} from server ensures that our local ◊code{L1} is always a prefix of the global history on the server.

In case of disconnection and lost messages, a site can simply re-request ops from the server and resend its local ops, there’s no special synchronization or fixup required.

That’s about it for the ◊sc{do} part. This is basically what Google Wave does. I omitted some details but readers should be able to see how it manages context to ensure ops always have the correct context for the transformation we need to do.

For ◊sc{undo}, I used tombstones. Now I need to translate internal doc position (which counts tombstones) and external editor document position. This is still vastly simpler than trying to use a control algorithm to avoid ◊scom{ip2} and ◊scom{ip3}. If someone has a good way to implement undo without tombstones, I’m eager to hear.

◊section{Real world implementations}

Most of the algorithms mentioned in the history section have an accompanying implementation. Most notably Sun et al implemented CoWord and CoMaya, which brings collaborative editing to Word and Maya. Very impressive work. The latest algorithm used in CoWord and CoMaya is ◊sc{cot}. ◊sc{cot} is also used in ◊link["https://www.codox.io"]{CodoxWord} and ◊sc{ibm} OpenCoWeb according to the ◊fnref["POT"]{◊sc{pot} paper}.

The CKE editor based their editor on ◊fnref["ot-tree"]{this paper.} They showed some of the data structures they use in their ◊fnref["CKE-editor-compress"]{other blog post}, and it looks just like the one in the paper.

◊section{Appendix A, OT vs CRDT cont.}

◊fnref["tiny"]{People also say ◊sc{crdt} can’t work well with rich text}, because it can only handle insertion and deletion, so it’s difficult for it to preserve user intent on complicated operations like “add table row”. And all the commercial rich text editors are implemented in ◊sc{ot} (◊fnref["tiny"]{TinyMCE editor}, ◊fnref["CKE-editor"]{◊sc{cke} editor}, Codox, Google Doc, etc). But, I’m pretty sure that ◊sc{crdt} can handle rich text. ◊fnref["prominent"]{The author of CoWord, CoPPT, and CoMaya} uses  ◊fnref["coword-coppt"]{◊sc{ta} (transparent adaptation)}, which works by converting high-level operations in the application into three primitive operations: insert, delete, and update. You just need to properly implement ◊sc{crdt} with rich text, rather than encoding rich text in ◊sc{json} and using ◊sc{crdt} on the plain ◊sc{json}.

◊fndef["prominent"]{A group lead by Chengzheng Sun, a prominent figure in ◊sc{ot} research.}

Though to be fair, ◊sc{ot} algorithms rarely have more than a handful of basic operations either, because you need to define transformation between each pair of basic operations, so the number of transformations you need to define is the number of basic operations squared. The common approach is ◊sc{ta} (transparent adaptation). Basically translating high-level operations into basic operations. I didn’t look into this very deeply.

Space complexity: ◊sc{crdt} document keeps all the characters ever inserted, both visible characters and tombstones. ◊sc{ot} needs to store all the concurrent operations. It doesn’t need to store ◊em{all} the operations: once an operation is known to be applied at all nodes, it can be discarded.

So it seems ◊sc{ot} has better space complexity in general? Not quite. If you want to make the ◊sc{ot} system fully distributed, you’ll have to keep all the history, since it’s impossible to tell if an operation is applied at every node when you don’t even know all the nodes. And if you think about it, “full history” and “all characters ever inserted in to the document” ◊fnref["undo-history"]{sounds familiar, no?}

◊fndef["undo-history"]{Unless a user makes a million undo and redo. You don’t want to just store undo operations as references to original operations, since they need to be transformed and thus will become different.}

On the other hand, if you do know when all sites have applied an operation, you can garbage-collection tombstones in ◊sc{crdt}.

Time complexity: it varies greatly depending on the algorithm, optimization, etc. Suffice to say both can be efficient and it really depends.

Finally, there is a ◊fnref["real-difference"]{detailed paper} that compares ◊sc{ot} and ◊sc{crdt}. It was later expanded into a three-part series: ◊fnref["real-difference-1"]{Ⅰ}, ◊fnref["real-difference-2"]{Ⅱ}, ◊fnref["real-difference-3"]{Ⅲ}. The main idea is that ◊sc{crdt} in its essence is still based on transformations, and there are a lot of hidden complications in applying the algorithm on paper to editors. The paper makes ◊sc{crdt} sound like being inferior to ◊sc{ot}, but the bias towards ◊sc{ot} is pretty palpable, so I’d take it with a grain of salt ◊smile{}

◊section{Appendix B, OT history}

◊fnref["deOPT"]{deOPT (◊sc{grove})} is (I think) the first ◊sc{ot} algorithm, created in ◊om{1989}. Later, people found out a flaw: the deOPT puzzle.

◊fnref["DistEdit"]{DistEdit} in ◊om{1994} explores selective undo and undo ◊sc{ui}.

◊fnref["Jupiter"]{Jupiter} in ◊om{1995} is the basis of ◊fnref["wave"]{Google Wave}.

The Sun et al lineage starts from ◊fnref["GOT"]{◊sc{got}} in ◊om{1998}. ◊sc{got} aims to  solve the deOPT puzzle. Then there is ◊sc{goto} also in ◊om{1998} by the same author.

After that came ◊fnref["ANYUNDO"]{◊sc{anyundo}} in 2002, which can undo any operation at any time. Previous algorithms usually have some restriction on what operation can be undone and when can you undo it. ◊sc{anyundo} only concerns with undo so it pairs with ◊sc{goto} to become a complete solution.

◊fnref["COT"]{◊sc{cot}} is the final algorithm, iterated upon ◊sc{goto-anyundo}, published in ◊om{2009}. ◊fnref["POT"]{◊sc{pot}} is the final final algorithm, proposed in a theoretical paper in ◊om{2016}. (They also proposed a improved version of ◊sc{tibot}, ◊scom{tibot 2.0}, in that paper.)

Control algorithm-wise, it seems to end there for Sun et al. After ◊sc{cot}, they went on to research collab editing for 3D modeling software (e.g., Co-Maya).

◊fnref["TIBOT"]{◊sc{tibot}} by Li & Li and Sun in ◊om{2004} is an interesting algorithm that uses logic time intervals, doesn’t need central ordering and doesn’t need a vector timestamp. (Still, every node needs to constantly sync with every other node, so join/leave will be a challenge.)

There are some other algorithms proposed by other researchers: ◊sc{soct3/4}, ◊sc{nice}, etc. You can check out the ◊link["https://en.wikipedia.org/wiki/Operational_transformation"]{wiki page on operational transformation}. It has a nice table summarizing a lot of the algorithms.

Post ◊sc{cot}, the trend seems to have shifted to two major directions: One is ◊fnref["ttf"]{◊sc{ttf}} (by ◊link["◊sc{hal}"]{◊sc{hal}}), which incorporates tomstones into ◊sc{ot} to improve undo complexity. Another is ◊fnref["abt"]{◊sc{abt}} (by Li & Li), which moves operations in the history buffer such that an operation that inserts a character always comes before an operation that deletes that character.

Along the ◊sc{ttf} line, you have ◊fnref["UNO"]{◊sc{uno}}, ◊fnref["ST-Undo"]{ST-Undo}, etc. Along the ◊sc{abt} line, you have ◊fnref["ABTU"]{◊sc{abtu}}.

Here I quote the author of ST-Undo:

◊bquote{
  The follow-up OT-based undo solutions invent functional components to address the abnormal ordering problem. TTF [36] introduces an object sequence to keep deleted objects. ABT [39] introduces a special history buffer in which insert operations are placed before delete operations. UNO [25, 26] is a selective undo algorithm built on TTF. As deleted objects are never lost, UNO can preserve the ordering relations among objects. Except for the object sequence, UNO stores both do and undo operations in the history buffer. The time and space complexity of UNO is linear in the size of the object sequence plus the number of operations in the history buffer. ABTU [28] is developed from ABT [39]. In ABTU, undo operations are stored in the form of inverse operations of the corresponding do operations in the history buffer. As an operation may be transformed with both do and undo operations, ABTU arranges the operations in the history buffer according to their effect positions. ABTU has a linear time and space complexity in the size of history buffer.
}

I only looked into ST-Undo and ◊sc{abtu} closely. ◊sc{abtu} is very complicated and uses vector timestamps, ST-Undo is moderately complicated, but to me, part of the complexity (using a tree to store tombstones) is unnecessary in practice. They could’ve just used a cache around cursor, and the performance would be even better in practice, and it’s dead simple to implement. But I guess caching isn’t interesting for an academic paper.

I also found ◊fnref["OPTIC"]{◊sc{optic}}, seems like a “truly distributed” algorithm that handles node joining and leaving.

◊section{Resources}

◊fndef["tiny"]{TinyMCE: ◊link["https://www.tiny.cloud/blog/real-time-collaboration-ot-vs-crdt/"]{To OT or CRDT, that is the question}}

◊fndef["CKE-editor"]{CKE Editor: ◊link["https://ckeditor.com/blog/Lessons-learned-from-creating-a-rich-text-editor-with-real-time-collaboration/"]{Lessons learned from creating a rich-text editor with real-time collaboration}}

◊fndef["real-difference"]{◊sc{ot} vs ◊sc{crdt}: ◊link["https://arxiv.org/abs/1810.02137"]{Real Differences between OT and CRDT for Co-Editors}}

◊fndef["real-difference-1"]{◊sc{ot} vs ◊sc{crdt} part 1:◊link["https://arxiv.org/abs/1905.01517"]{Real Differences between OT and CRDT in Building Co-Editing Systems and Real World Applications}}

◊fndef["real-difference-2"]{◊sc{ot} vs ◊sc{crdt} part 2: ◊link["https://arxiv.org/abs/1905.01302"]{Real Differences between OT and CRDT in Correctness and Complexity for Consistency Maintenance in Co-Editors}}

◊fndef["real-difference-3"]{◊sc{ot} vs ◊sc{crdt} part 3:◊link["https://arxiv.org/abs/1905.01518"]{Real Differences between OT and CRDT under a General Transformation Framework for Consistency Maintenance in Co-Editors}}

◊fndef["deOPT"]{deOPT: Concurrency control in groupware systems}

◊fndef["DistEdit"]{DistEdit: A framework for undoing actions in collaborative systems}

◊fndef["Jupiter"]{Jupiter: High-latency, low-bandwidth windowing in the Jupiter collaboration system}

◊fndef["GOT"]{◊sc{got}: Achieving convergence, causality preservation, and intention preservation in real-time cooperative editing systems}

◊fndef["ANYUNDO"]{◊sc{anyundo}: Undo as concurrent inverse in group editors}

◊fndef["wave"]{◊link["https://svn.apache.org/repos/asf/incubator/wave/whitepapers/operational-transform/operational-transform.html"]{Google Wave Operational Transformation}}

◊fndef["COT"]{◊sc{cot}: Context-based Operational Transformation for Distributed Collaborative Editing Systems}

◊fndef["POT"]{◊sc{pot}: Conditions and Patterns for Achieving Convergence in OT-Based Co-Editors}

◊fndef["TIBOT"]{◊sc{tibot}: A Time Interval Based Consistency Control Algorithm for Interactive Groupware Applications}

◊fndef["OPTIC"]{◊sc{optic}: Coordination Model for Real-Time Collaborative Editors}

◊fndef["UNO"]{◊sc{uno}: An Undo Framework for P2P Collaborative Editing}

◊fndef["ABTU"]{◊sc{abtu}: An Algorithm for Selective Undo of Any Operation in Collaborative Applications}

◊; ◊fndef["mvsd"]{The ◊em{update} operation, and ◊sc{mvsd} (multiple view, single display): Operational Transformation for Collaborative Word Processing}

◊fndef["ot-tree"]{◊sc{ot} for ◊sc{xml}: Generalizing Operational Transformation to the Standard General Markup Language}

◊fndef["CKE-editor-compress"]{CKE Editor, data compression: ◊link["https://ckeditor.com/blog/How-we-reduced-traffic-10-20-times-data-compression-in-real-time-collaboration/"]{How we reduced traffic 10–20 times—data compression in real-time collaboration}}

◊;fndef["string"]{String-wise transformation: Achieving convergence, causality preservation, and intention preservation in real-time cooperative editing systems}

◊;fndef["string-puzzle"]{String-wise transformation puzzles: Exhaustive Search and Resolution of Puzzles in OT Systems Supporting String-Wise Operations}

◊fndef["coword-coppt"]{CoWord, CoPPT, ◊sc{ta}: Transparent adaptation of single-user applications for multi-user real-time collaboration. Extends the ◊fnref["ot-tree"]{◊sc{xml} paper}}

◊fndef["ttf"]{◊sc{ttf}: Tombstone Transformation Functions for Ensuring Consistency in Collaborative Editing Systems}

◊fndef["abt"]{◊sc{abt}: An Admissibility-Based Operational Transformation Framework for Collaborative Editing Systems}

◊fndef["tricks"]{◊link["https://josephg.com/blog/crdts-go-brrr/"]{5000x faster CRDTs: An Adventure in Optimization}}