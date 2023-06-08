#lang pollen

◊define-meta[date]{<2023-05-01 Mon 12:26>}
◊define-meta[uuid]{df4b6d54-e6e2-11ed-96f5-bf97b2144f64}
◊define-meta[tags]{Programming}
◊define-meta[lang]{en}

◊meta{
  ◊title{Practical Intro to Operational Transformation}
  ◊subtitle{Just give me the best algorithm}
}

Unfortunately, there isn’t a best algorithm that you can just learn, implement, and be done with it. There are a bunch of trade-offs, and subtleties, which I hope to explain in this article, or at least point to further reads.

A huge disclaimer upfront: I’m not an expert, only someone that read a bunch of papers and blog posts, so take what I say with a grain of salt.

◊section{What’s OT & CRDT}

Nowadays you can’t talke about ◊sc{ot} (operational transformation) without mentioning ◊sc{crdt} (conflict-free replicated data types). Broadly speaking, they are the two approaches to collaborative editing. ◊sc{ot} is the first to be conceived and researched on since the ◊om{90}’s. ◊sc{crdt} is a database/distributed system concept that was brought into collaborative editing in around ◊om{2006}. I’ll briefly introduce both and give them a comparison, before delving into ◊sc{ot}.

◊sc{ot} works by transforming operations like insert, delete, update, etc. Consider this example:

◊ol{
  ◊li{Two users A, B work on a document containing only “x”;}
  ◊li{A inserts “a” before “x” (insert “a” at position 0), propagates its operation to B;}
  ◊li{Not yet aware of A’s modification, B deletes “x” (delete at position 0), propagates its operation to A.}
}

Now if A applies B’s operation verbatim, it would delete the character at position 0, which is “a” rather than the intended “x”. The correct way is to transform B’s deletion so that it deletes at position 1, which deletes “x”.

This is a trivial example, but things get very complicated very fast once you mix more concurrent operations and have ◊om{3+} users.

The problem, in essence, is that B’s operation is based on document “x”, but A applied it on document “ax”. Once we transform the operation to be based on document “ax”, the operation can apply cleanly.

◊sc{crdt} is a data type plus a set of operations, such that operations can be transmitted in different order and eventually still get the same result, as long as everybody eventually receives all the operations. Then, you don’t need a central synchronization, a distributed gossip protocol can ensure that every node in the system eventually reach the same state. (I omitted some details for brevity.)

Turns out you can design a ◊sc{crdt} that represents a document and a set of operations that covers all text editing operations. You model the document as a series of characters with unique ids (plus invisible character for beginning and end of the document), and have two operations: insert after the character with id x, and hide character with id x.

Take the same example. A’s operation is now “insert after the beginning of the document”, B’s operation is now “hide character with id 1 (which is character x)”. Note that operations are not position-based anymore. When A applies B’s operation, it first finds the position of “the character with id 1”, and deletes it.

You’ll notice that it only has “hide” and no “delete”, because ◊sc{crdt} handles delete as hide. ◊sc{crdt} can’t delete a character since other operations might need that character as an anchor. (The hidden characters are called tombstones.)

◊section{OT vs CRDT}

I’ll say this right off the bat: I don’t think either is strictly better than the other, they just have different characteristics and trade-offs. You need to consider the things you absolutely want in your system to decide which to use.

Collaborative editing is a distributed systems problem, and as everything else in distributed systems, you can simplify your system by either centralizing things (so make it less distributed), or you limit the number of things you can do (limit the functionality). ◊sc{crdt} and ◊sc{ot} moves around these axes of trade-offs.

The advantage of ◊sc{cdrt} is that it’s relatively simple to make fully distributed. There are many ◊sc{ot} algorithms that claims to be distributed, but they usually have some catch: vector timestamp, requires global synchronization, bad space/time complexity that scales with number of nodes in the system, no mention of handling join/leave, etc. (Not to say there are no truly distributed ◊sc{ot} algorithms, there are.)

People often credit ◊sc{crdt} for being simpler than ◊sc{ot}, which it is, ◊em{on paper}. The papers and algorithms only talk about the concept. But if you are to actually implement it, and do it efficiently, things get complicated.

For example, in ◊sc{crdt}, every character has a unique id. In reality, you’re not going to store a unique id for every character. You’ll probably use landmarks: the first character in a range stores an unique id, and the rest are represented as an offset from that character. Now you are back with position juggling not unlike in ◊sc{ot}. And when you delete, how do you find the character with the id? Are you going to scan the whole document? Or store the document in a balanced tree?

To be fair, ◊sc{ot} is only a little better. The algorithms on papers are already non-trivial to understand (and to convince yourself of its correctness); on top of that, they often don’t directly translate to implementation either (but better than ◊sc{crdt}). And once you add synchronization, failure, etc into the mix, you get just as much headache.

◊fnref["tiny"]{People will also say ◊sc{crdt} can’t work well with rich text}, because it can only handle insertion and deletion, so it’s difficult for it to preserve user intent on complicated operations like “add table row”, “mark this piece of text in the middle as bold”. I tend to agree, considering that all the real world commercial rich text editors that I know of are implemented in ◊sc{ot} (◊fnref["tiny"]{TinyMCE editor}, ◊fnref["CKE-editor"]{◊sc{cke} editor}, Codox, Google Doc, etc). However, I want to mention that ◊fnref["prominent"]{the author of CoWord, CoPPT and CoMaya} uses  ◊fnref["coword-coppt"]{◊sc{ta} (transparent adaptation)}, which works by converting high-level operations in the application into three primitive operations: insert, delete, update. I could be wrong, but the update operation doesn’t look that difficult to support in ◊sc{crdt}, sooooo?

◊fndef["prominent"]{A group lead by Chengzheng Sun, a prominent figure in ◊sc{ot} research.}

◊; Though to be clear, ◊sc{ot} algorithms rarely have more than a handful of basic operations either, because you need to define transformation between each pair of basic operations, so the number of transformations you need to define is the number of basic operations squared. The common approach is ◊sc{ts} (transparent adaptation). Basically translating high-level operations into basic operations. TODO

Space complexity: ◊sc{crdt} document keeps all the characters ever inserted, both visible characters and tombstones. ◊sc{ot} needs to store all the concurrent operations. It doesn’t need to store ◊em{all} the operations: once an operation is known to be applied at all nodes, it can be discarded.

So it seems ◊sc{ot} has better space complexity in general? Not quite. If you want to make the ◊sc{ot} system fully distributed, you’ll have to keep all the history, since it’s impossible to tell if an operation is applied at every node when you don’t even know all the nodes. And if you think about it, “full history” and “all characters even inserted in to the document” ◊fnref["undo-history"]{sounds familiar, no?} I guess the conclusion is a) if you want full distribution, you need to store a lot of data (or use some complicated ◊fnref["both-gc"]{garbage collection}); b) if you don’t need full distribution, ◊sc{ot} gives you more options.

◊fndef["undo-history"]{Unless a user makes a million undo and redo. You don’t want to just store undo operations as references to original operations, since they need to be transformed and thus will become different.}

◊fndef["both-gc"]{There are ◊sc{gc} discussion for both ◊sc{crdt} and ◊sc{ot}.}

Time complexity: readers are advice to read other materials.

Finally, there is a ◊fnref["real-difference"]{detailed paper} that compares ◊sc{ot} and ◊sc{crdt}. It was later expanded into a three-part series: ◊fnref["real-difference-1"]{Ⅰ}, ◊fnref["real-difference-2"]{Ⅱ}, ◊fnref["real-difference-3"]{Ⅲ}. They probably explained everything better than I could, but I’ll note the possibility of ◊sc{ot} bias.

◊section{Intro to OT}

An ◊sc{ot} algorithm is made of three parts, a set of basic operations; the transformation function that transforms operations against each other; and a control algorithm that determines which operation to transform against which.

◊section{Some history}

◊fnref["deOPT"]{deOPT (◊sc{grove})} is (I think) the first ◊sc{ot} algorithm, created in ◊om{1989}. Later people found out a flaw: the deOPT puzzle.

◊fnref["DistEdit"]{DistEdit} in ◊om{1994} explores selective undo and undo ◊sc{ui}.

◊fnref["Jupiter"]{Jupiter} in ◊om{1995} is the basis of ◊fnref["wave"]{Google Wave}.

◊fnref["GOT"]{◊sc{got}} in ◊om{1998} by Sun et al, aims to  solve the deOPT puzzle. Then there is ◊sc{goto} also in ◊om{1998} by the same author.

Then there is ◊fnref["ANYUNDO"]{◊sc{anyundo}} by Sun in ◊sc{2002}, that can undo any operation at any time. Previous algorithms usually have some restriction on what operation can be undone and when can you undo it.

◊fnref["COT"]{◊sc{cot}} is the final algorithm, by Sun, and David Sun, iterated upon ◊sc{goto-anyundo}, published in ◊om{2009}.

◊fnref["POT"]{◊sc{pot}} is the final final algorithm, by Yi Xu and Sun, proposed in a theoretical paper in ◊om{2016}. (They also proposed a improved version of ◊sc{tibot}, ◊sc{tibot 2.0}, in that paper.)

◊fnref["TIBOT"]{◊sc{tibot}} by Li & Li and Sun in ◊om{2004} is an interesting algorithm that uses logic time intervals, doesn’t need central ordering and doesn’t need vector timestamp. (Still, every node needs to constantly sync with every other node, so join/leave will be a challenge.)

There are some others: ◊sc{soct3/4}, ◊sc{nice}, etc. You can check out the ◊link["https://en.wikipedia.org/wiki/Operational_transformation"]{wiki page on operational transformation}. It has a nice table summarizing a lot of the algorithms.

Some less-known work:

◊fnref["OPTIC"]{◊sc{optic}, seems like a “truly distributed” algorithm that handles node joining and leaving.}

◊fnref["UNO"]{◊sc{uno}}, published in ◊om{2008}, combines ◊sc{crdt} with ◊sc{ot}.

◊fnref["ABTU"]{◊sc{abtu}}, published in ◊om{2010}, achieves O(|H|) time complexity in both do and undo, theoretical work. (H is the history buffer.)

◊fnref["ST-Undo"]{ST-Undo}, also combines ◊sc{crdt}. Simpler and better time complexity than ◊sc{uno} and ◊sc{abtu}, but is limited to plain text.

◊section{TP1 and TP2}

◊section{State matrix}

◊section{Real world implementations}

Most of the algorithms mentioned in the history section has accompanying implementation. Most notably Sun et al implemented CoWord and CoMaya, which brings collaborative editing to Word and Maya. Very impressive work. The latest algorithm used in CoWord and CoMaya is ◊sc{cot}. ◊sc{cot} is also used in ◊link["https://www.codox.io"]{CodoxWord} and ◊sc{ibm} OpenCoWeb according to the ◊fnref["POT"]{◊sc{pot} paper}.

CKE editor based their editor on ◊fnref["ot-tree"]{this paper.} They showed some data structure they use in their ◊fnref["CKE-editor-compress"]{other blog post}, and looks just like the one in the paper.

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

String-wise transformation: Achieving convergence, causality preservation, and intention preservation in real-time cooperative editing systems
String-wise transformation puzzles: Exhaustive Search and Resolution of Puzzles in OT Systems Supporting String-Wise Operations

◊fndef["coword-coppt"]{CoWord, CoPPT, ◊sc{ta}: Transparent adaptation of single-user applications for multi-user real-time collaboration. Extends the ◊fnref["ot-tree"]{◊sc{xml} paper}}