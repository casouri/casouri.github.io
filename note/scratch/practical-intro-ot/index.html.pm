#lang pollen

◊define-meta[date]{<2023-05-01 Mon 12:26>}
◊define-meta[uuid]{df4b6d54-e6e2-11ed-96f5-bf97b2144f64}
◊define-meta[tags]{Programming}
◊define-meta[lang]{en}

◊meta{
  ◊title{Practical Intro to Operational Transformation}
  ◊subtitle{Just give me the best algorithm}
}

◊(define ot '(sc "ot"))
◊(define crdt '(sc "ctdt"))

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

The problem, in essence, is that B’s operation is based on document state “x”, so A can’t apply it verbatim on document “ax”. Once we transform the operation to be based on document “ax”, the operation can apply cleanly.

◊sc{crdt} is a data type plus a set of operations, such that operations can be transmitted in different order and eventually still get the same result, as long as everybody eventually receives all the operations. Then, you don’t need a central synchronization, a distributed gossip protocol can ensure that every node in the system eventually reach the same state. (I omitted some details for brevity.)

Turns out you can design a ◊sc{crdt} that represents a document and a set of operations that covers all text editing operations (on plain text). You model the document as a series of characters with unique ids, and have two operations: insert after the character with id x, and hide character with id x.

Take the same example above. A’s operation is now “insert after the beginning of the document”, B’s operation is now “hide character with id 1 (which is character x)”. Note that operations are not position-based anymore. When A applies B’s operation, it first finds the position of “the character with id 1”, and deletes it.

You’ll notice that it only has “hide” and no “delete”, because ◊sc{crdt} handles delete as hide. ◊sc{crdt} can’t delete a character since other operations might need that character as an anchor. (The hidden characters are called tombstones.)

◊section{OT vs CRDT}

Both ◊ot and ◊crdt are viable approaches, with their own trade-offs.

Collaborative editing is a distributed systems problem, and as everything else in distributed systems, you can simplify your system by either centralizing things (so make it less distributed), or you limit the number of things you can do (limit the functionality). ◊sc{crdt} and ◊sc{ot} moves around these axes of trade-offs.

The advantage of ◊sc{cdrt} is that it’s relatively simple to make fully distributed. There are many ◊sc{ot} algorithms that claims to be distributed, but they usually have some catch: vector timestamps, requirement of global synchronization, bad space/time complexity that scales with number of nodes in the system, no mention of handling join/leave, etc. (Not to say there are no truly distributed ◊sc{ot} algorithms, there are.)

People often credit ◊sc{crdt} for being simpler than ◊sc{ot}, which it is, ◊em{on paper}. The papers and algorithms only talk about the concept. But if you are to actually implement it, and do it efficiently, things get complicated.

Recall that we described an ◊crdt operation as “find the position of the character with id 1”, a lot of complexity is hidden in the “find”. How do you find the character with id 1? Are you going to scan the whole document every time?

Also, to use ◊crdt in the real-world for an editor, you need to translate “delete character with id 1” into something an editor can actually apply, namely, delete character at position ◊em{x}. And once you find the position of the character with id 1 in the ◊crdt data structure, you’ll need to subtract the hidden characters from that position, because the editor’s document doesn’t contain those. Same for the reverse direction, you need to translate an editor operation made by the user into a ◊crdt operation.

There’re ◊fnref["tricks"]{tricks} that we can do to speed up the translation, but it’s still a lot of complexity. On the other hand, a basic ◊ot algorithm is much easier to implement.

Even though ◊ot ◊sc{do} is simple, ◊ot ◊sc{undo} is very complicated and inefficient. We’ll expand on this later sections. Undo ◊crdt is simple and can be handled as normal operations.

I hope I’ve demonstrated that neither ◊ot or ◊crdt is strictly superior to another. Some further comparison can be found in Appendix A.

◊section{Intro to OT}

An ◊sc{ot} algorithm is made of three parts, a set of basic operations; the transformation function that transforms operations against each other; and a control algorithm that determines which operation to transform against which.

◊section{TP1 and TP2}

◊section{State matrix}

◊section{OT UNDO}

AX example

two approaches: tombstone or transformation

◊section{Real world implementations}

Most of the algorithms mentioned in the history section has accompanying implementation. Most notably Sun et al implemented CoWord and CoMaya, which brings collaborative editing to Word and Maya. Very impressive work. The latest algorithm used in CoWord and CoMaya is ◊sc{cot}. ◊sc{cot} is also used in ◊link["https://www.codox.io"]{CodoxWord} and ◊sc{ibm} OpenCoWeb according to the ◊fnref["POT"]{◊sc{pot} paper}.

CKE editor based their editor on ◊fnref["ot-tree"]{this paper.} They showed some data structure they use in their ◊fnref["CKE-editor-compress"]{other blog post}, and looks just like the one in the paper.

◊section{Appendix A, OT vs CRDT cont.}

◊fnref["tiny"]{People will also say ◊sc{crdt} can’t work well with rich text}, because it can only handle insertion and deletion, so it’s difficult for it to preserve user intent on complicated operations like “add table row”. And all the commercial rich text editors are implemented in ◊sc{ot} (◊fnref["tiny"]{TinyMCE editor}, ◊fnref["CKE-editor"]{◊sc{cke} editor}, Codox, Google Doc, etc). But, I’m pretty sure that ◊crdt can handle rich text. ◊fnref["prominent"]{The author of CoWord, CoPPT and CoMaya} uses  ◊fnref["coword-coppt"]{◊sc{ta} (transparent adaptation)}, which works by converting high-level operations in the application into three primitive operations: insert, delete, update. You just need to properly implement ◊crdt with rich text, rather than encoding rich text in ◊sc{json}, and use ◊crdt on the plain ◊sc{json}.

◊fndef["prominent"]{A group lead by Chengzheng Sun, a prominent figure in ◊sc{ot} research.}

Though to be fair, ◊sc{ot} algorithms rarely have more than a handful of basic operations either, because you need to define transformation between each pair of basic operations, so the number of transformations you need to define is the number of basic operations squared. The common approach is ◊sc{ts} (transparent adaptation). Basically translating high-level operations into basic operations. I didn’t look into this very deeply.

Space complexity: ◊sc{crdt} document keeps all the characters ever inserted, both visible characters and tombstones. ◊sc{ot} needs to store all the concurrent operations. It doesn’t need to store ◊em{all} the operations: once an operation is known to be applied at all nodes, it can be discarded.

So it seems ◊sc{ot} has better space complexity in general? Not quite. If you want to make the ◊sc{ot} system fully distributed, you’ll have to keep all the history, since it’s impossible to tell if an operation is applied at every node when you don’t even know all the nodes. And if you think about it, “full history” and “all characters even inserted in to the document” ◊fnref["undo-history"]{sounds familiar, no?}

◊fndef["undo-history"]{Unless a user makes a million undo and redo. You don’t want to just store undo operations as references to original operations, since they need to be transformed and thus will become different.}

On the other hand, if you do know when all sites have applied an operation, you can garbage-collection tombstones in ◊|crdt|.

Time complexity: readers are advised to read other materials.

Finally, there is a ◊fnref["real-difference"]{detailed paper} that compares ◊sc{ot} and ◊sc{crdt}. It was later expanded into a three-part series: ◊fnref["real-difference-1"]{Ⅰ}, ◊fnref["real-difference-2"]{Ⅱ}, ◊fnref["real-difference-3"]{Ⅲ}. The main idea is that ◊crdt in its essence is still based on transformations, and there are a lot of hidden complications applying the algorithm on paper to editors. The paper makes ◊crdt sound like being inferior to ◊ot, but the bias towards ◊ot is pretty clear, so I’d take it with a grain of salt ◊smile{}

◊section{Appendix B, OT history}

◊fnref["deOPT"]{deOPT (◊sc{grove})} is (I think) the first ◊sc{ot} algorithm, created in ◊om{1989}. Later people found out a flaw: the deOPT puzzle.

◊fnref["DistEdit"]{DistEdit} in ◊om{1994} explores selective undo and undo ◊sc{ui}.

◊fnref["Jupiter"]{Jupiter} in ◊om{1995} is the basis of ◊fnref["wave"]{Google Wave}.

The Sun et al lineage starts from ◊fnref["GOT"]{◊sc{got}} in ◊om{1998}. ◊sc{got} aims to  solve the deOPT puzzle. Then there is ◊sc{goto} also in ◊om{1998} by the same author.

After that came ◊fnref["ANYUNDO"]{◊sc{anyundo}} in ◊sc{2002}, that can undo any operation at any time. Previous algorithms usually have some restriction on what operation can be undone and when can you undo it. ◊sc{anyundo} only concerns with undo so it pairs with ◊sc{goto} to become a complete solution.

◊fnref["COT"]{◊sc{cot}} is the final algorithm, iterated upon ◊sc{goto-anyundo}, published in ◊om{2009}. ◊fnref["POT"]{◊sc{pot}} is the final final algorithm, proposed in a theoretical paper in ◊om{2016}. (They also proposed a improved version of ◊sc{tibot}, ◊sc{tibot 2.0}, in that paper.)

Control algorithm wise, it seems to end there for Sun et al. After ◊sc{cot}, they went on to research collab editing for 3D modeling software (eg, Co-Maya).

◊fnref["TIBOT"]{◊sc{tibot}} by Li & Li and Sun in ◊om{2004} is an interesting algorithm that uses logic time intervals, doesn’t need central ordering and doesn’t need vector timestamp. (Still, every node needs to constantly sync with every other node, so join/leave will be a challenge.)

There are some other algorithms proposed by other researchers: ◊sc{soct3/4}, ◊sc{nice}, etc. You can check out the ◊link["https://en.wikipedia.org/wiki/Operational_transformation"]{wiki page on operational transformation}. It has a nice table summarizing a lot of the algorithms.

Post ◊sc{cot}, the trend seems to have shifted to two major directions: One is ◊fnref["ttf"]{◊sc{ttf}} (by ◊link["◊sc{hal}"]{◊sc{hal}}), which incorporates tomstones into ◊ot to improve undo complexity. Another is ◊fnref["abt"]{◊sc{abt}} (by Li & Li), which moves operations in the history buffer such that operation that inserts a character always come before operation that deletes that character.

Along the ◊sc{ttf} line, you have ◊fnref["UNO"]{◊sc{uno}}, ◊fnref["ST-Undo"]{ST-Undo}, etc. Along the ◊sc{abt} line, you have ◊fnref["ABTU"]{◊sc{abtu}}.

Here I quote the author of ST-Undo:

◊bquote{
  The follow-up OT-based undo solutions invent functional components to address the abnormal ordering problem. TTF [36] introduces an object sequence to keep deleted objects. ABT [39] introduces a special history buffer in which insert operations are placed before delete operations. UNO [25, 26] is a selective undo algorithm built on TTF. As deleted objects are never lost, UNO can preserve the ordering relations among objects. Except for the object sequence, UNO stores both do and undo operation in the history buffer. The time and space complexity of UNO is linear in the size of the object sequence plus the number of operations in the history buffer. ABTU [28] is developed from ABT [39]. In ABTU, undo operations are stored in the form of inverse operations of the corresponding do operations in the history buffer. As an operation may be transformed with both do and undo operations, ABTU arranges the operations in the history buffer according to their effect positions. ABTU has a linear time and space complexity in the size of history buffer.
}

I only looked into ST-Undo and ◊sc{abtu} closely. ◊sc{abtu} is very complicated and uses vector timestamps, ST-Undo is moderately complicated, but to me, part of the complexity (using a tree to store tomstones) is unnecessary in practice. They could’ve just used a cache around cursor, and the performance would be even better in practice, and it’s dead simple to implement. But I guess caching isn’t interesting for an academic paper.

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

String-wise transformation: Achieving convergence, causality preservation, and intention preservation in real-time cooperative editing systems
String-wise transformation puzzles: Exhaustive Search and Resolution of Puzzles in OT Systems Supporting String-Wise Operations

◊fndef["coword-coppt"]{CoWord, CoPPT, ◊sc{ta}: Transparent adaptation of single-user applications for multi-user real-time collaboration. Extends the ◊fnref["ot-tree"]{◊sc{xml} paper}}

◊fndef["ttf"]{◊sc{ttf}: Tombstone Transformation Functions for Ensuring Consistency in Collaborative Editing Systems}

◊fndef["abt"]{◊sc{abt}: An Admissibility-Based Operational Transformation Framework for Collaborative Editing Systems}

◊fndef["tricks"]{◊link["https://josephg.com/blog/crdts-go-brrr/"]{5000x faster CRDTs: An Adventure in Optimization}}