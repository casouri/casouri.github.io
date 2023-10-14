#lang pollen

◊define-meta[date]{<2023-02-15 Wed>}
◊define-meta[uuid]{43cbedf8-ac05-11ed-b63d-1f5ec28d824a}
◊define-meta[tags]{Programming}
◊define-meta[lang]{en}

◊meta{
  ◊title{Notes of CSE 221: Operating Systems}
  ◊subtitle{UCSD recap, ep1}
}

During my time at ◊sc{ucsd}, I took all the main courses in their systems track and enjoyed them greatly. It would be a shame if I forgot those wonderful things I learn, due to my extraordinarily bad memory. So here is a hodgepodge of lecture notes, personal notes, and faint recollections.

Just be aware, I wrote this to help myself rather than introducing stuff to others, so this article might not be very interesting or useful for others. Neither do I guarantee the correctness of any information in this article.

◊sc{cse} ◊om{221} is the entry course, introducing students to reading papers and the essential systems papers. The cast of papers is pretty stable over the years and across instructors.

Here is a syllabus of a ◊scom{cse 221} similar to the one I took: ◊link["https://cseweb.ucsd.edu/classes/wi21/cse221-a/readings.html"]{◊em{CSE 221: Reading List and Schedule, Winter 2021}}. (I took the one in Fall ◊om{2021} taught by Dr. Y. Y. Zhou.)

◊section{THE System}

◊em{The Structure of the “THE”-Multiprogramming System}, ◊om{1968}, by none other than Edsger W. Dijkstra.

The main take away is “central abstraction in a hierarchy”. The central abstraction is sequential process, and hierarchy is basically “layers”. The benefit of layers is that it’s easy to verify soundness and prove correctness for each individual layer, which is essential to handle complexity. To quote Dijkstra:

◊bquote{
  In testing a general purpose object (be it a piece of hardware, a program, a machine, or a system), one cannot subject it to all possible cases: for a computer this would imply that on feeds it with all possible programs! Therefore one must test it with a set of relevant test cases. What is, or is not, relevant cannot be decided as long as one regards the mechanism as a black box; in other words, the decision has to be based upon the internal structure of the mechanism to to be tested. It seems to be the designer’s responsibility to construct his mechanism in such a way—i.e. so effectively structured—that at each state of the testing procedure the number of relevant test cases will be so small that he can try them all and that what is being tested will be so perspicuous that the will not have overlooked any situation.
}

He then mentioned that “industrial software makers” has mixed feelings of this methodology: they agree that it’s the right thing to do, but doubt whether it’s applicable in the real world, away from the shelter of academia. Dijkstra’s stance is that the larger the project, the more essential the structuring. This stance is apparent in ◊fnref["ewd1041"]{his other writings}.

◊fndef["ewd1041"]{◊link["https://www.cs.utexas.edu/users/EWD/transcriptions/EWD10xx/EWD1041.html"]{◊em{EWD 1041}}. Now, I don’t think it is realistic to write proofs for every system you design, but good structuring, and designing with testing in mind are certainly essential.}

The downside of layering is, of course, the potential loss of efficiently, due to either the overhead added by layering, or the lack of detail hidden by lower layers. For example, the graphic subsystem in win◊om{32} was moved into the kernel in ◊scom{nt4}, because there were too many boundary crossing.

And sometimes it’s hard to separate the system into layers at all, eg, due to circular dependency, etc. For example, in Linux, memory used by the scheduler is pinned and never page.

Professor also noted some interesting terminology used at the time, like “harmonious cooperation”—no deadlock, and “deadly embrace”—deadlock. Also, do you know why the function name for wait and signal for semaphores are P and V? Because they are in Dutch, P stands for proberen, and V stands for verhogen.

◊section{Nucleus}

◊em{The Nucleus of a Multiprogramming System}, ◊om{1970}.

Basically they want a “nucleus” (small kernal) that supports multiple simultaneous operating system implementations. So the user can have their ◊sc{os} however they want. (Another example of “mechanism instead of policy”, sort of.) This school of thought would later lead to exokernel and micro kernel.

The nucleus provides a scheduler (for process and ◊sc{i/o}), communication (messages passing), and primitive for controlling processes (create, start, stop, remove).

In their design, the parent process is basically the ◊sc{os} of their child processes, controlling allocation of resources for them: start/stop them, allocate memory and storage to them, etc. Although a parent process can start/stop a child process, it doesn’t have control over the exact scheduling: the nucleus is in charge of that, as it divides computing time by round-robin scheduling among all active processes. This breaks their design a bit: ideally the nucleus should schedule the top-level processes and let those processes schedule their children themselves. Perhaps it would be too inconvenient if you need to implement scheduler for every “◊sc{os}” you want to run.

Comparing their system, ◊scom{rc400}, to ◊sc{the} system, ◊scom{rc400} uses message passing for communication, whereas ◊sc{the} system doesn’t have inter-process communication (it’s a batch system); ◊scom{rc400} uses mutex for synchronization and ◊sc{the} system uses semaphore.

◊section{HYDRA}

◊em{HYDRA: The Kernel of a Multiprocessor Operating System}, ◊om{1974}.

Their design goals include separation of mechanism and policy, and rejection of strict hierarchy layering for access control, because they consider access control more of a net than layers. Their design goal also emphasized on protection: not only comprehensive protection, but also flexible protection. They provide protection mechanism that can be used for not only for regular things like ◊sc{i/o}, etc, but also arbitrary things that a higher-level program want to protect/control. It would be nice if ◊sc{unix} has something similar to offer...

◊em{Capability} is basically the right to use some resource, a key to a door. Possessing the capability means you have the right of using whatever resource it references. For example, file descriptors in ◊sc{unix} are capabilities: when you open a file, the kernel checks if you are allowed to read/write that file, and if the check passes, you get a file descriptor. Then you are free to read/write to that file using the file descriptor. Kernel knows you have the right since you hold that file descriptor.

In an access-controlled ◊sc{os}, you have
◊ul{
  ◊li{Resources (data, device);}
  ◊li{Execution domains (eg, execute as a user); and}
  ◊li{access control in domain to resource}
}

In ◊sc{hydra}, you have ◊em{procedure}, ◊sc{lns}, and ◊em{process}. Procedure is things like a executable program or a subroutine. ◊sc{lns} (local name space) is the execution domain. Conceptually it is a collection of capabilities, ie, it determines what you can and cannot do. Each invocation of a procedure has a ◊sc{lns} attached to it. Paraphrase in ◊sc{unix} terms, when a user Alice runs a program ◊code{ls}, the capabilities Alice has is the ◊sc{lns}, and ◊code{ls} is the procedure. Finally, a process is conceptually a (call) stack of procedures with their accompanying ◊sc{lns}.

Since each invocation of procedures have an accompanying ◊sc{lns}, the callee’s ◊sc{lns} could have more or different capabilities from its caller, so you can have things like ◊em{right amplification}.

Right amplification is when caller has more privilege than the caller (of course, privilege is a ◊sc{unix} concept, ◊sc{hydra} rejects hierarchy). For example, in ◊sc{unix}, when a program uses a syscall, that syscall executed by the kernel has far more privilege than the caller. For another example (in ◊sc{unix}), when Alice runs ◊code{passwd} to change her password, that program modifies the password file which Alice has no access to. In ◊sc{unix}, the first example is the result of kernel having ultimate privilege, and the second example is implemented by euid (effective user id).

Another concept often used in security (but not in ◊sc{hydra}) is ◊sc{acl} (access control list). It’s basically a table recording who has access to what. To use an ◊sc{acl}, you need to know the user; with capabilities, anyone with the capability can have access, you don’t need to know the particular user—this is firstly easier to check, and secondly useful for distributed systems or simply very large systems, where storing information of all users/entities is not possible.

However, capabilities are unforgettable, ie, you can’t take it back. Maybe you can make them expire, but that’s more complexity. Capabilities can also be duplicated and given away, which has it’s own virtues and vices.

Since ◊sc{acl} is easy to store and manage, and capability is easy to check, they are often used together. In ◊sc{unix}, opening a file warrens a check in the ◊sc{acl}, and the file descriptor returned to you is a capability.

It’s interesting to think of the access control systems used around us. Windows certainly has a more sophisticated ◊sc{acl} than ◊sc{unix}. What about Google Docs, eh? On top of the regular features, they also support “accessible through links”, “can comment but not edit”, etc.

◊section{TENEX}

◊em{TENEX, a Paged Time Sharing System for the PDP-10}, ◊om{1972}.

◊sc{tenex} is the predecessor of ◊sc{multics}, which in turn is the predecessor of ◊sc{unix}. It runs on ◊scom{pdp-10}, a machine very popular at the time: used by Harvard, ◊sc{mit}, ◊sc{cmu}, to name a few. It was manufactured by ◊sc{bbn}, a military contractor at the time. ◊scom{pdp-10} is micro-coded, meaning its instructions are programmable.

In ◊sc{bbn}’s pager, each page is ◊om{512} words, the ◊sc{tlb} is called “associative register”. Their virtual memory supports ◊om{256}K words and copy-on-write. A process in ◊sc{tenex} always have exactly one superior (parent) process and any number of inferior (child) processes. Processes communicate through (a) sharing memory, (b) direct control (parent to child only), and (c) pseudo (software simulated) interrupts. Theses are also the only ways of communication we have to date in ◊sc{unix}. Would be nice if we had messages or something...

◊sc{tenex} is able to run binary programs compiled for ◊scom{dec 10/50}, the original ◊sc{os} for the ◊scom{pdp-10}. All the ◊sc{tenex} syscalls “were implemented with the ◊sc{jsys} instruction, reserving all old monitor [◊sc{os}/kernel] calls for their previous use”. They also implemented all the ◊scom{dec 10/50} syscalls as a compatibility package. The first time a program calls a ◊scom{dec 10/50} syscall, that package is mapped “to a remote portion of the process address space, and area not usually available ton a ◊om{10/50} system”.

◊sc{tenex} use balanced set scheduling. A balanced set is a set of highest priority processes whose total working set fits in memory, where working set means the pages these processes reference. This way we reduce the page faults. According to my professor, Linux now uses fault frequency as the measure for scheduling.

Guess what is an “executive command language interpreter”? They descried it as “...which provides direct access to a large variety of small, commonly used system functions, and access to and control over all other subsystems and user programs”. It’s a shell!

Some interesting facts: ◊sc{tenex} supports at most 5 levels in file paths; the paper mentions file extensions; files in ◊sc{tenex} are versioned, a new version is created every time you write to a file, old versions are automatically deleted by the system; it has five access rights: directory listing, read, write, execute, and append; they have a debugger residing in the core memory alongside the kernel.

The file operations is the same as in ◊sc{unix}, opening a file gives you a file descriptor, called ◊sc{jfn} (job file number), and you can read or write the file. The effect of the write is seen immediately by readers (so I guess no caching or buffering). They even have “unthawed access”, meaning only one writer is allowed while multiple reader can read from the file at the same time. ◊sc{unix} really cut a lot of corners, didn’t it?

Their conclusion section is also interesting:

◊bquote{
  One of the most valuable results of our work was the knowledge we gained of how to organize a hardware/software project of this size. Virtually all of the work on TENEX from initial inception to a usable system was done over a two year period. There were a total of six people principally involved in the design and implementation. An ◊om{18} month part-time study, hardware design and implementation culminated in a series of documents which describe in considerable detail each of the important modules of the system. These documents were carefully and closely followed during the actual coding of the system. The first state of coding was completed in ◊om{6} months; at this point the system was operating and capable of sustaining use by nonsystem users for work on their individual projects. The key design document, the JSYS Manual (extended machine code), was kept updated by a person who devoted full time to insuring its consistency and coherence; and in retrospect, it is out judgment that this contributed significantly to the overall integrity of the system.

  We felt it was extremely important to optimize the size of the tasks and the number of people working on the project. We felt that too many people working on a particular task or too great an overlap of people on separate tasks would result in serious inefficiency. Therefore, tasks given to each person were as large as could reasonably  be handled by that person, and insofar as possible, tasks were independent or related in ways that were well defined and documented. We believe that this procedure was a major factor in the demonstrated integrity of the system as well as in the speed with which it was implemented.
}

◊section{MULTICS}

◊em{Protection and the Control of Information Sharing in Multics}, ◊om{1974}.

The almighty ◊sc{multics}, running on the equally powerful Honeywell ◊om{6180}. There are multiple papers on ◊sc{multic}, and this one is about its protection system.

Their design principles are

◊ol{
  ◊li{Permission rather than exclusion (ie, default is no permission)}
  ◊li{◊fnref["selinux"]{Check every access to every object}}
  ◊li{The design is not secret (ie, security not by obscurity)}
  ◊li{Principle of least privilege}
  ◊li{Easy to use and understand (human interface) is important to reduce human mistakes}
}

◊fndef["selinux"]{Early-day Linux doesn’t do this, which led to SELinux. It has merged into main Linux long ago.}

◊sc{multics} has a concept of ◊em{descriptor segments}. The virtual memory is made of segments, and each segment has a descriptor, containing access right, protection domain, etc. Thus ◊sc{multics} has access-control for memory. The access check are done by hardware for speed. That also means ◊sc{multics} depends on the hardware and are not portable, unlike ◊sc{unix}.

◊sc{multic} uses an regular ◊sc{acl} for file-access control. When opening a file, the kernel checks for access rights, creates a segment descriptor (capability), and maps the whole file into virtual memory as a segment. In the paper, the ◊sc{acl} is described as the first level access-control, and the hardware-based access-control the second level. Note that files in ◊sc{multics} you can’t read a file as a stream: the whole file is mmaped into memory, essentially.

◊sc{multics} also has ◊em{protected subsystems}. It’s a collection of procedure and data that can only be used through designated entry points called “gates” (think of an ◊sc{api}). To me, it’s like modules (public/private functions and variables) in programming languages, but in an ◊sc{os}. All subsystems are put on a hierarchy, every subsystem within a process get a number, lower-numbered subsystems can use descriptors containing higher-numbered subsystems. And the protection is guaranteed by the hardware. They call it “rings of protection”.

Speaking of rings, ◊om{x86} supports four ring levels, this is how kernel protects itself from userspace programs. Traditionally userspace is on ring ◊om{3} and kernel is on ring ◊om{0}. Nowadays with virtual machines, the guest ◊sc{os} is put on ring ◊sc{1}.

◊section{Protection}

◊em{Protection}, ◊om{1974}.

This paper by Butler Lampson give an overview of protection in systems, and introduces a couple useful concepts.

A ◊em{protection domain} is anything that has certain rights to do something and has some protection from other things, eg, kernel or userspace, a process, a user. A lot of words are used to describe it: protection context, environment, state, sphere, capability list, ring, domain.

Then there are ◊em{objects}, things needs to be protected. Domains themselves can be objects. The relationship between domains and objects form a matrix, the ◊em{access matrix}. Each relationship between a domain and an object can be a list of ◊em{access attributes}, like owner, control, call, read, write, etc.

When implementing the access matrix, the system might want to attach the list of accessible object of a domain to that domain. Each element of this list is essentially a capability.

Alternatively, the system can attach a list of domains that can access an object to that object. An object would have a procedure that takes a domain’s name as input and returns its access rights to this object. The domain’s name shouldn’t be forge-able. One idea is to use capability as the domain identifier: a domain would ask the supervisor (kernel) for an identifier (so it can’t be forged), and pass it to objects’ access-control procedure. An arbitrary procedure is often an overkill, and an ◊em{access lock list} is used instead.

Many system use a hybrid implementation in which a domain first access an object by access-key to obtain a capability, which is used for subsequent access. (Opening a file.)

◊section{UNIX}

◊em{The UNIX Time-Sharing System}, ◊om{1974}.

The good ol’ ◊sc{unix}! This paper describe the “modern” ◊sc{unix} written in C, running on ◊scom{pdp-11}.

Comparing to systems like ◊sc{tenex} and ◊sc{multics}, ◊sc{unix} has a simpler design and does not require special hardware supports, since it has always been designed for rather limited machines, and for its creators’ own use only, free from requirements imposed by other people.

The paper spends major portions describing the file system, something we tend to take for granted from an operating system, and ◊fnref["filesystem"]{view as swappable nowadays}. We are all too familiar with “everything as a file”. ◊sc{unix} treats files as a linear sequence of bytes, but that’s not the only possible way. ◊sc{ibm} filesystems has the notion of “records” ◊fnref["fs-database"]{like in a database}. And on ◊sc{multics}, as we’ve seen, the whole file is ◊fnref["fs-mmap"]{mmaped to the memory}.

◊fndef["filesystem"]{Because most filesystems we use expose the same interface, namely the ◊sc{posix} standard. They all have read, write, open, close, seek, makedir, etc. I wish in the future we can plug in custom filesystems to the ◊sc{os} and expose new interfaces for programs to use. For example, a network filesystem that can tell the program “I’m downloading this file from remote, the progress is xx%”. Right now network filesystems either block or immediately error out.}

◊fndef["fs-database"]{As every idea in ◊sc{cs}, this might be coming back in another form. For example, Android uses (modified) SQLite for its filesystem.}

◊fndef["fs-mmap"]{Again, this might be coming back, in the form of persistent memory.}

◊sc{unix} uses mounting to integrate multiple devices into a single namespace. If you look at ◊sc{ms dos}, they use filenames to represent devices.

This version of ◊sc{unix} only has seven protections bits, one of which switches set-user-id, so there is no permission for “group”. set-user-id is just the effective user id thing we talked about earlier in the ◊sc{hydra} section.

The paper talked about the shell in detail, for example the ◊code{| < > ; &} operators. Judging from the example, the ◊code{<} and ◊code{>} are clearly intended to be prefixes rather than operators:

◊bcode{
  ls >temp1
  pr -2 <temp1 >temp2
  opr <temp2
}

◊section{Plan 9}

◊em{Plan 9 From Bell Labs}, ◊om{1995}.

According to the paper, by the mid ◊om{1980}’s, people have moved away from centralized, powerful timesharing systems (on mainframes and mini-computers) to small personal micro-computers. But a network of machines have difficulty serving as seamlessly as the old timesharing system. They want to build a system that feels like the old timesharing system, but is made of a bunch of micro-computers. Instead of having a single powerful computer that does everything, they will have individual micro-computers for each task: a ◊fnref["computing"]{computing (◊sc{cpu}) server}, a file server, routers, terminals, etc.

◊fndef["computing"]{Of course, this wouldn’t make sense anymore, since ◊sc{cpu}’s are so much faster than networks now.}

The central idea is to expose every service as files. Each user can compose their own private namespace, mapping files, devices, and services (as files) into a single hierarchy. Finally, all communication are made through a single protocol, ◊scom{9p}. Compare that to what we have now, where the interface is essentially C ◊sc{abi}, it certainly sounds nice. But I have to say, using text stream as the sole interface for everything is a bit questionable too.

Their file server has an interesting storage called ◊sc{worm} (write-once, read many), it’s basically a time machine. Everyday at ◊om{5} ◊sc{am}, a snapshot of all the disks is taken and put into the ◊sc{worm} storage. People can get back old versions of their files by simply reading the ◊sc{worm} storage. Nowadays ◊sc{worm} snapshot is often used to defend against ransom attacks. And you better make sure the ◊sc{worm} is absolutely read-only ◊wink{}

◊section{Medusa}

◊em{Medusa: An Experiment in Distributed Operating Systems Structure}, ◊om{1980}.

A distributed system made at ◊sc{cmu}, to closely match and maximally exploit its hardware: the distributed-processor Cm* (computer modules) system.

On a distributed processor hardware, they can place the kernel code in memory in three ways:

◊ol{
  ◊li{Replicate the kernel on every node}
  ◊li{Kernel code on one node, other nodes’ processors execute code remotely}
  ◊li{Distributed kernel}
}

They chose the third approach: divide the kernel into ◊em{utilities} (kernel module) and distribute them among all the processors, “with no guarantee that any particular processor contains a copy of the code for any particular utility”. When a running program needs to invoke a certain utility (some syscall provided by some kernel module), it migrates to the processor that has that utility. Each utility could be on multiple processors, so programs don’t have to fight for a single popular utility.

The design is primarily influenced by efficiency (on the given hardware), but it also has nice structure properties. Boundaries between utilities are “rigidly enforced”; each utility can only send messages to each other and not modify other’s memory. This improves security and robustness (error in one utility won’t affect other utilities).

One problem that might occur when you split the kernel into modules is circular dependency and, in exertion, deadlocks. If the filesystem utility calls into the memory manager utility (eg, get a buffer), and the memory manager utility calls into the filesystem utility (eg, swap pages), you have a circular dependency. Mix in locks and you might get a deadlock.

To be deadlock-free, Medusa further divides each utility into ◊em{service classes} so service classes don’t have circular dependencies. It also makes sure each utility use separate and statistically allocated resources, so they don’t step on each other.

Programs written to run on Medusa are mostly concurrent in nature. Instead of conventional processes, program execution are carried out by ◊em{task forces}, which is a collection of ◊em{activities}, where each activity is like a thread but runs on different processors. Activities access kernel objects (resources like memory page, pipe, file, etc) through descriptors. Each activity has a ◊em{private descriptor list} (◊sc{pdl}), and all activities in a task force share a ◊em{shared descriptor list} (◊sc{sdl}). There are also ◊em{utility descriptor list} (◊sc{udl}) for utility entry points (syscalls), and ◊em{external descriptor list} (◊sc{xdl}) referencing remote ◊sc{udl} and ◊sc{pdl}. Both ◊sc{udl} and ◊sc{xdl} are processor-specific.

The task force notion is useful for scheduling: Medusa schedules activities that are in the same task force to run in the same time. It’s often referred to as ◊em{gang scheduling} or ◊em{coscheduling}, where you schedule inter-communicating processes to run together, just like working sets in paging. In addition, Medusa does not schedule out an activity immediately when it starts waiting (eg, for a lock), and spin-waits for a short while (◊em{pause time}), in the hope that the wait is short (shorter than context switch).

Utilities stores information for an activity alongside the activity rather than storing it on the utility’s processor. This way if an utilities fails, another utility can come in, read the information, and carry on the work. (Remember that utilities are duplicated on multiple processors.) The utility ◊em{seals} the information stored with the activity, so user programs can’t muddle with it. Only other utilities can unseal and use that information. Implementation wise, unsealing means mapping the kernel object into the ◊sc{xdl} (of the processor running the utility), and sealing it means removing it from the ◊sc{xdl}.

Medusa’s kernel also provide some handy utilities like the exception reporter and a debugger/tracer. When an exception occurs, the kernel on the processor sends exception data to the reporter, which sends that information to other activities (◊em{buddy activity}) to handle. And you can use the debugger/tracer to online-debug programs. Must be nice if the kernel drops you into a debugger when your program segfaults, no? (Looking at Common Lisp.) I feel that Ken Thompson being too good a programmer negatively impacted the computing device we have today. If he isn’t that good, perhaps he would add a kernel debugger in ◊sc{unix} ◊wink{}

◊section{Pilot}

◊em{Pilot: An Operating System for a Personal Computer}, ◊om{1980}.

A system developed by Xerox ◊sc{parc} on their personal work stations. Since it is intended for personal computing, they made some interesting design decisions. The kernel doesn’t worry about fairness in allocating resources, and can take advices from userspace. For example, userspace programs can mark some process as high priority for ◊fnref["pilot-scheduling"]{scheduling}, or pin some pages in the memory so it’s never swapped out. (These are just examples, I don’t know for sure if you can do these things in Pilot.)

◊fndef["pilot-scheduling"]{Very recently we start to see big/small cores in Apple M1 and Intel 12th gen, and “quality of service” in macOS.
}

Pilot uses the same language, Mesa, for operating system and user programs, which allows the two to coupe tightly.

Pilot provides defense (against errors) but not ◊fnref["absolute-protection"]{absolute protection}. And protection is language-based, provided by (and only by) type-checking in Mesa, the language used to write both the operating system and programs.

◊fndef["absolute-protection"]{This is before Internet, and malicious software isn’t a thing yet, I think?}

Lastly, Pilot has integrated support for networks. It is designed to be used in a network (of Pilots). In fact, the first distributed email system is created on Pilot.

The device on which Pilot runs is also worth noting: a powerful machine, with high-resolution bitmap display, keyboard, and a “pointing device”. Xerox ◊sc{parc} basically invented personal computer, and  ◊sc{gui} and mouse that go with it.

The filesystem is flat (no directory hierarchy), though higher level software are free to implement additional structure. Files are accessed through mapping its pages (blocks) into virtual memory. Files and volumes (devices) are named by a 64-bit unique id (uid), which means files created anywhere anytime can be uniquely identified across different machines (and thus across the network). They used a classic trick, unique serial number plus real-time clock, to guarantee uniqueness.

A file can be marked immutable. An immutable file can’t every be modified again, and can be shared across machines without changing its uid. This is useful for, eg, sharing programs.

◊section{Monitor}

◊em{Monitors: An Operating System Structuring Concept}, ◊om{1974}, by C. A. R. Hoare.

◊em{Experience with Processes and Monitors in Mesa}, ◊om{1980}.

◊em{Monitor} is a synchronization concept. Think of it as a class that manages some resource and synchronizes automatically. In C, you would manually create a mutex and lock/unlock it; in Java, you just add some keyword in front of a variable and the runtime creates and manages the lock for you.

The Hoare paper introduced the concept and gave a bunch of examples. The Mesa paper describes how did they implement and use monitors in Mesa. If you recall, Mesa is the system and application language for Pilot.

Pilot uses monitors provided by Mesa to implement synchronization in the kernel, another example of the tight coupling of Pilot and Mesa.

I have some notes on the differences between Mesa’s monitors and Hoare’s monitors, but they aren’t very interesting. Basically Mesa folks needed to figure out a lot of details for using monitors for Pilot, eg, nested wait, creating monitor, handling exceptions in monitor, scheduling, class level vs instance level, etc.

◊; The Mesa paper explores three ways to deadlock:

◊; ◊ol{
◊;   ◊lo{Two processes in a single monitor do a wait, expecting to be waked by each other. This is usually easy to locate and correct according to the paper.}
◊;   ◊li{Cyclic call between two monitors: monitor M calls an entry procedure in monitor N, which calls an entry procedure in M. To avoid it, just never have circular dependency between monitors.}
◊;   ◊li{If M call N, nd N waits for a condition which can only occur when another process entres N though M which makes the condition true.}
◊; }

◊em{Priority inversion} is when a low priority processes holds the lock, preventing a high priority process from running because the high priority process needs to acquire the lock. This is a big issue in real-time systems: maybe an emergency handler needs to run immediately but can’t acquire a lock. Priority inversion is usually solved by promoting the low priority process to run and release the lock.

Mesa folks didn’t use mutual monitors between devices. If two devices with substantial difference in processing speed shares a monitor, the fast device could be slowed down by waiting for the slower device to finish its critical section.

◊section{V Kernel}

◊em{The Distributed V Kernel and its Performance for Diskless Workstations}, ◊om{1983}.

Papers we’ve read up to this point are more of a description of the system the author’s built, their experiences and lessons learned. Back in the day, professors and their grad students work together to build an awesome and cutting-edge system, and journals invite them to write down their thoughts and experiences.

This paper is a bit different, it uses performance measurements to argue a claim: The conventional approach to build a distributed workstation is to use a small local disk as cache and use specialized protocols. This papar tries to build a distributed workstation without local disks (hence diskless) and with generic message-based ◊sc{ipc}, and argue that the overhead added by this two decisions are ok.

The paper introduces V message, it is synchronous (request and reply), has a small message size (◊om{32} bytes), and has separate control data messages. Thought they also have a control+data message (ReplyWithSegment), presumably to squeeze out some performance.

They used a various of measures to reduce the overhead. They put everything into the kernel, including the file server. They didn’t use ◊sc{tcp} but used Ethernet frames directly. There is no separate ◊sc{ack} message, instead ◊sc{ack} is implied by a response.

The paper analyzed what network penalty consists of. When you send a message from one host to another, it goes from the ◊sc{ram} to the network interface, then is transferred on wire to the destination interface, then copied into ◊sc{ram}. Their argument is that message layer doesn’t add much overhead comparing to the base network penalty. They also argued that remote file access adds small overhead comparing to already-slow disk access.

Overall, their arguments aren’t without flaws. For example, they argue that there is no need for specialized message protocol, but their protocol ends up specializing. They also argued that no streaming is needed, but large data packet are effectively streaming.

◊section{Sprite}

◊em{The Sprite Network Operating System}, ◊om{1988}.

Sprite is another distributed system. It tries to use large memory cache to improve file access, tries to be transparent, giving the user the illusion of a local system. It also has a very cool process migration feature, even though process migration was never adopted by the industry.

At the time, several trends influenced Sprite’s design. Distributed system was very popular (at least in academia); memories are getting larger and larger; and more and more systems are featuring multiple processors.

To present the illusion of a local file system, Sprite uses ◊em{prefix tables}. A prefix is a path prefix. When the userspace accesses a file, the kernel looks at the prefix table, entries in the prefix table point to the local filesystem, or to a remote filesystem. If it points to a remote filesystem, the kernel makes ◊sc{rpc} calls to the remote host, when ends up reaching the local filesystem of the remote host. Prefix table isn’t only used by distributed system, though. Any ◊sc{os} that uses file paths will cache the directories and files it reads in a prefix table.

With cache, the biggest problem is consistency: if two clients get a file and stored it in their cache, and both write to their cache, you have a problem. If only one writer is allowed at a time and the system tracks the last writer of every file. Files are versioned. When a client needs to read a file, it finds the last write and requests the file from it. This is ◊em{sequential write-sharing}.

If multiple clients needs to write the same file (◊em{concurrent write-sharing}), Sprite just turns off caching. This is rare enough to not worth complicating the system. (And you probably need substantially complication to handle this...)

◊section{Grapevine}

◊em{Experience with Grapevine: The Growth of a Distributed System}, ◊om{1984}.

A classic paper in distributed systems, even considered the ◊sc{multics} of distributed systems by some. Grapevine is a distributed email delivery and management system, provides message delivery, naming, authentication, resource location, access control—you name it.

The main takeaway is the experience they got from running Grapevine. To support scaling, the cost of any computation/operation should not grow as the size of thet system grows. But on the other hand, sometimes you can afford to have complete information—maybe that information can never get too large, regardless of how large the system grows.

◊; The general design of their system is pretty good and held up well, although they did encountered some problems. Their system supports mailing lists, and the load grew with user population; frequency of adding/removing user also grew as the user population grew. They solved the mailing list problem by adding another layer of indirection, adding sublists.

Grapevine generally tries to hide the distributed nature of the system, but that caused some problem. First of all, they can’t really hide everything: update in the sytem takes time to propagate, and sometimes users get duplicated messages, all of which are confusing for someone accustomed to the mail service on time-sharing systems. More importantly, user sometimes needs to know more information of the underlying system to understand what’s going on: when stuff doesn’t work, people want to know why. For example, removing an inbox is an expensive operation and removing a lot of them in the same time could overload the system. System administrators needs to understand this, and to understand this they to understand roughly how the system works under the hood.

The lesson is, complete transparency is usually note possible, and often not a good decision either. When you design a system, it is important to decide what to make transparent and what not to.

Finally, the paper mentioned some considerations about managing the system. Maintaining a geographically dispersed system involves on-site operators, who need to be able to carry out operations with little to no understanding of the underlying system, and system experts, who are in short supply and almost always remote from almost all servers. Grapevine has remote monitoring and debugging features so an expert can diagnose and repair a server remotely.

◊fig{
  ◊image["./grapevine.jpg"]{The system structure of Grapevine.}
  ◊figcap{The system structure of Grapevine.}
}


◊section{Global memory}

◊em{Implementing Global Memory Management in a Workstation Cluster}, ◊om{1995}.

This paper is purely academic, but pretty cool. They built a cluster that shares physical memory at a very low level, below ◊sc{vm}, paging, file-mapping, etc. This allows the system to utilize the physical memory much better, and allow more file-caching, which is beneficial since the ◊sc{cpu} was becoming much more faster than the disk.

Each node in the cluster divides their memory into ◊em{local memory} and ◊em{global memory}. Local memory stores pages requested by local processes; global memory stores pages in behave of other nodes in the cluster.

When a fault occurs on a node P, one of four things could happen.

◊ol{
  ◊li{
    If the requested page is in the global memory of another node Q, P uses a random page in its global memory to trade the desired page with Q. (See illustration 1.) ◊; P has one more local page and one less global page; Q has the same number of local and global pages.
  }
  ◊li{
    If the requested page is in the global memory of another node Q, but P doesn’t have any page in its global memory, P use the ◊sc{lru} (least-recently used) local page to trade with Q.
  }
  ◊li{
    If the requested page is on local disk, read it into P’s local memory, and evict the oldest page in the ◊em{entire cluster} to make room for the new page. If the oldest page is on P, evict that; if the oldest page is on a node Q, evict the page on Q, and send a page of P to Q. Send a random global page on P, or the ◊sc{lru} local page of P if there is no global page on P, just like in case 1 and 2. (See illustration 2.)
  }
  ◊li{
    If the requested page is a local page of another node Q, duplicate that page into the local memory of P, and evict the oldest page in the entire cluster. Again, if the oldest page is on another node R, send one of P’s global pages or P’s ◊sc{lru} page.
  }
}

◊fig{
  ◊image["./global-memory-1.jpg"]{Illustration of page exchange in case 1.}
  ◊figcap{Illustration 1: Page exchange in case 1.}
}

◊fig{
  ◊image["./global-memory-2.jpg"]{Illustration of page exchange in case 3.}
  ◊figcap{Illustartion 2: Page exchange in case 3.}
}

This whole dance can improve performance of memory-intensive tasks because fetching a page from remote memory is about two to ten times faster than disk access. However, local hit is over three magnitudes faster than fetching remote memory, so the algorithm has to be very careful not to evict the wrong page.

This algorithm is nice and all, but how do memory management code running on each node know which page is the oldest page in the entire cluster?

In the best but impossible scenario, the system is truly managed by a single entity, a central controller; the controller keeps track of every single page’s age and tells each node which node to evict. Of course, this is impossible because that’s way too slow, the controller has to be running at a much faster speed than the other nodes and the communication speed between nodes must be very  fast.

Instead, each node must make local independent decisions that combines to achieve a global goal (evict the oldest page). The difficulty is that local nodes usually don’t have complete, up-to-date information.

The solution to this kind of scenario is probability-based algorithm. We don’t aim to make the optimal decision for every single case, but use probability to approximate the optimal outcome, with the incomplete and out-of-date information each node does know.

We divide time into epochs, in each epoch, the cluster expects to replace ◊em{M} oldest pages. (◊em{M} is predicted from date from previous epochs.) At the beginning of each epoch, every node sens a summary of its pages and their age to a ◊em{initiator node} (central controller). The initiator node sorts all the pages by their age, and find ◊em{W} = the set of ◊em{M} oldest pages in the cluster. Then, it assigns each node ◊em{i} a weight ◊em{w◊sub{i}}, where ◊em{w◊sub{i}} is

◊; I want this to work equally well in RSS and screen reader.
◊; ◊div{
◊;   ◊frac-num{◊em{the number of old pages in W that are in node i}}
◊;   ◊frac-denom{◊em{W}}
◊; }

◊image["./global-memory-frac.png"]{A math expression: the number of old pages in W that are in node i, divided by W.}

Basically, ◊em{w◊sub{i}} means “among the ◊em{M} oldest pages in the cluster, how many of them are in node ◊em{i}”.

The initiator node tells each node every node’s weight, and when a node P encounters case 3 or 4 and wants to evict “the oldest page in the cluster”, it randomly picks a node by each node’s weight, and tells that node to ◊fnref["global-mem-tlb"]{evict its oldest page}.

◊fndef["global-mem-tlb"]{
  Actually, tracking page age isn’t that simple. For one, in a mmaped file, memory access bypasses pagefault handler and goes straight to the ◊em{tlb}. More importantly, the ◊sc{os} uses ◊sc{fifo} second-chance page caching and hides many page request/eviction from their memory manager, because the memory manager runs at a lower level (presumably in pagefault handlers).
  
The authors resorted to hacking the ◊sc{tlb} handler of the machine with PALcode (microcode). This would’ve been impossible on x86—it’s ◊sc{tlb} is handled purely in hardware.
}

Probability-based algorithms sometimes feels outright magical—they seem to just bypass trade-offs. In reality, they usually just add a new dimension to the trade-off. We’ll see this again later in lottery scheduling.

◊section{μ-kernel}

◊em{The Performance of μ-Kernel-Based Systems}, ◊om{1997}.

This paper is a measurement paper. It uses benchmarks to argue that a) micro kernel can deliver comparable performance, and b) the performance doesn’t depend on a particular architecture.

The authors built a micro kernel L4, and ported Linux to run on it (called L⁴Linux). And they ported L4 from Pentium to Alpha and ◊sc{mips} architecture, to show that L4 is architecture-independent. They also conducted some experiment to show L4’s extensibility and performance.

The paper considers micro kernels like Mach and Chrous to be first-generation, which evolved from earlier monolithic kernels. Later kernels like L4 and ◊sc{qnx} are considered second-generation; they are designed more rigorously from scratch, ie, more “pure”.

L4 allows user programs to control memory allocation, like nucleus did: kernel manages top-level tasks’ memory, top-level tasks manages their children’s memory. And scheduling? Hard priorities with round-robin scheduling per priority, not unlike nucleus.

L⁴Linux only modifies the architecture-dependent part of Linux, ie, not hacky. The authors also restricted themselves to not make any Linux-specific change to L4, as a test of the design of L4. The result is not bad. In micro benchmarks, L⁴Linux is ×◊om{2.4} times slower than native Linux; in macro benchmarks, L⁴Linux is about ◊om{5–10%} slower than native Linux. More over L⁴Linux is much faster Linux running on top of other micro kernels, like MkLinux (Linux + Mach 3.0).

The paper also mentions supporting tagged ◊sc{tlb}s. Normal ◊sc{tlb} needs to be flashed on context switch, which is a main reason why context switch is expensive. But if you tag each entry in the ◊sc{tlb} with a tag to associate that entry with a specific process, then you don’t need to flush ◊sc{tlb} anymore. Tagged ◊sc{tlb} needs some form of software-managed ◊sc{tlb}, after all, hardware don’t know about processes.

The benefit of micro kernels is of course the extensibility. For example, when a page is swapped out, instead of writing to disk, we can swap to a remote machine, or encrypt the page and write to disk, or compress the page and write to page, etc. A database program could bypass the filesystem and file cache, and control the layout of data on physical disk for optimization; it can control caching and keep pages in memory and not swapped out.

All of these are very nice perks, and the performance doesn’t seem too bad, then why micro kernels are still not popular? My professor has a very convencing argument: the big companies can just hire kernel developers to ◊fnref["mu-kernel-linux"]{modify Linux to their need}, smaller companies don’t have special requirements for the ◊sc{os} and can just use Linux. That leaves only the companies in the middle: have special requirements, but don’t want to modify Linux. And don’t forget that extending micro kernel is still work, it might be easier than modifying Linux, but how much easier? If there are a lot of Linux kernel developers, perhaps modifying Linux is more economical. Eg, Nintendo Switch and Playstation use their modified ◊sc{bsd}, and Steam Deck is built on top of Linux.

◊fndef["mu-kernel-linux"]{And they did. Since the paper has been written, Linux has added many features of L4 described in the paper.}

Beyond monolithic and micro kernel, there are many other designs for kernel: hybrid, exokernel, even virtual machines. Hybrid kernels include Windows ◊sc{nt}, NetWave, BeOS, etc. Eg, they can leave ◊sc{ipc}, driver, ◊sc{vm}, and scheduling in the kernel, but put filesystem in the userspace.

◊section{Exokernel}

◊em{Exokernel: An Operating System Architecture for Application-Level Resource Management}, ◊om{1997}.

The idea is to go a step further than micro kernels and make the kernel into a library, the kernel exposes hardware resources and provide multiplexing and protection, and leaves management to the application. The motivation is that traditional kernel abstraction hides key information and obstructs application-specific optimizations.

This idea can be nicely applied to single-purpose applicants, when the whole purpose of a machine is to run a single application, eg, a database, a web server, or a embedded application. In this case, thing that a traditional kernel provides: users, permissions, fairness are all unnecessary overhead. ◊link["https://dl.acm.org/doi/10.1145/2490301.2451167"]{Unikernel} explored exactly this use-case.

Exokernel exports hardware resources and protection, and leaves management to the (untrusted) application. Applications can request for resources and handle events securely by ◊em{secure bindings}. Each application cooperatively share the limited resources by participating in a ◊em{resource revocation} protocol, ie, the exokernel might tell an application to release some resources for others to use. Finally, the exokernel can forcibly retract resources held by uncooperative applications by the ◊em{abort protocol}.

Exokenel doesn’t provide many of the traditional abstractions, like ◊sc{vm} or ◊sc{ipc}, those are left for the application to implement.

The protection provided by an exokernel is inevitably weaker: an application error could corrupt on-disk data; and because the kernel and application runs in the same ◊sc{vm}, application error could corrupt kernel!

The existence of abort protocol kind of breaks the “no management” principle—retracting resources from an application ◊em{is} management.

Finally, their benchmark isn’t very convincing: there are only micro benchmarks and no macro benchmark; they only benchmark mechanism (context switch, exception handler, etc) but not application.

◊section{Xen}

◊em{Xen and the Art of Virtualization}, ◊om{2003}.

Xen is a ◊sc{vmm}, virtual machine monitor, also called hypervisor—the thing sits between an ◊sc{os} and the hardware. The goal of Xen is to be able to run hundreds of guest ◊sc{os}’s in the same time.

Xen provides a virtual machine abstraction (◊em{paravirtualization}) rather than a full virtual hardware (◊em{full virtualization}). Paravirtualization has better performance and gives the monitor more control, but requires modification to the guest ◊sc{os}. On the other hand, full virtualization monitor, for example VMWare, can work with unmodified guest ◊sc{os}.

◊; Virtualization is an old idea, dating back to the early systems, like ◊sc{ibm} 370, ◊sc{vms}, etc.

Nowadays there are a plethora of virtual machine solutions, like VMWare, Hyper-V, VirtualBox, ◊sc{kvm}, Xen, to name a few; on top of that, there are containers like ◊sc{lxc}, docker, etc. They all have different configuration for hardware, host ◊sc{os}, ◊sc{vmm}/container engine, guest ◊sc{os}, and guest app: The ◊sc{vmm} can sit on the host ◊sc{os} or directly on the hardware; you can run one guest ◊sc{os} per app, or run a single guest ◊sc{os} for multiple apps, etc. And on the old ◊sc{ibm} and ◊sc{vms} systems, the ◊sc{vmm} supports both a batch processing ◊sc{os} and an interactive ◊sc{os}.

Let’s look at how does Xen virtualize and how does it compare to VMWare.

Scheduling virtualization: Xen uses the Borrowed Virtual Time (◊sc{bvt}) algorithm. This algorithm allows a guest ◊sc{os} to borrow future execution time to respond to latency-critical tasks.

Instructions virtualization: Boring instructions like ◊code{add} can just pass-through to the hardware, but privileged instructions needs intervention from the monitor.

In Xen, the guest ◊sc{os} is modified so that it is aware of the ◊sc{vmm}, and instead of doing privileged task by itself, the guest ◊sc{os} delegates the work to the ◊sc{vmm} by ◊em{hypercalls}. In VMWare, since they can’t modify the guest ◊sc{os}, privileged instructions simply trap into ◊sc{vmm}. If you remember, we talked about rings in the ◊sc{multics} section. On ◊om{x86}, The ◊sc{cpu} will trap if it’s asked to execute a privileged instruction when in a low ring level.

Memory virtualization: The guest ◊sc{os} isn’t managing physical memory anymore, though we still call it physical memory; under it, the ◊sc{vmm} has real access to the phyiscal memory, often called machine memory.

Then, how is the virtual memory address in the guest ◊sc{os} translated into machine memory address?

In Xen, they modify the guest ◊sc{os}, so the guest ◊sc{os} is aware of the virtualization. It’s page table can map directly from virtual address to machine address, and ◊sc{mmu} can just read off of guest ◊sc{os}’s page table. The ◊sc{vmm} just need to verify writes to the page table to enforce protection.

In VMWare, however, the guest ◊sc{os} is completely unaware of the ◊sc{vmm}, and its page table maps from virtual address only to physical address. Also the guest ◊sc{os} writes to its page table without bothering to notify anyone. To deal with all this, VMWare maintains a shadow page table that maps virtual address to actual machine address. It also use dirty bits to make sure whenever the guest ◊sc{os} writs to the page table, it is notified and can update its shadow page table accordingly. (I forgot exactly how.) And ◊sc{mmu} reads off the shadow page table. (Presumably by trapping to ◊sc{vmm} when the guest ◊sc{os} tries to modify the ◊scom{cr3} register, and let ◊sc{vmm} override ◊scom{cr3} to its shadow page table?)

◊fig{
  ◊image["./xen.jpg"]{Diagram illustrating Xen and VMWare’s memory remapping approach.}
  ◊figcap{Illustration of  Xen and VMWare memory virtualization.}
}

Note that VMWare needs all these complication only because ◊om{x86}’s memory management is completely hardware-based—the kernel can only point the ◊sc{mmu} to the page table and has no other control over the ◊sc{mmu}. Other “higher-end” architectures usually support software-managed and tagged ◊sc{tlb}.

Xen uses a ◊em{balloon driver} to “squeeze” memory out of the guest ◊sc{os} when needed. When the ◊sc{vmm} wants to retract memory from the guest ◊sc{os}, it enlarges the “balloon”, so the guest ◊sc{os} automatically gives up memory. A cute trick.

◊section{VMS}

◊em{Virtual Memory Management in VAX/VMS}, ◊om{1982}.

This paper mainly concerns the implementation of the virtual memory for the ◊sc{vms}. The ◊sc{vms} has to run on a variety of low-end hardware with small memories and slow ◊sc{cpu}’s, and support all sorts of uses: real time, timeshared, and batch.

◊sc{vms}’s virtual memory has three regions: program region, control region and system region. The highest two bits of an address indicates the region, after that are the regular stuff: ◊om{20} bits of virtual page number and ◊om{8} bits of byte offset. The system region is shared by all processes (kernel stack); program and control region are process-specific. The paper mentions a trick they used: they mark the first page in the ◊sc{vm} as no access, so that uninitialized pointers (pointing to ◊code{0x0}) cause an exception. Linux’s ◊sc{vm} layout isn’t much different.

◊sc{vms} uses a process-local page replacement policy. When a process requests for memory that needs to be paged in, kernel swaps out a page from the set of pages currently used by the process (called the ◊em{resident set}). This way a heavily faulting process can only slow down itself.

When a page is removed from the resident set, it doesn’t go out of the memory immediately, but is appended to one of two lists: the free page list if it hasn't been modified, or the modified page list if it has. When kernel needs a fresh page to swap data in, it takes a page from the head of the free list. And when kernel decides to write pages back to paging file (swap file), it takes the page from the head of the modified list. If a page in these two lists are requested again before it moves to the head and is consumed, it is pulled out and put into the requesting process’s resident set. This is basically second chance caching: we keep the page in the memory for a while before really discarding it, in case it is used again soon.

Because ◊sc{vms} uses a relatively small ◊fnref["vms-page"]{◊om{512} byte page size}, pages causes a lot of ◊sc{i/o}, which is obviously not good. To reduce the number of disk operations, they try to read and write several pages at once (they call this clustering).

◊fndef["vms-page"]{To be compatible with ◊scom{pdp-11} and because of the promise of low-latency semiconductor disk technologies (which obviously didn’t come true on time, we’ll see this happen a few more times in other papers ;-).}

The paper also mentions some other nice features, like on-demand zeroed page and copy-on-reference page. On-demand zeroed page are only allocated and zeroed when it’s actually referenced. Similarly, copy-on-reference pages are only copied when it’s actually referenced. I wonder why didn’t they make it copy-on-write though, they say it’s used for sharing executable files.

A fun anecdote: our professor once had a grad student doing their qualifying exam, and one of the council asked: “does the kernel know about every memory access?” The grad student said yes, and it was wrong. The council  member later complained to my professor that a grad student can get it wrong. The kernel only get to know about memory use in the form of pagefault handlers. If there’s no pagefault, the memory access is handled silently by the ◊sc{mmu}.

◊section{Mach}

◊em{Machine-Independent Virtual Memory Management for Paged Uniprocessor and Multiprocessor Architectures}, ◊om{1987}.

Mach was a popular research ◊sc{os}. Our professor did her PhD on Mach’s virtual memory. It actually influenced both Windows and Mac: one of the prominent Mach research went to Windows and worked on Windows ◊sc{nt}, and Mac ◊sc{osx} was Mach plus ◊sc{bsd} plus NextStep.

The main point of this paper is machine-independent ◊sc{vm} and the idea is to treat hardware information (ie, machine-dependent, eg, ◊sc{tlb}) as a cache of machine-independent information.

Unlike ◊sc{vms}, Mach supports sparse address. Its page table is a sorted doubly linked list of ◊em{virtual regions}. Each virtual region stores some machine-independent info like address range, inheritance (we’ll come back to it later), protection, and some machine-dependent cache info. The machine-dependent part is a cache and can be re-constructed from the machine-independent info.

Each virtual region maps a range of virtual addresses to a range in a ◊em{memory object}. A memory object is an abstraction over a piece of data/storage, and can even be remote data, I think?

A memory object is associated with a pager, which handles pagefault and page-out requests. This pager is outside of the kernel and is customizable. So it can do elaborate things like encrypted memory, etc.

Copy-on-write is implemented by creating a shadow memory object, which only contains the written page. The kernel will look for the unmodified page in its original object. They are just like keymaps in Emacs. Shadow memory objects themselves can be shadowed, and large chains of shadow objects will manifest, Mach had to garbage collect intermediate shadow objects when the chain gets long. Reading from the paper, this is probably quite an annoyance to the designers.

When a task inherits memory from its parent task, the parent can set the inheritance flag of any page to one of ◊em{shared} (read-write), ◊em{copy} (copy-on-write), or ◊em{none} (no access). This would be 
 helpful for sandboxing.

◊section{FFS}

◊em{A Fast File System for UNIX}, ◊om{1984}.

This paper literally describes a faster file system they implemented for ◊sc{unix}. It was widely adopted.

The author identifies a series of shortcomings of the default file system of ◊sc{unix}: 

The free list (linked list storing all free blocks) starts out ordered, but over time becomes random, so when the file system allocates blocks for files, those block are not physically continuous but rather scatter around.

The inodes are stored in one place, and the data (blocks) another. File operations (list directory, open, read, write) involve editing meta information interleaved with writing data, causing long seeks between the inodes and the blocks.

The block size is 512 bytes, which is too small and creates indirection and fragmentation. Smaller block size also means it takes more disk transactions to transfer the same amount of data.

All these combined renders the default file system only able to produce ◊om{2%} of the full bandwidth.

◊sc{ffs} improves performance by creating locality as much as possible. It uses a larger block size, decides a disk partition into ◊em{cylinder groups}. Each cylinder groups has its own (duplicated) superblock, inodes, and a free list implemented with a bitmap. This way inodes and data blocks are resonably close to each other. Each cylinder has a fixed number of inodes.

◊sc{ffs} uses a smart allocation policy when allocating blocks for files and directories. Eg, it tries to place inodes of files in the same directory in the same cylinder group; it places new directories in a cylinder group that has more free inocdes and less excising directories; it tries to place all data blocks of a file in the same cylinder group; etc.

Larger block size wastes space, because most ◊sc{unix} systems are composed of many small files. ◊sc{ffs} allows a block to be splitted into ◊em{fragments}. A block can be broken into 2, 4, or 8 fragments. At the end, the author claims that ◊sc{ffs} with 4096-byte blocks and 512-byte fragments has about the same  disk utilization as the old 512-byte block file system.

◊sc{ffs} also requires some percent of free space to maintain it’s performance. Because when the disk is too full, it’s hard for ◊sc{ffs} to keep the blocks of a file localized. ◊sc{ffs} performs best when there are around ◊om{10%} of free space.

To maximally optimize the file system, ◊sc{ffs} is parameterized so it can be tuned according to the physical property of the disk (number of blocks on a track, spin speed), processor speed (speed of interrupt and disk transfer), etc. Two physically consecutive blocks on the disk can’t be read consecutively because there’s some processing time when reading a block of data. ◊sc{ffs} can calculate the number of blocks to skip according to processor speed and spin speed, such that when the ◊sc{os} finished reading one block, the next block of the file comes into position right under the disk head.


◊section{LFS}

◊em{The Design and Implementation of a Log-Structured File System}, ◊om{1991}.

When the paper came out, it stirred quote some controversy on ◊sc{lfs} vs extent-based ◊sc{ffs}. The main complain for ◊sc{lfs} is that it needs garbage collection.

The main idea of ◊sc{lfs} is to buffer writes in the file cache and writing them all at once sequentially. As for read, since now machines have large ◊sc{rams}, file cache should ensure read is fast.

This approach solves several shortcoming os ◊sc{ffs}. In ◊sc{ffs}, even though inodes are close to the data, they are still separate and requires seeking when writing both. The same goes for directories and files. The typical work load of the filesystem alternates between metadata and data, producing a lot of separate small writes. It doesn’t help that most of the files are small, and most writes are writing metadata. And while data write is asynchronous, metadata write is synchronous.

◊sc{lfs} consinders the whole disk as an append-only log. When writing a file, the filssytem just appends what it wants to write to the end of the log, followed by the new inode, pointing to the newly written blocks, followed by the new inode map, pointing to the newly written inode. And the inode map copied in the memory for fast access. Writes are done in batches, every batch appends new inodes if the file is edited, and appends a new inode map at the end.

To read, we look into the inode map, find the inodes, and read the inode to find the blocks (that scatter around the log), and piece together the part we want to read.

Clearly, reading is not very optimized in ◊sc{lfs}, another challenge of ◊sc{lfs} is garbage collection. ◊sc{lfs} devides the disk into ◊em{segments}, each consisting of a number of blocks. ◊sc{lfs} only writes in a segment if it’s completely free. During garbage collection, ◊sc{lfs} copies the live blocks in a segment to the end of the log, and reclaim the segment. The challenge is to choose the best segment to clean.

The authors first tried to clean least utilized segment first, ie, chose the segment with the least amount of live data. This didn’t go well, because segments don’t get cleaned until they cross the threshold, and a lot of segments lingers around the threshold, don’t get cleaned, and holds up a lot of space.

The authors found that it’s best to divide segments into hot and cold segments. Hot segments are the ones actively updated, where blocks are actively marked unlive. Cleaning hot segments aren’t  very valuable, since even if we don’t clean it, more and more of its blocks will become unlive and thus free by themselves. On the other hand, cold segments are valuable to clean, since it’s unlikely/slow to free up blocks by itself.

The authors also mentioned some crash recovery and checkpoint mechanism in the paper.

◊section{Soft update}

◊em{Soft Updates: A Solution to the Metadata Update Problem in File Systems}, ◊om{2000}.

In ◊sc{LFS} we mentioned that metadata edit requires synchronize writes. That’s because you want to ensure the data on disk (or any persistent storage) is always consistent. If the system writes only a partial of the data it wishes to write, then crashed, the disk should be in a consistent or at least recoverable state. For example, when adding a file to a directory, adding the new inode must happen before adding the file entry to the directory.

Folks has long sought to improve the performance of updating metadata, this paper lists several existing solutions.

◊dl{
◊dt{
Nonvolatile ◊sc{ram} (◊sc{nvram})
}
◊dd{
Use ◊sc{nvram} to store metadata. Updating metadata is as fast as accessing ◊sc{ram}, and it persists.
}
◊dt{
Write-ahead logging
}
◊dd{
Ie, journaling. The filesystem first log the operation it’s about to perform, and performs it. If a crash happens, the filesystem can recover using the log.
}
◊dt{
Scheduler-enforced ordering
}
◊dd{
Modify disk request scheduler to enforce synchronous edit of metadata. Meanwhile, the filesystem is free to edit metadata asynchronously (since the disk request scheduler will take care of it)
}
◊dt{
Interbuffer dependencies
}
◊dd{
Use write cache, and let the cache write-back code enforce metadata ordering.
}
}

Soft update is similar to interbuffer dependencies. It maintains a log of metadata updates, and tracks dependencies at a fine granularity (per field or pointer), and can move the order of operations around to avoid circular dependencies, and subsequently group some updates together and make less writes.

◊section{Rio}

◊em{The Rio File Cache: Surviving Operating System Crashes}, ◊om{1996}.

So the main point of Rio (◊sc{ram/Io}) is to enable memory to survive crashes such that the ◊sc{os} doesn’t need to write the cache to persistent storage all the time.

Power outages can be solved by power supply with battery and dumping memory to persistent storage when power outage occurs; or use persistent memory. Then when rebooting, the ◊sc{os} goes through the dumped memory file to recover data (file cache). The authors call this “warm reboot”.

System crash is the main challenge, because kernel crash can corrupt the memory. The authors argue that the reason why people consider persistent storage to be reliable and memory to be unreliable is because of their interface: writing to disk needs drivers and explicit procedures, etc, while writing to memory only takes a mov instruction.

Then, protecting the file cache is just a matter of write-protecting the memory. For which a myriad of techniques are available. For example, you can use protection virtual memory already provides. Just turn off the write-permission bits in the page table for file cache pages. The problem is that some systems allow kernel to bypass virtual memory protection. In that case, the authors resorts to disabling processor’s ability to bypass ◊sc{tlb}. This is of course, architecture-dependent.

Another way is to install checks for every kernel memory access, but that’s a heavy penalty on the performance.

What’s more interesting is perhaps the effect of having a reliable memory on the filesystem. First, you can turn off reliable sync writes (the motivation for this paper in the first place). But also, since memory is now permanent,  metadata updates must be ordered, so that a crash in the middle of an operation doesn’t create an inconsistent state.

Professor also mentioned the development of persistent memory. Now persistent memory is getting larger than cheaper to the point that it seems possible to use it to improve ◊sc{io} performance in datacenters.

Problem is, every update has to be ordered, and you can’t control L1 L2 L3 cache. They can decide to write to memory at different orders than you intended.

Currently there are two approaches: treat the persistent memory as a super fast ◊sc{ssd}, and slap a filesystem on it, the filesystem will take care of the dirty work. The other camp wants to unlock the full potential of persistent memory, don’t want to pay for the overhead of a filesystem, and want to use it as a memory. To go this route, the programmer have to deal with the complications of consistency/ordering.