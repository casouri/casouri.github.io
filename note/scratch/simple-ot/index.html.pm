#lang pollen

◊define-meta[date]{<2023-05-01 Mon 12:26>}
◊define-meta[uuid]{55b0d836-7166-11ee-9d74-c7998ffeb66c}
◊define-meta[tags]{Programming}
◊define-meta[lang]{en}

◊meta{
  ◊title{A Simple OT algorithm}
  ◊subtitle{With tombstones, I’m afraid}
}

This article is for those who want to build a collaborative algorithm from scratch, learned a bit about ◊sc{ot} vs ◊sc{crdt}, but struggles to find concret description of real-world implementations to get started. (Basically, me when I started my project.)

I will introduce the ◊sc{ot} algorithm I used for my collaboration editing plugin, and explain the reason behind some decisions. I’ll also touch on things that I learned from reading papers and articles, and trying to implement a working algorithm for a real-world editor.

◊section{Goals}

◊section{Handling DO}

◊section{Handling UNDO}