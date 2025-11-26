#lang pollen

◊define-meta[date]{<2025-11-08 Sat 23:46>}
◊define-meta[uuid]{2c25cecc-bd40-11f0-8062-ab5b5d3e5f23}
◊define-meta[tags]{Misc}
◊define-meta[lang]{en}

◊meta{
  ◊title{Monty Hall Problem}
}

When I first heard about Monty Hall problem, I found it very hard to convince myself of the solution. And when I encountered it again a few days ago, lo and behold, I fell for it again. This time I want to make sure I’m absolutely crystal-clear convinced, so I won’t fall for it yet again.

Cognitive psychologists think people fell for the problem because of endowment effect, status quo bias, and errors of omission vs errors of commission effect. I think they’re full of shit. I think the reason why people fall for it, even educated ones, is that the correct solution looks damn like a statistics fallacy sold with snake oil. Even the follow-up explanation that’s supposed to clear it up looks so suspicious:

◊bquote{
  Suppose there are a million doors, and you pick door ◊om{#1}. Then the host, who knows what’s behind the doors and will always avoid the one with the prize, opens them all except door ◊om{#777,777}. You’d switch to that door pretty fast, wouldn’t you?
}

And people who are convinced after reading it looks like they’re tricked. Yeah, the host knows which door has the car and have to avoid it, so they added information by choosing to open all the doors except ◊om{#777,777}. But how does that affect the odd of the ◊om{#777,777}?

To make it more concrete and finally convince myself, let’s go over this one million door analogy with numbers. Suppose there are ◊om{100} doors, one of them have a car behind, and the rest have goats behind. You randomly pick a door. The host, who knows which door has the car behind and can only open doors with a goat, opens ◊om{98} doors.

Now let’s think about the probability of the two remaining doors of having a car behind. Let the door you picked be M, let the other closed door be N. You picked M randomly, so the probability for M to have a car is ◊om{1/100}. For N, before the host opens any doors, the probability for N to have a car is also ◊om{1/100}.

After the host opened ◊om{98} doors, the probability of M to have a car is still ◊om{1/100}. That means the probability of the rest of ◊om{99} doors having the car is ◊om{99/100}. Conveniently, the host has eliminated ◊om{98} of them. So if there is a car in these ◊om{99} doors, it has to be in N. So picking N is the same as picking the rest of 99, and the probability for N to have the car is ◊om{99/100}.

Basically, choosing between switch and non-switch is like choosing between ◊om{99} doors and  1 door. If you choose the ◊om{99} doors and the car is in one of them, the host basically just tells you which door has the car.

Apologies to cognitive psychologists, they’re obviously not full of shit. But it’s funny to said it like that.