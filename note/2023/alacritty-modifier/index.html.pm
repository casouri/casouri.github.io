#lang pollen

◊define-meta[date]{<2023-02-24 Fri 22:53>}
◊define-meta[uuid]{19af9006-b4d9-11ed-aec6-9fe7ebf47538}
◊define-meta[tags]{Tech}
◊define-meta[lang]{en}

◊meta{
  ◊title{Remap modifiers in Linux Desktop and Alacritty}
  ◊subtitle{Mac bindings in Linux, the ultimate solution}
}

I’m used to macOS’s key binding, that means for a desktop environment, I want three things:

◊ol{
  ◊li{Caps lock act as Control}
  ◊li{System bindings are on the Command key (ie, the Windows key), specifically, Command+C/V for copy/paste}
  ◊li{In the terminal emulator, Command+C/V works as usual, and Ctrl+C/V sends respective control codes, as usual}
}

I’m a simple man, and this is all I want, but Thy Voice From Above hath spoken: ◊em{“lol no think again”}

◊section{Command+C/V for copy and paste}

Remapping Caps lock to Control is easy and there are plenty tutorials online for it. However, there is ◊em{absolutely no way} to change the default bindings of copy/paste on a Linux desktop reliably. Because there is simply no unified configuration for the keybinding of copy & paste. Qt supports rebinding copy & paste and Gtk straight up ◊fnref["gtk"]{doesn’t support it}. On top of that, applications bind their own keys and completely disregard the toolkit’s setting, except in some toolkit widgets they use, then you have different bindings within the same application.

◊fndef["gtk"]{Gtk 3 seems to support it through ◊sc{css} themes, which is removed in Gtk 4. Anyway, I never got it to work.}

The whole situation is pretty laughable, but live must go on. There are things like ◊fnref["xkeysnail"]{xkeysnail} that literally intercepts every keystroke you type and translate them into other keys depending on the application currently in focus. It requires some nontrivial configuration and may or may not work reliably on X11, ◊fnref["wayland"]{definitely doesn’t work on Wayland}, and I don’t know how do I feel about a Python program running as root, intercepting and translating every key I type. There are Rust alternatives, but I didn’t have much luck with those either.

◊fndef["xkeysnail"]{◊link["https://github.com/mooz/xkeysnail"]{xkeysnail}. There are also projects like ◊link["https://github.com/rbreaves/kinto"]{kinto.sh} that pre-configures it for you on both Linux and Windows. (On Windows it uses AutoHotkey.)
}

◊fndef["wayland"]{These type of program use X11 protocol, and Wayland just doesn’t support program intercepting and translating other program’s input.}

The real way, the only good way, to do it is to just swap Control with Super (ie, Command) at X11 level. (Wayland picks it up so it works on Wayland too, or so I’m told). Since we also want to swap Caps lock and Control, we actually do a three-way swap:

◊ul{
  ◊li{Super → Control}
  ◊li{Control → Caps lock}
  ◊li{Caps lock → Super}
}

So now when you press Command+C, the application gets Control+C.

To actually swap the modifiers, we edit

◊mono{/usr/share/X11/xkb/keycodes/evdev}

and reboot—no adding command to X init or some config file or some other crap. You edit the file, reboot, and it works, and keeps working. I learned this from a ◊link["https://askubuntu.com/questions/929744/how-to-remap-key-in-ubuntu-17-10-wayland-up-key-to-shift"]{StackExchange question}.

Below are the exact edit you need to make in that file, and their effect:

To map Left Control (keycode 37) to Caps lock:
Change ◊code{<CAPS> = 66} to ◊code{<CAPS> = 37}

To map Left Super (keycode 133) to Control:
Change ◊code{<LCTL> = 37} to ◊code{<LCTL> 133}

To map Caps lock (keycode 66) to Left Super:
Change ◊code{<LWIN> = 133} to ◊code{<LWIN> = 66}

◊; ◊ul{
◊;   ◊li{Left Control (keycode 37) → Caps lock:
◊;     Change ◊code{<CAPS> = 66} to ◊code{<CAPS> = 37}
◊;     }
◊;   ◊li{Left Super (keycode 133) → Control:
◊;     Change ◊code{<LCTL> = 37} to ◊code{<LCTL> 133}
◊;   }
◊;   ◊li{Caps lock (keycode 66) → Left Super:
◊;     Change ◊code{<LWIN> = 133} to ◊code{<LWIN> = 66}
◊;     }
◊; }

If you use Emacs, you need to swap Super and Control back. Add this to your ◊mono{init.el}:

◊bcode{
  (setq x-super-keysym 'ctrl)
  (setq x-ctrl-keysym 'super)
}

◊section{Command+C/V in terminal}

Now Command+C/V works in normal applications, but in terminal, Caps lock+C/V (appears as Super+C/V) will not send control keys and Command+C/V (appears as Control+C/V) will not do what you want—again, you need to swap Super and Control back, as we did for Emacs.

I looked at every terminal emulator on Linux, and ◊link["https://github.com/alacritty/alacritty"]{Alacritty} is the only one that allows remapping modifier keys, has sane configuration so that I can actually configure the remap, and has sane dependencies.

You want to remap all Control+◊em{x} keys to simply ◊em{x}, except for Control+C/V/F, etc, which are bind to actions like Copy, Paste, SearchForward. And you want to remap all Super+◊em{x} keys to Control+◊em{x}. In effect, you have:

◊ul{
  ◊li{Command+C/V → Control+C/V → Copy/Paste}
  ◊li{Caps lock+C/V → Super+C/V → Control+C/V}
}

To do that, add this to the beginning of ◊mono{~/.config/alacritty/alacritty.yml}:

◊bcode{
key_bindings:  
  - { key: At, mods: Control, chars: "@" }
  - { key: A, mods: Control, chars: "a" }
  - { key: B, mods: Control, chars: "b" }
  - { key: C, mods: Control, action: Copy }
  - { key: D, mods: Control, chars: "d" }
  - { key: E, mods: Control, chars: "e" }
  - { key: F, mods: Control, action: SearchForward }
  - { key: F, mods: Control, mode: ~Search, action: SearchForward }
  - { key: F, mods: Control|Shift, action: SearchBackward }
  - { key: F, mods: Control|Shift, mode: ~Search, action: SearchBackward }
  - { key: G, mods: Control, chars: "g" }
  - { key: H, mods: Control, chars: "h" }
  - { key: I, mods: Control, chars: "i" }
  - { key: J, mods: Control, chars: "j" }
  - { key: K, mods: Control, chars: "k" }
  - { key: L, mods: Control, chars: "l" }
  - { key: M, mods: Control, chars: "m" }
  - { key: N, mods: Control, action: CreateNewWindow }
  - { key: O, mods: Control, chars: "o" }
  - { key: P, mods: Control, chars: "p" }
  - { key: Q, mods: Control, action: Quit }
  - { key: R, mods: Control, chars: "r" }
  - { key: S, mods: Control, chars: "s" }
  - { key: T, mods: Control, chars: "t" }
  - { key: U, mods: Control, chars: "u" }
  - { key: V, mods: Control, action: Paste }
  - { key: W, mods: Control, action: Quit }
  - { key: X, mods: Control, chars: Cut }
  - { key: Y, mods: Control, chars: "y" }
  - { key: Z, mods: Control, chars: "z" }
  - { key: LBracket, mods: Control, chars: "[" }
  - { key: Backslash, mods: Control, chars: "\\" }
  - { key: RBracket, mods: Control, chars: "]" }
  - { key: Grave, mods: Control, chars: "^" }
  - { key: Underline, mods: Control, chars: "_" }

  - { key: At, mods: Super, chars: "\x00" }
  - { key: A, mods: Super, chars: "\x01" }
  - { key: B, mods: Super, chars: "\x02" }
  - { key: C, mods: Super, chars: "\x03" }
  - { key: D, mods: Super, chars: "\x04" }
  - { key: E, mods: Super, chars: "\x05" }
  - { key: F, mods: Super, chars: "\x06" }
  - { key: G, mods: Super, chars: "\x07" }
  - { key: H, mods: Super, chars: "\x08" }
  - { key: I, mods: Super, chars: "\x09" }
  - { key: J, mods: Super, chars: "\x0a" }
  - { key: K, mods: Super, chars: "\x0b" }
  - { key: L, mods: Super, chars: "\x0c" }
  - { key: M, mods: Super, chars: "\x0d" }
  - { key: N, mods: Super, chars: "\x0e" }
  - { key: O, mods: Super, chars: "\x0f" }
  - { key: P, mods: Super, chars: "\x10" }
  - { key: Q, mods: Super, chars: "\x11" }
  - { key: R, mods: Super, chars: "\x12" }
  - { key: S, mods: Super, chars: "\x13" }
  - { key: T, mods: Super, chars: "\x14" }
  - { key: U, mods: Super, chars: "\x15" }
  - { key: V, mods: Super, chars: "\x16" }
  - { key: W, mods: Super, chars: "\x17" }
  - { key: X, mods: Super, chars: "\x18" }
  - { key: Y, mods: Super, chars: "\x19" }
  - { key: Z, mods: Super, chars: "\x1a" }
  - { key: LBracket, mods: Super, chars: "\x1b" }
  - { key: Backslash, mods: Super, chars: "\x1c" }
  - { key: RBracket, mods: Super, chars: "\x1d" }
  - { key: Grave, mods: Super, chars: "\x1e" }
  - { key: Underline, mods: Super, chars: "\x1f" }
}

This configuration remaps ◊fnref["ascii"]{all possible modifier keybindings available in a terminal environment}.

◊fndef["ascii"]{See this ◊link["https://www.physics.udel.edu/~watson/scen103/ascii.html"]{◊sc{ascii} table}.}

◊section{Conclusion}

At this point you should be able to copy & paste with Command+C/V in every application and terminal, and use Caps lock as Control in Emacs and terminal, ◊em{as it should be}.