#lang pollen

◊define-meta[date]{<2025-08-19 Tue>}
◊define-meta[uuid]{8dc77daa-7d31-11f0-bd5b-4b4e67100343}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{A Baseline Indentation Algorithm for C-like Languages Utilizing a Parse-tree}
  ◊subtitle{Emacs Technology and Innovations, issue 108}
}

A few month ago when working on tree-sitter indentation issues, I discovered a baseline indent rule for tree-sitter major modes in Emacs that covers 90% of the cases for C-like languages. So now we can replace lines and lines of indentation rule with just ◊code{c-ts-common-baseline-indent-rule}, and add a few override rules for the final adjustment. The best part is that it’s language-agnostic, so major mode doesn’t need to teach it what’s the node name for if-statement, function definition, etc. It just works.

I’ve used it in ◊code{c-ts-mode}, and John Muhl reported success using it in ◊code{lua-ts-mode}. Non-builtin major modes will be able to utilize it when Emacs 31 releases.

Now, let’s go over the cases it covers and how it works.

◊section{The rule}

First let’s define some basic concepts. When indenting a line using the parse tree, ◊code{treesit-indent} always finds the largest node that starts on that line, let’s call it ◊em{node}. For example, in the following code, ◊em{node} is the return statement ◊code{return 2 + 3;} even though technically the ◊code{return} keyword also starts on the beginning of the line.

◊bcode{
  int main() {
    int a = 0;
    return 2 + 3;
  }
}

Given a ◊em{node}, the ◊em{prev-node} is the largest node that starts on the previous line. In our example, ◊em{prev-node} would be the ◊code{int a = 0;} statement.

Finally, a node is ◊em{standalone} if it’s on its own line, ie, there’s no other node in front of it on it’s starting line. For example:

◊bcode{
  int main() {
    // Here, the block node {...} is standalone
    if (true)
    {
      ...
    }
    // But here, the block node {...} is not,
    // because "if" is in front of it.
    if (true) {
      ...
    }
  }
}

Now let’s go over the rules.

◊subsection{Rule 0}

A closing brace ◊code{◊cbk{}} aligns with the first parent that’s standalone. This handles both styles of opening brace placement.

◊bcode{
  int main() { <-- Aligns to the whole function definition
    ...
  } <--- This closing brace


  int main()
  { <--- Aligns to the block node {...}
    
  } <--- This closing brace


  int main() {
    if (true) { <--- Aligns to the whole if-statement
      ...
    } <--- This closing brace
  }
}

◊subsection{Rule 1}

If ◊em{node} and ◊em{prev-node} are siblings, align this line to previous line. This just handles the basic statement alignment.

◊bcode{
  int main() {
    int a = 0; <--- Aligns to this statement
    return 2 + 3; <--- This statement
  }
}

Once nice perk of this rule is that it allows user to override indentation of a statement:

◊bcode{
  int main() {
    ...
    ...
        int a = 0; <--- For whatever reason I want to align it here.
        int b = 1; <--- The following line will follow it.
    double c = 100; <--- And I can move the next line back.
    double d = 200; <--- The following lines will follow it.
  }
}

It’s kind of an outdated practice nowadays, but still nice to have.

◊subsection{Rule 2}

If the parent of ◊em{node} is a list, ie, ◊code{(...)} or ◊code{[...]} or ◊code{◊obk{}...◊cbk{}}, we have two options.

The ◊em{align} style:

◊bcode{
  function main() {
    const a = [
               1, 2, 3,
               4, 5, 6,
              ];
  }
}

The ◊em{simple} style:

◊bcode{
  function main() {
    const a = [
      1, 2, 3,
      4, 5, 6,
    ];
  }
}

Usually in a given language, one is preferred over the other. So in ◊code{c-ts-common.el} I provided both, and a variable decides which style to use (◊code{c-ts-common-list-indent-style}).

For the actual algorithm, we need to handle the first sibling and the rest siblings differently. For the first sibling, either indent to opening bracket/paren + 1 for ◊em{align} style, or align to standalone parent + one indent level for ◊em{simple} style.

For non-first siblings, align to the previous ◊em{standalone} sibling. Without the standalone condition, our indentation would become this:

◊bcode{
function main() {
    const a = [
        1, 2, 3,
              4, 5, 6,
    ];
}
}

This rule also handles another very common case (yes, code blocks are also lists):

◊bcode{
function main() {
    const a = 100 + 200 <--- This is the standalone sibling
        + 300 + 400 + 500;
    const b = 600; <--- This line should align to "const a",
                        not "+"
}
}

For closing bracket/paren, either align to opening bracket for ◊em{align} style, or align to standalone parent for ◊em{simple} style.

Finally, when we say “first sibling”, we actually mean “first non-comment sibling”, because we don’t want this kind of indentation to happen:

◊bcode{
  function main() {
    const a = [    // Some comments.
                   1, 2, 3,
                   4, 5, 6,
              ];
  }
}

◊subsection{Rule 3}

This is the fallback rule: for ◊em{node} that didn’t match rule 0, 1, 2, just find it’s standalone parent and align to the parent + one indent level. In practice, this usually applies for a) the first statement in a block and b) the second line of a continued statement.

◊bcode{
  function main() {
    func <--- Applies here, align to the whole function + one level.
      .method() <--- Applies here, align to the whole statement + one level.
      .method(); <--- Aligns to previous standalone sibling.

    const a = 2 + 24 * 10000
      + func() + func() <--- Applies here.
      + func() + func(); <--- Aligns to previous standalone sibling.

    if (true) {
      ...
    } else if {
      const a = 0; <--- Applies here, it goes all the way up to the whole
    }                   if statement to get the standalone parent.
  }
}

There’s one exception: if ◊em{node} is the child of the root node, it obviously doesn’t indent one more level.

◊section{Why it works}

To me, it’s interesting that a relatively simple rule like this can work across so many language grammars without even needing to ◊fnref["knowledge"]{know anything about the grammar}. I think it’s because people define C-like languages’ grammar in very similar structure. For example, the rule for lists work across grammars because most of the time, a list is defined in a flat structure:

◊bcode{
list = ("[" "1" "," "2" "," "3" "," ... "," "6" "]")
}

rather than a nested structure:

◊bcode{
list = ("[" list-content "]")
list-content = ("1" "," "2" "," "3" "," ... "," "6")
}

And grammars always use the anonymous node ◊code{"["} rather than making it a name one like ◊code{open_bracket} or something.

And for if-statement, it’s always

◊bcode{
if_statement = ("if" condition body1 else_part)
condition = ("(" ... ")")
body = ("◊obk{}" ... "◊cbk{}")
else_part = ("else" else_body)
else_body = can be another if_statement or just a block
...
}

Essentially, we’re exploiting the common structure that people use for C-like grammars; and by finding the standalone parent, we’re using the grammar’s structure itself to indent a node.

◊fndef["knowledge"]{
The baseline rule doesn’t need any language-specific info, except for comments—most languages name the comment node just ◊code{comment}, but not all. I use a regexp that matches the comment node in all languages, it works pretty well in practice:

◊bcode{
(defvar c-ts-common--comment-regexp
  ;; These covers C/C++, Java, JavaScript, TypeScript, Rust, C#.
  (rx (or "comment" "line_comment" "block_comment" "//" "/*"))
  "Regexp pattern that matches a comment in C-like languages.")
}
}

◊section{Some further details}

◊subsection{Method chaining}

I had to add a special case in the condition of standalone so method chaining works. Otherwise, in a chained method, the “◊code{.}” usually prevents the node after it from being recognized as a standalone node.

◊bcode{
func()
.method() <--- I want the method() node to be considered standalone.
.method() <--- So this sibling aligns to the previous sibling.
}

◊subsection{Why not just parent?}

For a lot of the cases, simply finding the parent and indent from it will suffice. But there are cases that it’ll miss. In the example below, the immediate parent of the return statement is the block ◊code{◊obk{} ... ◊cbk{}}.

◊bcode{
int main() {
  if (cond1
      || cond2
      || cond 3) { <--- The immediate parent is the block
    return 0; <--- For this node
  }
}
}

That means if we indent the return statement according to it’s parent, we get this:

◊bcode{
int main() {
  if (cond1  <--- But we actually want to align to this line
      || cond2
      || cond 3) { <--- Align to the beginning of this line
        return 0;
  }
}
}
