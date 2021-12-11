#lang pollen

◊define-meta[date]{<2021-12-11 Sat 14:36>}
◊define-meta[uuid]{b973fd64-5ad2-11ec-87a6-b729d55f528f}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{A prelude for writing Emacs dynamic modules}
}

When writing ◊fnref["xeft"]{an Emacs dynamic module for Xapian}, I found that calling Lisp functions in a dynamic module is painfully tedious. For example, the equivalent of

◊bcode{
  (define-error 'xeft-error "Generic Xeft error" 'error)
}

is

◊bcode{
emacs_value Qdefine_error = env->intern (env, "define-error");
emacs_value Qxeft_error = env->intern (env, "xeft-error");
emacs_value Qerror = env->intern (env, "error");
char **text = "Generic Xeft error";
emacs_value message = env->make_string (env, text , strlen (text));
emacs_value args[] = {Qxeft_error, message, Qerror};
int nargs = 3;
env->funcall (env, Qdefine_error, nargs, args);
}

Even though we usually only write a little Lisp for  defining the exposed functions and errors in a dynamic module, this is too much. Naturally I wrote some wrappers. With my wrappers, I can write the following instead:

◊bcode{
emp_funcall (env, "define-error", 3,
             emp_intern (env, "xeft-error"),
             emp_build_string (env, "Generic Xeft error"),
             emp_intern (env, "error"));  
}

I put these wrappers together into ◊code{emacs-module-prelude}. Currently it provides these functions:

◊ul{
  ◊li{◊code{emp_define_function}}
  ◊li{◊code{emp_funcall}}
  ◊li{◊code{emp_intern}}
  ◊li{◊code{emp_provide}}
  ◊li{◊code{emp_signal_message1}}
  ◊li{◊code{emp_define_error}}
  ◊li{◊code{emp_nilp}}
  ◊li{◊code{emp_copy_string_contents}}
  ◊li{◊code{emp_build_string}}
}

You can find it at ◊link["https://github.com/casouri/emacs-module-prelude"]{◊em{emacs-module-prelude}}. I can’t say that I’m a seasoned C programmer so use at your own risk. Corrections are very welcome.

◊fndef["xeft"]{For my note-searching package: ◊link["https://github.com/casouri/xeft"]{Xeft}.}
