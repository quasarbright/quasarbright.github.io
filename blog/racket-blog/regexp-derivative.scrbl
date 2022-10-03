#lang scribble/manual
@require[scribble/example @for-label[racket]]

@title[#:style 'unnumbered #:tag "regexp-derivative"]{Matching Regular Expressions by Computing Their Derivatives}
@author{Mike Delmonaco}

Regular expressions allow us to describe patterns in text. They are very useful and show up all over the place in programming,
but matching regular expressions can be difficult. One well-known technique for matching regular expressions is converting the regular
expression to a finite state machine. This is pretty elegant, but can get complicated and messy.

An alternative technique, which is the subject of this blog post, involves something called a Brzozowski derivative. This technique can be
used to compute the derivative of a generalized regular expression.

@section{The Derivative of a Regular Expression?}

The derivative of a regular expression @racket[re] with respect to a character (or string) @racket[c] is another regular expression which matches strings that,
when appended to @racket[c], are matches of @racket[re]. Here are a few examples:
@itemlist[
  @item{The derivative of @racket[#rx"abc"] with respect to @racket[#\a] is @racket[#rx"bc"]}
  @item{The derivative of @racket[#rx"(ab)|(xy)"] with respect to @racket[#\a] is @racket[#rx"b"]. The second alternative is eliminated}
  @item{The derivative of @racket[#rx"(abc)*"] with respect to @racket[#\a] is @racket[#rx"bc(abc)*"]}
  @item{The derivative of @racket[#rx"b"] with respect to @racket[#\a] is a regular expression that has no matches ever.}
]

We take the derivative of a regular expression with respect to a string by repeatedly taking the derivative of the regular expression with respect to each character.

That's the idea. If the derivative of the regular expression with respect to the target string is a regular expression which matches the empty string, we have a match.

We will represent regular expressions with racket datums:

@racketgrammar*[
  #:literals (list quote character empty null or and not seq star)
  [rx character (quote empty) (quote null) (list (quote or) rx rx) (list (quote and) rx rx) (list (quote not) rx) (list (quote seq) rx rx) (list (quote *) rx)]
]

@itemlist[
  @item{@racket[character] matches the character @racket[character]}
  @item{@racket[(quote empty)] matches the empty string}
  @item{@racket[(quote null)] doesn't match any strings}
  @item{@racket[(list (quote or) a b)] matches strings that match @racket[a] or @racket[b]}
  @item{@racket[(list (quote and) a b)] matches strings that match @racket[a] and @racket[b]. This is a feature that most regular expression implementations do not support.}
  @item{@racket[(list (quote not) a)] matches strings that do not match @racket[a]. This is a feature that most regular expression implementations do not support.}
  @item{@racket[(list (quote seq) a b)] matches strings that can be partitioned into a substring that matches @racket[a] followed by a substring that matches @racket[b].
        This is the concatenation of two regular expressions.}
  @item{@racket[(list (quote *) a)] matches strings that match zero or more concatenations of @racket[a] with itself}
]

First, we must define a helper function that determines whether a regular expression matches the empty string. If it does, we return @racket[(quote empty)]. Otherwise, we return @racket[(quote null)].

@(define eval (make-base-eval '(require racket/match)))
@examples[
  #:eval eval
  #:label #f
(define (v re)
  (match re
    ['null 'null]
    ['empty 'empty]
    [(? char?) 'null]
    [(list '* _) 'null]
    [(list (or 'and 'seq) a b) (if (equal? (v a) 'null) 'null (v b))]
    [`(or ,a ,b) (if (equal? (v a) 'empty) 'empty (v b))]
    [`(not ,a) (if (equal? (v a) 'null) 'empty 'null)]))
(v 'empty)
(v 'null)
(v #\a)
(v '(and empty #\a))
(v '(or #\a empty))
]

Now, we're ready to implement the derivative:

@examples[
  #:eval eval
  #:label #f
(define (d/dc re c)
  (match re
    [(? char? c^) (if (eq? c c^) 'empty 'null)]
    [(or 'empty 'null) 'null]
    [`(* ,a) `(seq ,(d/dc a c) (* ,a))]
    [`(seq ,a ,b)
     `(or (seq ,(d/dc a c) ,b)
          (seq ,(v a) ,(d/dc b c)))]
    [`(and ,a ,b)
     `(and ,(d/dc a c) ,(d/dc b c))]
    [`(or ,a ,b)
     `(or ,(d/dc a c) ,(d/dc b c))]
    [`(not ,a) `(not ,(d/dc a c))]))
(d/dc #\a #\a)
(d/dc #\a #\b)
(d/dc `(seq #\a #\b) #\a)
(d/dc `(seq #\a #\b) #\z)
(d/dc '(and (seq #\a #\b) (seq #\a #\c)) #\a)
(d/dc '(and (seq #\a #\b) (seq #\a #\c)) #\z)
]

For the character case, we check if the target character matches the regular expression character. If it does, then the only thing that can follow the target character in a string
is the empty string, so that's the derivative. If it doesn't, there are no strings that can follow the character, so we return null. Remember, the result of a derivative is a regular expression
that matches strings that, when appended after the target character, match the original regular expression. If the derivative is @racket[(quote null)], then the match failed because there are
no strings that can follow the character and produce a match. In other words, the target character is not the first character of any matches of the regular expression.

For the @racket[(quote empty)] expression, we return @racket[(quote null)] because the empty expression does not match a character. The empty expression only matches the empty string.

For the @racket[(quote null)] expression, we return @racket[(quote null)] because the null expression does not match any strings.

For star expressions, we concatenate the sub-expression's derivative with the star expression. Concatenating the original star expression gives us the repetition behavior.

For sequences, there are two cases. The intuitive case is the first one, @racket[`(seq ,(d/dc a c) ,b)]. This is the derivative of the first regular expression concatenated with the second expression.
However, if the first regular expression matches the empty string, the derivative of the sequence could also be @racket[(d/dc b c)]. We implement this using our @racket[v] helper and properties of regular expressions.

If @racket[(v a)] returns @racket[(quote empty)], the second case becomes @racket[`(seq empty ,(d/dc b c))], which is equivalent to @racket[(d/dc b c)].
If @racket[(v a)] is @racket['null], we get @racket[`(seq null ,(d/dc b c))], which is equivalent to @racket['null]. So we only ever "try" the derivative of @racket[b]
when @racket[a] matches the empty string.

We want the derivative to match all strings matched by either of these two cases, so we use @racket['or].

The other cases are just simple recursive cases, threading the derivative through the sub-expressions.

The  first two examples show the simple character cases. The following examples seem complicated, but if we use the properties of regular expressions, we can simplify the results.
For example, @racket[(d/dc `(seq #\a #\b) #\a)] evaluates to @racket['(or (seq empty #\b) (seq null null))], which is equivalent to @racket[#\b]. Try to simplify the other outputs and make sure the results are what you expect.
@;TODO string derivative and put it all together in a match predicate
