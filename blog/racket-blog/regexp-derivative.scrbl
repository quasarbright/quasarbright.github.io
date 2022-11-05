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
    [`(* ,a) 'empty]
    [`(,(or 'and 'seq) ,a ,b) (if (equal? (v a) 'null) 'null (v b))]
    [`(or ,a ,b) (if (equal? (v a) 'empty) 'empty (v b))]
    [`(not ,a) (if (equal? (v a) 'null) 'empty 'null)]))
(v 'empty)
(v 'null)
(v #\a)
(v '(and empty #\a))
(v '(or empty #\a))
(v '(* #\a))
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
(d/dc '(or (seq #\a #\b) (seq #\a #\c)) #\a)
(d/dc '(or (seq #\a #\b) (seq #\a #\c)) #\z)
(d/dc '(and (seq #\a #\b) (seq #\a #\c)) #\a)
(d/dc '(and (seq #\a #\b) (seq #\a #\c)) #\z)
(d/dc '(* #\a) #\a)
(d/dc '(* (seq #\a #\b)) #\a)
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

The other cases are just straightforward recursive cases, threading the derivative through the sub-expressions.

The  first two examples show the simple character cases. The following examples seem complicated, but if we use the properties of regular expressions, we can simplify the results.
For example, @racket[(d/dc `(seq #\a #\b) #\a)] evaluates to @racket['(or (seq empty #\b) (seq null null))], which is equivalent to @racket[#\b]. Try to simplify the other outputs and make sure the results are what you expect.

@subsection{Detour: A Simplifier}

If you simplified those by hand, you'd probably agree that this is a computer's job. So let's make a function to do it for us!
This will make it easier to analyze our derivatives and it's a fun exercise.

We'll start by writing the rewrite rules:

@examples[
  #:eval eval
  #:label #f
(define (simplify-step re)
  (match re
    [(or 'empty 'null (? char?)) re]
    [`(* null) 'empty]
    [`(* empty) 'empty]
    [`(seq null ,b) 'null]
    [`(seq ,a null) 'null]
    [`(seq empty ,b) b]
    [`(seq ,a empty) a]
    [`(and null ,b) 'null]
    [`(and ,a null) 'null]
    [`(and ,(? char? a) ,(? char? b)) (if (eq? a b) a 'null)]
    [`(and ,a ,a) a]
    [`(or null ,b) b]
    [`(or ,a null) a]
    [`(or ,a ,a) a]
    [`(not (not ,a)) a]
    [_ re]))
]

These are some simple rewrite rules for simplifying regular expressions. For example, the rule @racket[`(and null ,b) ~> 'null] makes sense because a string has to match both @racket['null] and some regular expression @racket[b]
to match @racket[`(and null ,b)], but it won't match @racket['null], so the whole thing is equivalent to @racket['null]. Make sure all of the rules make sense to you.

Now we'll implement the recursive part:

@examples[
  #:eval eval
  #:label #f
(define (simplify-step* re)
  (let ([re^ (simplify-step re)])
    (if (equal? re re^)
        re^
        (simplify-step* re^))))
(define (simplify re)
  (match re
    [(or 'empty 'null (? char?)) re]
    [`(* ,a) (simplify-step* `(* ,(simplify a)))]
    [`(seq ,a ,b) (simplify-step* `(seq ,(simplify a) ,(simplify b)))]
    [`(and ,a ,b) (simplify-step* `(and ,(simplify a) ,(simplify b)))]
    [`(or ,a ,b) (simplify-step* `(or ,(simplify a) ,(simplify b)))]
    [`(not ,a) (simplify-step* `(not ,(simplify a)))]))
]

@racket[simplify-step*] repeatedly applies @racket[simplify-step] until the result is unchanged.
This is guaranteed to terminate because our rewrite rules either produce a sub-expression, an atomic expression,
or the unchanged input expression. Atomic expressions are returned unchanged, so eventually, we'll either produce
an expression with no rewrite rule (gets returned unchanged) or an atomic and then terminate.

@racket[simplify] simplifies the expression bottom-up, simplifying sub-expressions first and then repeatedly
applying our rewrite rules on the input expression with simplified children.

This is by no means an exhaustive, rigorous simplifier, but it gets the job done for our purposes.

Finally, we can simplify those derivatives:

@examples[
  #:eval eval
  #:label #f
(simplify (d/dc #\a #\a))
(simplify (d/dc #\a #\b))
(simplify (d/dc `(seq #\a #\b) #\a))
(simplify (d/dc `(seq #\a #\b) #\z))
(simplify (d/dc '(or (seq #\a #\b) (seq #\a #\c)) #\a))
(simplify (d/dc '(or (seq #\a #\b) (seq #\a #\c)) #\z))
(simplify (d/dc '(and (seq #\a #\b) (seq #\a #\c)) #\a))
(simplify (d/dc '(and (seq #\a #\b) (seq #\a #\c)) #\z))
(simplify (d/dc '(* #\a) #\a))
(simplify (d/dc '(* (seq #\a #\b)) #\a))
]

Beautiful!

@section{Putting it All Together}

Now that we can differentiate a regular expression with respect to a character, we can differentiate with respect to a string by
sequencing character derivatives.

@examples[
  #:eval eval
  #:label #f
(define (d/ds re s)
  (for/fold ([re re])
            ([c (in-string s)])
    (d/dc re c)))
(simplify (d/ds '(seq #\a #\b) "ab"))
(simplify (d/ds '(seq (seq #\a #\b) #\c) "ab"))
(simplify (d/ds '(* #\a) ""))
(simplify (d/ds '(* #\a) "a"))
(simplify (d/ds '(* #\a) "aaaaa"))
(simplify (d/ds '(* (seq #\a #\b)) ""))
(simplify (d/ds '(* (seq #\a #\b)) "a"))
(simplify (d/ds '(* (seq #\a #\b)) "ab"))
(simplify (d/ds '(* (seq #\a #\b)) "ababababa"))
]

With this, we can finally determine if a string matches a regular expression:

@examples[
  #:eval eval
  #:label #f
(define (our-regexp-match re s)
  (match (v (d/ds re s))
    ['empty #t]
    ['null #f]))
(our-regexp-match #\a "a")
(our-regexp-match #\a "b")
(our-regexp-match '(seq #\a #\b) "ab")
(our-regexp-match '(seq #\a #\b) "az")
(our-regexp-match '(seq #\a #\b) "abc")
(our-regexp-match '(or (seq #\a #\b) (seq #\a #\c)) "ab")
(our-regexp-match '(or (seq #\a #\b) (seq #\a #\c)) "ac")
(our-regexp-match '(or (seq #\a #\b) (seq #\a #\c)) "az")
(our-regexp-match '(* #\a) "")
(our-regexp-match '(* #\a) "a")
(our-regexp-match '(* #\a) "aaaaaaaaaa")
(our-regexp-match '(* (seq #\a #\b)) "")
(our-regexp-match '(* (seq #\a #\b)) "a")
(our-regexp-match '(* (seq #\a #\b)) "ab")
(our-regexp-match '(* (seq #\a #\b)) "aba")
(our-regexp-match '(* (seq #\a #\b)) "abab")
]

This method is cool and simple, but does not generalize well to features like capture groups. Regardless,
I think it is a very interesting idea and a fun exercise.
