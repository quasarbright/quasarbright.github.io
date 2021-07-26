I recently made a web-app on aws full-stack. Here is what I wish I knew before starting, or what I'd do differently next time I make one

# my app

I made a web-app using aws lambda (python), api gateway, dynamodb, s3, elasticsearch, python for the backend. Made a react frontend (ts) deployed with amplify

# use flask (or a different language)

I had a single lambda for the whole api and did routing internally. I made my own flask-like routing decorator with a path-argument system. Flask has that and much more.
Should've used it from the beginning, too late to migrate to it. 

# mock aws in backend tests

with libraries like python moto, you can mock out aws. Never found a good elasticsearch mock

pretty much essential. unit tests are super slow otherwise, and you have to worry about cleaning up after yourself between tests (wipe db between tests)

# design dynamodb for scale from the beginning

make sure dynamodb does all the work for you, you have a uniform way of making ddb do joins and sorts for you with GSIs, and dynamodb can do pagination for you. You shouldn't end
up doing any sorting/filtering/joining in the backend.

# use cdk (for both ends)

it's good. make amplify in cdk too

# pass api url as environment variable from cdk

frontend had hardcoded api url. should've been an environment variable passed in cdk via amplify

# use a paginator abstraction (both ends)

for paginated functions, return a `Paginator`, which wraps a function that takes in a start key and pagination config (page size, etc.) and returns a page, containing items
and a last key

make it iterable (probably with generators) by just fetching the next page until no last key and yielding all results

make a mapped paginator that takes in a paginator and applies the function to all items of every page (just wrap the query function contained in the inner paginator).
mapped of a mapped should just compose functions (fusion)

same thing in frontend

# abstract away your hash prefix scheme

Make a small library for creating primary keys for items by passing in arguments, preferably such that you get static checking on args you pass in. probably want to define the
schema somewhere as data and make code generators for documentation and those pk generator functions

add support for partial prefix on secondary key too. shouldn't need that if the sk is just for uniqueness though

# use GSIs (ddb)

with a gsi, as long as every element has a unique pk,sk for the table's key, gsi doesn't care if gsi keys are duplicated/missing. Opens up a nice design for join-sort ops

define a gsi for every sort-able field. gsi pk is table pk, gsi sk is sort-able field. There will be duplicate gsi keys, but that's actually ok

so you have actual items you care about like users, content items, etc. and relationship items representing relationships between them, all in the same table.

for every join-able relationship, define an entity type where the pk specifies the parent and the query. Only need an sk for uniqueness, since a parent (single pk) has many 
children, which would cause duplication if you didn't have an sk. If you need reversible/bi-directional relationships, make the sk have the same properties as the pk
and add an inverse gsi (sk is pk, pk is sk).

Relation items have child_pk, child_sk, parent_pk, parent_sk fields for uniform shape. Also have sort-able fields if applicable.

to do a join-sort, query on the sort-field's gsi with parent pk. This will return relation items. map child ids to their index (need to remember order). batch get child keys.
this will be un-sorted. use the id->index mapping from before as a key function and sort. Done

This should all be paginated, so this operation should be in a `Paginator`'s `query_fn` and pass in pagination config to the dynamodb query call. So this algorithm will only
run on a single page of data at a time. Good since batch get item can only handle 100 keys anyway

# statically defined data types

In my backend, I ended up working with dicts all the time. Would be better to have been working with actual objects where fields are statically known.

I prefer functional/module organization rather than class-based organization, but some kind of statically verified mechanism of constructing and accessing these items would
have been nice, and classes are the only way to do something close to that in python. Typescript has record types, which solves the problem. hell, maybe use purescript!

# consider purescript/typescript/haskell backend

good record-types, haskell-like, might be nicer than python. small ecosystem though. maybe typescript would be a good middle ground. haskell might be good too, can use
aeson to get around the lack of record types :(

whatever you use, make sure it has good aws mocks

# utc unix timestamps

don't use a stringy datetime. use the number of milliseconds since jan 1 1970 utc or something like that (int). much easier to work with

# make a swagger api spec first, then use code-gen, especially for clients

documented api and actual api were never in sync, never had accurate api docs, initially forgot some lambda endpoints

ended up making an api client for frontend in ts and one for integration testing in python. big pain. 

swagger fixes these problems

# react-query

use react-query from the beginning

make an api client which uses promises indepent of any hooks or anything. Then, make an api hook module which wraps that api client with react-query hooks. 

# react functional components and hooks

class components are gross, hooks are amazing. make custom hooks for things since hooks compose well

# use something like material ui right away

migrating to a component lib is annoying, get on one asap

# mock backend right away

make an mock api client in the frontend to make testing easier. Don't bother trying to make pure render components vs stateful hooky components, it's probably not going to scale.
Mocking hooks ad-hoc probably won't scale unless you figure out a totally different way to structure a frontend such that you can. I couldn't easily on my app. mocking out
backend isn't bad, just make the ops that you need and implement the bare minimum and disregard efficiency

# frontend tests shouldn't talk to backend

much faster if you mock everything and just focus on rendering logic. don't want to be waiting for web requests for frontend tests

# figure out auth right away

I never did. It'd require changes across the stack though. Should figure it out and have it in place asap while the app is small
