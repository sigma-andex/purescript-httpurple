# Requests

This section describes how to work with requests bodys. For information about routing see the [routing docs](Routing.md).

## TOC
1. [tl;dr](#tldr)
1. [introduction](#introduction)
1. [json request parsing](#1-json-request-parsing)
1. [data validation](#2-data-validation)
1. [business logic](#3-your-business-logic)
1. [output json](#4-output-json)

## tl;dr 

```purescript
router { route: Home, method: Post, body } = usingCont do
    jsonRequest :: MyRequest <- fromJson Argonaut.jsonDecoder body
    input :: MyValidatedInput <- fromValidated validateMyRequest jsonRequest
    output :: MyOutput <- lift $ doSomethingAndReturnAff input
    ok' jsonHeaders $ toJson Argonaut.jsonEncoder output
```

## Introduction

A typical micro-service scenario looks like this: 
1. You get a json request, so the first thing you need to do is to parse your json request format and return a bad request if it doesn't match your format.
2. Then you do some kind of data/business validation of the input. Is the field `email` really an email? So you want to do this semantic validation and return a bad request (with some error code) in case it is invalid.
3. Now since you have the validated input data, you can do your actual logic and produce some output data.
4. Finally you want to dump this output data as json

HTTPurple 🪁 provides a very simple way of handling this scenario based on continuations. Let's go through each step:

## 1. Json request parsing

HTTPurple 🪁 provides a `fromJson` method that makes json parsing of your body super simple. `fromJson` is library agnostic, so you can use [`argonaut`](https://github.com/purescript-contrib/purescript-argonaut) using the [`argonaut-driver`](https://github.com/sigma-andex/purescript-httpurple-argonaut), use [`yoga-json`](https://github.com/rowtype-yoga/purescript-yoga-json) using the [`yoga-json-driver`](https://github.com/sigma-andex/purescript-httpurple-yoga-json) or write your own json driver.
If you don't know which one to use, I would recommend to go with the `argonaut` driver.

```bash
# for argonaut install the argonaut driver:
spago install httpurple-argonaut

# for yoga-json install the yoga-json driver:
spago install httpurple-yoga-json
```

`fromJson` returns a continuation, so just start your handler implementation with `usingCont` and then use `fromJson` with `Argonaut.jsonDecoder` or `Yoga.jsonDecoder`:

```purescript
router { route: Home, method: Post, body } = usingCont do
    jsonRequest :: MyRequest <- fromJson Argonaut.jsonDecoder body
```
As you can see, really simple. This parses the body into your format and returns a bad request if it fails. If you need to customise the bad request, there is `fromJsonE` which allows you to pass a custom bad request handler.

## 2. Data validation

Next, we will want do validate our json data. Or put in other words, we want to transform the weakly typed json model into a strongly-typed internal data model.

`fromValidated` takes a validation function of the shape `input -> Either error validated`:

```purescript
router { route: Home, method: Post, body } = usingCont do
    jsonRequest :: MyRequest <- fromJson Argonaut.jsonDecoder body
    input :: MyValidatedInput <- fromValidated validateMyRequest jsonRequest
```

If you need a custom bad request handler, you can use `fromValidatedE` which takes an bad request handler as second parameter.

## 3. Your business logic

Now that you got your validated input, you can pass it to your business logic. 
There is one caveat though, we are currently in a continuation, so you will need to `lift` your `Aff` returning function into the continuation monad:

```purescript
router { route: Home, method: Post, body } = usingCont do
    jsonRequest :: MyRequest <- fromJson Argonaut.jsonDecoder body
    input :: MyValidatedInput <- fromValidated validateMyRequest jsonRequest
    output :: MyOutput <- lift $ myAffReturningFunction input
```

## 4. Output json

Finally you can return your json. Use the `toJson` function which takes the json driver and your output your data as json:

```purescript
router { route: Home, method: Post, body } = usingCont do
    jsonRequest :: MyRequest <- fromJson Argonaut.jsonDecoder body
    input :: MyValidatedInput <- fromValidated validateMyRequest jsonRequest
    output :: MyOutput <- lift $ doSomethingAndReturnAff input
    ok' jsonHeaders $ toJson Argonaut.jsonEncoder output
```

If your business logic output is strongly typed, I would write a transformer function to a weakly typed json model instead of writing custom json encoders:

```purescript
    ok' jsonHeaders $ toJson Argonaut.jsonEncoder $ transformToApi output
```
