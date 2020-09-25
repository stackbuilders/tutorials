---
title: Let's build a Cloudflare Worker with WebAssembly and Haskell
published: 2020-09-25
ghc: 8.8
tags: haskell, webassembly, wasm, cloudflare, cloudflare workers, cfw
libraries: asterius
language: haskell
author-name: Cristhian Motoche
github-profile: CristhianMotoche
description: Let's combine the power of Haskell and WebAssembly in a Cloudflare Worker!
---

At [Stack Builders][sb], we believe that Haskell’s system of expressive static types offers many benefits to the software industry and the world-wide community that depends on our services. In order to fully realize these benefits, it is necessary to have proper training and access to an ecosystem that allows for reliable deployment of services. In exploring the tools that help us run our systems based on Haskell, our developer Cristhian Motoche has created a tutorial that shows how to compile Haskell to WebAssembly using Asterius for deployment on Cloudflare.

## What is a Cloudflare Worker

[Cloudflare Workers][cfw] (CFW) is a Function as a Service (FaaS) platform that allows us to run our code on the edge of the Cloudflare infrastructure. It's built on Google V8, so it’s possible to write functionalities in JavaScript or any other language that targets WebAssembly.

[WebAssembly][wasm] is a portable binary instruction format that can be executed fast in a memory-safe sandboxed environment. For this reason, it’s especially useful for tasks that need to perform resource-demanding and self-contained operations.

## Why use Haskell to target WebAssembly?

Haskell is one of the few pure functional [languages][wasm-languages] that can target WebAssembly. As such, It helps developers break down complex tasks into small functions that can later be composed to do complex tasks. Additionally, it’s statically typed and has type inference, so it will complain if there are type errors at compile time. Because of that and [much more][haskell-matters], Haskell is a good source language for targeting WebAssembly.

## From Haskell to WebAssembly

We’ll use [Asterius][asterius-site] to target WebAssembly from Haskell. It’s a well documented tool that is updated often and supports a lot of Haskell features.

First, as suggested in the [documentation][asterius-docker-images], we’ll use `podman` to pull the Asterius prebuilt container from Docker hub. In this tutorial, we will use Asterius version [`200617`][asterius-200617], which works with GHC 8.8.

```
podman run -it --rm -v $(pwd):/workspace -w /workspace terrorjack/asterius:200617
```

Now we’ll create a Haskell module called `fact.hs` file that will export a pure function:


```haskell
module Factorial (fact) where

fact :: Int -> Int
fact n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n - 1) (n*acc)

foreign export javascript "fact" fact :: Int -> Int
```

In this module, we define a pure function called `fact`, optimized with tail recursion and exported using the [Asterius JavaScript FFI][asterius-jffi], so that it can be called when a WebAssembly module is instantiated in JavaScript.

Next, we’ll create a JavaScript file called `fact_node.mjs` that contains the following code:

```javascript
import * as rts from "./rts.mjs";
import module from "./fact.wasm.mjs";
import req from "./fact.req.mjs";

async function handleModule(m) {
  const i = await rts.newAsteriusInstance(Object.assign(req, {module: m}));
  const result = await i.exports.fact(5);
  console.log(result);
}

module.then(handleModule);
```
This code imports `rts.mjs` (common runtime), WebAssembly loaders, and the required parameters for the Asterius instance. It creates a new Asterius instance, it calls the exported function `fact` with the input `5`, and prints out the result.

You probably have noted that `fact` is done __asynchronously__. This happens with any exported function by Asterius, even if it’s a pure function.

Next, we’ll compile this code using the Asterius command line interface (CLI) `ahc-link` and we’ll run the JavaScript code in Node:

```sh
ahc-link \
  --input-hs fact.hs \
  --no-main \
  --export-function=fact \
  --run \
  --input-mjs fact_node.mjs \
  --output-dir=node
```

This command takes `fact.hs` as a Haskell input file, specifies that no `main` function is exported, and exports the `fact` function. Additionally, it takes `fact_node.mjs` as the entry JavaScript file that replaces the generated file by default, and it places the generated code in a directory called `node`.

Running the `ahc-link` command from above will print the following output in the console:

```
[INFO] Compiling fact.hs to WebAssembly
...
[INFO] Running node/fact.mjs
120
```

As you can see, the result is executed in `node` and it prints out the result of `fact` in the console.

## Push your code to Cloudflare Workers

Now we’ll set everything up for deploying our code to Cloudflare workers.


First, let’s add a `metadata.json` file with the following content:

```json
{
  "body_part": "script",
  "bindings": [
    {
      "type": "wasm_module",
      "name": "WASM",
      "part": "wasm"
    }
  ]
}
```
This file is needed to specify the `wasm_module` binding. The `name` value corresponds to the global variable to access the WebAssembly module from your Worker code. In our example, it’s going to have the name `WASM`.

Our next step is to define the main point of the CFW script.

```javascript
import * as rts from "./rts.mjs";
import fact from "./fact.req.mjs";

async function handleFact(param) {
  const i = await rts.newAsteriusInstance(Object.assign(fact, { module: WASM }));
  return await i.exports.fact(param);
}

async function handleRequest(req) {
  if (req.method == "POST") {
    const data = await req.formData();
    const param = parseInt(data.get("param"));
    if (param) {
      const resp = await handleFact(param);
      return new Response(resp, {status: 200});
    } else {
      return new Response(
        "Expecting 'param' in request to be an integer",
        {status: 400},
      );
    }
  }
  return new Response("Method not allowed", {status: 405});
}

addEventListener("fetch", event => {
  event.respondWith(handleRequest(event.request))
})
```

There are a few interesting things to point out in this code:

1. We import `rts.mjs` and `fact.req.mjs` to load the exported functions from our WebAssembly module.
2. `handleFact` is an asynchronous function that creates an instance of Asterius with the global `WASM` module, as a CFW global variable, and calls the exported function `fact` with some input.
3. `handleRequest` handles the request of the CFW. It expects a `POST` request, with a parameter called `param` in the request body. If `param` is a number, it calls `handleFact` to respond with the result of `fact`.
4. Using the Service Workers API, we listen to the `fetch` event that will respond with the result of `handleRequest`.

We need to build and bundle our code in a single JavaScript file, because CFW accepts only one script per worker. Fortunately, Asterius comes with Parcel.js, which will bundle all the necessary code in a single JavaScript file.

```sh
ahc-link \
  --input-hs fact.hs \
  --no-main \
  --export-function=fact \
  --input-mjs fact_cfw.mjs \
  --bundle \
  --browser \
  --output-dir worker
```

`ahc-link` will generate some files inside a  directory called `worker`. For our CFW, we’re only interested in the JavaScript file (`fact.js`) and the WebAssembly module (`fact.wasm`). Now, it’s time to submit both of them to CFW. We can do this with the provided REST API.

Make sure you have an account id (`$CF_ACCOUNT_ID`), a name for your script (`$SCRIPT_NAME`), and an API Token (`$CF_API_TOKEN`):

```sh
cd worker
curl -X PUT "https://api.cloudflare.com/client/v4/accounts/$CF_ACCOUNT_ID/workers/scripts/$SCRIPT_NAME" \
     -H  "Authorization: Bearer $CF_API_TOKEN" \
     -F "metadata=@metadata.json;type=application/json" \
     -F "script=@fact.js;type=application/javascript" \
     -F "wasm=@fact.wasm;type=application/wasm"
```
Now, visit CFW, where you can use the editor to view, edit, and test the script. Also, you can enable it to be on a `workers.dev` subdomain (`$CFW_SUBDOMAIN`); in that case, you could then simply:

```sh
curl -X POST $CFW_SUBDOMAIN \
       -H 'Content-Type: application/x-www-form-urlencoded' \
       --data 'param=5'
```

## Beyond a simple Haskell file

So far, we’ve created a WebAssembly module that exports a pure Haskell function we ran in CFW. However, we can also create and build a Cabal project using Asterius `ahc-cabal` CLI, and then use `ahc-dist` to compile it to WebAssembly.

First, let’s create the project:

```sh
ahc-cabal init -m -p cabal-cfw-example
```

Then, let’s add some dependencies to our cabal project. The cabal file will look like this:

```sh
cabal-version:       2.4
name:                cabal-cfw-example
version:             0.1.0.0
license:             NONE

executable cabal-cfw-example
  ghc-options: -optl--export-function=handleReq
  main-is:             Main.hs
  build-depends:
    base,
    bytestring,
    aeson >=1.5 && < 1.6,
    text
  default-language:    Haskell2010
```

It’s a simple cabal file, except for the `-optl--export-function=handleReq` ghc flag. This is [necessary][asterius-gh-issue-362] when exporting a function from a cabal project.

In this example, we’ll define a simple `User` record, and we’ll define its instance automatically using Template Haskell!

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Asterius.Types
import           Control.Monad
import           Data.Aeson                 hiding (Object)
import qualified Data.Aeson                 as A
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Text


main :: IO ()
main = putStrLn "CFW Cabal"

data User =
  User
    { name :: Text
    , age  :: Int
    }

$(deriveJSON defaultOptions 'User)
```

**NOTE:** It’s not necessary to create a Cabal project for this example, because the prebuilt container comes with a lot of [prebuilt packages][asterius-gh-issue-354] (aesona included). Nevertheless, it will help us show the potential of `ahc-cabal` and `ahc-dist`.

Next, we’ll define `handleReq,` which we’ll export using JavaScript FFI just like we did before.

```haskell
handleReq :: JSString -> JSString -> IO JSObject
handleReq method rawBody =
  case fromJSString method of
    "POST" ->
      let eitherUser :: Either String User
          eitherUser = eitherDecode (B8.pack $ fromJSString rawBody)
       in case eitherUser of
            Right _  -> js_new_response (toJSString "Success!") 200
            Left err -> js_new_response (toJSString err) 400
    _ -> js_new_response (toJSString "Not a valid method") 405

foreign export javascript "handleReq" handleReq :: JSString -> JSString -> IO JSObject

foreign import javascript "new Response($1, {\"status\": $2})"
  js_new_response :: JSString -> Int -> IO JSObject
```

This time, we define `js_new_response`, a Haskell function that creates a JavaScript object, to create a `Response`. `handleReq` takes two string parameters from JavaScript and it uses them to prepare a response.

Now let’s build and install the binary in the current directory:

```
ahc-cabal new-install --installdir . --overwrite-policy=always
```

This will generate a binary for our executable, called `cabal-cfw-example`. We’re going to use `ahc-dist` to take that binary and target WebAssembly:

```
ahc-dist --input-exe cabal-cfw-example --export-function=handleReq --no-main --input-mjs cabal_cfw_example.mjs --bundle --browser
```

`cabal_cfw_example.mjs` contains the following code:

```javascript
import * as rts from "./rts.mjs";
import cabal_cfw_example from "./cabal_cfw_example.req.mjs";

async function handleRequest(req) {
  const i = await rts.newAsteriusInstance(Object.assign(cabal_cfw_example, { module: WASM }));
  const body = await req.text();
  return await i.exports.handleReq(req.method, body);
}

addEventListener("fetch", event => {
  event.respondWith(handleRequest(event.request))
});
```

Finally, we can deploy our code to CFW by defining a `metadata.json` file and uploading the script and the WebAssembly module using CFW API as we did before.

### Caveats

CFW [limits][cfw-limits] your JavaScript and WebAssembly in file size. Therefore, you need to be careful with any dependencies you add.

## Conclusion

Stack Builders builds better software for better living through technologies like expressive static types. We used Asterius to compile Haskell to WebAssembly and deployed it to Cloudflare Worker using the Workers API. Asterius supports a lot of Haskell features (e.g. Template Haskell) and it provides an easy-to-use JavaScript FFI to interact with JavaScript. Additionally, it provides prebuilt containers that contain a lot of Haskell packages, so you can start writing a script right away.

Following this approach, we can write functional type-safe code in Haskell, target it to WebAssembly, and publish it to CFW, which runs on the edge of the Cloudflare infrastructure.

For more content check our [blogs][sb-blogs] and [tutorials][sb-tutorials]!

[sb]: https://www.stackbuilders.com/
[sb-blogs]: https://www.stackbuilders.com/news/page/1
[sb-tutorials]: https://www.stackbuilders.com/tutorials/
[cfw]: https://developers.cloudflare.com/workers/
[cfw-limits]: https://developers.cloudflare.com/workers/about/limits/#script-size
[wasm]: https://webassembly.org/
[wasm-languages]: https://github.com/appcypher/awesome-wasm-langs
[asterius-site]: https://asterius.netlify.app/
[asterius-docker-images]: https://asterius.netlify.app/images.html#using-prebuilt-container-images
[asterius-200617]: https://hub.docker.com/layers/terrorjack/asterius/200617/images/sha256-8e6009198c3940d1b4938b0ab6c0b119d4fce3d7ac0f6336214e758f66887b80?context=explore
[asterius-jffi]: https://asterius.netlify.app/jsffi.html
[asterius-gh-issue-362]:https://github.com/tweag/asterius/issues/362#issuecomment-561576162
[asterius-gh-issue-354]: https://github.com/tweag/asterius/issues/354
[haskell-matters]: https://wiki.haskell.org/Why_Haskell_matters
