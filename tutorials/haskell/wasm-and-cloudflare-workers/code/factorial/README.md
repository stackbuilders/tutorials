# Simple Haskell script

## Start

Pull the `200617` version of `asterius` from Docker hub:

```
$ podman run -it --rm -v $(pwd):/workspace -w /workspace terrorjack/asterius:200617
```

## Build and run in Node

Run the command:

```
(asterius)# ahc-link \
  --input-hs fact.hs \
  --no-main \
  --export-function=fact \
  --run \
  --input-mjs fact_node.mjs \
  --output-dir=node
```

or the script:

```
(asterius)# sh run_node.sh
```

## Build and bundle for CFW

Run the command:

```
(asterius)# ahc-link \
  --input-hs fact.hs \
  --no-main \
  --export-function=fact \
  --input-mjs fact_cfw.mjs \
  --bundle \
  --browser \
  --output-dir=worker
```

or the script:

```
(asterius)# sh build.sh
```

## Upload to Cloudflare Workers

Run the commands:

```
cd worker
curl -X PUT "https://api.cloudflare.com/client/v4/accounts/$CF_ACCOUNT_ID/workers/scripts/$SCRIPT_NAME" \
     -H  "Authorization: Bearer $CF_API_TOKEN" \
     -F "metadata=@metadata.json;type=application/json" \
     -F "script=@fact.js;type=application/javascript" \
     -F "wasm=@fact.wasm;type=application/wasm"
```

or, copy the `.env.sample` to `.env`, set the variables and run the script with `dotenv`:

```
$ dotenv 'sh upload.sh'
```

Now, visit your Cloudflare account, select CFW and test your worker. Make sure
to set the `Content-type` header as `application/x-www-form-urlencoded`
when doing a `POST` request.
