cd worker && \
curl -X PUT "https://api.cloudflare.com/client/v4/accounts/$CF_ACCOUNT_ID/workers/scripts/$SCRIPT_NAME" \
     -H  "Authorization: Bearer $CF_API_TOKEN" \
     -F "metadata=@metadata.json;type=application/json" \
     -F "script=@cabal-cfw-example.js;type=application/javascript" \
     -F "wasm=@cabal-cfw-example.wasm;type=application/wasm"
