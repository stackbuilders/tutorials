import * as rts from "./rts.mjs";
import cabal_cfw_example from "./cabal-cfw-example.req.mjs";

async function handleRequest(req) {
  const i = await rts.newAsteriusInstance(Object.assign(cabal_cfw_example, { module: WASM }));
  const body = await req.text();
  return await i.exports.handleReq(req.method, body);
}

addEventListener("fetch", event => {
  event.respondWith(handleRequest(event.request))
});
