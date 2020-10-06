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
