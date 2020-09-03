import * as rts from "./rts.mjs";
import module from "./fact.wasm.mjs";
import req from "./fact.req.mjs";

async function handleModule(m) {
  const i = await rts.newAsteriusInstance(Object.assign(req, {module: m}));
  const result = await i.exports.fact(5);
  console.log(result);
}

module.then(handleModule);
