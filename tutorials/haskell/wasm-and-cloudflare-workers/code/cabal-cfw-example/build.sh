ahc-cabal new-install \
  --installdir . \
  --overwrite-policy=always && \
ahc-dist \
  --input-exe cabal-cfw-example \
  --export-function=handleReq \
  --no-main \
  --input-mjs cabal_cfw_example.mjs \
  --bundle \
  --browser \
  --output-dir worker
