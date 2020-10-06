ahc-link \
  --input-hs fact.hs \
  --no-main \
  --export-function=fact \
  --input-mjs fact_cfw.mjs \
  --bundle \
  --browser \
  --output-dir=worker
